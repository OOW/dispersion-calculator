library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(shiny)
library(zoo)

# turn off daylight savings time
Sys.setenv(TZ='EST')

# change the default maximum upload file size
options(shiny.maxRequestSize=300*1024^2)

#' Takes the path to a matrix timeseries file in which the matrices 
#' are stacked vertically and returns a list where each item in the list 
#' is the matrix corresponding to that timestep.
#'
#' @param path The filepath to the timeseries file
#' @return a list of the matrices, one for each timestep
stackedTimeseriesToList <- function(path) {
    # read the file as a string
    raw.txt <- readChar(path, file.info(path)$size)
    # define the regular expression pattern that matches those lines that start a new timestep.
    # this pattern matches a line containing a single decimal number with optional white space on either side of it
    timestep.pat <- '(?m)^\\s*\\d+\\.\\d+\\s*$'
    # extract all the timestep lines from the file
    timesteps <- unlist(regmatches(raw.txt, gregexpr(timestep.pat, raw.txt, perl=TRUE)))
    # remove whitespace from the timesteps
    timesteps <- gsub('[[:space:]]', '', timesteps)
    # partition the file at the timestep lines
    txt.sp <- unlist(strsplit(raw.txt, timestep.pat, perl=TRUE))[-1]
    # read each partition into a data.frame and store these data.frames in a list
    lst <- lapply(txt.sp, function(t) read.table(text=t))
    # use the timesteps as the names of the entries in the list 
    setNames(lst, timesteps)
}

#' Takes a list of matrices and returns a 3D array
#'
#' @param lst List of conformant matrices
#' @param exclude.cols Columns to exclude accross all matrices.
#' @return a 3D array
listTo3DArray <- function(lst, exclude.cols=c()) {
    # turn each entry in lst into a matrix and remove the exclude.cols
    matrix.list <- lapply(lst, function(timestep) {
        cols <- timestep[setdiff(names(timestep), exclude.cols)]
        as.matrix(cols)
    })
    # get the dimensions of the matices
    matrix.dim <- dim(matrix.list[[1]])
    # turn the list of matrices into a 3D array.
    array(unlist(matrix.list), dim=c(matrix.dim, length(matrix.list)), dimnames=c('cell', 'axis', 'timestep'))
}

#' Takes a list of data.frames that each have column for x, y, and z axes and measurement for each 
#' x-y-z pair and pivots the data.frames along one of the axes, turning each value for the specified 
#' axis into a column. For example, if the incoming data.frames look like this:
#' 
#' x  y   z    concentration
#' 1  10  100  .5
#' 2  12  120  .75
#'
#' if the axis. value is 'x' and the var is 'concentration', then the resulting data.frames will
#' look like this:
#'
#' y    z     1   2
#' 10   100  .5   NA
#' 12   120  NA   .75
#'
#' @param axis. The axis along which to pivot
#' @param other.axes The names of the other axes that are not being pivoted on
#' @param var The name of the measurement column
#' @param list.raw The list of molten data.frames that have a column for each axis and a measurement column
#' @return A list of pivoted data.frames
pivot.raw <- function(axis., other.axes, var, list.raw) {
    lapply(list.raw, function(timestep) { 
        timestep <- timestep[c(axis., other.axes, var)] #timestep is a data.frame
        cast.form <- paste(paste(other.axes, collapse='+'), axis., sep='~')
        dcast(timestep, as.formula(cast.form), value.var=var)
    })
}

#' Takes the delta data, the coordinates for each grid cell, and the dye data and returns 
#' the concentration-weighted average second moment for each timestep along the specified axis.
#'
#' @param axis. The axis along which the second moment should be calculated
#' @param delta.list.raw A list of delta data.frames in order of timestep. Each of the data.frames 
#'      in this list has 6 columns: x, y, and z grid cell indexes, and the delta x, delta y, and delta z
#'      for each grid cell.
#' @param coor.list.raw A list of the coordinate data.frames in order of timestep. Each of these data.frames
#'      has 6 columns: x, y, and z grid cell indexes, and the x coordinate, y coordinate, and z coordinate
#'      for each grid cell.
#' @param dye.list.raw A list of the dye concentration data.frames in order of timestep. Each 
#'      of these data.frames has 4 columns: x, y, and z grid cell indexes, and the dye concentration at
#'      at each grid cell.
#' @return a vector of the concentration second moments along the specified axis, ordered by timestep
getAxisSecondMoments <- function(axis., delta.list.raw, coor.list.raw, dye.list.raw) {
    #browser()
    # define the names of the axes and identify the other axes according the axis. argument
    axes. <- c('x', 'y', 'z')
    other.axes <- setdiff(axes., axis.)

    # create variables containing the delta and coordinate column names in delta.list.raw and coor.list.raw. 
    # For example, the delta column name for the x-axis is 'dx' and the coordinate column for the x-axis is 'xcoor'.
    delta.var <- paste0('d', axis.)
    coor.var <- paste0(axis., 'coor')

    # pivot each of the lists along the desired axis. Consult the definition of pivot.raw for 
    # a description of what the output looks like.
    dye.list <- pivot.raw(axis., other.axes, 'dye', dye.list.raw)  # "dye" is a constant column name
    coor.list <- pivot.raw(axis., other.axes, coor.var, coor.list.raw)
    delta.list <- pivot.raw(axis., other.axes, delta.var, delta.list.raw)

    # create vectors that hold the delta and coordinate column names for the axes that aren't chosen
    other.delta.vars <- paste0('d', other.axes)
    other.coor.vars <- paste0(other.axes, 'coor')

    # in what follows I use 'z' in my variable names for simplification, but this is just a 
    # generic designation and z, cz, dz, etc. will be x, cx, dx or y, cy, dy depending on the value
    # of axis. argument

    # turn the pivoted list of data.frames into 3D arrays, where the 3rd dimension is timestep
    z <- listTo3DArray(coor.list, exclude.cols=c(other.axes, other.coor.vars))
    cz <- listTo3DArray(dye.list, exclude.cols=c(other.axes))
    dz <- listTo3DArray(delta.list, exclude.cols=c(other.axes, other.delta.vars))

    # replace negative values in the coordinates and deltas with their absolute values
    z <- abs(z)
    dz <- abs(dz)

    # integrate the concentrations along the specified axis
    cz.dz <- cz * dz
    int.cz.dz <- apply(cz.dz, c(1,3), sum, na.rm=TRUE)

    # calculate the first moment
    z.cz.dz <- z * cz.dz
    z2.cz.dz <- z^2 * cz.dz
    int.z.cz.dz <- apply(z.cz.dz, c(1,3), sum, na.rm=TRUE)
    moment1 <- int.z.cz.dz / int.cz.dz

    # calculate the second moment
    int.z2.cz.dz <- apply(z2.cz.dz, c(1,3), sum, na.rm=TRUE)
    moment2 <- int.z2.cz.dz / int.cz.dz - moment1^2

    # for each of the other axes, get the delta data.frame lists, then multiply them
    # together and convert the list of data.frames to a 3D array
    dxdy.lists <- lapply(other.delta.vars, pivot.raw, axis.=axis., other.axes=other.axes, list.raw=delta.list.raw)
    dxdy <- mapply(`*`, dxdy.lists[[1]], dxdy.lists[[2]], SIMPLIFY=FALSE)
    dxdy <- listTo3DArray(delta.list, exclude.cols=other.axes)

    # average along the specified axis the products of the deltas of the other axes 
    # and multiply the averaged products by the integrated concentrations along the specified axis,
    # then sum to get the double integral of the concentrations along the specified axis.
    Cxy.dxdy <- int.cz.dz * apply(dxdy, c(1,3), mean, na.rm=TRUE)
    dint.Cxy.dxdy <- apply(Cxy.dxdy, 2, sum, na.rm=TRUE)

    # calculate the concentration-weighted sum of the second moments
    z2.Cxy.dxdy <- moment2 * Cxy.dxdy
    dint.z2.Cxy.dxdy <- apply(z2.Cxy.dxdy, 2, sum, na.rm=TRUE)

    moment2.mean <- dint.z2.Cxy.dxdy / dint.Cxy.dxdy
    moment2.mean
}

#' @param delta.list.raw A list of delta data.frames in order of timestep. Each of the data.frames 
#'      in this list has 6 columns: x, y, and z grid cell indexes, and the delta x, delta y, and delta z
#'      for each grid cell.
#' @param dye.list.raw A list of the dye concentration data.frames in order of timestep. Each 
#'      of these data.frames has 4 columns: x, y, and z grid cell indexes, and the dye concentration at
#'      at each grid cell.
calculateDyeMass <- function(delta.raw, dye.raw){

  d <- delta.raw %>%
    mutate_(volume = ~(dx * dy * dz)) %>%
    select_( ~x, ~y, ~z, ~volume)%>%
    cbind(., dye = dye.raw$dye)
  
  dye_mass_cell <- d %>%
    mutate_(dye_mass = ~(volume * dye))
  
  # Sum dye mass for all cells, convert from g to kg (assumption)
  dye_mass <- sum(dye_mass_cell$dye_mass) / 10^6
  
  return(dye_mass)
}

#' Takes data input paths and returns concentration weighted average second moments 
#' for each axis.
#' 
#' @param dye.path Path to the dye concentration input file
#' @param dxdy.inp.path Path to the delta x and delta y input file
#' @param depth.path Path to the depth input file
#' @param start.datetime POSIXct datetime of the start of the analysis
#' @param hours Number of hours after start.datetime to process
#' @return A list with the concentration weight average second moments for each axis
performAnalysis <- function(dye.path, dxdy.inp.path, depth.path, start.datetime, hours) {
    #browser()
    # read in the delta data and name the columns
    dxdy.inp <- read.table(dxdy.inp.path, skip=4, header=FALSE, fill=TRUE)[1:4]
    names(dxdy.inp) <- c('x', 'y', 'dx', 'dy')

    # define the number of layers in the z direction
    nlayers <- 5

    # read in the dye concentration data, melt the data for each timestep
    # into a table where there are four columns, x, y, z, and concetration
    dye.list <- stackedTimeseriesToList(dye.path)
    dye.list.raw <- lapply(dye.list, function(dye.timestep)  {
        d <- data.frame(dxdy.inp[1:2], setNames(dye.timestep, 1:nlayers), check.names=FALSE)
        d <- melt(d, id.vars=c('x', 'y'), variable.name='z', value.name='dye')
        # convert the z column to integers
        transform(d, z=as.integer(as.character(levels(z)))[as.numeric(z)])
    })

    # extract the julian day timesteps from dye.list.raw and convert them to
    # full datetime strings
    timestamps.chr <- strsplit(names(dye.list.raw), '.', fixed=TRUE)
    timestamps.chr <- lapply(timestamps.chr, function(ts, year) {
        tot.mins <- round(as.numeric(paste0('.', ts[2])) * 24 * 60)
        hours <- tot.mins %/% 60
        mins <- tot.mins %% 60
        paste(ts[1], hours, mins, year)
    }, year=format(start.datetime, '%Y'))
    # convert the timestep datetime strings to POSIXct datetimes
    timestamps <- lapply(timestamps.chr, as.POSIXct, format='%j %H %M %Y')
    # define the end of the analysis
    end.datetime <- start.datetime + as.difftime(hours, units='hours')
    # get the indexes that correspond to the desired time frame
    timestamp.idxs <- which(timestamps >= start.datetime & timestamps <= end.datetime)

    # subset the dye data to the desired timeframe
    dye.list.raw <- dye.list.raw[timestamp.idxs]

    # pivot the dxdy data so that the x direction index are the columns and the y direction indexes are the rows.
    # the value in each cell in this matrix is the delta x value corresponding to that cell
    dx.matrix <- dcast(dxdy.inp, y ~ x, value.var='dx')
    # For x-y pairs that don't have a delta x value, fill these pairs with the last delta x value along the 
    # y direction, or if there is no delta x before some cell, fill it with the next non-missing delta x value.
    dx.matrix.filled <- na.locf(na.locf(dx.matrix), fromLast=TRUE)
    # set the rownames to the y direction indexes and remove the column of y indexes
    rownames(dx.matrix.filled) <- dx.matrix.filled[[1]]
    dx.matrix.filled <- as.matrix(dx.matrix.filled[-1])

    # this is the x direction index corresponding to the lowest, left-most grid cell in the domain
    x.idx.first.cell <- 237

    # for each grid cell, calculate that grid cells x coordinate by summing up all of the 
    # delta x's between the cell and the first cell
    x.coors <- t(apply(dx.matrix.filled, 1, function(row) {
        c(-rev(cumsum(row[x.idx.first.cell:1])), cumsum(c(0, row[(x.idx.first.cell + 1):(length(row)-1)])))
    }))
    # Put back the missing values that were filled
    x.coors[is.na(as.matrix(dx.matrix[-1]))] <- NA
    dimnames(x.coors) <- dimnames(dx.matrix.filled)

    # repeat the coordinate calculation process for the y axis
    dy.matrix <- dcast(dxdy.inp, x ~ y, value.var='dy')
    dy.matrix.filled <- na.locf(na.locf(dy.matrix), fromLast=TRUE)
    rownames(dy.matrix.filled) <- dy.matrix.filled[[1]]
    dy.matrix.filled <- as.matrix(dy.matrix.filled[-1])

    y.coors <- t(apply(dy.matrix.filled, 1, function(row) cumsum(c(0, head(row, -1)))))
    y.coors[is.na(as.matrix(dy.matrix[-1]))] <- NA
    dimnames(y.coors) <- dimnames(dy.matrix.filled)

    # add the y column back to x coordinates, melt the x columns back into rows, remove 
    # the entries that have missing values
    x.coors.final <- data.frame(cbind(dx.matrix['y'], x.coors), check.names=FALSE)
    x.coors.final <- melt(x.coors.final, id.var='y', variable.name='x', value.name='xcoor')
    x.coors.final <- x.coors.final[complete.cases(x.coors.final), ]

    y.coors.final <- data.frame(cbind(dy.matrix['x'], y.coors), check.names=FALSE)
    y.coors.final <- melt(y.coors.final, id.var='x', variable.name='y', value.name='ycoor')
    y.coors.final <- y.coors.final[complete.cases(y.coors.final), ]

    # merge the x and y coordinates into a single data.frame
    coors <- merge(x.coors.final, y.coors.final, by=c('x', 'y'), sort=FALSE)

    # read in the depth data to calculate the coordinates for the z direction, combine this 
    # with the x-y coordinate data
    depth.list <- stackedTimeseriesToList(depth.path)
    coor.list.raw <- lapply(depth.list, function(depth.timestep) {
        # final depth is the sum of first and second columns of the depth input
        depth.adj <- depth.timestep[1] + abs(depth.timestep[2])
        # for each x-y pair, create the depth coordinates
        depths <- t(apply(depth.adj, 1, function(depth) seq(from=depth/nlayers, to=depth, by=depth/nlayers)))
        # combine the depth coordinates with the x-y coordinates and melt the z columns in rows
        d <- data.frame(coors, setNames(as.data.frame(depths), 1:nlayers), check.names=FALSE)
        d <- melt(d, id.vars=c('x', 'y', 'xcoor', 'ycoor'), variable.name='z', value.name='zcoor')
        # convert the z indexes to integers
        transform(d, z=as.integer(as.character(levels(z)))[as.numeric(z)])
    })

    # subset the coordinate data to the desired time range
    coor.list.raw <- coor.list.raw[timestamp.idxs]

    # calculte the delta z values and combine them with the delta x and delta y values
    delta.list.raw <- lapply(depth.list, function(depth.timestep) {
        # final depth is the sum of the first and second columns of the depth input
        depth.adj <- depth.timestep[1] + abs(depth.timestep[2])
        # calculate the delta z series for each x-y pair
        deltas <- t(apply(depth.adj, 1, function(depth) rep(depth/nlayers, nlayers)))
        # combine the delta z data with the delta x and delta y data, then melt the z columns into rows
        d <- data.frame(dxdy.inp, setNames(as.data.frame(deltas), 1:nlayers), check.names=FALSE)
        d <- melt(d, id.vars=c('x', 'y', 'dx', 'dy'), value.name='dz', variable.name='z')
        # convert the z direction indexes to integers
        transform(d, z=as.integer(as.character(levels(z)))[as.numeric(z)])
    })

    # subset the delta data to the desired time range 
    delta.list.raw <- delta.list.raw[timestamp.idxs]

    dye.mass.inventory <- mapply(calculateDyeMass, delta.list.raw, dye.list.raw)
    
    # get the concentrated weighted average second moment timeseries for each axis
    avg.moment2s <- lapply(c('x', 'y', 'z'), getAxisSecondMoments, delta.list.raw, coor.list.raw, dye.list.raw)
    
    # return list of dye mass and second moments
    list(avg.moment2s, dye.mass.inventory)
    
}

#' Takes a list of the concentration weighted average second for each axis and returns a line plot
#'
#' @param moment2.by.time List of three entries, the second moment time series for each axis
#' @param duration the number of hours 
make.moment.plot <- function(moment2.by.time, start.datetime, end.datetime) {
    hour.range <- seq(as.integer(as.numeric(end.datetime - start.datetime, units='hours')))
    # calculate the dispersion coefficient for each axis as the slope between the first second moment
    # and the last second moment, scale by seconds
    secs.in.hr <- 3600
    Ks <- lapply(moment2.by.time, function(axis) {
        # old method, slope from first and last data point
        #(.5 * ((tail(axis, 1) - head(axis, 1)) / (tail(hour.range, 1) - head(hour.range, 1)))) / secs.in.hr
        d <- data.frame(x=seq_along(axis), y=axis)
        mod <- lm(y ~ x, d)
        .5 * coef(mod)[[2]] / secs.in.hr
    })
    # for each axis, add a timeseries column and scale the second moments by the standard deviation. The second 
    # moments are scaled so that they can be plotted
    tab.list <- lapply(moment2.by.time, function(series) data.frame(timestep_hour=seq_along(series), series)) #scale(series)))
    # merge the second moments for each axis by the timestep
    full.tab <- Reduce(function(...) merge(..., by='timestep_hour'), tab.list)
    names(full.tab) <- c('timestep_hour', 'x', 'y', 'z')
    # melt the data.frame into three columns: timestep, axis, avg.second.moment
    full.tab.raw <- melt(full.tab, id.var='timestep_hour', variable.name='axis', value.name='avg.second.moment')

    # make the plot
    date.range <- paste(format(start.datetime, '%m/%d/%Y %H:%M'), '--', format(end.datetime, '%m/%d/%Y %H:%M'))
    p <- ggplot(data=full.tab.raw, aes(x=timestep_hour, y=avg.second.moment)) + geom_point() + geom_line() + 
        stat_smooth(method='lm', se=FALSE) +
        facet_wrap(~ axis, ncol=1, scales='free_y') + 
        labs(title=bquote(atop(.(date.range), 
                   paste(list(K[x]==.(round(Ks[[1]], 2)), K[y]==.(round(Ks[[2]], 2)), K[z]==.(round(Ks[[3]], 8))), ~~(frac(m^2, s))))),
             y=expression("Weighted Average of Concentration Variance"~~(m^2)))  +
        scale_x_continuous(breaks=hour.range) + theme(axis.text=element_text(vjust=-.4))
    p
}

#' Takes a named vector of the dye mass inventory and returns a line plot
#'
#' @param dye.mass.by.time List of three entries, the second moment time series for each axis
#' @param duration the number of hours 
make.dye.mass.plot <- function(dye.mass.by.time, start.datetime, end.datetime) {

  timestep_hour <- c(0, seq(as.integer(as.numeric(end.datetime - start.datetime, units='hours'))))

  d <- data.frame(timestep_hour, dye.mass.by.time)
  # make the plot
  date.range <- paste(format(start.datetime, '%m/%d/%Y %H:%M'), '--', format(end.datetime, '%m/%d/%Y %H:%M'))
  p <- ggplot(data=d, aes(x=timestep_hour, y=dye.mass.by.time)) + geom_point() + geom_line() + 
    stat_smooth(method='lm', se=FALSE) +
    labs(title=bquote(atop(.(date.range))), y=expression("Dye Mass (kg)"))  +
    scale_x_continuous(breaks=timestep_hour) + theme(axis.text=element_text(vjust=-.4))
  p
}
