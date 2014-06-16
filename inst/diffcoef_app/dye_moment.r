required.packages <- c('reshape2', 'ggplot2', 'RColorBrewer', 'grid', 'devtools', 'shiny', 'zoo')

# enable R to use proxy settings, in case required packages need to be installed
setInternet2()

# set default package respository URL
options(repos=list(CRAN="http://cran.us.r-project.org"))

# check if the required packages are installed. If they're not, install them. Then load the packages
for (pkg in required.packages) {
    if (! pkg %in% installed.packages()) {
        install.packages(pkg)
    }
    library(pkg, character.only=TRUE)
}

# check if the roow package is installed. If not install it from Github
if (! 'roow' %in% installed.packages()) {
    install_github('roow', 'oow', subdir='roow')
}

# show line number of exception when an error is encountered
options(show.error.locations=TRUE)

# turn off daylight savings time
Sys.setenv(TZ='EST')

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

new.delta <- function(old.delta, new.centers, other.axis) {
    section.diff.half.pcts <- ave(old.delta, other.axis, FUN=function(section) {
        section.halved <- section/2
        half.pairwise.sum <- tail(filter(section.halved, c(1,1), sides=1), -1)
        half.pairwise.sum <- c(half.pairwise.sum[1], half.pairwise.sum)
        section.halved / half.pairwise.sum
    })

    center.diffs <- ave(new.centers, other.axis, FUN=function(centers) {
        center.diff <- diff(centers)
        c(center.diff[1], center.diff)
    })

    2 * (center.diffs * section.diff.half.pcts)
}

#pivot.raw <- function(axis., var, list.raw) {
#    lapply(list.raw, function(timestep) {
#        dcast(timestep, as.formula(paste('...', axis., sep='~')), value.var=var, drop=FALSE)
#    })
#}


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
        timestep <- timestep[c(axis., other.axes, var)]
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
    # define the names of the axes and identify the other axes according the axis. argument
    axes. <- c('x', 'y', 'z')
    other.axes <- setdiff(axes., axis.)

    # create variables containing the delta and coordinate column names in delta.list.raw and coor.list.raw. 
    # For example, the delta column name for the x-axis is 'dx' and the coordinate column for the x-axis is 'xcoor'.
    delta.var <- paste0('d', axis.)
    coor.var <- paste0(axis., 'coor')

    # pivot each of the lists along the desired axis. Consult the definition of pivot.raw for 
    # a description of what the output looks like.
    dye.list <- pivot.raw(axis., other.axes, 'dye', dye.list.raw)
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

getSecondMoments <- function(dye.path, dxdy.inp.path, lxly.inp.path, depth.path, start.datetime, hours, recalc.centers=TRUE) {

    dxdy.inp <- read.table(dxdy.inp.path, skip=4, header=FALSE, fill=TRUE)[1:4]
    names(dxdy.inp) <- c('x', 'y', 'dx', 'dy')

    nlayers <- 5
    dye.list <- stackedTimeseriesToList(dye.path)
    dye.list.raw <- lapply(dye.list, function(dye.timestep)  {
        d <- data.frame(dxdy.inp[1:2], setNames(dye.timestep, 1:nlayers), check.names=FALSE)
        d <- melt(d, id.vars=c('x', 'y'), variable.name='z', value.name='dye')
        transform(d, z=as.integer(as.character(levels(z)))[as.numeric(z)])
    })

    #dxdy.inp <- dxdy.inp[1:9746, ] # exclude dummy rows at the end

    timestamps.chr <- strsplit(names(dye.list.raw), '.', fixed=TRUE)
    timestamps.chr <- lapply(timestamps.chr, function(ts, year) {
        tot.mins <- round(as.numeric(paste0('.', ts[2])) * 24 * 60)
        hours <- tot.mins %/% 60
        mins <- tot.mins %% 60
        paste(ts[1], hours, mins, year)
    }, year=format(start.datetime, '%Y'))
    timestamps <- lapply(timestamps.chr, as.POSIXct, format='%j %H %M %Y')
    end.datetime <- start.datetime + as.difftime(hours, units='hours')
    timestamp.idxs <- which(timestamps >= start.datetime & timestamps <= end.datetime)

    dye.list.raw <- dye.list.raw[timestamp.idxs]

    first.dye.timestep <- which.max(sapply(dye.list.raw, function(timestep) any(timestep$dye > 0)))
    dye.init.ts <- dye.list.raw[[first.dye.timestep]]
    y.center.idx <- ceiling(sum(with(dye.init.ts, range(y[dye > 0]))) / 2)
    x.center.idx <- ceiling(sum(with(dye.init.ts, range(x[y == y.center.idx & dye > 0]))) / 2)

    lxly.inp <- read.table(lxly.inp.path, skip=4, header=FALSE, fill=TRUE)[1:4]
    names(lxly.inp) <- c('x', 'y', 'xcoor', 'ycoor')
    centers <- unlist(with(lxly.inp, lxly.inp[x==x.center.idx & y==y.center.idx, c('xcoor', 'ycoor'), drop=TRUE]))
    lxly.inp[c('xcoor', 'ycoor')] <- sweep(lxly.inp[c('xcoor', 'ycoor')], 2, centers)


    dx.matrix <- dcast(dxdy.inp, y ~ x, value.var='dx')
    dx.matrix.filled <- na.locf(na.locf(dx.matrix), fromLast=TRUE)
    rownames(dx.matrix.filled) <- dx.matrix.filled[[1]]
    dx.matrix.filled <- as.matrix(dx.matrix.filled[-1])

    x.coors <- t(apply(dx.matrix.filled, 1, function(row) c(-rev(cumsum(row[237:1])), cumsum(c(0, row[238:(length(row)-1)])))))
    x.coors[is.na(as.matrix(dx.matrix[-1]))] <- NA
    dimnames(x.coors) <- dimnames(dx.matrix.filled)

    dy.matrix <- dcast(dxdy.inp, x ~ y, value.var='dy')
    dy.matrix.filled <- na.locf(na.locf(dy.matrix), fromLast=TRUE)
    rownames(dy.matrix.filled) <- dy.matrix.filled[[1]]
    dy.matrix.filled <- as.matrix(dy.matrix.filled[-1])

    y.coors <- t(apply(dy.matrix.filled, 1, function(row) cumsum(c(0, head(row, -1)))))
    y.coors[is.na(as.matrix(dy.matrix[-1]))] <- NA
    dimnames(y.coors) <- dimnames(dy.matrix.filled)

    x.coors.final <- data.frame(cbind(dx.matrix['y'], x.coors), check.names=FALSE)
    x.coors.final <- melt(x.coors.final, id.var='y', variable.name='x', value.name='xcoor')
    x.coors.final <- x.coors.final[complete.cases(x.coors.final), ]

    y.coors.final <- data.frame(cbind(dy.matrix['x'], y.coors), check.names=FALSE)
    y.coors.final <- melt(y.coors.final, id.var='x', variable.name='y', value.name='ycoor')
    y.coors.final <- y.coors.final[complete.cases(y.coors.final), ]


    coors <- merge(x.coors.final, y.coors.final, by=c('x', 'y'), sort=FALSE)

    # code for generating coordinate heat maps
    #par(mfrow=c(2,2))

    #plot.start <- 240
    #k <- dcast(lxly.inp, y ~ x, value.var='xcoor')
    #image(t(as.matrix(k[-1, plot.start:ncol(k)])), col=heat.colors(50))
    #image(t(x.coors[,plot.start:ncol(x.coors)]), col=heat.colors(50))

    #j <- dcast(lxly.inp, y ~ x, value.var='ycoor')
    #l <- as.matrix(dcast(y.coors.final, y ~ x, value.var='ycoor')[-1])
    #image(t(as.matrix(j[-1, plot.start:ncol(j)])), col=heat.colors(50))
    #image(t(l[, plot.start:ncol(l)]), col=heat.colors(50))

    #Reduce(function(a,b) intersect(a, b), split(dxdy.inp$x, dxdy.inp$y), accumulate=TRUE)


    depth.list <- stackedTimeseriesToList(depth.path)
    coor.list.raw <- lapply(depth.list, function(depth.timestep) {
        coordinates <- if (recalc.centers) lxly.inp else coors
        depth.adj <- depth.timestep[1] + abs(depth.timestep[2])
        depths <- t(apply(depth.adj, 1, function(depth) seq(from=depth/nlayers, to=depth, by=depth/nlayers)))
        d <- data.frame(coordinates, setNames(as.data.frame(depths), 1:nlayers), check.names=FALSE)
        d <- melt(d, id.vars=c('x', 'y', 'xcoor', 'ycoor'), variable.name='z', value.name='zcoor')
        transform(d, z=as.integer(as.character(levels(z)))[as.numeric(z)])
    })

    coor.list.raw <- coor.list.raw[timestamp.idxs]

    dxdy.real <- within(data.frame(dxdy.inp, lxly.inp[c('xcoor', 'ycoor')]), {
        dx <- new.delta(dx, xcoor, y)
        dy <- new.delta(dy, ycoor, x)
        rm(xcoor, ycoor)
    })

    delta.list.raw <- lapply(depth.list, function(depth.timestep) {
        dxdy <- if (recalc.centers) dxdy.real else dxdy.inp
        depth.adj <- depth.timestep[1] + abs(depth.timestep[2])
        deltas <- t(apply(depth.adj, 1, function(depth) rep(depth/nlayers, nlayers)))
        d <- data.frame(dxdy, setNames(as.data.frame(deltas), 1:nlayers), check.names=FALSE)
        d <- melt(d, id.vars=c('x', 'y', 'dx', 'dy'), value.name='dz', variable.name='z')
        transform(d, z=as.integer(as.character(levels(z)))[as.numeric(z)])
    })

    delta.list.raw <- delta.list.raw[timestamp.idxs]

    avg.moment2s <- lapply(c('x', 'y', 'z'), getAxisSecondMoments, delta.list.raw, coor.list.raw, dye.list.raw)
    avg.moment2s
}

main <- function(dye.path='', dxdy.inp.path='', lxly.inp.path='', depth.path='', 
    injection.time=NA, start.hrs=NA, duration.hrs=NA, recalc.centers=FALSE) {

    #while (! file.exists(dye.path)) {
    #    dye.path <- readline('dye concentration path >> ')
    #}

    #while (! file.exists(dxdy.inp.path)) {
    #    dxdy.inp.path <- readline('dxdy.inp path >> ')
    #}

    #while (! file.exists(lxly.inp.path)) {
    #    lxly.inp.path <- readline('lxly.inp path >> ')
    #}

    #while (! file.exists(depth.path)) {
    #    depth.path <- readline('water depth path >> ')
    #}

    #while (is.na(start.datetime)) {
    #    start.chr <- readline('start datetime (YYYY-mm-dd HH:MM) >> ')
    #    start.datetime <- as.POSIXct(start.chr, format='%Y-%m-%d %H:%M')
    #}

    #while (is.na(hours)) {
    #    hours <- as.numeric(readline('hours >> '))
    #}

    start.datetimes <- lapply(start.hrs, function(x) injection.time + as.difftime(x, units='hours'))
    hours <- duration.hrs

    cat('Processing ... ')
    all.moment2.by.time <- mapply(getSecondMoments, 
        dye.path=dye.path, 
        dxdy.inp.path=dxdy.inp.path,
        lxly.inp.path=lxly.inp.path,
        depth.path=depth.path,
        start.datetime=start.datetimes,
        hours=hours,
        recalc.center=recalc.centers,
        SIMPLIFY=FALSE)


    outfile <- 'diffusion_coef_plots'
    if (file.exists(outfile)) {
        unlink(outfile, recursive=TRUE)
    }
    dir.create(outfile)

    for (i in seq_along(start.hrs)) {
        moment2.by.time <- all.moment2.by.time[[i]]
        hour.range <- seq(duration.hrs[i])
        Ks <- lapply(moment2.by.time, function(axis) {
            (.5 * ((tail(axis, 1) - head(axis, 1)) / (tail(hour.range, 1) - head(hour.range, 1)))) / 3600
        })
        tab.list <- lapply(moment2.by.time, function(series) data.frame(timestep_hour=seq_along(series), scale(series)))
        full.tab <- Reduce(function(...) merge(..., by='timestep_hour'), tab.list)
        names(full.tab) <- c('timestep_hour', 'x', 'y', 'z')
        full.tab.raw <- melt(full.tab, id.var='timestep_hour', variable.name='axis', value.name='avg.second.moment')

        hour.frame <- paste(start.hrs[i], start.hrs[i] + duration.hrs[i], sep='-')
        p <- ggplot(data=full.tab.raw, aes(x=timestep_hour, y=avg.second.moment)) + geom_point() + geom_line() + 
            facet_wrap(~ axis, ncol=3) + 
            labs(title=paste0(paste(hour.frame, 'hour period after injection '), 
                              '\nKx=', round(Ks[[1]], 2), ', Ky=', round(Ks[[2]], 2), ', Kz=', round(Ks[[3]], 8)), 
                 y='Normalized Average Second Moments') 

        outname <- paste(hour.frame, 'diffusion_coefficients.png', sep='_')
        ggsave(filename=file.path(outfile, outname), plot=p)
    }

}

injection.time <- as.POSIXct('2012-8-5 9:15')

start.times <- list(injection.time, 
                 injection.time + as.difftime(13, units='hours'),
                 injection.time + as.difftime(20, units='hours'))
start.hrs <- c(0, 13, 20)
duration.hrs <- c(12, 7, 5)

#start.times <- start.times[1]
#hours <- hours[1]

main(dye.path='DYEDMPF.ASC', dxdy.inp.path='dxdy.inp', lxly.inp.path='lxly.inp', depth.path='SELDMPF.ASC',
     injection.time=injection.time, start.hrs=start.hrs, 
     duration.hrs=duration.hrs, recalc.centers=TRUE)


ui <- pageWithSidebar(
    headerPanel('Diffusion coefficient calculator'),
    sidebarPanel(wellPanel(
        fileInput('dye_input', strong('dye input (*.asc)')),
        fileInput('dxdy_input', strong('dxdy input (*.inp)')),
        fileInput('depth_input', strong('depth input (*.asc)')),
        dateInput('inject_date', strong('injection date')),
        numericInput('inject_hour', strong('injection hour'), value=0, min=0, max=24, step=1),
        numericInput('inject_min', strong('injection minute'), value=0, min=0, max=59)
    ))
)
