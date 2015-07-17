##' @export
#include.datetimeInput <- function() {
#    tags$head(
#        #tags$script(type='text/javscript', "var jQuery = jQuery.noConflict(true);")
#        #tags$script(type='text/javascript', src="roow/jquery-ui-1.10.4.min.js")
#        #,tags$script(type='text/javascript', 'var jQuery_1_10 = jQuery.noConflict(true);')
#        #,tags$link(href="roow/jquery-ui.css", rel='stylesheet', type='text/css')
#        #,tags$script(type='text/javascript', src="roow/timepicker/jquery-ui-timepicker-addon.js")
#        #,tags$script(type='text/javascript', src="roow/timepicker/timepicker_bindings.js")
#        #,tags$link(href="roow/timepicker/jquery-ui-timepicker-addon.css", rel='stylesheet', type='text/css')
#
#
#        #, tags$script(type='text/javascript', src="roow/jquery.js")
#        #, tags$script(type='text/javascript', 'var $ = jQuery.noConflict(true);')
#    )
#}

validate.datetime.input <- function(x) {
    if (! is.null(x)) {
        if (! inherits(x, 'POSIXct')) stop("datetimeInput: datetime arguments must be NULL or POSIXct.")
        #format(x, format='%Y-%m-%d %H:%M')
        # local format used because browsers such as firefox use localtime format
        format(x, format='%m/%d/%Y %H:%M')
        #format(x, format='%Y-%m-%dT%H:%M:%S')
    } 
}


#' @export
datetimeInput <- function(inputId, label, value=NULL, min=NULL, max=NULL) {
    #value <- validate.datetime.input(value)
    #min <- validate.datetime.input(min)
    #max <- validate.datetime.input(max)

    dep2 <- htmlDependency('datetime_Input2', "1", c(href='bigpicture'), 
        script='jquery.datetimepicker.js',
        stylesheet='jquery.datetimepicker.css')

    dep3 <- htmlDependency('datetime_Input3', "1", c(href='bigpicture'), 
        script='timepicker/timepicker_bindings.js')

    attachDependencies(
        tags$div(id=inputId, class='shiny-datetime-input form-group shiny-input-container',
            tags$label(class="control-label", `for`=inputId, label),
            tags$input(type='text', class='form-control datetimepicker', 
                `data-initial-value`=value, `data-min-date`=min, `data-max-date`=max))
        , list(dep2, dep3)
    )
}

#' @export
updateDatetimeInput <- function(session, inputId, value=NULL, min=NULL, max=NULL) {
    value <- validate.datetime.input(value)
    min <- validate.datetime.input(min)
    max <- validate.datetime.input(max)

    message <- list(value=value, min=min, max=max)
    session$sendInputMessage(inputId, message)
}

#' @export
test.datetimeInput <- function(include_dateInput=FALSE) {
   ui <- fluidPage(
       #if (include_dateInput) dateInput('date_input', 'date input', value='1/1/1999') else div(),
       include.datetimeInput(),
       datetimeInput2('dtime_input', 'datetime input', 
            min=as.POSIXct('1990-1-1 00:00', format='%Y-%m-%d %H:%M'), 
            max=as.POSIXct('1990-2-1 00:00', format='%Y-%m-%d %H:%M'), 
            value=as.POSIXct('1990-1-2 12:00', format='%Y-%m-%d %H:%M')
            ),
       sliderInput('bacon', 'bits', 1, 3, 1),
       textOutput('text_out')
   )
   server <- function(input, output, session) {
       output$text_out <- renderText({
           updateDatetimeInput(session, 'dtime_input', 
            value=as.POSIXct('1999-1-3 00:00', format='%Y-%m-%d %H:%M'),
            min=as.POSIXct('1999-1-2 00:00', format='%Y-%m-%d %H:%M'),
            max=as.POSIXct('1999-1-3 00:00', format='%Y-%m-%d %H:%M'))
           input$dtime_input
       })
   }
   runApp(list(ui=ui, server=server))
}
