#' diffcoef.
#'
#' @name diffcoef
#' @docType package
#' @import reshape2 ggplot2 RColorBrewer grid shiny zoo roow

library(shiny)

#' @export 
diffcoef <- function() {
    runApp(system.file('diffcoef_app', package='diffcoef'))
}
