#' DispersionCalculator
#'
#' @name DispersionCalculator
#' @docType package
#' @import reshape2 ggplot2 dplyr RColorBrewer grid shiny zoo htmltools

library(shiny)

#' @export 
DispersionCalculator <- function() {
    runApp(system.file('dispersion-calculator-app', package='DispersionCalculator'), launch.browser=TRUE)
}
