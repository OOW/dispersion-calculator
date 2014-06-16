#' diffcoef.
#'
#' @name diffcoef
#' @docType package

library(shiny)

#' @export 
runUI <_ function() {
    runApp(systemfile('diffcoef_app', package='diffcoef'))
}
