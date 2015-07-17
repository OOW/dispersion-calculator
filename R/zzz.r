.onLoad <- function(libname, pkgname) {
    addResourcePath('DispersionCalculator', system.file('dispersion-calculator-app/www', package='DispersionCalculator'))
    addResourcePath('shiny', system.file('www/shared/', package='shiny'))
}

.onAttach <- function(libname, pkgname) {
    #packageStartupMessage("Attaching bigpicture. Environment variable TZ set to 'EST'")
    #Sys.setenv(TZ='EST')
    #options(shiny.reactlog=TRUE, show.error.locations=TRUE, useFancyQuotes=FALSE, shiny.trace=TRUE)
    #options(warn=1)
}
