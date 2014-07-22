DispersionCalculator
================================
Calculates the concentration weighted average second moments and corresponding dispersion rates
for the x, y, and z directions in an open channel water body.

Installation and Usage
================================
Install the *devtools* to allow R to install packages from github.

```r
setInternet2() # enable R to use your proxy settings, if necessary
options(repos=c(CRAN='http://cran.us.r-project.org')) # set your repository
install.packages('devtools')
```

Load *devtools*, and install this repository and one of its dependencies, *roow*.

```r
library(devtools)
install_github('roow', 'oow')
install_github('dispersion-calculator', 'oow')
```

Load and launch *DispersionCalculator*:

```r
library(DispersionCalculator)
DispersionCalculator()
```

Examining the Code
================================
You can find the code used to calculate the average second moments and the dispersion rates
in the file inst/dispersion-calculator-app/global.r. The function `getSecondMoments` calculates the 
average second moments for each axis. The function `make.plot` takes these average second moments, calculates
their corresponding dispersion rates, then outputs the plot.

For a detailed explation of the methodology, please visit the [project homepage](http://oow.github.io/dispersion-calculator/).
