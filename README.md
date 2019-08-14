DispersionCalculator
================================
Calculates the concentration weighted average second moments and corresponding dispersion rates
for the x, y, and z directions in an open channel water body. For a detailed explation of the methodology, please visit the [project homepage](http://oow.github.io/dispersion-calculator/).

Installation and Usage
================================
Install the *devtools* to allow R to install packages from github.

```r
setInternet2() # enable R to use your proxy settings, if necessary
options(repos=c(CRAN='http://cran.us.r-project.org')) # set your repository
install.packages('devtools')
```

Load *devtools*, and install this repository.

```r
library(devtools)
#set httr proxy, if necessary
library(httr)
set_config(use_proxy("webproxy.phila.gov", 8080))
install_github('OOW/dispersion-calculator')
install_github('OOW/dispersion-calculator', ref = "working_r_v351")
```

If `install_github` still fails, [download](https://github.com/OOW/dispersion-calculator/archive/working_r_v351.zip) a zipfile of this repository from github and follow these steps.

1.  Extract the zipfile and browse to the directory in this repository containing the inst folder.
2.  Open R and set your working directory to this directory.
3.  load devtools: `library(devtools)`
4.  Set your CRAN repository: `options(repos=list(CRAN="http://cran.us.r-project.org"))`
5.  Install **dispersion-calculator**: `install()`


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

