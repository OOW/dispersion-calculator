DispersionCalculator
================================
Calculates the concentration weighted average second moments and corresponding dispersion rates
for the x, y, and z directions in an open channel water body. For a detailed explation of the methodology, please visit the [project homepage](http://oow.github.io/dispersion-calculator/).

Installation and Usage
================================
Install the *devtools* to allow R to install packages from github.

```r
options(repos=c(CRAN='http://cran.us.r-project.org')) # set your repository
install.packages('devtools')
```

Load *devtools*, and install this repository.

```r
library(devtools)
#set httr proxy, if necessary
# Or setup the proxy for R with the instruction in the link below
# https://support.rstudio.com/hc/en-us/articles/200488488-Configuring-R-to-Use-an-HTTP-or-HTTPS-Proxy
library(httr)
set_config(use_proxy(proxy_address, port)) # update prox_address and port with your own proxy settings

# install package from github
install_github('OOW/dispersion-calculator')
```

If `install_github` still fails, [download](https://github.com/OOW/dispersion-calculator/archive/master.zip) a zipfile of this repository from github and follow these steps.

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

