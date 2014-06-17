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
install_github('difcoef', 'oow')
```

Load and launch *DispersionCalculator*:

```r
library(DispersionCalculator)
DispersionCalculator()
```
