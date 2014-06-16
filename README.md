diffcoef
================================

installation
================================
Install _devtools_.

```r
setInternet2() # enable R to use your proxy settings
options(repos=c(CRAN='http://cran.us.r-project.org')) # set your repository
install.packages('devtools')
```

Load devtools, and install this repository and one of its dependencies.

```r
library(devtools)
install_github('roow', 'oow')
install_github('difcoef', 'oow')
```

Load and launch _diffcoef_:

```r
library(diffcoef)
diffcoef()
```
