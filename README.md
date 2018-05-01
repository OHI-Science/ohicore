ohicore
=======


`ohicore` is a package of core functions for calculating the Ocean Health Index. For more, please visit [OHI-Science.org](http://ohi-science.org).

## Installation


```R
# install packages

# devtools is needed for installing packages from Github:
install.packages('devtools')
library(devtools)

install_github('ohi-science/ohicore')
library(ohicore)

# these packages are used by the repository (conf/functions.R) 
install.packages(c('zoo', 'psych')) # these are used in functions.R
```



