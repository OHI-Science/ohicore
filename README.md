ohicore
=======

`ohicore` is a package of core funtions for calculating the Ocean Health Index. For more, please visit [OHI-Science.org](http://ohi-science.org).

## Installation

```R
# install.packages("devtools")
devtools::install_github("ohi-science/ohi-core")
=======
To install: 

```
# install dependencies
for (p in c('devtools', 'zoo', 'psych')){
  if (!require(p, character.only=T)){
    install.packages(p)
    require(p, character.only=T)
  }
}

# install packages
install_github('ohi-science/rCharts')
install_github('ohi-science/ohicore')
require(ohicore)
```


