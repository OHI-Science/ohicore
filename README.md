ohicore
=======

`ohicore` is a package of core funtions for calculating the Ocean Health Index. For more, please visit [OHI-Science.org](http://ohi-science.org).

## Installation


```R
# install dependencies
for (p in c('devtools', 'zoo', 'psych')){
  if (!require(p, character.only=T)){
    install.packages(p)
    require(p, character.only=T)
  }
}

# install packages
install.packages(c('zoo', 'psych', 'tidyr'))
install_github('ohi-science/rCharts')
install_github('ohi-science/ohicore')
require(ohicore)
```


