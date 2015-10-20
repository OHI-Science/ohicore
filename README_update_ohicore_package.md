## README_update_ohicore_package.md

[github.com/yihui/roxygen2](https://github.com/yihui/roxygen2#running)


To add a new `.r` script to the ohicore package: 

```
setwd('~/github/ohicore')

roxygen2::roxygenise() 
devtools::document()
```