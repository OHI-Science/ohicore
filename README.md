
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- Note: If any R/functions.R files are added or deleted this needs to be changed in the DESCRIPTION file, and the NEWS.md (changelog) file. -->
<!-- If changes are made to any of the R/functions.R (or other files), the following should be run: pkgdown::build_site() -->
OHICORE
=======

The `ohicore` is a package of core functions for calculating the Ocean Health Index (OHI). The goal of ohicore is to facilitate calculation of OHI scores both for the annual global assessments and regional assessments, within a [tailorable framework](https://peerj.com/articles/1503/). For more information, please visit [OHI-Science.org](http://ohi-science.org). See [ohi-science.org/toolbox-training](http://ohi-science.org/toolbox-training) for more on using the `ohicore` for ocean health assessments.

Installation
------------

You can install ohicore from github with:

``` r
# devtools is needed for installing packages from Github
# install.packages("devtools")

# install packages
devtools::install_github('ohi-science/ohicore')
library(ohicore)
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

### Preview pkgdown Website

``` r
library(pkgdown)
preview_site()
```
