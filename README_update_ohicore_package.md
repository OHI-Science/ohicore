## README_update_ohicore_package.md

[github.com/yihui/roxygen2](https://github.com/yihui/roxygen2#running)


To add a new `.r` script to the ohicore package: 

```
setwd('~/github/ohicore')

roxygen2::roxygenise() 
devtools::document()

# to recreate the pdf manual
system("cd /home/frazier/ohicore")
system("R CMD Rd2pdf man/")

```
https://gist.github.com/cboettig/2656075
https://gist.github.com/richfitz/2656053