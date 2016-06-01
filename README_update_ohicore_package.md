## README_update_ohicore_package.md

[github.com/yihui/roxygen2](https://github.com/yihui/roxygen2#running)

If any R/functions.R files are added or deleted this should be reflected in the DESCRIPTION file.

If changes are made to any of the R/functions.R (or other files), the following should be run.

```

roxygen2::roxygenise() 
devtools::document()

# to recreate the pdf manual
system("cd /home/frazier/ohicore")
system("R CMD Rd2pdf man/")

# move the file that is created to correct location:
file.remove("doc/ohicore.pdf")  # delete current copy
file.copy(from = "Rd2.pdf", to = "doc/ohicore.pdf")
file.remove("Rd2.pdf")  # delete 
```
In the "Build" tab is the "Check" button which seems helpful. I definitely don't understand everything, but I have been able to improve the package with some of the output.

https://gist.github.com/cboettig/2656075
https://gist.github.com/richfitz/2656053

 