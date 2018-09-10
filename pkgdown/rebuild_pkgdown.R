## run this script to rebuild the pkgdown website when changes are made

library(devtools) # install.packages("devtools")
library(roxygen2) # install.packages("roxygen2")
library(pkgdown) # install.packages("pkgdown")

## check that these reflects correct repo links...
## any ohi code links external to the package/repo used in vignettes etc, specify here

demo_folder <- "https://github.com/OHI-Science/ohicore/blob/dev/pkgdown/demo" # "https://github.com/OHI-Science/ohicore/blob/master/pkgdown/demo"

demo_functions <- paste(demo_folder, "conf/functions.R", sep = "/")
demo_calculate_scores <-  paste(demo_folder, "calculate_scores.R", sep = "/")
demo_config_toolbox <-  paste(demo_folder, "configure_toolbox.R", sep = "/")

demo_layers_csv <- paste(demo_folder, "layers.csv", sep = "/")
demo_layers_csv <- paste(demo_folder, "referencePoints.csv", sep = "/")


## build
devtools::load_all()
devtools::document() # check 'man' folder to confirm documentation looks good
pkgdown::build_site() # pkgdown::build_site(examples = FALSE)
