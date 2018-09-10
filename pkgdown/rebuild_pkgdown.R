## run this script to rebuild the pkgdown website when changes are made

library(devtools) # install.packages("devtools")
library(roxygen2) # install.packages("roxygen2")
library(pkgdown) # install.packages("pkgdown")

## check that these reflects correct repo links...
## any ohi code links external to the package/repo used in vignettes etc, specify here
demo_folder <- "https://github.com/OHI-Science/ohicore/tree/dev/pkgdown/toolbox-demo"
demo_functions <- "https://github.com/OHI-Science/ohicore/blob/dev/pkgdown/toolbox-demo/conf/functions.R"
demo_calculate_scores <- "https://github.com/OHI-Science/ohicore/blob/dev/pkgdown/toolbox-demo/calculate_scores.R"

## build
devtools::load_all()
devtools::document() # check 'man' folder to confirm documentation looks good
pkgdown::build_site() # pkgdown::build_site(examples = FALSE)
