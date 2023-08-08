### Prepare data files for R
### if data needs to be updated, do this in the source files and then resave in ohicore
### eventually this will have its own package ('rohiprep')

library(devtools)

rgn_synonyms <- read.csv('data_raw/rgn_eez_v2013a_synonyms.csv', na = "", stringsAsFactors = FALSE)
usethis::use_data(rgn_synonyms, overwrite = TRUE)


rgn_master <- read.csv('data_raw/eez_rgn_2013main.csv', na = "", stringsAsFactors = FALSE)
usethis::use_data(rgn_master, overwrite = TRUE)

## The following have not been updated, will do this when I figure out how they are used.

georegion_labels <- read.csv('data_raw/georegion_labels.csv')
usethis::use_data(georegion_labels, overwrite = TRUE)


georegions <- read.csv('data_raw/georegions.csv')
usethis::use_data(georegions, overwrite = TRUE)

sovregion_labels <- read.csv('data_raw/sovregion_labels.csv')
usethis::use_data(sovregion_labels, overwrite = TRUE)

sovregions <- read.csv('data_raw/sovregions.csv')
usethis::use_data(sovregions, overwrite = TRUE)


