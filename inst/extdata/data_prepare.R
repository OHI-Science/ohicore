### Prepare data files for R

library(devtools)

georegion_labels <- read.csv('data_raw/georegion_labels.csv')
devtools::use_data(georegion_labels, overwrite = TRUE)


georegions <- read.csv('data_raw/georegions.csv')
devtools::use_data(georegions, overwrite = TRUE)

sovregion_labels <- read.csv('data_raw/sovregion_labels.csv')
devtools::use_data(sovregion_labels, overwrite = TRUE)

sovregions <- read.csv('data_raw/sovregions.csv')
devtools::use_data(sovregions, overwrite = TRUE)

rgn_synonyms <- read.csv('data_raw/rgn_synonyms.csv')
devtools::use_data(rgn_synonyms, overwrite = TRUE)
