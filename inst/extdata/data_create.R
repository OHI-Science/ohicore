# Create package datasets for lazy loading. Document in R/data.R.

library(ohicore)

# layers.* ----
# 
# Create layers.[scenario] dataset for all layers.[scenario].csv based on csv
# layer files in layers.[scenario].  See: R/Layers.R for expected formats.
for (csv in list.files('inst/extdata', pattern=glob2rx('layers.*.csv'), full.names=T)){ # csv = list.files('inst/extdata', pattern=glob2rx('layers.*.csv'), full.names=T)[1]
  
  # get directory and ensure exists
  dir = tools::file_path_sans_ext(csv)
  stopifnot(file.exists(dir))  
  scenario = basename(dir)
  
  # get layers and assign same variable name as dataset
  assign(scenario, Layers(csv, dir))
  
  # save to data folder for lazy loading
  save(list=c(scenario), file=sprintf('data/%s.rda', scenario))
}

# scores.* ----
# 
# Create scores.[scenario] dataset for all scores.[scenario].csv.
# See: R/Scores.R for expected formats.
