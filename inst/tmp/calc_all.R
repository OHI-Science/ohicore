# NOTE: now handled in data_create.R, but useful summary execution and comparison code below

library(devtools)
load_all()

# ensure `do.years.www2013 = c(2012,2013)` in data_create.R
#source('inst/extdata/data_create.R')

# load layers and conf ----
for (scenario in c('Global2013.www2013','Global2012.www2013')){
  conf       = Conf(sprintf('inst/extdata/conf.%s', scenario))
  layers     = Layers(layers.csv = sprintf('inst/extdata/layers.%s.csv', scenario), 
                      layers.dir = sprintf('inst/extdata/layers.%s'    , scenario))

  # calculate scores ----
  scores = CalculateAll(conf, layers, debug=T)
  write.csv(scores, sprintf('inst/extdata/scores.%s.csv', scenario), na='', row.names=F)
}