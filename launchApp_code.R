require(ohicore)
require(ohigui)

rm(list=ls())
setwd('~/Documents/ohigit/ohicore/inst/extdata')
#setwd('/Users/bbest/Github_Mac/ohicore_Canada-CHONe/inst/extdata')
unlink('conf/*');unlink('layers/*');unlink('spatial/*');unlink('conf.Canada-CHONe2014/*');unlink('layers.Canada-CHONe2014/*');unlink('spatial.Canada-CHONe2014/*')

# create new layers.csv, and layers folder for Canada-CHONe2014 ####################
file.copy('layers.Global2013.www2013.csv', 'layers.Canada-CHONe2014.csv', overwrite = T)
file.copy('scores.Global2013.www2013.csv', 'scores.Canada-CHONe2014.csv', overwrite = T)
fl=list.files('layers.Global2013.www2013')
file.copy(paste('layers.Global2013.www2013/',fl,sep = ""),paste('layers.Canada-CHONe2014/',fl,sep = ""), overwrite = T)
fl=list.files('conf.Global2013.www2013')
file.copy(paste('conf.Global2013.www2013/',fl,sep = ""),paste('conf.Canada-CHONe2014/',fl,sep = ""), overwrite = T)
fl=list.files('spatial.www2013')
file.copy(paste('spatial.www2013/',fl,sep = ""),paste('spatial.Canada-CHONe2014/',fl,sep = ""), overwrite = T)

source("layers_Canada-CHONE2014.R")

# set weights
weights=read.csv('rawdata.Canada-CHONe2014/weights/weights_55to64.csv')
goals = read.csv('conf.Canada-CHONe2014/goals.csv', stringsAsFactors=F);

#calculate weights, chose either "equal", "importance", "BWrank", "lmc", "lmc1", or "lmc10"
goals$weight <- reweigh(weights,"BWrank")

# write back updated goals.csv
write.csv(goals, 'conf.Canada-CHONe2014/goals.csv', na='', row.names=F)


# remember to change CS habitat limits to
# limit to CS habitats
#rk = subset(rk, habitat %in% c('mangrove','saltmarsh','seagrass','clathrates','permafrost'))

# create new layers.csv, and layers folder for hard coded app ####################
file.copy('layers.Canada-CHONe2014.csv', 'layers.csv', overwrite = T)
file.copy('scores.Canada-CHONe2014.csv', 'scores.csv', overwrite = T)
fl=list.files('layers.Canada-CHONe2014')
file.copy(paste('layers.Canada-CHONe2014/',fl,sep = ""),paste('layers/',fl,sep = ""), overwrite = T)
fl=list.files('conf.Canada-CHONe2014');
file.copy(paste('conf.Canada-CHONe2014/',fl,sep = ""),paste('conf/',fl,sep = ""), overwrite = T)
fl=list.files('spatial.Canada-CHONe2014')
file.copy(paste('spatial.Canada-CHONe2014/',fl,sep = ""),paste('spatial/',fl,sep = ""), overwrite = T)


wd = getwd()
ohigui::launchApp(scenario=list(
  conf   = ohicore::Conf("conf"),
  layers = ohicore::Layers("layers.csv", "layers"),
  scores = read.csv("scores.csv", na.strings=""),
  spatial = file.path(wd, "spatial"),
  dir    = wd), launch.browser=T)








# calculating scores independently of gui ----
#setwd('/Users/bbest/Github_Mac/ohicore_Canada-CHONe/inst/extdata')
setwd('~/Documents/ohigit/ohicore/inst/extdata')


library(devtools)
#load_all('/Users/bbest/Github_Mac/ohicore')
load_all('~/Documents/ohigit/ohicore')


layers = Layers(layers.csv='layers.csv', layers.dir='layers')
conf   = Conf('conf')
# source layers_Canada-CHONe2014.R AN = function...

scores = CalculateAll(conf, layers, debug=T)
