require(ohicore)
require(ohigui)

setwd('~/Documents/ohigit/ohicore/inst/extdata')
unlink('conf/*');unlink('layers/*');unlink('spatial/*');unlink('conf.Canada-CHONe2014/*');unlink('layers.Canada-CHONe2014/*');unlink('spatial.Canada-CHONe2014/*')
source("layers_Canada-CHONE2014.R")

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

