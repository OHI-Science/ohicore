require(ohicore)
require(ohigui)

setwd('~/Documents/ohigit/ohicore/inst/extdata')
source("layers_Canada-CHONE2014.R")

wd = getwd()
ohigui::launchApp(scenario=list(
  conf   = ohicore::Conf("conf.Canada-CHONe2014"),
  layers = ohicore::Layers("layers.Canada-CHONe2014.csv", "layers.Canada-CHONe2014"),
  scores = read.csv("scores.Canada-CHONe2014.csv", na.strings=""),
  spatial = file.path(wd, "spatial.Canada-CHONe2014"),
  dir    = wd), launch.browser=T)
