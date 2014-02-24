require(ohicore)
require(ohigui)


setwd('C:/Users/Remi-Work/Documents/ohigit/ohicore/inst/extdata')

wd = getwd()
ohigui::launchApp(scenario=list(
  conf   = ohicore::Conf("conf.Global2013.www2013"),
  layers = ohicore::Layers("layers.Global2013.www2013.csv", "layers.Global2013.www2013"),
  scores = read.csv("scores.Global2013.www2013.csv", na.strings=""),
  spatial = file.path(wd, "spatial.www2013"),
  dir    = wd), launch.browser=T)