require(ohicore)

wd = "~/Documents/ohigit/ohicore/inst/extdata"
scenario=list(
  conf   = ohicore::Conf(file.path(wd, "conf.Canada-CHONe2014")),
  layers = ohicore::Layers(file.path(wd, "layers.Canada-CHONe2014.csv"), file.path(wd, "layers.Canada-CHONe2014")),
  scores = read.csv(file.path(wd, "scores.Canada-CHONe2014.csv"), na.strings=""),
  spatial = file.path(wd, "spatial.Canada-CHONe2014"),
  dir    = wd)
