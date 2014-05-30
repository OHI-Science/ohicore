library(devtools)
load_all()

yr = 2012
scenario = sprintf('Global%d.www2013', yr)
conf     = Conf(sprintf('inst/extdata/conf.%s', scenario))
layers   = Layers(layers.csv = sprintf('inst/extdata/layers.%s.csv', scenario), 
                    layers.dir = sprintf('inst/extdata/layers.%s'    , scenario))

g='TR'
#scores = CalculateAll(conf, layers, debug=T) ----
if (exists('scores')){ rm(scores)}
preindex_function = subset(conf$goals, goal==g, preindex_function, drop=T)
scores.g = eval(parse(text=preindex_function), envir=conf$functions)
  
  if (exists('scores')){ 
    scores = rbind(scores, scores.g)
  } else {
    scores = scores.g
  }
  
  if (sprintf('%s_status_gapfill_georegions', g) %in% names(attributes(scores.g))){
    georgndebug.g = attr(scores.g, sprintf('%s_status_gapfill_georegions', g)) %.%
      mutate(
        goal=g,
        dimension='status')
    if (exists('georgndebug')){ 
      georgndebug = rbind(georgndebug, georgndebug.g)
    } else {
      georgndebug = georgndebug.g
    }
  }
}
