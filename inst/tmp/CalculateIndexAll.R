load_all()

# defaults
#conf=conf.Global2013.www2013
layers=layers.Global2013.www2013
#scores = scores.Global2013.www2013; head(scores)
#table(scores.Global2013.www2013$data[,c('goal','dimension')])
#subset(scores.Global2013.www2013$data, region_id==0)
#scores = subset(scores.Global2013.www2013$data, region_id!=0)

# update configuration
dir_conf = '/Users/bbest/Code/ohicore/inst/extdata/conf.Global2013.www2013'
conf = Conf(dir_conf)

# Pressures, all goals
scores.P = CalculatePressuresAll(layers, conf, gamma=conf$config$pressures_gamma)

# Resilience, all goals
scores.R = CalculateResilienceAll(layers, conf)

# Status and Trend, by goal
if (exists('scores.X')) rm('scores.X')
goals.X = subset(conf$goals, !is.na(function_call))
goals.X = goals.X[order(nchar(goals.X$id), decreasing=T),] # order by length of goal id so subgoals first
for (i in 1:nrow(goals.X)){ # i=2
  
  # calculate Status and Trend
  x = eval(parse(text=goals.X$function_call[i]), envir=conf$functions)  
  
  # bind to other goal scores
  if (exists('scores.X')){
    scores.X = rbind(scores.X, x)
  } else {
    scores.X = x
  }
}

# Goal Score and Likely Future
if (exists('scores.G')) rm('scores.G')
scores = rbind(scores.P, scores.R, scores.X); head(scores)
goals.G = as.character(unique(subset(scores, dimension=='status', goal, drop=T)))
for (g in goals.G){ # g = goals.G[1]
  
  # cast data
  data.g = dcast(
    scores, 
    region_id ~ dimension, 
    subset = .(goal==g),
    value.var='score')

  # calculate Goal Score and Likely Future
  x = CalculateGoalIndex(
    id         = data.g$region_id,
    status     = data.g$status,
    trend      = data.g$trend,
    resilience = data.g$resilience,
    pressure   = data.g$pressure,
    DISCOUNT      = conf$config$goal_discount, 
    BETA          = conf$config$goal_beta, 
    default_trend = conf$config$default_trend)
  
  # melt to scores format
  y = melt(rename(x[,c('id','xF','score')], 
                  c('id'='region_id', 
                    'xF'='future')), 
           id.vars='region_id', 
           variable.name='dimension',
           value.name='score')
  y$goal = g
  
  # bind to other goal scores
  if (exists('scores.G')){
    scores.G = rbind(scores.G, y)
  } else {
    scores.G = y
  }
}

# regional Index Score by goal weights
scores = rbind(scores.P, scores.R, scores.X, scores.G); head(scores); table(scores[,c('goal','dimension')])
supragoals = subset(conf$goals, is.na(parent), goal, drop=T); supragoals
data.i = merge(subset(scores, dimension=='score' & goal %in% supragoals),
               conf$goals[,c('goal','weight')])
scores.I = ddply(
  data.i, .(region_id), summarize,
  goal='Index',
  dimension='score',
  score = round(weighted.mean(score, weight, na.rm=T), 2)); head(scores.I)

# global (region_id-0) scores by area weighting
scores = rbind(scores.P, scores.R, scores.X, scores.G, scores.I); head(scores); table(scores[,c('dimension','goal')])
region_areas = rename(SelectLayersData(layers, layers=conf$config$layer_region_areas, narrow=T), 
                      c('id_num'='region_id','val_num'='area')); head(region_areas)
data.0 = merge(subset(scores, dimension %in% c('score','status','future')), 
               region_areas); head(data.0)
scores.0 = ddply(
  data.0, .(goal, dimension), summarize,
  region_id = 0,
  score = round(weighted.mean(score, area, na.rm=T), 2))

# final
scores = rbind(scores.P, scores.R, scores.X, scores.G, scores.I, scores.0); head(scores); table(scores[,c('dimension','goal')])





