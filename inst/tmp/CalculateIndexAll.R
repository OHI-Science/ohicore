load_all()

# load layers and configuration
layers=layers.Global2013.www2013
conf = Conf('/Users/bbest/Code/ohicore/inst/extdata/conf.Global2013.www2013')

# Pressures, all goals
#scores.P = CalculatePressuresAll(layers, conf, gamma=conf$config$pressures_gamma)  # DEBUG
#write.csv(scores.P, '~/Downloads/scores.P.csv', na='', row.names=F)
scores.P = read.csv('~/Downloads/scores.P.csv', na.strings='')
scores = scores.P

# Resilience, all goals
#scores.R = CalculateResilienceAll(layers, conf) # DEBUG
#write.csv(scores.R, '~/Downloads/scores.R.csv', na='', row.names=F)
scores.R = read.csv('~/Downloads/scores.R.csv', na.strings='')
scores = rbind(scores, scores.R)

# Status and Trend, by goal
goals.X = subset(conf$goals, !is.na(function_call))
goals.X = goals.X[order(nchar(goals.X$goal), decreasing=T),] # order by length of goal id so subgoals first
for (i in 1:nrow(goals.X)){ # i=2
  
  # calculate Status and Trend
  scores.X = eval(parse(text=goals.X$function_call[i]), envir=conf$functions)  
  
  # bind to other goal scores
  scores = rbind(scores, scores.X)
}

# Goal Score and Likely Future
goals.G = as.character(unique(subset(scores, dimension=='status', goal, drop=T)))
for (g in goals.G){ # g = goals.G[1]
  
  # cast data
  v = dcast(
    scores, 
    region_id ~ dimension, 
    subset = .(goal==g),
    value.var='score')

  # calculate Goal Score and Likely Future
  x = CalculateGoalIndex(
    id         = v$region_id,
    status     = v$status,
    trend      = v$trend,
    resilience = v$resilience,
    pressure   = v$pressure,
    DISCOUNT      = conf$config$goal_discount, 
    BETA          = conf$config$goal_beta, 
    default_trend = conf$config$default_trend)
  
  # melt to scores format
  scores.G = melt(rename(x[,c('id','xF','score')], 
                  c('id'='region_id', 
                    'xF'='future')), 
           id.vars='region_id', 
           variable.name='dimension',
           value.name='score')
  scores.G$goal = g
  
  # bind to other scores
  scores = rbind(scores, scores.G)
}

# regional Index Score by goal weights
supragoals = subset(conf$goals, is.na(parent), goal, drop=T); supragoals
data.i = merge(subset(scores, dimension=='score' & goal %in% supragoals),
               conf$goals[,c('goal','weight')])
scores.I = ddply(
  data.i, .(region_id), summarize,
  goal='Index',
  dimension='score',
  score = round(weighted.mean(score, weight, na.rm=T), 2)); head(scores.I)
scores = rbind(scores, scores.I)

# global (region_id-0) scores by area weighting
region_areas = rename(SelectLayersData(layers, layers=conf$config$layer_region_areas, narrow=T), 
                      c('id_num'='region_id','val_num'='area')); head(region_areas)
data.0 = merge(subset(scores, dimension %in% c('score','status','future')), 
               region_areas); head(data.0)
scores.0 = ddply(
  data.0, .(goal, dimension), summarize,
  region_id = 0,
  score = round(weighted.mean(score, area, na.rm=T), 2))
scores = rbind(scores, scores.0)

# final
print(table(scores[,c('dimension','goal')]))


# debug compare
#conf=conf.Global2013.www2013
#scores = scores.Global2013.www2013; head(scores)
#table(scores.Global2013.www2013$data[,c('goal','dimension')])
#subset(scores.Global2013.www2013$data, region_id==0)
#scores = subset(scores.Global2013.www2013$data, region_id!=0)