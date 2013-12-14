debug=T
setwd('~/Code/ohicore')
load_all()

# load layers and configuration
scenario = 'Global2013.www2013'
layers     = Layers(layers.csv = sprintf('inst/extdata/layers.%s.csv', scenario), 
                    layers.dir = sprintf('inst/extdata/layers.%s'    , scenario))
conf       = Conf(sprintf('inst/extdata/conf.%s', scenario))

# Pressures, all goals
scores_P = CalculatePressuresAll(layers, conf, gamma=conf$config$pressures_gamma)  # DEBUG
#write.csv(scores_P, '~/Downloads/scores_P.csv', na='', row.names=F)
#scores_P = read.csv('~/Downloads/scores_P.csv', na.strings='', stringsAsFactors=F)
scores = scores_P

# Resilience, all goals
scores_R = CalculateResilienceAll(layers, conf) # DEBUG
#write.csv(scores_R, '~/Downloads/scores_R.csv', na='', row.names=F)
#scores_R = read.csv('~/Downloads/scores_R.csv', na.strings='', stringsAsFactors=F)
scores = rbind(scores, scores_R)

# pre-Index functions: Status and Trend, by goal
goals_X = subset(conf$goals, !is.na(preindex_function))
goals_X = goals_X[order(nchar(goals_X$goal), decreasing=T),] # order by length of goal id so subgoals first
for (i in 1:nrow(goals_X)){ # i=2
  
  # calculate Status and Trend
  scores = rbind(scores, eval(parse(text=goals_X$preindex_function[i]), envir=conf$functions))
  
  # bind to other goal scores
  scores = scores_X)
}

# Goal Score and Likely Future
goals_G = as.character(unique(subset(scores, dimension=='status', goal, drop=T)))
for (g in goals_G){ # g = goals_G[9]
  
  # cast data
  v = dcast(
    scores, 
    region_id ~ dimension, 
    subset = .(goal==g),
    value.var='score')

  # calculate Goal Score and Likely Future
  x = CalculateGoalIndex(
    id         = v$region_id,
    status     = v$status/100,
    trend      = v$trend,
    resilience = v$resilience/100,
    pressure   = v$pressure/100,
    DISCOUNT      = conf$config$goal_discount, 
    BETA          = conf$config$goal_beta, 
    default_trend = conf$config$default_trend)
  x$score = x$score * 100
  x$xF    = x$xF * 100
  
  # melt to scores format
  scores_G = melt(rename(x[,c('id','xF','score')], 
                  c('id'='region_id', 
                    'xF'='future')), 
           id.vars='region_id', 
           variable.name='dimension',
           value.name='score')
  scores_G$goal = g
  
  # bind to other scores
  scores = rbind(scores, scores_G)
}

# post-Index functions: supragoals
goals_Y = subset(conf$goals, !is.na(postindex_function))
goals_Y = goals_Y[order(nchar(goals_Y$goal), decreasing=T),] # order by length of goal id so subgoals first
for (i in 1:nrow(goals_Y)){ # i=2
  
  # run function
  scores = eval(parse(text=goals_Y$postindex_function[i]), envir=conf$functions)    
}

# regional Index score by goal weights
supragoals = subset(conf$goals, is.na(parent), goal, drop=T); supragoals
scores_I = ddply(
  merge(subset(scores, dimension=='score' & goal %in% supragoals),
    conf$goals[,c('goal','weight')]), 
  .(region_id), summarize,
  goal='Index',
  dimension='score',
  score = weighted.mean(score, weight, na.rm=T))
scores = rbind(scores, scores_I)

# regional Future score by goal weights
scores_F = ddply(
  merge(subset(scores, dimension=='future' & goal %in% supragoals),
        conf$goals[,c('goal','weight')]), 
  .(region_id), summarize,
  goal='Index',
  dimension='future',
  score = weighted.mean(score, weight, na.rm=T))
scores = rbind(scores, scores_F)

# post-process scores, but pre-global calculation
scores = conf$functions$PreGlobalScores(layers, conf, scores)

# global (region_id-0) scores by area weighting
# NOTE: Index.future.0 (goal.dimension.region_id) is now 65.61 vs previous 64.71 which was just the mean, not area weighted
region_areas = rename(SelectLayersData(layers, layers=conf$config$layer_region_areas, narrow=T), 
                      c('id_num'='region_id','val_num'='area')); head(region_areas); subset(region_areas, region_id==213)
data_0 = merge(subset(scores, dimension %in% c('score','status','future')), 
               region_areas); head(data_0)
scores_0 = ddply(
  data_0, .(goal, dimension), summarize,
  region_id = 0,
  score = weighted.mean(score, area, na.rm=T))
scores = rbind(scores, scores_0)

# post-process
scores = conf$functions$FinalizeScores(layers, conf, scores)


# # Compare with scores published on website ----
# scores_www = read.csv(sprintf('inst/extdata/scores.%s.csv', scenario), na.strings='', stringsAsFactors=F)
# v = merge(scores,
#           rename(scores_www, c('score'='score_www')), all=T)
# v$score_dif = with(v, score - score_www)
# v_dif = v[!is.na(v$score) & !is.na(v$score_www) & (abs(v$score_dif) > 0.1),]
# 
# # print comparisons
# print(all.equal(v$score, v$score_www))
# 
# print(table(scores[,c('goal','dimension')]) - table(scores_www[,c('goal','dimension')]), zero.print='.')
# print(table(v[!is.na(v$score), c('goal','dimension')]) - table(v[!is.na(v$score_www), c('goal','dimension')]), zero.print='.')
# print(v[is.na(v$score) != is.na(v$score_www), ])
# 
# print(table(v_dif[,c('goal','dimension')]))
# print(v_dif, row.names=F)
# 
# print(sum(duplicated(scores[,c('region_id','goal','dimension')])))
# 
# 
# z = with(
#   v, rbind(data.frame(
#     region_id=region_id,
#     goal = goal,
#     dimension = dimension,
#     score = score,
#     source = 'calc'
#   ), data.frame(
#     region_id=region_id,
#     goal = goal,
#     dimension = dimension,
#     score = score_www,
#     source = 'www'
#   )))
# w = dcast(z, region_id + goal ~  dimension + source, value.var='score', 
#           subset=.(goal=='LIV' & region_id!=0)); head(w)
# print(w, row.names=F)