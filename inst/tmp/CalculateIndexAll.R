debug=T
setwd('~/Code/ohicore')
load_all()
options(warn=2)

# load layers and configuration
scenario = 'Global2013.www2013'
scores_www = read.csv(sprintf('inst/extdata/scores.%s.csv', scenario), na.strings='')
layers     = Layers(layers.csv = sprintf('inst/extdata/layers.%s.csv', scenario), 
                    layers.dir = sprintf('inst/extdata/layers.%s'    , scenario))
conf       = Conf(sprintf('inst/extdata/conf.%s', scenario))

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

# pre-Index functions: Status and Trend, by goal
goals.X = subset(conf$goals, !is.na(preindex_function))
goals.X = goals.X[order(nchar(goals.X$goal), decreasing=T),] # order by length of goal id so subgoals first
for (i in 1:nrow(goals.X)){ # i=2
  
  # calculate Status and Trend
  scores.X = eval(parse(text=goals.X$preindex_function[i]), envir=conf$functions)  
  
  # bind to other goal scores
  scores = rbind(scores, scores.X)
}

# Goal Score and Likely Future
goals.G = as.character(unique(subset(scores, dimension=='status', goal, drop=T)))
for (g in goals.G){ # g = goals.G[1]
  
  if (debug) print(g)
  
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
  
  if (debug) print(names(scores.G))
  
  # bind to other scores
  scores = rbind(scores, scores.G)
}

# post-Index functions: supragoals
goals.Y = subset(conf$goals, !is.na(postindex_function))
goals.Y = goals.Y[order(nchar(goals.Y$goal), decreasing=T),] # order by length of goal id so subgoals first
for (i in 1:nrow(goals.Y)){ # i=2
  
  if (debug) print(goals.Y$goal[i])
  
  # run function
  scores.Y = eval(parse(text=goals.Y$postindex_function[i]), envir=conf$functions)  
  
  if (debug) print(names(scores.Y))
  
  # bind to other goal scores
  scores = rbind(scores, scores.Y)
}

# regional Index Score by goal weights
browser()
supragoals = subset(conf$goals, is.na(parent), goal, drop=T); supragoals
data.i = merge(subset(scores, dimension=='score' & goal %in% supragoals),
               conf$goals[,c('goal','weight')])
scores.I = ddply(
  data.i, .(region_id), summarize,
  goal='Index',
  dimension='score',
  score = round(weighted.mean(score, weight, na.rm=T), 2)); head(scores.I)
scores = rbind(scores, scores.I)
browser()

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

# cleanup factors
scores$dimension = factor(as.character(scores$dimension))

# final
#print(table(scores[,c('dimension','goal')]))

rgns_global      = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T)
rgns_s_islands   = subset(merge(rgns_global, SelectLayersData(layers, layers='rnk_rgn_georegions', narrow=T)), category=='r2' & val_num==999); rgns_s_islands
rgns_population  = ddply(SelectLayersData(layers, layers='rny_le_popn', narrow=T), .(id_num), summarize,
                         count = val_num[which.max(year)])
rgns_unpopulated = subset(merge(rgns_global, rgns_population, all.x=T), count==0 | is.na(count), c(id_num, val_chr)); rgns_unpopulated

# # get Southern islands for Goals_no_gapfill_Aug30_2013.xlsx, tab:"Southern island gapfill"
# s.islands = subset(read.csv(file.path(root.data,'model/GL-NCEAS-OceanRegions_v2013a/manual_output/rgn_georegions_wide_2013c_Antarctica.csv'), na.strings=''),
#                    r2==999, rgn_id, drop=T)
# rgn.unpop = subset(read.csv(file.path(root.data,'model/GL-WorldBank-Statistics_v2012/data/rgn_wb_pop_2013a_updated.csv'), na.strings=''),
#                    count==0, rgn_id, drop=T); rgn.unpop
# 
# # Southern islands overwrite    
# if (g %in% c('LIV','ECO','TR')){
#   rgn.na = c(s.islands, rgn.unpop)
#   cat(sprintf('  S Islands overwrite for %s.{s|t}.%s to NA from: %s\n', g, s, paste(d[d$rgn_id %in% rgn.na, sprintf('%s.s.%s',g,s)], collapse=',')))
#   d[d$rgn_id %in% rgn.na, sprintf('%s.s.%s',g,s)] = NA
#   d[d$rgn_id %in% rgn.na, sprintf('%s.t.%s',g,s)] = NA
# }


# compare with scores published on website
v = merge(scores,
          rename(scores_www, c('score'='score_www')), all=T)
print(all.equal(v$score, v$score_www))
print(table(v[!is.na(v$score), c('goal','dimension')]) - table(v[!is.na(v$score_www), c('goal','dimension')]))
print(v[is.na(v$score) != is.na(v$score_www) & v$goal=='CP',], row.names=F)
print(table(scores[duplicated(scores[,c('region_id','goal','dimension')]), c('goal','dimension')]))

#sum(duplicated(scores[,c('region_id','goal','dimension')]))

#        dimension
# goal    future pressures resilience score status trend
#   CP         0         0          0     0      1     1  # Antarctica (region_id=213)
#   CW         0         0          0     0     20     1
#   ECO        7         0          0     7     16    16
#   HAB        0         0          0     0      1     1
#   ICO        0         0          0     0      2     2
#   LIV        7         0          0     7     16    16
#   LSP        0         0          0     0      2    49
#   SPP        0         0          0     0     20    20
#   FP         0       206        206     0      0     0
#   LE         7       211        220     7     16    16
#   SP         0       440        440     0     22    46
#   Index   -221         0          0     0      0     0
#   BD      -221         0          0  -221   -221  -220


# TODO: Antarctica (region_id=213) to NA: CP
# TODO: LIV/ECO to NA: unpopulated places