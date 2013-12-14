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


# DEBUG: NP ----
# scores_old = rbind(cbind(rename(read.csv('/Volumes/local_edit/src/toolbox/scenarios/global_2013a/results/NP_status_2013a.csv', na.strings=''),
#                                 c('rgn_id'='region_id', 'status'='score_old')), 
#                          goal='NP', dimension='status'),
#                    cbind(rename(read.csv('/Volumes/local_edit/src/toolbox/scenarios/global_2013a/results/NP_trend_2013a.csv', na.strings=''),
#                                 c('rgn_id'='region_id', 'trend'='score_old')), 
#                          goal='NP', dimension='trend'))
# scores_old$score_old = round(scores_old$score_old, 2)
# x = merge(rename(scores_www, c(score='score_www')), scores_old); head(x)
# x = merge(x, scores)
# x$score = round(x$score, 2)
# x = within(x, {score_dif = score - score_www}); head(x)
# print(subset(x, score_dif > 0.02), row.names=F)
# browser()
# head(x)

# Goal Score and Likely Future
goals.G = as.character(unique(subset(scores, dimension=='status', goal, drop=T)))
for (g in goals.G){ # g = goals.G[9]
  
  # cast data
  v = dcast(
    scores, 
    region_id ~ dimension, 
    subset = .(goal==g),
    value.var='score')

  # calculate Goal Score and Likely Future
  print(g)
  print(summary(v[,c('status','trend','resilience','pressures')]))
  
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
  scores.G = melt(rename(x[,c('id','xF','score')], 
                  c('id'='region_id', 
                    'xF'='future')), 
           id.vars='region_id', 
           variable.name='dimension',
           value.name='score')
  scores.G$goal = g
  
  if (debug) cat(sprintf('pre-Index %s: %d\n', g, nrow(scores.G)))
  
  # bind to other scores
  scores = rbind(scores, scores.G)
}

# post-Index functions: supragoals
goals.Y = subset(conf$goals, !is.na(postindex_function))
goals.Y = goals.Y[order(nchar(goals.Y$goal), decreasing=T),] # order by length of goal id so subgoals first
for (i in 1:nrow(goals.Y)){ # i=2
  
  # run function
  scores = eval(parse(text=goals.Y$postindex_function[i]), envir=conf$functions)    
}

# regional Index score by goal weights
supragoals = subset(conf$goals, is.na(parent), goal, drop=T); supragoals
data.i = merge(subset(scores, dimension=='score' & goal %in% supragoals),
               conf$goals[,c('goal','weight')])
scores.I = ddply(
  merge(subset(scores, dimension=='score' & goal %in% supragoals),
    conf$goals[,c('goal','weight')]), 
  .(region_id), summarize,
  goal='Index',
  dimension='score',
  score = weighted.mean(score, weight, na.rm=T))
scores = rbind(scores, scores.I)

# regional Future score by goal weights
scores.F = ddply(
  merge(subset(scores, dimension=='future' & goal %in% supragoals),
        conf$goals[,c('goal','weight')]), 
  .(region_id), summarize,
  goal='Index',
  dimension='future',
  score = weighted.mean(score, weight, na.rm=T))
scores = rbind(scores, scores.F)

# post-process scores, but pre-global calculation
scores = conf$functions$PreGlobalScores(layers, conf, scores)

# global (region_id-0) scores by area weighting
# NOTE: Index.future.0 (goal.dimension.region_id) is now 65.61 vs previous 64.71 which was just the mean, not area weighted
region_areas = rename(SelectLayersData(layers, layers=conf$config$layer_region_areas, narrow=T), 
                      c('id_num'='region_id','val_num'='area')); head(region_areas); subset(region_areas, region_id==213)
data.0 = merge(subset(scores, dimension %in% c('score','status','future')), 
               region_areas); head(data.0)
scores.0 = ddply(
  data.0, .(goal, dimension), summarize,
  region_id = 0,
  score = weighted.mean(score, area, na.rm=T))
scores = rbind(scores, scores.0)

# post-process
scores = conf$functions$FinalizeScores(layers, conf, scores)



# Compare with scores published on website ----
scores_www = read.csv(sprintf('inst/extdata/scores.%s.csv', scenario), na.strings='', stringsAsFactors=F)
v = merge(scores,
          rename(scores_www, c('score'='score_www')), all=T)
v$score_dif = with(v, score - score_www)

# print comparisons
print(all.equal(v$score, v$score_www))
print(v[is.na(v$score) != is.na(v$score_www), ])
#v_dif = v[!is.na(v$score) & !is.na(v$score_www) & (abs(v$score_dif) > 0.1),]
print(subset(v_dif, !goal %in% c('NP','Index')), row.names=F)
print(v_dif, row.names=F)
browser()

c_dif = dcast(subset(v, goal=='NP'), region_id ~ dimension, value.var='score_dif'); print(c_dif[,c('region_id','status','trend','future','score')], row.names=F)

nrow(v_dif)
print(table(v_dif[,c('goal','dimension')]))


dim(scores)

browser()

print(v[is.na(v$score) != is.na(v$score_www), ])
print(table(v[!is.na(v$score), c('goal','dimension')]) - table(v[!is.na(v$score_www), c('goal','dimension')]), zero.print='.')
print(table(scores[,c('goal','dimension')]) - table(scores_www[,c('goal','dimension')]), zero.print='.')

print(table(scores_www[,c('goal','dimension')]), zero.print='.')

print(table(scores[,c('goal','dimension')]), zero.print='.')


table(subset(v, (is.na(score) != is.na(score_www)) & dimension=='status', c(region_id, goal)))
v[(is.na(v$score) != is.na(v$score_www)) & dimension=='status', c('region_id','goal')]

# Antarctica: CP,CW,HAB,ICO,LSP,SPP,SP,BD
# fao: CW,SPP,LSP,SP,BD

subset(v, region_id==213)

# Problematic: NP trend
print(dcast(v, goal ~ dimension, mean, value.var='score_dif'), digits=4, row.names=F)

w = ddply(v, .(goal, dimension), summarize,
          score_dif_min = ifelse(length(na.omit(score_dif)) > 0, min(score_dif, na.rm=T), NA),
          score_dif_max = ifelse(length(na.omit(score_dif)) > 0, max(score_dif, na.rm=T), NA),
          score_dif_avg = ifelse(length(na.omit(score_dif)) > 0, mean(score_dif, na.rm=T), NA),
          score_dif_cnt = sum(abs(na.omit(score_dif)) > 0.02))

print(subset(arrange(w, score_dif_cnt, goal, dimension), score_dif_cnt > 1 & dimension %in% c('status','trend')), row.names=F)
#  goal dimension score_dif_min score_dif_max score_dif_avg score_dif_cnt
#    NP     trend         -0.18          0.53 -0.0006134969            20
print(tail(arrange(subset(v, goal=='NP' & dimension=='trend' & abs(score_dif) > 0.01), abs(score_dif))), row.names=F)
#  region_id goal dimension score score_www score_dif
#         43   NP     trend -0.53     -0.37     -0.16
#         41   NP     trend -0.18      0.00     -0.18
#         15   NP     trend  0.83      0.30      0.53
browser()

print(arrange(w, score_dif_cnt, goal, dimension), row.names=F)
#   goal  dimension score_dif_min score_dif_max score_dif_avg score_dif_cnt
#     CP     future          0.00          0.05  6.285714e-04             1
#     CP      score         -0.01          0.04  2.285714e-04             1
#     CP     status          0.00          2.30  1.314286e-02             1
#     CS     future         -0.01          0.03  4.794521e-04             1
#     CW     future         -0.01          0.02  2.714932e-04             1
#     CW     status          0.00         13.12  5.936652e-02             1
#    HAB     status          0.00          0.82  3.744292e-03             1
#    LSP     status         -3.71          0.00 -1.678733e-02             1
#    MAR     future          0.00          0.05  4.166667e-04             1
#    MAR      score          0.00          0.05  4.166667e-04             1
#    MAR     status          0.00          0.05  4.166667e-04             1
#     NP     status         -0.03          0.00 -1.829268e-04             1
#    SPP     status         -2.14          0.00 -9.683258e-03             1
#     SP     status         -0.57          0.01 -2.352941e-03             1
#     BD     future         -0.01          0.02  1.357466e-04             1
#     BD     status         -3.22          0.00 -1.457014e-02             1
#  Index      score         -1.02          0.97 -6.832579e-03            28
#  Index     future         -2.04          1.93 -9.773756e-03            36
#     NP      trend         -0.18          0.53 -6.134969e-04            20
#     NP      score         -2.28          1.48 -1.689024e-02            38
#     NP     future         -4.58          2.94 -3.365854e-02            50
#    ICO     status          0.00          2.62  1.343590e-02             1
#    ICO      score         -0.10          0.12  7.179487e-04            89
#    ICO     future         -0.20          0.24  1.743590e-03            90
#    LSP      score        -16.34         19.37 -5.873303e-02            13
#    LSP     future        -32.68         38.74 -1.174208e-01            14
#    LSP      trend         -1.00          1.00 -1.691860e-02            26
#     SP      trend         -1.00          0.50  1.259615e-02            58
#     SP      score         -8.17          9.68 -3.873303e-02            84
#     SP     future        -16.33         19.37 -7.737557e-02            98


#print(subset(v, goal=='HAB' & dimension %in% c('status','trend','pressures','resilience') & score_dif!=0))

#subset(v, goal=='HAB' & dimension %in% c('status','trend','pressures','resilience') & score_dif>0.001)

#print(dcast(v, goal ~ dimension, mean, na.rm=T, value.var='score'))
#print(dcast(v, goal ~ dimension, mean, na.rm=T, value.var='score_www'))

#subset(v, goal=='CS' & dimension %in% c('status','trend','pressures','resilience') & score_dif>0.001)
#sweep(v, c(1,2), mean, na.rm=T)

z = with(
  v, rbind(data.frame(
    region_id=region_id,
    goal = goal,
    dimension = dimension,
    score = score,
    source = 'calc'
  ), data.frame(
    region_id=region_id,
    goal = goal,
    dimension = dimension,
    score = score_www,
    source = 'www'
  )))

w = dcast(z, region_id + goal ~  dimension + source, value.var='score', subset=.(goal=='LIV' & region_id!=0)); head(w)
#print(w, row.names=F)
#subset(v, goal=='AO' & dimension=='trend')

#print(table(scores[duplicated(scores[,c('region_id','goal','dimension')]), c('goal','dimension')]))
#print(v[is.na(v$score) != is.na(v$score_www) & v$goal=='SP',], row.names=F)
#print(v[is.na(v$score) != is.na(v$score_www) & v$goal=='ECO',], row.names=F)
#subset(v, goal=='LIV' & dimension=='status' & region_id==213)


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