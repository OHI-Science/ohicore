#setwd('~/Code/ohicore')
library(devtools)
load_all()

# load layers and conf ----
scenario = 'Global2013.www2013'
conf       = Conf(sprintf('inst/extdata/conf.%s', scenario))
layers     = Layers(layers.csv = sprintf('inst/extdata/layers.%s.csv', scenario), 
                    layers.dir = sprintf('inst/extdata/layers.%s'    , scenario))


# calculate scores ----
scores = CalculateAll(conf, layers, debug=T)
#scores.0 = scores
write.csv(scores, sprintf('inst/extdata/scores.%s.csv', scenario), na='', row.names=F)

# compare scores ----

# convert strings to factors
scores = with(scores, data.frame(goal=as.character(goal), dimension=as.character(dimension), region_id=region_id, score=score, stringsAsFactors=F))
scores_www = read.csv(sprintf('inst/extdata/scores.%s.csv', scenario), na.strings='', stringsAsFactors=F)
v = merge(scores,
          rename(scores_www, c('score'='score_www')), all=T)
v$score_dif = with(v, score - score_www)
v_dif = v[!is.na(v$score) & !is.na(v$score_www) & (abs(v$score_dif) > 0.1),]

# print comparisons
print(all.equal(v$score, v$score_www))

print(table(scores[,c('goal','dimension')]) - table(scores_www[,c('goal','dimension')]), zero.print='.')
print(table(v[!is.na(v$score), c('goal','dimension')]) - table(v[!is.na(v$score_www), c('goal','dimension')]), zero.print='.')
print(v[is.na(v$score) != is.na(v$score_www), ])
print(table(v_dif[,c('goal','dimension')]))
print(v_dif, row.names=F)

print(sum(duplicated(scores[,c('region_id','goal','dimension')])))


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
w = dcast(z, region_id + goal ~  dimension + source, value.var='score', 
          subset=.(goal=='LIV' & region_id!=0)); head(w)
print(w, row.names=F)