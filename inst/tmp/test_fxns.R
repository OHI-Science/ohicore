# setwd('/Users/bbest/Code/ohicore')
devtools::load_all()
# 
# # read from file (vs lazy loading data)
# scenario = 'Global2013.www2013'
# scores_www = read.csv(sprintf('inst/extdata/scores.%s.csv', scenario), na.strings='')
layers     = Layers(layers.csv = sprintf('inst/extdata/layers.%s.csv', scenario), 
                    layers.dir = sprintf('inst/extdata/layers.%s'    , scenario))
conf       = Conf(sprintf('inst/extdata/conf.%s', scenario))

# run function
g = 'TR'
scores = conf$functions[[g]](layers)

# compare with scores published on website
v = merge(scores,
          rename(subset(scores_www, goal==g & dimension %in% c('status','trend') & region_id!=0), 
                 c('score'='score_www')), all=T)
print(all.equal(v$score, v$score_www))
print(v[is.na(v$score) != is.na(v$score_www),], row.names=F)

# TODO: Antarctica (region_id=213) to NA: CP