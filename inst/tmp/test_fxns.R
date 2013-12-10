setwd('/Users/bbest/Code/ohicore')
devtools::load_all()

# read from file (vs lazy loading data)
scores_www = read.csv('inst/extdata/scores.Global2013.www2013.csv', na.strings='')
layers     = Layers(layers.csv = 'inst/extdata/layers.Global2012.www2013.csv', 
                layers.dir = 'inst/extdata/layers.Global2012.www2013')
conf       = Conf('inst/extdata/conf.Global2013.www2013')

# run function
scores = conf$functions$ICO(layers)

# compare with scores published on website
v = merge(scores,
          rename(subset(scores_pub, goal=='ICO' & dimension %in% c('status','trend')), c('score'='score_www')))
subset(v, score!=score_www)
