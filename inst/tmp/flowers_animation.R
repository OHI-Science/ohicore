# borrowing from ohicore::ReportScores() which sets up and uses inst/report/report.Rmd

library(devtools)
load_all()
conf   = ohicore::conf.Global2013.www2013
scores = ohicore::scores.Global2013.www2013

dir_fig = 'inst/tmp/flowers'
dir.create(dir_fig, showWarnings=F)

# get goals for flowers, all and specific to weights
goals.all = arrange(conf$goals, order_color)[['goal']]

# get colors for aster, based on 10 colors, but extended to all goals. subselect for goals.wts
cols.goals.all = colorRampPalette(RColorBrewer::brewer.pal(10, 'Spectral'), space='Lab')(length(goals.all))
names(cols.goals.all) = goals.all

# get subgoals and goals, not supragoals, for doing flower plot
goals_supra = na.omit(unique(conf$goals$parent))
wts = with(subset(conf$goals, !goal %in% goals_supra, c(goal, weight)), setNames(weight, goal))
goal_labels = gsub('\\n', '\n', with(conf$goals, setNames(name_flower, goal))[names(wts)], fixed=T)

rgn_id = 0 # global only

# region scores    
x = with(subset(scores, dimension=='score' & region_id==rgn_id & goal %in% names(wts)),
         setNames(score, goal))[names(wts)]

# customize flower scores # paste(sprintf('%s=%d', names(x), rep(0, length(x))), collapse=', ')
flowers = list(
  Global2013 = x,
  blank      = c(FIS=0, MAR=0, AO=0, NP=0, CS=0, CP=0 , TR=0, LIV=0, ECO=0, ICO=0, LSP=0, CW=0, HAB=0, SPP=0),
  CP69       = c(FIS=0, MAR=0, AO=0, NP=0, CS=0, CP=69, TR=0, LIV=0, ECO=0, ICO=0, LSP=0, CW=0, HAB=0, SPP=0))

for (i in 1:length(flowers)){
  
  # get flower spec
  rgn_name = names(flowers)[i]
  x = flowers[[i]]
  
  #fig_png = sprintf('%s/flower_%s.png', dir_fig, gsub(' ','_', rgn_name))
  #res=72; png(fig_png, width=res*7, height=res*7)
  fig_pdf = sprintf('%s/flower_%s.pdf', dir_fig, gsub(' ','_', rgn_name))
  pdf(fig_pdf)
  PlotFlower(main = rgn_name,
             lengths=x,
             widths=wts,
             fill.col=ifelse(is.na(x), 
                             'grey80', 
                             cols.goals.all[names(wts)]),
             labels  =ifelse(is.na(x), 
                             paste(goal_labels, '-', sep='\n'), 
                             paste(goal_labels, round(x), sep='\n')),
             center=round(weighted.mean(x, wts, na.rm=T)),
             max.length = 100, disk=0.4, label.cex=0.9, label.offset=0.155, cex=2.2, cex.main=2.5)
  dev.off()
  system(sprintf('open %s', fig_pdf))
  
}