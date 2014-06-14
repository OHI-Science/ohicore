# borrowing from ohicore::ReportScores() which sets up and uses inst/report/report.Rmd

library(devtools)
library(RColorBrewer)
load_all()
conf   = Conf('../ohi-global/eez2013/conf')
scores = scores.Global2013.www2013
wts = read.csv('inst/tmp/flowers_weights.csv')

dir_fig = 'inst/tmp/flowers'
dir.create(dir_fig, showWarnings=F)

# get goals for flowers without subgoals
goals = conf$goals %.%
  filter(is.na(parent)) %.%
  arrange(order_color) %.%
  mutate(name_flower = gsub('\\\\n', '\n', name_flower))
n = nrow(goals) # 10

# get colors for aster, based on 10 colors, but extended to all goals. subselect for goals.wts
cols = brewer.pal(n, 'Spectral')

# region scores    
x = scores %.%
  filter(region_id==0 & dimension=='score' & goal %in% goals$goal) %.%
  join(goals, by='goal') %.%
  arrange(order_color) %.%
  select(goal, score)

pdf(sprintf('%s/flower_1-pie.pdf', dir_fig))
PlotFlower(
  main       = 'Ocean Health?',
  lengths    = 100,
  widths     = 1,
  fill.col   = 'black',
  labels     = '',
  center     = '',
  disk       = 0, 
  max.length = 100, label.cex=0.9, label.offset=0.155, cex=2.2, cex.main=2.5)
dev.off()

pdf(sprintf('%s/flower_2-goals.pdf', dir_fig))
PlotFlower(
  main       = 'Ocean Health Goals',
  lengths    = rep(100, n),
  widths     = goals$weight,
  fill.col   = cols,
  labels     = paste(goals$name_flower, '', sep='\n'),
  center     = '',
  disk       = 0, 
  max.length = 100, label.cex=0.9, label.offset=0.155, cex=2.2, cex.main=2.5)
dev.off()

pdf(sprintf('%s/flower_3-scores.pdf', dir_fig))
PlotFlower(
  main       = 'Ocean Health Goal Scores',
  lengths    = x$score,
  widths     = goals$weight,
  fill.col   = cols,
  labels     = paste(goals$name_flower, round(x$score), sep='\n'),
  center     = '',
  disk       = 0, 
  max.length = 100, label.cex=0.9, label.offset=0.155, cex=2.2, cex.main=2.5)
dev.off()
system(sprintf('open %s/flower_3-scores.pdf', dir_fig))

pdf(sprintf('%s/flower_4-index.pdf', dir_fig))
PlotFlower(
  main       = 'Ocean Health Index',
  lengths    = x$score,
  widths     = goals$weight,
  fill.col   = cols,
  labels     = paste(goals$name_flower, round(x$score), sep='\n'),
  center     = round(weighted.mean(x$score, goals$weight, na.rm=T)),
  disk       = 0.4, 
  max.length = 100, label.cex=0.9, label.offset=0.155, cex=2.2, cex.main=2.5)
dev.off()

pdf(sprintf('%s/flower_5-preservationist.pdf', dir_fig))
w = wts[match(goals$goal, wts$goal),'preservationist']
PlotFlower(
  main       = 'Ocean Health Index',
  lengths    = x$score,
  widths     = w,
  fill.col   = cols,
  labels     = paste(goals$name_flower, round(x$score), sep='\n'),
  center     = round(weighted.mean(x$score, w, na.rm=T)),
  disk       = 0.4, 
  max.length = 100, label.cex=0.9, label.offset=0.155, cex=2.2, cex.main=2.5)
dev.off()
system(sprintf('open %s/flower_5-preservationist.pdf', dir_fig))

pdf(sprintf('%s/flower_5-extractive.pdf', dir_fig))
w = wts[match(goals$goal, wts$goal),'extractive']
PlotFlower(
  main       = 'Ocean Health Index',
  lengths    = x$score,
  widths     = wts[match(goals$goal, wts$goal),'extractive'],
  fill.col   = cols,
  labels     = paste(goals$name_flower, round(x$score), sep='\n'),
  center     = round(weighted.mean(x$score, w, na.rm=T)),
  disk       = 0.4, 
  max.length = 100, label.cex=0.9, label.offset=0.155, cex=2.2, cex.main=2.5)
dev.off()
system(sprintf('open %s/flower_5-extractive.pdf', dir_fig))