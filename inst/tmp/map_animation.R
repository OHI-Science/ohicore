# get map data
library(maptools)
library(maps)
library(dplyr)
library(RColorBrewer)

# vars
regions_g8 = c('United States','United Kingdom','China','France','Australia','Canada','South Africa','Russia')
shp = 'inst/extdata/spatial.www2013/shp/regions_gcs.shp'

# setup temporary output folder
dir_fig = 'inst/tmp/map_animation_fig'
dir.create(dir_fig, showWarnings=F)

# read shp which extends beyond 180 limit
set_ll_warn(FALSE)
set_ll_TOL(0.01)
rgn = readShapePoly(shp, proj4string=CRS('+proj=longlat'))

# get sores and regions
d = read.csv('/Volumes/data_edit/git-annex/Global/NCEAS-OHI-Scores-Archive/scores/scores.Global2013.www2013_2014-05-30.csv', na.strings='')

region_names = read.csv('inst/extdata/layers.Global2013.www2013/rgn_labels.csv', na.strings='') %.%
  filter(type=='eez') %.%
  select(region_id=rgn_id, name=label)

# limit to region scores
v = d %.%
  filter(goal=='Index' & dimension=='score' & region_id!=0 & !is.na(score)) %.%
  select(region_id, score) %.%
  merge(region_names)

# get subsets of data
v_g8 = v %.%
  inner_join(data.frame(
    name     = regions_g8, 
    seq      = 1:length(regions_g8), 
    cat_key  = 'g8',
    cat_name = 'G8'), by='name') %.%
  arrange(seq)
v_top10 = v %.%
  anti_join(v_g8, by='name') %.%
  arrange(desc(score)) %.%
  head(10) %.%
  mutate(
    seq      = 1:n(),
    cat_key  = 'top10',
    cat_name = 'Top 10')
v_bot10 = v %.%
  anti_join(v_g8, by='name') %.%
  arrange(score) %.%
  head(10) %.%
  mutate(
    seq      = 1:n(),
    cat_key  = 'bot10',
    cat_name = 'Bottom 10')
v_rest = v %.%
  anti_join(v_g8, by='name') %.%
  anti_join(v_top10, by='name') %.%
  anti_join(v_bot10, by='name') %.%
  arrange(desc(score)) %.%
  mutate(
    seq      = 1:n(),
    cat_key  = 'rest',
    cat_name = 'Rest (top to bottom)')
v_a = rbind_all(list(v_g8, v_top10, v_bot10, v_rest))

map_fig = function(fname, dat=NULL, text_left='', text_center='', text_right=''){
  png(file=fname, width=800, height=400, pointsize=16) # , type='cairo')
  par(oma=c(0,0,0,0),
      mar=c(0,0,0,0))
  
  col_brks = seq(0,100,length.out=11)  
  map('world', col='gray80', border='grey60', fill=T)  
  
  if (is.null(dat)){
    plot(rgn, border='grey40', add=T)
  } else {
    plot(rgn, border='grey40', add=T,
         col=brewer.pal(10, 'RdYlBu')[cut(dat[match(rgn@data$rgn_id, dat$region_id), 'score'], col_brks, labels=1:10)])
  }  
  
  # get plotting dimensions
  p=par('usr'); px=diff(p[1:2]); py=diff(p[3:4]) # c(x1, x2, y1, y2)
  
  # add label
  mtext(text_center, side=3, line=0, cex=1)
  mtext(text_left  , side=3, line=0, cex=0.7, adj=0)
  mtext(text_right , side=3, line=0, cex=0.7, adj=1)
  
  # plot legend
  yh=0.05; x1=p[1]+px*0.2; x2=p[2]-px*0.2; y1=p[3]; y2=p[3]+py*yh
  ix = seq(x1, x2, length.out=10)
  ixd = diff(ix)[1]/2
  par(xpd=TRUE) # turn off clipping to plot region
  image(x = ix,
        y = c(y1, y2),
        z = matrix(1:10), col=brewer.pal(10, 'RdYlBu'), add=T)
  rect(x1-ixd,y1,x2+ixd,y2, border='gray40')
  text(x = seq(x1-ixd, x2+ixd, length.out=11),
       y = y1, cex=0.6, pos=1, # adj=c(0.5,0), # , offset=0.1,
       labels=as.character(col_brks))
  
  dev.off()
}

map_fig(sprintf('%s/gl2013_beg_001.png', dir_fig))

# make map images
for (i in 1:nrow(v_a)){ # i=1
  dat        = v_a[1:i,]
  text_right = sprintf('%d/%d', i, nrow(v_a))
  with (v_a[i,], {
    map_fig(
      fname = sprintf('%s/gl2013_%s_%03d.png', dir_fig, cat_key, seq),
      dat   = dat,
      text_left   = cat_name, 
      text_center = ifelse(cat_key!='rest', sprintf('%s: %d', name, round(score)), ''), 
      text_right  = text_right)
  })
}

# make movies at specified frames per second (fps)
fps = c(beg='1/1.5', g8='1/1.5', top10='1', bot10='1', rest='10')
for (i in 1:length(fps)){ # i=2
  sfx = names(fps)[i]
  cmd = sprintf('ffmpeg -y -r %s -i %s/gl2013_%s_%%03d.png -r 10 -f mp4 -vcodec h264 -pix_fmt yuv420p %s/gl2013_%s.mp4', fps[i], dir_fig, sfx, dir_fig, sfx)
  system(cmd)  
}

# paste movies together
txt = file.path(dir_fig, 'gl2013_videos.txt')
vid = sprintf('%s/gl2013_animation.mp4', dirname(dir_fig))
writeLines(sprintf('file gl2013_%s.mp4', names(fps)), txt)
system(sprintf('ffmpeg -y -f concat -i %s -c copy %s', txt, vid))

# remove temporary files
unlink(dir_fig, recursive=T)