# get map data
library(maptools)
library(maps)
library(dplyr)
library(RColorBrewer)
library(classInt)

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
  anti_join(v_g8   , by='name') %.%
  anti_join(v_top10, by='name') %.%
  anti_join(v_bot10, by='name') %.%
  arrange(desc(score)) %.%
  mutate(
    seq      = 1:n(),
    cat_key  = 'rest',
    cat_name = 'Rest (top to bottom)')
v_a = rbind_all(list(v_g8, v_top10, v_bot10, v_rest))

map_fig = function(fname, dat=NULL, text_left='', text_center='', text_right=''){
  dpi=300; png(file=fname, width=11*dpi, height=8.5*dpi, pointsize=16, res=dpi, type='cairo')
  #pdf(file=sprintf('%s.pdf', tools::file_path_sans_ext(fname)), width=11, height=8.5, pointsize=16)
  par(oma=c(0,0,0,0),
      mar=c(0,0,0,0))
  
  map('world', col='gray80', border='grey60', fill=T)  
  
  if (is.null(dat)){
    plot(rgn, border='grey40', add=T)
  } else {
    
    ncol = 10 # 11=max for brewer.pal
    cols = brewer.pal(ncol, 'RdYlBu')
    brks = classIntervals(na.exclude(dat$score), n=ncol, style='fisher')[['brks']]
    rgn_score = dat$score[match(rgn@data$rgn_id, dat$region_id)]
    plot(rgn, col=cols[ findInterval(rgn_score, brks, all.inside=T) ], border='grey40', add=T)

    # regions with data and no spatial feature: presumably lost b/c of simplification of shapefile
    region_names %.%
      inner_join(
        data.frame(region_id = setdiff(dat$region_id, rgn@data$rgn_id)),
        by='region_id') %.% print(row.names=F)
    #  region_id               name
    #    215                 Jordan
    #    232 Bosnia and Herzegovina
    setdiff(rgn@data$rgn_id, dat$region_id) # 255 = DISPUTED    
    
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
          z = matrix(1:10), col=cols, add=T)
    rect(x1-ixd,y1,x2+ixd,y2, border='gray40')
    text(x = seq(x1-ixd, x2+ixd, length.out=11),
         y = y1, cex=0.6, pos=1, # adj=c(0.5,0), # , offset=0.1,
         labels=as.character(round(brks)))
    
    dev.off()
  }
}

map_fig(
  fname       = sprintf('%s/gl2013_all.png', dir_fig),
  dat         = v_a,
  text_left   = '', 
  text_center = 'Ocean Health Index Scores', 
  text_right  = '')

map_fig(sprintf('%s/gl2013_beg_001.png', dir_fig))

# make map images
#for (i in 1:nrow(v_a)){ # i=192
for (i in nrow(v_a)){ # i=192  
  dat        = v_a[1:i,]
  text_right = sprintf('%d/%d', i, nrow(v_a))
  with (v_a[i,], {
    map_fig(
      fname       = sprintf('%s/gl2013_%s_%03d.png', dir_fig, cat_key, seq),
      dat         = dat,
      text_left   = cat_name, 
      text_center = ifelse(cat_key!='rest', sprintf('%s: %d', name, round(score)), ''), 
      text_right  = text_right)
  })
}

# http://office.microsoft.com/en-us/powerpoint-help/file-formats-that-are-supported-in-powerpoint-2010-HP010338214.aspx
# wmv 1024 x 768, 30 frames per second

# make movies at specified frames per second (fps)
fps = c(beg='1/1.5', g8='1/1.5', top10='1', bot10='1', rest='10')
for (i in 1:length(fps)){ # i=2
  
  
  sfx = names(fps)[i]
  cmd = sprintf('ffmpeg -y -r %s -i %s/gl2013_%s_%%03d.png -r 24 -f mov -vcodec h264 -pix_fmt yuv420p %s/gl2013_%s.mov', fps[i], dir_fig, sfx, dir_fig, sfx)
  system(cmd)  
}

# ffmpeg -i Melker_Emergency_Crico_Set.flv -q:v 4 -c:v wmv2 -c:a wmav2 -b:a 128k melker.avi

# paste movies together
txt = file.path(dir_fig, 'gl2013_videos.txt')
vid = sprintf('%s/gl2013_animation', dirname(dir_fig))
writeLines(sprintf('file gl2013_%s.mov', names(fps)), txt)
system(sprintf('ffmpeg -y -f concat -i %s -c copy %s.mov', txt, vid))

# # also create *.mov since *.mp4 timing is super slow in PowerPoint 2010
# system(sprintf('ffmpeg -i %s.mp4 -acodec copy -vcodec copy -f mov %s.mov', vid)

# remove temporary files
#unlink(dir_fig, recursive=T)