# Create package datasets for lazy loading. Document in R/data.R.
#
# If opening ohicore.Rproj as RStudio project, then working directory should be properly set.

# load ohicore, development mode
library(devtools)
load_all()

# get paths based on host machine name, as lowercase name (without domain suffix)
dir_conf = list(
  'salacia'=list(  # BB's Mac
    neptune_data    = '/Volumes/data_edit',
    neptune_local   = '/Volumes/local_edit'),    
  'beastie3'=list(  # Melanie's Windows
    neptune_data  = '//neptune.nceas.ucsb.edu/data_edit',
    neptune_local = '//neptune.nceas.ucsb.edu/local_edit')
)[[ tolower(sub('\\..*', '', Sys.info()[['nodename']])) ]]
dir_conf$ohiprep = '../ohiprep'

# variables and flags for turning on/off time consuming code
do.years.www2013 = c(2012,2013)
do.layers.www2013 = T
do.scores.www2013 = F
do.spatial.www2013 = F
do.layers.Global2012.Nature2012ftp = F
scores.source = 'calculate'  # 'calculate' OR path, eg 'src/toolbox/scenarios/global_2013a/results/OHI_results_for_Radical_2013-12-13.csv'
scores.compare = 'by year'   # file.path(dir_conf$neptune_local, 'src/toolbox/scenarios/global_2013a/results/OHI_results_for_Radical_2013-10-09.csv') # OHI_results_for_Radical_2013-12-13.csv')

# conf.* ----
# Create conf.[scenario] dataset for all conf.[scenario] directories.
for (dir in list.files('inst/extdata', pattern=glob2rx('conf.*'), full.names=T)){ # dir = list.files('inst/extdata', pattern=glob2rx('conf.*'), full.names=T)[2]
  
  # get directory and ensure exists
  conf = basename(dir)
  
  # get layers and assign same variable name as dataset
  assign(conf, ohicore::Conf(dir))
  
  # save to data folder for lazy loading
  save(list=c(conf), file=sprintf('data/%s.rda', conf))
}

# [layers|scores].Global[2013|2012].v2013web ----

# load Google spreadsheet
g.url = 'https://docs.google.com/spreadsheet/pub?key=0At9FvPajGTwJdEJBeXlFU2ladkR6RHNvbldKQjhiRlE&output=csv'
g0 = read.csv(textConnection(RCurl::getURL(g.url, ssl.verifypeer = FALSE)), skip=1, na.strings='')
write.csv(g0, 'inst/extdata/tmp/layers_navigation_2012a_2013a.csv', na='', row.names=F)
g = subset(g0, ingest==T )

# iterate over scenarios
for (yr in do.years.www2013){ # yr=2013
  scenario=sprintf('Global%d.www2013', yr)
  cat(sprintf('\n\n\n## Scenario: %sa\n', scenario))
  conf = ohicore::Conf(sprintf('inst/extdata/conf.%s', scenario))
  
  if (do.layers.www2013){
  
    # copy files
    dir.to     = sprintf('inst/extdata/layers.%s', scenario)
    dir.create(dir.to, showWarnings=F)
    cat(sprintf('  copy to %s\n', dir.to))
    g$filename = g[, sprintf('fn_%da' , yr)]
    stopifnot(nrow(subset(g, is.na(filename))) == 0)  
    g$directory = sapply(str_split(g[, sprintf('dir_%da', yr)], ':'),
                         function(x){ sprintf('%s/%s', dir_conf[x[1]], x[2])})
    g$path = sprintf('%s/%s', g$directory, g$filename)
    if(!all(file.exists(g$path))) stop('files not found:\n  ', paste(g$path[!file.exists(g$path)], '\n  '))
    for (f in sort(g$path)){ # f = sort(g$path)[1]
      cat(sprintf('    copying %s\n', f))
      stopifnot(file.copy(f, file.path(dir.to, basename(f)), overwrite=T))
      
      # HACK: manual edit of rny_le_popn to remove duplicates
      if (basename(f)=='rgn_wb_pop_2013a_updated.csv'){
        f.to = file.path(dir.to, basename(f))
        d = read.csv(f.to)
        d = d[!duplicated(d[,c('rgn_id','year')]),]
        write.csv(d, f.to, row.names=F, na='')
      }
    }
    
    # delete extraneous files
    files.used = as.character(g[, sprintf('fn_%da' , yr)])
    unlink(sprintf('%s/%s', dir.to, setdiff(list.files(dir.to), files.used)))
              
    # create conforming layers navigation csv  
    layers.csv = sprintf('%s.csv', dir.to)
    g$targets = gsub('_', ' ', as.character(g$target), fixed=T)
    #g$description = g$subtitle
    g$citation = g$citation_2013a
    write.csv(g[,c('targets','layer','name','description','citation','units','filename','fld_value')], layers.csv, row.names=F, na='')
    
    # run checks on layers
    CheckLayers(layers.csv, dir.to, flds_id=conf$config$layers_id_fields)
  }
}
  
  if (scores.source == 'calculate' & do.scores.www2013){
    # calculate scores 
    layers     = Layers(layers.csv = sprintf('inst/extdata/layers.%s.csv', scenario), 
                        layers.dir = sprintf('inst/extdata/layers.%s'    , scenario))
    #conf   = ohicore::Conf(sprintf('inst/extdata/conf.%s', scenario))
    scores = CalculateAll(conf, layers, debug=T)
    write.csv(scores, sprintf('inst/extdata/scores.%s.csv', scenario), na='', row.names=F)
    
    # archive scores on disk (out of github, for easy retrieval later)
    csv = sprintf('%s/git-annex/Global/NCEAS-OHI-Scores-Archive/scores/scores.%s_%s.csv', dir_conf$neptune_data, scenario, format(Sys.Date(), '%Y-%m-%d'))
    write.csv(scores, csv, na='', row.names=F)
    
    # compare with published results
    if (length(scores.compare) > ''){
            
      # Radical format scores_old
      # scores_old = read.csv(scores.compare, na.strings='') %.%
      #   filter(scenario==yr) %.%
      #   select(goal, dimension, region_id, score_old=value) %.%
      #   mutate(dimension = revalue(dimension, c('likely_future_state'='future')))        
      #   #head(scores_old); table(scores_old[,c('dimension','goal')])
      
      # toolbox generated scores_old
      csv = sprintf('%s/git-annex/Global/NCEAS-OHI-Scores-Archive/scores/scores.%s_2014-04-02a_pre-pressures-new.csv', dir_conf$neptune_data, scenario)
      scores_old = read.csv(csv, na.strings='') %.%
        select(goal, dimension, region_id, score_old=score); head(scores_old)
      
      # merge new and old scores, with region labels
      rgn_labels = SelectLayersData(layers, layers=conf$config$layer_region_labels) %.%
        select(region_id=id_num, region_label=val_chr)
      v = scores %.%
        merge(scores_old, by=c('goal','dimension','region_id'), all=T) %.%
        mutate(score_dif = score - score_old) %.%
        merge(rgn_labels, by='region_id') %.%
        select(goal, dimension, region_id, region_label, score, score_old, score_dif) %.%
        arrange(goal, dimension, region_id)      
        #head(v); dim(scores); dim(scores_old); dim(v)
      csv = sprintf('%s/git-annex/Global/NCEAS-OHI-Scores-Archive/scores/scores.%s_%s_new-pressures-dif.csv', dir_conf$neptune_data, scenario, format(Sys.Date(), '%Y-%m-%d'))
      write.csv(v, csv, row.names=F, na='')
      
      # print outputs      
      cat('\n\n### Compare NAs\n\n')
      
      cat('\nTable. Compare number of non-NA values (new - old) by goal and dimension.\n')
      print(table(v[!is.na(v$score), c('goal','dimension')]) - table(v[!is.na(v$score_old), c('goal','dimension')]), zero.print='.')
      
      cat('\nTable. Rows without matching NA (old vs new).\n')
      print(v %.%
              filter(is.na(score) != is.na(score_old)) %.%
              select(goal, dimension, region_id, region_label, score, score_old)
            , row.names=F)

      cat('\n### Compare Values\n\n')
      cat('\nTable. Differences (new - old) summarized by goal and dimension.\n')
      print(v %.%
              filter(!is.na(score) & !is.na(score_old) & (abs(score_dif) > 0.1)) %.%
              group_by(goal, dimension) %.%
              summarize(
                n=n(),
                dif_mean = round(mean(score_dif), 2),
                dif_min  = min(score_dif),
                dif_max  = max(score_dif)), 
            row.names=F)
      # TODO: as.data.frame for above so doesn't print row.names.
      # TODO: move to .Rmd and kable() the table outputs. pandoc to get TOC.
      
      if (yr=='2012'){
        #cat('\nDiscussion. Major differences for 2012 with LIV / ECO / LE score seem OK b/c of ECO Eritrea and other LIV substitutions.\n')
      }
    }
    
  } else if (do.scores.www2013) {    
    # create scores from published results
    
    # load results
    # TODO: update results to 10-09, not 10-08, per HAB +saltmarsh in OHI_results_for_Radical_2013-10-09.csv
    r = read.csv(file.path(dir_conf$neptune_local, scores.source), na.strings='') %.%
      filter(scenario==yr) %.%
      select(goal, dimension, region_id, score=value) %.%
      mutate(dimension = revalue(dimension, c('likely_future_state'='future')))        
    #head(scores_old); table(scores_old[,c('dimension','goal')])      
    write.csv(r, sprintf('inst/extdata/scores.Global%d.www2013.csv', yr), row.names=F, na='')
  }
  
}

# spatial.v2013web ----
if (do.spatial.www2013){
  
  # paths
  shp.from   = file.path(dir_conf$neptune_data, 'model/GL-NCEAS-OceanRegions_v2013a/data/rgn_simple_gcs.shp')
  dir.to     = path.expand(file.path(wd, 'inst/extdata/spatial.www2013'))
  shp.to     = file.path(dir.to, 'regions_gcs.shp')
  geojson.to = file.path(dir.to, 'regions_gcs.geojson')
  js.to      = file.path(dir.to, 'regions_gcs.js')
  del.to     = file.path(dir.to, 'regions_gcs.*')
  
  # prep paths
  dir.create(dir.to, showWarnings=F)
  unlink(del.to)
  
  # read and write shapefile
  sp::set_ll_warn(TRUE) # convert error to warning about exceeding longlat bounds
  x = maptools::readShapeSpatial(shp.from, proj4string=sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
  flds = c('rgn_id'='rgn_id', 'rgn_nam'='rgn_nam') #flds = c('rgn_id'='region_id', 'rgn_nam'='region_nam') # too long for shapefile columns
  x@data = plyr::rename(x@data[,names(flds)], flds) # drop other fields  
  rgdal::writeOGR(x, dsn=geojson.to, layer=basename(tools::file_path_sans_ext(geojson.to)), driver='GeoJSON')
  rgdal::writeOGR(x, dsn=shp.to, layer=basename(tools::file_path_sans_ext(shp.to)), driver='ESRI Shapefile')  
  fw = file(js.to, 'wb')
  cat('var regions = ', file=fw)
  cat(readLines(geojson.to, n = -1), file=fw)
  close(fw)
  
  # copy other reference files
  #rgn_simple_gcs.shp
  
}

# TODO: layers.Global2012.v2012Nature ----
#check.layers_navigation(layers_navigation.csv, layers_id_fields)
#system(paste('open', layers_navigation.csv))
#assemble.layers_data(layers_navigation.csv, layers_data.csv, layers_id_fields)
if (do.layers.Global2012.Nature2012ftp){
  CheckLayers('inst/extdata/layers.Global2012.Nature2012ftp.csv', 'inst/extdata/layers.Global2012.Nature2012ftp', flds_id=c('region_id','country_id','saup_id'))
}

# layers.* ----
# 
# Create layers.[scenario] dataset for all layers.[scenario].csv based on csv
# layer files in layers.[scenario].  See: R/Layers.R for expected formats.
for (csv in list.files('inst/extdata', pattern=glob2rx('layers.*.csv'), full.names=T)){ # csv = list.files('inst/extdata', pattern=glob2rx('layers.*.csv'), full.names=T)[2] 
  # csv = 'inst/extdata/layers.Global2012.www2013.csv'
  # layers.csv='inst/extdata/layers.Global2012.www2013.csv' # 171 x 22
  
  # get directory and ensure exists
  dir = tools::file_path_sans_ext(csv)
  stopifnot(file.exists(dir))  
  layers = basename(dir)
    
  # get layers and assign same variable name as dataset
  assign(layers, ohicore::Layers(csv, dir))
  
  # save to data folder for lazy loading
  save(list=c(layers), file=sprintf('data/%s.rda', layers))
}

# scores.* ----
# 
# Create scores.[scenario] dataset for all scores.[scenario].csv.
# See: R/Scores.R for expected formats.
for (csv in list.files('inst/extdata', pattern=glob2rx('scores.*.csv'), full.names=T)){ # csv = list.files('inst/extdata', pattern=glob2rx('scores.*.csv'), full.names=T)[1]
  
  # get directory and ensure exists
  scores = basename(tools::file_path_sans_ext(csv))
  
  # get layers and assign same variable name as dataset
  assign(scores, read.csv(csv, header=T, na.strings=''))
  
  # save to data folder for lazy loading
  save(list=c(scores), file=sprintf('data/%s.rda', scores))
}