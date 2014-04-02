# Create package datasets for lazy loading. Document in R/data.R.

library(devtools)
load_all()

# if opening ohicore as the RStudio versioned project (using Github) then should auto set to correct working directory.

# flags for turning on/off time consuming code
do.years.www2013 = c(2013,2012)
do.spatial.www2013 = F
do.layers.Global2012.Nature2012ftp = F
results.source = 'calculate' # 'calculate' OR path, eg 'src/toolbox/scenarios/global_2013a/results/OHI_results_for_Radical_2013-12-13.csv'

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

# set from root directory based on operating system
dir.from.root = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
                  'Darwin'  = '/Volumes/data_edit',
                  'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

# get paths configuration based on host machine name
dir_conf = list(
  'AMPHITRITE'=list(  # BB's Windows 8 on MacBook Pro VMWare
    dir_git   = 'G:/ohigit',
    dir_annex = 'Z:/bbest On My Mac/neptune_cyberduck'),
  'salacia.local'=list(  # BB's Mac
    dir_ohicore       = getwd(),
    dir_neptune_data  = '/Volumes/data_edit',
    dir_neptune_local = '/Volumes/local_edit',
    dir_annex         = '/Volumes/data_edit/git-annex'),
  'salacia.nceas.ucsb.edu'=list(  # BB's Mac
    dir_ohicore       = getwd(),
    dir_neptune_data  = '/Volumes/data_edit',
    dir_neptune_local = '/Volumes/local_edit',
    dir_annex         = '/Volumes/data_edit/git-annex'),
  'BEASTIE3'=list(  # Melanie's Windows 8 on MacBook Pro VMWare
    dir_ohicore       = 'C:/Users/Melanie/Github/ohicore',
    dir_neptune_data  = '//neptune.nceas.ucsb.edu/data_edit',
    dir_neptune_local = '//neptune.nceas.ucsb.edu/local_edit',
    dir_annex         = '//neptune.nceas.ucsb.edu/data_edit/git-annex')
)[[Sys.info()['nodename']]] # N: # temp working from UCSB campus


# load Google spreadsheet
g.url = 'https://docs.google.com/spreadsheet/pub?key=0At9FvPajGTwJdEJBeXlFU2ladkR6RHNvbldKQjhiRlE&output=csv'
     #  'https://docs.google.com/spreadsheet/pub?key=0At9FvPajGTwJdEJBeXlFU2ladkR6RHNvbldKQjhiRlE&output=csv&single=true&gid=0
g0 = read.csv(textConnection(RCurl::getURL(g.url, ssl.verifypeer = FALSE)), skip=1, na.strings='')
write.csv(g0, 'inst/extdata/tmp/layers_navigation_2012a_2013a.csv', na='', row.names=F)
g = subset(g0, ingest==T )

# iterate over scenarios
for (yr in do.years.www2013){ # yr=2013
  scenario=sprintf('Global%d.www2013', yr)
  cat(sprintf('\n---------\nscenario: %sa\n', scenario))
  
  # copy files
  dir.to     = sprintf('inst/extdata/layers.%s', scenario)
  dir.create(dir.to, showWarnings=F)
  cat(sprintf('  copy to %s\n', dir.to))
  g$filename = g[, sprintf('fn_%da' , yr)]
  stopifnot(nrow(subset(g, is.na(filename))) == 0)  
  g$path = file.path(dir.from.root, g[, sprintf('dir_%da', yr)], g$filename)
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
  conf   = ohicore::Conf(sprintf('inst/extdata/conf.%s', scenario))
  CheckLayers(layers.csv, dir.to, flds_id=conf$config$layers_id_fields)
  
  if (results.source == 'calculate'){
    # calculate scores 
    layers     = Layers(layers.csv = sprintf('inst/extdata/layers.%s.csv', scenario), 
                        layers.dir = sprintf('inst/extdata/layers.%s'    , scenario))
    #conf   = ohicore::Conf(sprintf('inst/extdata/conf.%s', scenario))
    scores = CalculateAll(conf, layers, debug=T)
    write.csv(scores, sprintf('inst/extdata/scores.%s.csv', scenario), na='', row.names=F)    
    
  } else {    
    # create scores from published results
    
    # load results
    results.csv = file.path(dir_conf$dir_neptune_local, results.source)
    # eg results.source='src/toolbox/scenarios/global_2013a/results/OHI_results_for_Radical_2013-12-13.csv'
    # TODO: update results to 10-09, not 10-08, per HAB +saltmarsh in OHI_results_for_Radical_2013-10-09.csv
    r = plyr::rename(read.csv(results.csv), c('value'='score'))
    r$dimension = plyr::revalue(r$dimension, c('likely_future_state'='future'))
    #table(r[,c('dimension','goal')])

    scores.csv = sprintf('inst/extdata/scores.Global%d.www2013.csv', yr)
    write.csv(r[r$scenario==yr, c('goal', 'dimension','region_id','score')], scores.csv, row.names=F, na='')
  }
  
}

# spatial.v2013web ----
if (do.spatial.www2013){
  
  # paths
  shp.from   = file.path(dir.from.root, 'model/GL-NCEAS-OceanRegions_v2013a/data/rgn_simple_gcs.shp')
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