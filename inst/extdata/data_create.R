# Create package datasets for lazy loading. Document in R/data.R.

# flags for turning on/off time consuming code
do.layers.Global.www2013 = F
do.layers.Global2012.Nature2012ftp = F

# [layers|scores].Global[2013|2012].v2013web ----

# set from root directory based on operating system
dir.from.root = c('Windows' = '//neptune/data_edit',
                  'Darwin'  = '/Volumes/data_edit',
                  'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

# load Google spreadsheet
g.url = 'https://docs.google.com/spreadsheet/pub?key=0At9FvPajGTwJdEJBeXlFU2ladkR6RHNvbldKQjhiRlE&output=csv'
g = subset(read.csv(textConnection(RCurl::getURL(g.url)), skip=1, na.strings=''), !is.na(ingest))

# load results
results.csv = '/Volumes/local_edit/src/toolbox/scenarios/global_2013a/results/OHI_results_for_Radical_2013-10-08.csv'
r = plyr::rename(read.csv(results.csv), c('value'='score'))
r$dimension = plyr::revalue(r$dimension, c('likely_future_state'='future'))
#table(r[,c('dimension','goal')])

# iterate over scenarios
for (yr in 2012:2013){ # yr=2012
  cat(sprintf('\n---------\nScenario: %da\n', yr))
  
  if (do.layers.Global.www2013){
    # copy files
    dir.to     = sprintf('inst/extdata/layers.Global%d.www2013', yr)
    dir.create(dir.to, showWarnings=F)
    cat(sprintf('  copy to %s\n', dir.to))
    g$filename = g[, sprintf('fn_%da' , yr)]
    g$path = file.path(dir.from.root, g[, sprintf('dir_%da', yr)], g$filename)
    for (f in sort(g$path)){ # f = sort(g$path)[1]
      cat(sprintf('    copying %s\n', f))
      stopifnot(file.copy(f, file.path(dir.to, basename(f)), overwrite=T))
    }
      
    # create conforming layers navigation csv  
    layers.csv = sprintf('%s.csv', dir.to)
    g$targets = gsub('_', ' | ', as.character(g$target), fixed=T)
    g$description = g$citation.2012n
    g$citation = g$X  
    write.csv(g[,c('targets','layer','title','description','citation','units','filename')], layers.csv, row.names=F, na='')
    
    # run checks on layers
    CheckLayers(layers.csv, dir.to, flds_id=c('rgn_id','cntry_key','saup_id'))
  }
  
  # create scores
  scores.csv = sprintf('inst/extdata/scores.Global%d.www2013.csv', yr)
  write.csv(r[r$scenario==yr, c('goal', 'dimension','region_id','score')], scores.csv, row.names=F, na='')
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
for (csv in list.files('inst/extdata', pattern=glob2rx('layers.*.csv'), full.names=T)){ # csv = list.files('inst/extdata', pattern=glob2rx('layers.*.csv'), full.names=T)[1]
  
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

# conf.* ----
# Create conf.[scenario] dataset for all conf.[scenario] directories.
for (dir in list.files('inst/extdata', pattern=glob2rx('conf.*'), full.names=T)){ # dir = list.files('inst/extdata', pattern=glob2rx('conf.*'), full.names=T)[1]
    
  # get directory and ensure exists
  conf = basename(dir)
  
  # get layers and assign same variable name as dataset
  assign(conf, ohicore::Conf(dir))
  
  # save to data folder for lazy loading
  save(list=c(conf), file=sprintf('data/%s.rda', conf))
}
