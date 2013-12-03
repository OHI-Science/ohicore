# Create package datasets for lazy loading. Document in R/data.R.

library(ohicore)

# layers.Global[2013|2012].v2013web ----

# set from root directory based on operating system
dir.from.root = c('Windows' = '//neptune/data_edit',
                  'Darwin'  = '/Volumes/data_edit',
                  'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

# load Google spreadsheet
library(RCurl)
g.url = 'https://docs.google.com/spreadsheet/pub?key=0At9FvPajGTwJdEJBeXlFU2ladkR6RHNvbldKQjhiRlE&output=csv'
g = subset(read.csv(textConnection(getURL(g.url)), skip=1, na.strings=''), !is.na(ingest))
write.csv(g, 'inst/extdata/layers.Global.vWeb2013b.csv', row.names=F, na='')

# iterate over scenarios
for (s in 2:3){ # s=2
  cat(sprintf('\n---------\nScenario: 201%sa\n', s))  
  
  # copy files
  dir.to     = sprintf('inst/extdata/layers.Global201%d.vWeb201%db', s, s)
  dir.create(dir.to)
  cat(sprintf('  copy to %s\n', dir.to))  
  g$path = file.path(dir.from.root, g[, sprintf('dir_201%da',s)], g[, sprintf('fn_201%da' ,s)])
  for (f in sort(g$path)){ # f = sort(g$path)[1]
    cat(sprintf('    copying %s\n', f))
    stopifnot(file.copy(f, file.path(dir.to, basename(f)), overwrite=T))
  }
  
  # generate layers navigation csv  
  layers.csv = sprintf('%s.csv', basename(dir.to))
  write.csv(g[,c('target','layer','title','subtitle','citation','units','filename','directory')], layers_navigation.csv, row.names=F, na='')
  
  #check.layers_navigation(layers_navigation.csv, layers_id_fields)
  #system(paste('open', layers_navigation.csv))
  #assemble.layers_data(layers_navigation.csv, layers_data.csv, layers_id_fields)
}

# layers.Global2012.v2012Nature ----


# layers.* ----
# 
# Create layers.[scenario] dataset for all layers.[scenario].csv based on csv
# layer files in layers.[scenario].  See: R/Layers.R for expected formats.
for (csv in list.files('inst/extdata', pattern=glob2rx('layers.*.csv'), full.names=T)){ # csv = list.files('inst/extdata', pattern=glob2rx('layers.*.csv'), full.names=T)[1]
  
  # get directory and ensure exists
  dir = tools::file_path_sans_ext(csv)
  stopifnot(file.exists(dir))  
  scenario = basename(dir)
  
  # get layers and assign same variable name as dataset
  assign(scenario, Layers(csv, dir))
  
  # save to data folder for lazy loading
  save(list=c(scenario), file=sprintf('data/%s.rda', scenario))
}

# scores.* ----
# 
# Create scores.[scenario] dataset for all scores.[scenario].csv.
# See: R/Scores.R for expected formats.
