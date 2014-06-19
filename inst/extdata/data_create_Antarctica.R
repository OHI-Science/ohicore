# Create package datasets for lazy loading. Document in R/data.R.
#
# If opening ohicore.Rproj as RStudio project, then working directory should be properly set.

# options(warn=2) # debug: turn warnings into errors

# setup ----
# load ohicore, development mode
library(devtools)
load_all()

# cd ""C:\Program Files\R\R-3.1.0\bin\x64""
# R --vanilla CMD INSTALL "C:\Users\Melanie\AppData\Local\Temp\RtmpgjiC2q\devtools549427e967cb\ohicore-master"  --library="C:/Users/Melanie/Documents/R/win-library/3.1" --install-tests

# get paths based on host machine name, as lowercase name (without domain suffix)
dir_conf = list(
  'amphitrite'=list(  # BB's Windows 8 on MacBook Pro VMWare
    annex = 'Z:/bbest On My Mac/neptune_cyberduck'),
  'salacia'=list(  # BB's Mac
    data  = '/Volumes/data_edit',
    local = '/Volumes/local_edit',
    annex = '/Volumes/data_edit/git-annex'),
  'beastie3'=list(  # Melanie's Windows 8 on MacBook Pro VMWare
    data  = '//neptune.nceas.ucsb.edu/data_edit',
    local = '//neptune.nceas.ucsb.edu/local_edit',
    annex = '//neptune.nceas.ucsb.edu/data_edit/git-annex')
)[[tolower(sub('\\..*', '', Sys.info()['nodename']))]]

# variables and flags for turning on/off time consuming code
do.years.www2013 = c(2012,2013)
do.layers.www2013 = F
do.spatial.www2013 = F
do.layers.Global2012.Nature2012ftp = F
scores.source = 'calculate'  # 'calculate' OR path, eg 'src/toolbox/scenarios/global_2013a/results/OHI_results_for_Radical_2013-12-13.csv'
scores.compare = 'by year'   # file.path(dir_conf$local, 'src/toolbox/scenarios/global_2013a/results/OHI_results_for_Radical_2013-10-09.csv') # OHI_results_for_Radical_2013-12-13.csv')

yr=2013
scenario=sprintf('Antarctica%d.a2014', yr)

# layers registry: layers_2014_HighSeas Google spreadshee ----
g.url = "https://docs.google.com/spreadsheet/pub?key=0ArcIhYsFwBeNdHNxNk1iRHc1S05KLWsyb0ZtZjRjZnc&single=true&gid=0&output=csv"
g0 = read.csv(textConnection(RCurl::getURL(g.url, ssl.verifypeer = FALSE)), skip=1, na.strings='')
write.csv(g0, 'inst/extdata/tmp/layers_navigation_Antarctica2013.a2014.csv', na='', row.names=F)
g = subset(g0, ingest==T )


# layers ----

# iterate over scenarios

cat(sprintf('\n\n\n## Scenario: %sa\n', scenario))
conf = ohicore::Conf(sprintf('inst/extdata/conf.%s', scenario))

# copy files
dir.to     = sprintf('inst/extdata/layers.%s', scenario)
dir.create(dir.to, showWarnings=F)
cat(sprintf('  copy to %s\n', dir.to))
g$filename  = g[, sprintf('fn_%da' , yr)]
g$directory = g[, sprintf('dir_%da', yr)]

# ensure directory, filename and fld_value all have values
stopifnot(nrow(subset(g, is.na(filename))) == 0)
stopifnot(nrow(subset(g, is.na(directory))) == 0)  
stopifnot(nrow(subset(g, is.na(fld_value))) == 0)  

# iterate through layers and copy locally
g$path = file.path(dir_conf$data, g$directory, g$filename)
for (f in sort(g$path)){ # f = sort(g$path)[1]
  cat(sprintf('    copying %s\n', f))
  stopifnot(file.copy(f, file.path(dir.to, basename(f)), overwrite=T))
}

# trim extraneous columns from region layers
x = list(
  'rgn_labels_ccamlr.csv'   = c('sp_id','label'),
  'rgn_area_ccamlr_eez.csv' = c('sp_id','area_km2'))
for (f in names(x)){
  p = file.path(dir.to, basename(f))
  d = read.csv(p, na.strings='')[,x[[f]]]
  write.csv(d, p, row.names=F, na='')
}


# delete extraneous files
files.used = as.character(g[, sprintf('fn_%da' , yr)])
unlink(sprintf('%s/%s', dir.to, setdiff(list.files(dir.to), files.used)))
          
# create conforming layers navigation csv  
layers.csv = sprintf('%s.csv', dir.to)
g$targets = gsub('_', ' ', as.character(g$target), fixed=T)
write.csv(g[,c('targets','layer','name','description','units','filename','fld_value')], layers.csv, row.names=F, na='')

# run checks on layers
CheckLayers(layers.csv, dir.to, flds_id=conf$config$layers_id_fields)

# scores ----

# calculate scores 
#library(devtools); load_all(); 
layers     = Layers(layers.csv = sprintf('inst/extdata/layers.%s.csv', scenario), 
                    layers.dir = sprintf('inst/extdata/layers.%s'    , scenario))
# load_all()
conf   = ohicore::Conf(sprintf('inst/extdata/conf.%s', scenario))

## This is the money function!  Calculates all the final scores:
scores = CalculateAll(conf, layers, debug=T)

write.csv(scores, sprintf('inst/extdata/scores.%s.csv', scenario), na='', row.names=F)

# archive scores on disk (out of github, for easy retrieval later)
csv = sprintf('%s/Global/NCEAS-OHI-Scores-Archive/scores/scores_antarctica2014.%s_%s.csv', dir_conf$annex, scenario, format(Sys.Date(), '%Y-%m-%d'))
write.csv(scores, csv, na='', row.names=F)
