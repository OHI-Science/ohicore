# create initial Baltic layers based on Global2013.www2013 layers

# load ohicore, development mode
library(devtools)
load_all()

# get paths based on host machine name, as lowercase name (without domain suffix)
dir_conf = list(
  # TEMPORARY LOCAL in Stockholm
  'salacia'=list(  # BB's Mac
    neptune_data    = '/Users/bbest/Desktop/neptune_cache', 
    neptune_local   = '/Users/bbest/Desktop/neptune_cache/local')
  )[[ tolower(sub('\\..*', '', Sys.info()[['nodename']])) ]]
dir_conf$ohiprep   = '../ohiprep'
dir_conf$ohibaltic = '../ohibaltic'

dir_out      = '../ohibaltic'
dir_conf_in  = 'inst/extdata/conf.Global2013.www2013'
dir_lyrs_in  = 'inst/extdata/layers.Global2013.www2013'
lyrs_in_csv  = 'inst/extdata/layers.ohibaltic.csv'
layers.csv   = sprintf('%s/layers.csv', dir_out)
f_spatial    = c(sprintf('%s/git-annex/Baltic/StockholmUniversity-Regions_v2014-04/data/regions_gcs.js', dir_conf$neptune_data))

lyrs = read.csv(lyrs_in_csv, stringsAsFactors=F) %.%
  mutate(filename = sprintf('%s_global2013.csv', layer)) %.%
  arrange(layer)

rgn_new_csv   = '../ohiprep/Baltic/StockholmUniversity-Regions_v2014-04/data/rgn_data.csv'
rgn_old_csv   = sprintf('%s/%s', dir_lyrs_in, subset(lyrs, layer=='rgn_labels', filename_old, drop=T))
cntry_old_csv = sprintf('%s/%s', dir_lyrs_in, subset(lyrs, layer=='cntry_rgn' , filename_old, drop=T))

# dir.create('../ohibaltic/layers', showWarnings=F)
# dir.create('../ohibaltic/conf', showWarnings=F)
dir.create('../ohibaltic/spatial', showWarnings=F)

# # old to new regions
# rgn_new = read.csv(rgn_new_csv) %.%
#   filter(rgn_type=='eez') %.%
#   select(rgn_id_new=rgn_id, rgn_name_new=rgn_name) %.%
#   mutate(rgn_name_old = sub('(.*)_(.*)', '\\1', rgn_name_new)) %.%
#   merge(
#     read.csv(rgn_old_csv, na.strings='') %.%
#       select(rgn_name_old=label, rgn_id_old=rgn_id),
#     by='rgn_name_old', all.x=T) %.%
#   select(rgn_id_new, rgn_name_new, rgn_id_old, rgn_name_old) %.%
#   arrange(rgn_name_new)
# 
# # old to new countries
# cntry_new = read.csv(cntry_old_csv) %.%
#   select(cntry_key, rgn_id_old=rgn_id) %.%
#   merge(
#     rgn_new,
#     by='rgn_id_old') %.%
#   group_by(cntry_key, rgn_id_new) %.%
#   summarise(n=n()) %.%
#   select(cntry_key, rgn_id_new) %.%
#   as.data.frame()
# 
# for (i in 1:nrow(lyrs)){ # i=1
#   csv_in  = sprintf('%s/%s', dir_lyrs_in, lyrs$filename_old[i])
#   csv_out = sprintf('%s/layers/%s', dir_out, lyrs$filename[i])
#   
#   d = read.csv(csv_in, na.strings='')
#   flds = names(d)
#   if ('rgn_id' %in% names(d)){
#     d = d %.%
#       filter(rgn_id %in% rgn_new$rgn_id_old) %.%
#       merge(rgn_new, by.x='rgn_id', by.y='rgn_id_old') %.%
#       mutate(rgn_id=rgn_id_new) %.%
#       subset(select=flds)
#   }
#   if ('cntry_key' %in% names(d)){
#     d = d %.%
#       filter(cntry_key %in% cntry_new$cntry_key)
#   }
#   write.csv(d, csv_out, row.names=F, na='')
# }
# 
# # layers registry
# flds = c('targets','layer','name','description','citation','units','fld_value','filename')
# write.csv(lyrs[,flds], layers.csv, row.names=F, na='')
# 
# # run checks on layers
# CheckLayers(layers.csv, 
#             sprintf('%s/layers', dir_out), 
#             flds_id=c('rgn_id','cntry_key','country_id','saup_id','fao_id','fao_saup_id'))

# order for layers for substitution old to new name in files
lyrs = lyrs %.%
  arrange(desc(nchar(layer_old)))

# copy configuration files
conf_files = c('config.R','functions.R','goals.csv','pressures_matrix.csv','resilience_matrix.csv','resilience_weights.csv')
for (f in conf_files){ # f = conf_files[2]
  
  f_in  = sprintf('%s/%s', dir_conf_in, f)
  f_out = sprintf('%s/conf/%s', dir_out, f)
  
  # substitute old layer names with new
  s = readLines(f_in, warn=F, encoding='UTF-8')
  for (i in 1:nrow(lyrs)){ # i=1
    s = gsub(lyrs$layer_old[i], lyrs$layer[i], s, fixed=T)
  }
  writeLines(s, f_out)
}

# calculate scores
layers = Layers(layers.csv=layers.csv, 
                 layers.dir=sprintf('%s/layers', dir_out))
conf   = Conf(sprintf('%s/conf', dir_out))
scores = CalculateAll(conf, layers, debug=T)
write.csv(scores, sprintf('%s/scores.csv', dir_out), na='', row.names=F)

# # spatial
# for (f in f_spatial){ # f = f_spatial[1]
#   file.copy(f, sprintf('%s/spatial/%s', dir_out, basename(f)))
# }
# 
# # create shortcuts
# WriteScenario(
#   scenario = list(
#     conf = conf, 
#     layers = layers, 
#     scores = scores,
#     spatial = sprintf('%s/spatial', dir_out),
#     dir    = dir_out))