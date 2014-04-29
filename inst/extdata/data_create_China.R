# Create package datasets for lazy loading. Document in R/data.R.

library(plyr)

# load ohicore
wd = '~/Code/ohicore'
setwd(wd)
load_all()

# set from root directory based on operating system
dir.from.root = c('Windows' = '//neptune/data_edit',
                  'Darwin'  = '/Volumes/data_edit',
                  'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]


# spatial ----

# paths
shp.from   = file.path(dir.from.root, 'model/CN-NCEAS-Regions/data/rgns_offshore12nm_gcs.shp')
dir.to     = path.expand(file.path(wd, 'inst/extdata/spatial.China2014'))
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
flds = c('rgn_id'='rgn_id', 'rgn_name'='rgn_nam') #flds = c('rgn_id'='region_id', 'rgn_nam'='region_nam') # too long for shapefile columns
x@data = plyr::rename(x@data[,names(flds)], flds) # drop other fields  
rgdal::writeOGR(x, dsn=geojson.to, layer=basename(tools::file_path_sans_ext(geojson.to)), driver='GeoJSON')
rgdal::writeOGR(x, dsn=shp.to, layer=basename(tools::file_path_sans_ext(shp.to)), driver='ESRI Shapefile')  
fw = file(js.to, 'wb')
cat('var regions = ', file=fw)
cat(readLines(geojson.to, n = -1), file=fw)
close(fw)