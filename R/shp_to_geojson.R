#' Create GeoJSON from Shapefile
#' 
#' Create GeoJSON file needed for interactive map in Shiny app
#' 
#' @param shp path to shapefile with .shp extension, needs rgn_id and rgn_name fields
#' @param js path to output javascript file with variable 'regions' of geojson content
#' @param geojson path to output GeoJSON file. defaults to *.geojson of *.js file.
#' 
#' @details Uses rgdal to write GeoJSON.
#'  
#' @keywords geojson shapefile
#' @export
shp_to_geojson = function(shp, js, geojson=sprintf('%s.geojson', tools::file_path_sans_ext(js))){
  
  require(sp)
  require(rgdal)
  require(maptools)
  require(dplyr)

  # # debug
  # shp      = '/Volumes/data_edit/git-annex/clip-n-ship/data/Albania/rgn_offshore_gcs.shp'
  # js      = '/Volumes/data_edit/git-annex/clip-n-ship/data/Albania/regions_gcs.js'
  # geojson = sprintf('%s.geojson', tools::file_path_sans_ext(js))

  # read and write shapefile
  x = maptools::readShapeSpatial(shp, proj4string=sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
  flds = c('rgn_id'='rgn_id', 'rgn_name'='rgn_nam') #flds = c('rgn_id'='region_id', 'rgn_nam'='region_nam') # too long for shapefile columns
  stopifnot(names(flds) %in% names(x))
  x@data = plyr::rename(x@data[,names(flds)], flds) # drop other fields  
  if (file.exists(geojson)) unlink(geojson)
  rgdal::writeOGR(x, dsn=tools::file_path_sans_ext(geojson), layer=basename(tools::file_path_sans_ext(geojson)), driver='GeoJSON')
  file.rename(tools::file_path_sans_ext(geojson), geojson)
  fw = file(js, 'wb')
  cat('var regions = ', file=fw)
  cat(readLines(geojson, n = -1), file=fw)
  close(fw)
}