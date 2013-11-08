#' Parse description of spatial scheme mappings used to convert layers.
#' 
#' @param file (string) filepath of Spatial Schemes Navigation file
#' @export
#' 
ParseSpatialSchemesNavigation <- function(file, header = T,
                                          req.columns = c('target_scheme', 
                                                          'origin_scheme', 
                                                          'method', 
                                                          'filepath')) {
  
  spatial.schemes.nav <- read.csv(file, header=header)
  
  data.column.indices <- which(names(spatial.schemes.nav) %in% req.columns)
  
  spatial.schemes.nav.data <- spatial.schemes.nav[, data.column.indices]
  
  json.metadata <- apply(
                         spatial.schemes.nav[, -data.column.indices], 
                         1, 
                         function (X) {
                           RJSONIO::toJSON(X, collapse='') 
                         })
  
  return (cbind(spatial.schemes.nav.data, json.metadata))
}
