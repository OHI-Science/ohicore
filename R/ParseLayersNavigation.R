#' Parse description of layers used in analysis
#' 
#' @param file (string) filepath of Layers Navigation file
#' @export
#' 
ParseLayersNavigation <- function(file, header = T,
                                  req.columns = c('layer_id', 'title', 
                                                  'units', 'target', 
                                                  'filepath')) {
  
  layers.navigation <- read.csv(file, header=header)
  
  data.column.indices = which(names(layers.navigation) %in% req.columns)
  
  layers.navigation.data <- layers.navigation[, data.column.indices]
  layers.navigation.metadata <- layers.navigation[, -data.column.indices]
  
  json.metadata <- apply(
                         layers.navigation.metadata, 
                         1, 
                         function (X) {
                           RJSONIO::toJSON(X, collapse='') 
                         })
  
  return (cbind(layers.navigation.data, json.metadata))
  
}


