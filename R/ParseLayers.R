#' Parse layers referenced in layers.navigation for use in analysis.
#' 
#' @param layers.navigation (list) list of filepaths and metadata
#' @description do stuff
#' @export
#' 
ParseLayers <- function(layers.navigation) {
  
  layers.list = layers.navigation
  
  layers.list$layer.data <- apply(layers.navigation,
                                  1,
                                  function (X) {
                                    data <- read.csv(X['filepath'], header=T)
                                    data$layer_id <- X['layer_id']
                                    return (data)
                                  })
  
  return (layers.list)

}
