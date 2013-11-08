#' Parses spatial scheme mappings referenced in spatial.schemes.navigation
#'   and returns a list containing the mappings and metadata.
#' 
#' @param spatial.schemes.navigation (data.frame) contains metadata and 
#'   locations of mappings
#' @return (list) 
#' @export
#' 
ParseSpatialSchemes <- function(spatial.schemes.navigation) {
  
  ParseScheme <- function (filepath, method, ...) {
    # 
    if (method == "matrix") {
      raw.csv <- read.csv(filepath, header=T, row.names=1)
      return (as.matrix(raw.csv))
    } else if (method == "data.frame") {
      raw.csv <- read.csv(filepath, header=T)
      return (raw.csv)
    } else if (method == "function") {
      return (NULL)
    }
  }
  
  
  spatial.schemes <- spatial.schemes.navigation
  
  spatial.schemes$scheme.data <- apply(spatial.schemes,
                                       1,
                                       plyr::splat(ParseScheme))
  return (spatial.schemes)
}
