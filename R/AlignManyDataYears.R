#' AlignManyDataYears
#' 
#' A wrapper for the AlignDataYears function when compiling multiple data layers
#' with the same variable names (e.g., coral, seagrass, mangroves).  
#' 
#' 
#' @param layer_names  name of the data layer, 
#' e.g. c('hab_mangrove_extent', 'hab_seagrass_extent', 'hab_saltmarsh_extent')
#' 
#' @return Returns an internally used dataframe with the layer data and the data year with the
#' corresponding scenario year.
#'#' 
#'
#' @keywords ohicore
#' @examples
#' \dontrun{
#' } 
#' @export

AlignManyDataYears <- function(layer_names){  
  data <- data.frame()
  for(e in layer_names){ # e="le_jobs_cur_base_value"
    data_new <- AlignDataYears(layer_nm=e, layers_obj = layers) 
    names(data_new)[which(names(data_new) == paste0(e, "_year"))] <- "data_year"
    data <- rbind(data, data_new)
  }
  return(data)
}
