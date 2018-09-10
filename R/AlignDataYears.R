#' AlignDataYears
#' 
#' Used in conf/functions.R to link the year data was collected to the 
#' appropriate scenario year for a data layer. A dataframe is returned with both the
#' data year and the corresponding scenario year.  The scenario year is used to join
#' data layers with different data years for use in models. 
#' 
#' The scenario year is typically the year the OHI 
#' assessment takes place.  For example, in year 2018, the scenario year is 2018.  
#' However, because there are often lags in data collection, the year associated with 
#' the data may be 2016. To make matters more complicated (*sigh*), goal models are calculated using
#' multiple datasets which may have different lag times.  This function using the information
#' provided in conf/scenario_data_years.csv to link the data years to the appropriate scenario
#' year.  
#' 
#' 
#' @param layer_nm  name of the data layer, e.g. "le_wage_cur_base_value"
#' @param ayers_obj  this is the OHI layers object created in calculate_scores.R which is a 
#' list of all the data layers and other model parameters needed to calculate OHI scores.
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

AlignDataYears <- function(layer_nm, layers_obj=layers) { #layer_nm="le_wage_cur_base_value"
  
  all_years <- conf$scenario_data_years %>%
    dplyr::mutate(scenario_year= as.numeric(scenario_year),
           data_year = as.numeric(data_year)) %>%
    dplyr::filter(layer_name %in% layer_nm) %>%
    dplyr::select(layer_name, scenario_year, year=data_year)
  
  
  layer_vals <- layers_obj$data[[layer_nm]]
  
  layers_years <- all_years %>%
    dplyr::left_join(layer_vals, by="year") %>%
    dplyr::select(-layer)
  
  names(layers_years)[which(names(layers_years)=="year")] <- paste0(layer_nm, "_year")  
  
  return(layers_years)
}

