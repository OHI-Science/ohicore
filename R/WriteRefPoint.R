#' WriteRefPoint
#' 
#' Writes information about reference points to a csv file located in temp/reference_pts.csv
#' 
#' 
#' @param year  default is the scenario year
#' @param goal  goal name (e.g., "FIS")
#' @param method method of identifying reference point (e.g., "95th quantile among regions")
#' @param reference_point reference point value
#' @param scenario_name folder name of scenario being calculated
#' 
#' @return rbinds provided information to the temp/reference_pts.csv so this can be 
#' referenced later on
#'#' 
#'
#' @keywords ohicore
#' @examples
#' \dontrun{
#' } 
#' @export

WriteRefPoint <- function(goal, method, ref_pt, scenario_name="eez") {
  
  if(is.null(layers$data$scenario_year)){
    scen_year <- format(format(Sys.Date(), "%Y")) 
  } else {
    scen_year <- layers$data$scenario_year
  }
  
  ref_pts <- read.csv(sprintf(here("%s/temp/reference_pts_%s.csv"), scenario_name, scen_year))  %>%
    rbind(data.frame(year   = layers$data$scenario_year,
                     goal   = goal,
                     method = method,
                     reference_point = ref_pt))
  readr::write_csv(ref_pts, sprintf(here("%s/temp/reference_pts_%s.csv"), scenario_name, scen_year))
  
}