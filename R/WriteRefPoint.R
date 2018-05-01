#' WriteRefPoint
#' 
#' Writes information about reference points to a csv file located in temp/reference_pts.csv
#' 
#' 
#' @param year  default is the scenario year
#' @param goal  goal name (e.g., "FIS")
#' @param method method of identifying reference point (e.g., "95th quantile among regions")
#' @param reference_point reference point value
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

WriteRefPoint <- function(goal, method, ref_pt) {
  
  ref_pts <- read.csv("temp/reference_pts.csv")  %>%
    rbind(data.frame(year   = layers$data$scenario_year,
                     goal   = goal,
                     method = method,
                     reference_point = ref_pt))
  write_csv(ref_pts, "temp/reference_pts.csv")
  
}