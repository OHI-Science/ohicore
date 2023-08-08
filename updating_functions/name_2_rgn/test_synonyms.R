#' test_synonyms.R
#'
#' function to test country names are being accounted for properly in name_2_rgn
#'
#' @param synonyms_list list of country names that are synonyms for names handled in the name_2_rgn function
#' @param append_synonyms if set to TRUE, will append the synonyms tested to the bottom of the comprehensive test_countries.csv file. default: FALSE
#'
#' @return output of name_2_rgn for those countries
#'
#' @examples
#' test_synonyms(c("test1", "test2"), append_synonyms = TRUE)
#'
#' @examples
#' # Example usage of your function
#' result <- your_function(arg1_value, arg2_value)
#' 
#' # read in existing countries csv
countries <- read_csv(here::here("updating_functions/name_2_rgn/test_countries.csv"))
#read in the updated synonyms csv
load(here::here("data/rgn_master.rda"))

#read in the region data frame
load(file = here::here("data/rgn_synonyms.rda"))

test_synonyms <- function(synonyms_list, append_synonyms = FALSE) {
  
  # add new synonyms to existing csv
  df_in <- rbind(countries, data.frame(country = synonyms_list))
  
  # run through name_2_rgn and see if all are taken care of properly
  ohi_names <- name_2_rgn(df_in = df_in, fld_name = "country")
  # append synonyms to the end of the csv if were used & relevant for future testing
  # so we ensure no updates to the function cause older fixed synonyms to become unfixed
  if (append_synonyms) {
    write_csv(df_in, here::here("updating_functions/name_2_rgn/test_countries.csv"))
    
  }
  return(ohi_names)
  
}