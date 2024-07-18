#' Update Synonyms function for OHI region names
#'
#' @param synonyms_list list of synonyms 
#' @param region_id_list list of OHI region IDs you want to associate these synonyms with
#' @param rgn_type_list type of region: 'ohi_region', 'landlocked', or 'disputed'
#'
#' @return
#' @export # append new synonyms to rgn_eez_v2013a_synonyms.csv
#'
#' @examples # see update_name_2_rgn.Rmd


update_synonyms <- function(synonyms_list, region_id_list, rgn_type_list) {
  
  # source old synonyms
  old_synonyms <- read_csv(here::here("data_raw/rgn_eez_v2013a_synonyms.csv"))
  
  # define valid values
  valid_values <- c('ohi_region', 'landlocked', 'disputed')
  
  # make them into a data frame
  rgn_data <- tibble(rgn_id_2013 = region_id_list, rgn_nam_2013 = synonyms_list,
                     rgn_typ = rgn_type_list)   
  
  
  rgn_main <- rgn_master %>% select(-c(rgn_nam_2013, rgn_typ))
  
  # lookup the correct information for each
  rgn_syn_new <- rgn_data %>%
    left_join(rgn_main, by = "rgn_id_2013") %>% 
    select(rgn_id_2013, rgn_nam_2013, rgn_key_2013,
           eez_iso3, rgn_typ) # select the rows we want
  cat("\nConfirm new line(s) are correct:\n")
  print(rgn_syn_new)
  
  
  name_check <- rgn_syn_new %>% 
    left_join(rgn_master, by ="rgn_id_2013")
  
  # check if region names are correct
  cat("\nConfirm these are the correct region names for your synonyms:\n")
  print(name_check$rgn_nam_2013.y)
  
  confirmation <- readline("Add these synonyms? (yes/no): ") 
  
  
  
  if (tolower(confirmation) == "yes") {
    
    final_synonyms <- rbind(old_synonyms, rgn_syn_new)
    # check that there are no duplicates
    final_synonyms_test <- final_synonyms %>% 
      mutate(rgn_nam_2013 = tolower(rgn_nam_2013)) %>% 
      mutate(rgn_nam_2013 = stringr::str_remove(rgn_nam_2013, ",")) %>% 
      mutate(rgn_nam_2013 = stringr::str_remove(rgn_nam_2013, "'")) %>% 
      mutate(rgn_nam_2013 = stringr::str_remove(rgn_nam_2013, "´")) %>% 
      mutate(rgn_nam_2013 = stringr::str_remove(rgn_nam_2013, "’")) %>% 
      group_by(rgn_nam_2013) %>% 
      summarize(n = n()) %>% filter(n >1)
    
    invalid_rgn_types <- rgn_data$rgn_typ[rgn_data$rgn_typ %in% c("landlocked", "disputed")]
    
    if (nrow(final_synonyms_test) > 0) {
      print("Duplicates detected, synonyms were not added.")
    } else if (!all(rgn_data$rgn_typ %in% valid_values)) {
      print("Invalid region type")
    } else if (length(invalid_rgn_types) > 0 && any(!is.na(rgn_data$rgn_id_2013[rgn_data$rgn_typ %in% invalid_rgn_types]))) {
      cat("rgn_id should be NA for landlocked or disputed regions.\n")
    } else {
      write_csv(final_synonyms, here::here("data_raw/rgn_eez_v2013a_synonyms.csv"))
      print("Synonyms Added")
    }
    
  } else {
    print("Operation cancelled.")
  }
  
}