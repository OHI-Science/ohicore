old_synonyms <- read_csv(here::here("data_raw/rgn_eez_v2013a_synonyms.csv"))

update_synonyms <- function(synonyms_list, region_id_list, rgn_type_list) {
 

  #make them into a data frame
  rgn_data <- tibble(rgn_id_2013 = region_id_list, rgn_nam_2013 = synonyms_list,
  rgn_typ = rgn_type_list)       
  
  rgn_main <- rgn_master %>% select(-c(rgn_nam_2013, rgn_typ))
  
  #lookup the correct information for each
  rgn_syn_new <- rgn_data %>%
    left_join(rgn_main, by = "rgn_id_2013") %>% 
    select(rgn_id_2013, rgn_nam_2013, rgn_key_2013,
             eez_iso3, rgn_typ) # select the rows we want
  
  print(rgn_syn_new)
  confirmation <- readline("Confirm you want to add this line to the synonyms file? (yes/no): ") 
  
  
  if (tolower(confirmation) == "yes") {
    
    final_synonyms <- rbind(old_synonyms, rgn_syn_new)
    #check that there are no duplicates
    final_synonyms_test <- final_synonyms %>% 
      mutate(rgn_nam_2013 = tolower(rgn_nam_2013)) %>% 
      mutate(rgn_nam_2013 = stringr::str_remove(rgn_nam_2013, ",")) %>% 
      mutate(rgn_nam_2013 = stringr::str_remove(rgn_nam_2013, "'")) %>% 
      mutate(rgn_nam_2013 = stringr::str_remove(rgn_nam_2013, "´")) %>% 
      mutate(rgn_nam_2013 = stringr::str_remove(rgn_nam_2013, "’")) %>% 
      group_by(rgn_nam_2013) %>% 
      summarize(n=n()) %>% filter(n >1)
    if(nrow(final_synonyms_test) == 0) {
      #write final csv
      write_csv(final_synonyms, here::here("data_raw/rgn_eez_v2013a_synonyms.csv"))
      print("Synonyms Added")
    } else{
      print("duplicates detected, synonyms were not added.")
    }
    
  } else {
    print("Operation cancelled.")
  }
  
}