### This function takes datesets that have country records that contain multiple OHI regions (macro-regions). 
### This is a common occurrence and there are many different combinations of country records that hold multiple OHI regions.
### This function attempts to account for all currently known versions of this; however, future OHI prepers may need to update this function to account for new cases.
### The function works by checking for common country names that often contain multiple OHI regions and then breaks those countries into OHI regions.
### Each new OHI Region's data is filled by a population weighting of the data value column.
### The population weights are pulled from a CSV with populations for each OHI region in question (often small islands).
### The decision was made that when a dataset contains both a macro-region and a sub-region that the sub-region's data will be calculated by
### summing the population weight of the macro region and the record for the region together. This is an imperfect solution; however, we believe it to be the best solution.
### The duplicate argument is used when the values should not be split up or subdevided between the new regions created such as when calculating sustainability scores. 

library(tidyverse)

region_split <- function(m, country_column = "country", value_column = "value", duplicate = FALSE) {
  
  # List of macro-regions to break down
  split_details <- list(
    `Netherlands Antilles` = c("Bonaire", "Sint Eustatius", "Saba", "Curaçao", "Sint Maarten", "Aruba"),
    `Bonaire/S.Eustatius/Saba` = c("Bonaire", "Sint Eustatius", "Saba"),
    `Saint Helena/Asc./Trist.` = c("Tristan da Cunha", "Saint Helena", "Ascension"),
    `Channel Islands` = c("Guernsey", "Jersey"),
    `United States Minor Outlying Island` = c("Wake Island", "Jarvis Island", "Palmyra Atoll", "Howland Island and Baker Island", "Johnston Atoll"),
    `French Southern Territories` = c("Glorioso Islands", "Juan de Nova Island", "Bassas da India", "Ile Europa", "Ile Tromelin", "Crozet Islands", "Amsterdam Island and Saint Paul Island", "Kerguelen Islands"),
    `Bonaire, Sint Eustatius and Saba` = c("Bonaire", "Sint Eustatius", "Saba"),
    `French Southern Terr` = c("Glorioso Islands", "Juan de Nova Island", "Bassas da India", "Ile Europa", "Ile Tromelin", "Crozet Islands", "Amsterdam Island and Saint Paul Island", "Kerguelen Islands"),
    `United States Minor Outlying Islands` = c("Wake Island", "Jarvis Island", "Palmyra Atoll", "Howland Island and Baker Island", "Johnston Atoll"),
    `Saint Helena, Ascension and Tristan da Cunha` = c("Saint Helena", "Ascension", "Tristan da Cunha"),
    `Caribbean Netherlands` = c("Bonaire", "Sint Eustatius", "Saba"),
    `Channel Isl. (UK)` = c("Jersey", "Guernsey"),
    `Saba and Sint Eustatius (Netherlands)` = c("Saba", "Sint Eustatius"),
    `Mozambique Channel Isl. (France)` = c("Juan de Nova Island", "Bassas da India", "Ile Europa")
  )
  
  # Rename data frame columns
  m <- m %>%
    rename(country = paste0(country_column), value = paste0(value_column))
  
  # Load population weighting data
  population <- split_pops
  
  # Loop over all macro region names within the country column
  for (country_name in names(split_details)) {
    
    # Check to see if macro country name is present
    if (country_name %in% m$country) {
      
      # Pull the regions associated with the macro country name
      regions <- split_details[[country_name]]
      
      # Calculate total population for the regions
      pop_sum <- sum(population$population[population$country %in% regions])
      
      # Calculate area weights for the regions
      area_weights <- population %>%
        filter(country %in% regions) %>%
        mutate(weight = ifelse(duplicate, 1, population / pop_sum)) %>% # Conditionally set weight
        select(country, weight) %>%
        mutate(id = row_number()) 
      
      # Split the data into different regions
      m_new <- m %>%
        filter(country == country_name) %>%
        uncount(length(regions), .id = "id") %>%
        left_join(area_weights, by = c("id" = "id")) %>%
        mutate(value = value * weight) %>%
        mutate(country = country.y) %>%
        select(-country.x, -country.y, -id, -weight)
      
      # Update m to remove the original country and add broken down rows
      m <- m %>%
        filter(!(country %in% country_name)) %>%
        rbind(m_new)
      
      # Sum duplicate rows if duplicate is False
      if (!duplicate) {
        m <- m %>%
          group_by(country, commodity, year, product) %>%
          summarize(value = case_when(all(is.na(value)) ~ NA,
                                      TRUE ~ sum(value, na.rm = TRUE))) %>%
          ungroup()
        
        # Remove duplicates if duplicate is True
      } else {
        
        # Remove duplicates
        m <- m[!duplicated(m), ]
        
      } # End duplicate if statement
      
    } # End country if statement
    
  } # End for loop
  
  # Rename columns to match data frame input
  m <- m %>%
    rename(paste0(country_column) = country, paste0(value_column) = value)
  
  # Return the new data frame
  return(m)
  
} # End function 
  
  
  
  