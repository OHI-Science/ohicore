#' Split Macro-Regions to OHI Regions
#' 
#' This function takes datasets containing country records that include multiple OHI regions 
#' (macro-regions) and breaks them down into individual OHI regions. It uses population-weighted
#' values to distribute data across the new regions. When a dataset contains both a macro-region 
#' and a sub-region, the sub-region's data is calculated by summing the population weight of the 
#' macro region and the record for the region. 
#'
#' @param m The input dataset containing countries and associated values.
#' @param country_column The column name in the dataset `m` representing the countries. Defaults to "country".
#' @param value_column The column name in the dataset `m` representing the values associated with each country. Defaults to "value".
#' @param duplicate A logical value. If TRUE, the values will not be split between new regions, e.g., when calculating sustainability scores. Defaults to FALSE.
#' 
#' @details The function is built to recognize common macro-region names and their corresponding OHI regions. 
#' It's imperative for users to be aware that this function might require updates if new macro-regions 
#' or changes to OHI regions occur in the future.
#'
#' Population data is used to weight the values for each newly split region. This data must be provided
#' in the `split_pops` data frame (external to this function). If `duplicate` is set to TRUE, values are 
#' not divided among regions but duplicated instead.
#'
#' @return A dataset with macro-regions split into individual OHI regions.
#' 
#' @examples 
#' # This assumes existence of a dataset similar in structure to expected input and `split_pops`
#' # updated_data <- region_split(original_data)
#'
#' @keywords ohi, macro-region, split
#' @export

split_regions <- function(m, country_column = "country", value_column = "value", duplicate = FALSE) {
  
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
    rename(!!paste0(country_column) := country, !!paste0(value_column) := value)
  
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
        grouping_vars <- setdiff(names(m), "value")
        
        m <- m %>%
          group_by(across(all_of(grouping_vars))) %>%
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
    rename(!!country_column := country, !!value_column := value)
  
  # Return the new data frame
  return(m)
  
} # End function