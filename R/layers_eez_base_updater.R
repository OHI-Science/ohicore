#' Update layers_eez_base.csv
#' By Peter Menzies
#' 
#' Convenient interface for changing directory paths of recently updated layers in layers_eez_base.csv
#' No arguments required - user will be prompted for needed information
#' 
#'  
#' @keywords ohi
#' @export


layers_eez_base_updater <- function() {
  
  require(here)
  require(tidyverse)
  
  # read in the csv
  layers_eez_base <- read_csv(here("metadata_documentation/layers_eez_base.csv"), col_types = cols())
  
  # prompt user for current version year (gsub() to remove 'v' if user adds it as well)
  message("")
  version_year <- paste0("v", gsub("\\D", "", readline(prompt = "enter version year: "))) 
  
  # empty vector that will later contain layer names - used in while loop
  possible_layers <- c()
  
  # loop that ends when a viable goal/subgoal abbr is supplied
  while (length(possible_layers) == 0) {
    
    message("")
    goal <- readline(prompt = "enter the goal/subgoal/prs/res abbreviation for the layers you're updating (e.g. 'np', 'hab', or 'cc'): ") %>% 
      tolower()
    
    possible_layers <- layers_eez_base$layer[startsWith(layers_eez_base$layer, goal)]
    
    if (length(possible_layers) == 0) {
      message("\nthere are no layers starting with that abbreviation\n")
    }
  }
  
  # the component layers of that goal/subgoal are printed below for user's convenience
  message("\nthese are the layers associated with that abbreviation:\n") 
  print(possible_layers)
  message("\nif you want to update all of these layers, enter 'all' at the next prompt —")
  message("if you are only updating certain ones you can copypaste the layer names above separated by commas\n")
  
  
  # prompt user for layers which have been updated
  updated_layers <- str_split(readline(prompt = "enter 'all' or layers separated only by commas: "), ",")[[1]] %>% 
    str_remove_all(" ")
  
  if (tolower(updated_layers[1]) == "all") {
    updated_layers <- possible_layers
  } 
  
  
  # loop that executes or repeats if entries don't match any of the layer names in the goal / subgoal
  while (length(intersect(updated_layers, possible_layers)) < length(updated_layers)) {
    
    unknown_layers <- setdiff(updated_layers, possible_layers)
    
    if (length(unknown_layers) == length(updated_layers)) {
      message("\nnone of the layers entered coincide with the chosen abbreviation\n")
    } else if (length(unknown_layers) == 1) {
      message(paste0("\nthe following entry does not coincide with the chosen abbreviation: \n"))
      print(unknown_layers)
      message("")
    } else {
      message("\nthe following entries do not coincide with the chosen abbreviation: \n")
      print(unknown_layers)
      message("")
    }
    
    updated_layers <- str_split(readline(prompt = "enter 'all' or layers separated only by commas: "), ",")[[1]] %>% 
      str_remove_all(" ")
    
    if (tolower(updated_layers[1]) == "all") {
      updated_layers <- possible_layers
    } 
  }
  
  # create df with updated file paths
  layers_eez_base_updated <- layers_eez_base %>% 
    mutate(dir = case_when(layer %in% updated_layers ~ 
                             gsub("v20\\d\\d", version_year, dir),
                           TRUE ~ dir))
  
  # vector of dir names which have changed from the original csv
  updated_dirs <- anti_join(layers_eez_base_updated, layers_eez_base, by = c("layer", "dir")) %>% 
    rename("dir (UPDATED)" = dir)
  
  # make sure the chosen year and layers elicited changes
  if (nrow(updated_dirs) != 0) { 
    message("\nthe selected 'dir' values will be updated as shown in the data viewer ↑ ")
    message("do you want to update layers_eez_base.csv with these changes?\n")
    View(updated_dirs)
    
    # request for permission to overwrite current csv with version containing updated dirs
    overwrite <- readline(prompt = "update? ('y' or 'n'): ")
    
    if (overwrite == "y") {
      write.csv(layers_eez_base_updated, here("metadata_documentation/layers_eez_base.csv"),
                row.names = FALSE)
      message("\nfile has been updated\n")
      
    } else {
      message("\nfile was *NOT* updated\n")
    }
    
    # if no changes were elicited to any file paths, end function with message
  } else {
    message("\nthe chosen file paths already contain that version year - no updates were made\n")
  }
  
}



