#' Name to region (New version: doesn't collapse data)
#' 
#' Convert country names to ohi region ids.
#' 
#' @param df_in dataset
#' @param fld_name field name of the region from the dataset
#' @param flds_unique unique identifying fields for the dataset, e.g., c("rgn_name", "year")
#' @param keep_fld_name keep original name
#' 
#' @details This function translates name to region id with a lookup.
#'  
#' @keywords ohi
#' @export
name_2_rgn <- function(df_in,   #df_in=empd
                     fld_name      = 'country',
                     flds_unique   = NULL,
                     keep_fld_name = TRUE) {
  ### DETAIL. Return a data.frame (vs add_rgn_id which writes to a csv) 
  ### and perform extra checks, including collapsing on duplicates.
  
  ### Read file of region names/IDs and select rgn_id, rgn_name, and rgn_type
  rgns <- rgn_master %>% 
    dplyr::select(rgn_id = rgn_id_2013, rgn_name = rgn_nam_2013, rgn_type = rgn_typ) %>% 
    dplyr::arrange(rgn_type, rgn_id, rgn_name) %>% 
    dplyr::group_by(rgn_id) %>% 
    dplyr::summarize(rgn_name = first(rgn_name), rgn_type = first(rgn_type)) %>% 
    dplyr::ungroup()
  
  ### attach rgn_synonyms; summarize eliminates duplicate rows (same tmp_name 
  ### and rgn_id)
  syns <- rgn_synonyms %>% 
    dplyr::select(rgn_id = rgn_id_2013, tmp_name = rgn_nam_2013, 
           tmp_type = rgn_typ)
  
  rgn_syn <- rgns %>% 
    dplyr::select(rgn_id, tmp_name = rgn_name, tmp_type = rgn_type) %>%
    dplyr::bind_rows(syns) %>% 
    dplyr::group_by(tmp_name) %>% 
    dplyr::summarize(rgn_id = first(rgn_id), tmp_type = first(tmp_type)) 
  
  ### create a temp field in the target data frame, for the field that is being combined.
  df_in['tmp_name'] <- df_in[fld_name]
  
  ### replace problematic symbols (accents and such) within target data frame.
  df_in <- df_in %>% 
    dplyr::mutate(tmp_name = stringr::str_trim(tmp_name),
           tmp_name = stringr::str_replace(tmp_name, "^'", "")) %>% 
    mutate(tmp_name = stringr::str_remove(tmp_name, ",")) %>% 

    
    mutate(tmp_name = tolower(tmp_name))
    
  #turn all of the names to lowercase and remove commas
  rgn_syn <- rgn_syn %>%
    mutate(tmp_name = tolower(tmp_name)) %>% 
    mutate(tmp_name = stringr::str_remove(tmp_name, ","))
  
  ### combine target data frame with region name data frame;
  ### filter to ones with 'eez' or 'ohi_region' in the type.  
  ### * 'eez' is original OHI rgn_name/rgn_id list;
  ### * 'ohi_region' is from the synonyms.  
  df_in <- df_in %>%
    dplyr::left_join(rgn_syn, by = "tmp_name") 
  
  df_matched <- df_in %>% 
    dplyr::filter(tmp_type %in% c("eez", "ohi_region"))
  
  ### This is the list of countries removed.  Why change tmp_name into a factor? (for table display?)
  df_removed <- df_in %>%
    dplyr::filter(!tmp_type %in% c("eez", "ohi_region") | is.na(tmp_type))
  
  ### countries in df_in_removed with no match - so tmp_type is NA (this is 
  ### why left_join() is used).
  ### Print a table of the non-matches.
  if (sum(is.na(df_removed$tmp_type)) > 0) {
    toprint <- df_removed %>% 
      dplyr::filter(is.na(tmp_type))
    cat("\nThese data were removed for not having any match in the lookup tables:\n")
    print(table(as.character(unique(toprint$tmp_name))))
  }
  
  ### print out the full table of removed names.
  if (sum(!is.na(df_removed$tmp_name)) > 0) {
    toprint <- df_removed %>% 
      dplyr::filter(!is.na(tmp_type))
    cat("\nThese data were removed for not being of the proper rgn_type (eez,ohi_region) or mismatching region names in the lookup tables:\n")
    print(table(select(toprint, tmp_name, tmp_type), useNA = "ifany"))
  }
  
  ### Sanity check of matched df to make sure none have NA rgn_id after the process.
  ### This would be a failure of the region lookups table.  
  df_matched_na <- filter(df_matched, is.na(rgn_id))
  if (nrow(df_matched_na) > 0) {
    cat("\nThese data have a valid tmp_type but no rgn_id:\n")
    print(table(df_matched_na[, c(fld_name, "tmp_type")], useNA = "ifany"))
    stop("FIX region lookups; one or more rows missing rgn_id values.")
  }
  
  ### Drop fld_name column ('country' e.g.) if desired. If kept, 
  ### and == 'rgn_name', rename so it doesn't conflict with new 'rgn_name'
  if(!keep_fld_name) {
    df_matched <- df_matched[ , -which(names(df_in_matched) == fld_name)]
  } else {
    if(fld_name == 'rgn_name') {
      df_matched <- df_matched %>%
        dplyr::rename(rgn_name_orig = rgn_name)
    }
  }
  
  ### Add rgn_name column, ditch the tmp_name and tmp_type column.
  df_out <- df_matched %>%
    dplyr::left_join(rgns %>% 
                       dplyr::select(rgn_id, rgn_name), by='rgn_id') %>%
    dplyr::select(-tmp_name, -tmp_type)
  
  ### Identify duplicated rgn_ids and report out.
  dups_data <- df_out[ , c(flds_unique, 'rgn_id')] 
  i_dupes <- duplicated(dups_data, fromLast = FALSE) | 
    duplicated(dups_data, fromLast = TRUE)
  
  if(sum(i_dupes) > 0) {
    message(sprintf("\nDUPLICATES found. Consider using collapse2rgn to collapse duplicates (function in progress).\n"))
    df_out_dupes <- unique(df_out[i_dupes, fld_name]) 
    print(df_out_dupes)
  } 
  return(df_out)
}


## exclude for now, better to do in steps, I think:
# name_to_rgn1 <- function(df_in,   # df_in = empd
#                          fld_name         = 'country', 
#                          flds_unique      = fld_name, 
#                          fld_value        = 'value', 
#                          collapse_fxn     = c('sum_na','mean','weighted.mean')[1],
#                          collapse_csv     = NULL,
#                          dir_lookup       = 'src/LookupTables',
#                          rgn_master.csv   = file.path(dir_lookup, 'eez_rgn_2013master.csv'),
#                          rgn_synonyms.csv = rgn_synonyms,
#                          add_rgn_name     = FALSE, 
#                          add_rgn_type     = FALSE) {
#   ### This function is intended to replace original name_to_rgn() by calling
#   ### name2rgn() and collapse2rgn() sequentially.
#   
# 
#   df_named <- df_in %>% 
#     name2rgn(fld_name      = fld_name, 
#              dir_lookup    = dir_lookup,
#              rgn_master    = rgn_master.csv,
#              rgn_synonyms  = rgn_synonyms.csv,
#              keep_fld_name = TRUE)
#   
#   df_collapsed <- df_named %>% 
#     collapse2rgn(fld_value = fld_value,
#                  fld_id    = 'rgn_id', 
#                  fld_keep  = setdiff(flds_unique, fld_name),
#                  collapse_fxn = switch(collapse_fxn,
#                                        'sum_na' = 'sum', 
#                                        'mean'   = 'mean',
#                                        'weighted.mean' = 'weighted_mean',
#                                        collapse_fxn),
#                  collapse_wts = collapse_csv)
#   
#   fld_keep_rgn_id <- c('rgn_id', setdiff(c(flds_unique), c('rgn_id', fld_name)))
#   
#   df_out = df_collapsed[, c(fld_keep_rgn_id, fld_value)]
#   
#   if(add_rgn_type | add_rgn_name) {
#     rgns <- read.csv(rgn_master.csv, na = "", stringsAsFactors = FALSE) %>% 
#       select(rgn_id = rgn_id_2013, rgn_name = rgn_nam_2013, rgn_type = rgn_typ) %>% 
#       arrange(rgn_type, rgn_id, rgn_name) %>% 
#       group_by(rgn_id) %>% 
#       summarize(rgn_name = first(rgn_name), rgn_type = first(rgn_type)) %>% 
#       ungroup()
#   }
#   if (add_rgn_type) {
#     df_out <- df_out %>% 
#       left_join(rgns %>% 
#                   select(rgn_id, rgn_type), 
#                 by = "rgn_id")
#   }
#   if (add_rgn_name) {
#     df_out <- df_out %>% 
#       left_join(rgns %>% 
#                   select(rgn_id, rgn_name), 
#                 by = "rgn_id")
#   }
#   stopifnot(duplicated(df_out[, c(fld_keep_rgn_id)]) == 0)
#   
#   return(as.data.frame(df_out))
# }
