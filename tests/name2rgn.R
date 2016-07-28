#' Get scenarios - NOTE THIS IS THE OLD DOCUMENTATION!!!
#' 
#' Get scenarios from Github.
#' 
#' @param d dataset
#' @param fld_name field name of the region from the dataset
#' @param flds_unique field name for the dataset
#' @param fld_value field with value, defaults to 'value'
#' @param collapse_fxn function to collapse duplicate regions into one (example: China, Macau, Hong Kong)
#' @param collapse_csv optional .csv file provided to collapse duplicate regions
#' @param collapse_flds_join optional list of fields identified to collapse duplicate regions
#' @param dir_lookup directory of name-to-region look up tables
#' @param rgn_master.csv .csv file of eez-to-region combinations
#' @param rgn_synonyms.csv .csv file of synonyms of eez-to-region combinations
#' @param add_rgn_name T or F whether to include a column with the region name
#' @param add_rgn_type T of F whether to include the region type (eez...)
#' 
#' @details This function translates name to region id with a lookup.
#'  
#' @keywords ohi
#' @export

name2rgn <- function(df_in, 
                     fld_name      = 'country', 
                     dir_lookup    = '~/github/ohiprep/src/LookupTables',
                     rgn_master    = file.path(dir_lookup, 'eez_rgn_2013master.csv'),
                     rgn_synonyms  = file.path(dir_lookup, 'rgn_eez_v2013a_synonyms.csv'),
                     keep_fld_name = TRUE) {
  ### DETAIL. Return a data.frame (vs add_rgn_id which writes to a csv) 
  ### and perform extra checks, including collapsing on duplicates.

  require(dplyr); require(tidyr); require(stringr)
  
  message(sprintf('In name2rgn(), dir_lookup = %s\n rgn_master = %s\n', dir_lookup, rgn_master))
  
  ### Read file of region names/IDs and select rgn_id, rgn_name, and rgn_type
  rgns <- read.csv(rgn_master, na = "", stringsAsFactors = FALSE) %>% 
    select(rgn_id = rgn_id_2013, rgn_name = rgn_nam_2013, rgn_type = rgn_typ) %>% 
    arrange(rgn_type, rgn_id, rgn_name) %>% 
    group_by(rgn_id) %>% 
    summarize(rgn_name = first(rgn_name), rgn_type = first(rgn_type)) %>% 
    ungroup()
  
  ### attach rgn_synonyms; summarize eliminates duplicate rows (same tmp_name 
  ### and rgn_id) - rgn type not critical?
  syns <- read.csv(rgn_synonyms, na = "") %>% 
    select(rgn_id = rgn_id_2013, tmp_name = rgn_nam_2013, 
           tmp_type = rgn_typ)

  rgn_syn <- rgns %>% 
    select(rgn_id, tmp_name = rgn_name, tmp_type = rgn_type) %>%
    dplyr::bind_rows(syns) %>% 
    group_by(tmp_name) %>% 
    summarize(rgn_id = first(rgn_id), tmp_type = first(tmp_type)) 

  ### create a temp field in the target data frame, for the field that is being combined.
  df_in['tmp_name'] <- df_in[fld_name]
  
  ### replace a bunch of problematic names (accents and such) within target data frame.
  ### Can't this be done with a lookup table and a loop?
  df_in <- df_in %>% 
    mutate(tmp_name = str_trim(tmp_name),
           tmp_name = str_replace(tmp_name, "^'", ""),
           tmp_name = str_replace(tmp_name, "C.+voire", "Ivory Coast"), 
           tmp_name = str_replace(tmp_name, "R.+union", "Reunion"), 
           tmp_name = str_replace(tmp_name, "R.+publique", "Republique"), 
           tmp_name = str_replace(tmp_name, "Cura.+ao", "Curacao"), 
           tmp_name = str_replace(tmp_name, "S.+lemy", "Saint Barthelemy"), 
           tmp_name = str_replace(tmp_name, "S.+Principe", "Sao Tome and Principe"))

  ### combine target data frame with region name data frame;
  ### filter to ones with 'eez' or 'ohi_region' in the type.  
  ### * 'eez' is original OHI rgn_name/rgn_id list;
  ### * 'ohi_region' is from the synonyms.  
  df_matched <- df_in %>%
    left_join(rgn_syn, by = "tmp_name") %>% 
    filter(tmp_type %in% c("eez", "ohi_region"))
  
  ### This is the list of countries removed.  Why change tmp_name into a factor? (for table display?)
  df_removed <- df_in %>%
    left_join(rgn_syn, by = "tmp_name") %>% 
    filter(!tmp_type %in% c("eez", "ohi_region"))
  
  ### countries in df_in_removed with no match - so tmp_type is NA (this is 
  ### why left_join() is used).
  ### Print a table of the non-matches.
  if (sum(is.na(df_removed$tmp_type)) > 0) {
    toprint <- df_removed %>% 
      filter(is.na(tmp_type))
    cat("\nThese data were removed for not having any match in the lookup tables:\n")
    print(table(as.character(unique(toprint$country))))
  }

  ### print out the full table of removed names.
  if (nrow(df_removed) > 0) {
    cat("\nThese data were removed for not being of the proper rgn_type (eez,ohi_region) or mismatching region names in the lookup tables:\n")
    print(table(select(df_removed, tmp_name, tmp_type), useNA = "ifany"))
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
        rename(rgn_name_orig = rgn_name)
    }
  }
  
  ### Add rgn_name column, ditch the tmp_name and tmp_type column.
  df_out <- df_matched %>%
    left_join(rgns %>% select(rgn_id, rgn_name), by='rgn_id') %>%
    select(-tmp_name, -tmp_type)
  
  ### Identify duplicated rgn_ids and report out.
  i_dupes <- duplicated(df_out$rgn_id, fromLast = FALSE) | 
    duplicated(df_out$rgn_id, fromLast = TRUE)
  if(sum(i_dupes) > 0) {
    message(sprintf("\nDUPLICATES found. Consider using collapse2rgn to collapse duplicates.\n"))
    df_out_dupes <- df_out[i_dupes, ] %>%
      arrange(rgn_id, rgn_name)
    print(df_out_dupes)
  }
  
  return(df_out)
}
  
complete2rgn <- function(df_in,
                         fld_name = 'rgn_name',
                         fld_id   = 'rgn_id') {
  rgn_names <- read.csv('~/github/ohi-global/eez2013/layers/rgn_global.csv', stringsAsFactors = FALSE) %>%
    rename(rgn_name = label)
  
  df_out <- df_in %>% 
    full_join(rgn_names, by = c(fld_id, fld_name))
}

collapse2rgn <- function(df_in, 
                         fld_value,
                         fld_id = 'rgn_id', 
                         fld_keep = fld_id,
                         collapse_fxn = c('sum','mean','weighted_mean')[1], 
                         collapse_wts = NULL) {
  ### Expectation: a data frame, with columns for rgn_id, maybe rgn_name, and some value.
  ### Group by rgn_id and unique fields; summarize value column(s) according to collapse_fxn.
  ### collapse_csv? collapse_flds_join (not used...?)?
  ### collapse_wts will be a .csv or a df with rgn_id and another column to be used for weighting.
  ### What if other variables not captured by "fld_unique"?
  ### DETAIL. Return a data.frame (vs add_rgn_id which writes to a csv) 
  ### and perform extra checks, including collapsing on duplicates.
  ### Note: The original fld_name lost because possible to collapse multiple 
  ### countries into a single region.

  require(dplyr); require(tidyr); require(stringr)
  
  ### check for valid arguments  stopifnot(fld_name %in% names(d))
  ### ? should be warning for these?
  stopifnot(fld_id       %in% names(df_in))
  stopifnot(fld_value    %in% names(df_in))
  stopifnot(all(fld_keep %in% names(df_in)))
#  stopifnot(sum(duplicated(df_in[ , fld_keep])) == 0)
  
  fld_keep_rgn_id <- c(fld_id, setdiff(c(fld_keep), fld_id))
  
  ### are there duplicates? create index of duplicated records
  i_dupes <- duplicated(df_in[, fld_keep_rgn_id], fromLast = FALSE) | 
    duplicated(df_in[, fld_keep_rgn_id], fromLast = TRUE)
  
  if (sum(i_dupes) == 0) {
    ### No duplicates - return dataframe as is.
    cat(sprintf('No duplicates found in %s. \n', paste(fld_keep_rgn_id, collapse = ', ')))
    df_out <- df_in
  } else {
    ### Duplicates found; collapse using function
    cat(sprintf('\nDuplicate values found for %s. \n', paste(fld_keep_rgn_id, collapse = ', ')))
    cat(sprintf('Resolving by collapsing %s with collapse_fxn: %s after first removing all NAs from duplicates...\n', 
                fld_id, collapse_fxn))
    
    fld_dropped <- names(df_in)[!names(df_in) %in% c(fld_value, fld_keep_rgn_id)]
    message(sprintf('Dropping variables: %s\n', paste(fld_dropped, collapse = ', ')))
    message('  Use argument fld_keep to prevent variables from being dropped.\n')
    
    ### create a data.frame of just the duplicated records, for collapsing
    df_in_dup <- df_in[i_dupes, ] %>%
      arrange(rgn_id, rgn_name)
    print(df_in_dup)

    ### set tmp_value to be the value, to protect original value
    df_in_dup$tmp_value <- df_in_dup[[fld_value]]
    
    if (collapse_fxn == "sum") {
      df_in_collapsed <- df_in_dup %>% 
        filter(!is.na(tmp_value)) %>%
        group_by_(.dots = as.list(fld_keep_rgn_id)) %>% 
        summarize(tmp_value = sum(tmp_value)) 
          ### NOTE: since NAs removed above, no need here... similar below
    } else if(collapse_fxn == "mean") {
      df_in_collapsed <- df_in_dup %>% 
        filter(!is.na(tmp_value)) %>% 
        group_by_(.dots=as.list(fld_keep_rgn_id)) %>% 
        summarize(tmp_value = mean(tmp_value))
    } else if(collapse_fxn == "weighted_mean") {
      wts_df <- switch(class(collapse_wts),
                       'character'  = read.csv(collapse_wts, stringsAsFactors = FALSE),
                       'data.frame' = collapse_wts,
                       'NULL'       = stop('Must set weighting values for weighted mean'),
                       as.data.frame(collapse_wts))
      
      flds_matched <- intersect(names(wts_df), names(df_in_dup))
      fld_collapse <- setdiff(names(wts_df), names(df_in_dup))
      stopifnot(length(fld_collapse) == 1)
      wts_df['tmp_weight'] <- wts_df[fld_collapse]
      wts_df <- wts_df[, c(flds_matched, "tmp_weight")]
      df_in_collapsed <- df_in_dup %>% 
        left_join(wts_df, by = flds_matched) %>% 
        filter(!is.na(tmp_value) & !is.na(tmp_weight)) %>% 
        group_by_(.dots = as.list(fld_keep_rgn_id)) %>% 
        summarize(tmp_value = sum(tmp_value * tmp_weight)/sum(tmp_weight))
    } else {
      stop("collapse_fxn needs to be a string of one of the following: sum, mean, weighted_mean.")
    }
    
    ### clean up tmp_value field, quick check
    df_in_collapsed <- df_in_collapsed %>%
      rename_(.dots = setNames('tmp_value', fld_value))
    head(df_in_collapsed)
  
    df_out <- rbind(df_in[!i_dupes, c(fld_keep_rgn_id, fld_value)],
                    df_in_collapsed)
  }
  
  ### limit to same subset of fields for consistent behavior regardless of duplicates presents
  df_out <- df_out[, c(fld_keep_rgn_id, fld_value)]
  
  ### check to ensure no duplicates remaining in kept fields
  stopifnot(duplicated(df_out[, c(fld_keep_rgn_id)]) == 0) 
  
  return(as.data.frame(df_out))
}

name_to_rgn1 <- function(df_in, 
                         fld_name         = 'country', 
                         flds_unique      = fld_name, 
                         fld_value        = 'value', 
                         collapse_fxn     = c('sum_na','mean','weighted.mean')[1],
                         collapse_csv     = NULL,
                         dir_lookup       = '~/github/ohiprep/src/LookupTables',
                         rgn_master.csv   = file.path(dir_lookup, 'eez_rgn_2013master.csv'),
                         rgn_synonyms.csv = file.path(dir_lookup, 'rgn_eez_v2013a_synonyms.csv'),
                         add_rgn_name     = FALSE, 
                         add_rgn_type     = FALSE) {
  ### This function is intended to replace original name_to_rgn() by calling
  ### name2rgn() and collapse2rgn() sequentially.
  
  require(dplyr); require(tidyr); require(stringr)
  
  message('name_to_rgn is old school; please use name2rgn and collapse2rgn\n')

  df_named <- df_in %>% 
    name2rgn(fld_name      = fld_name, 
             dir_lookup    = dir_lookup,
             rgn_master    = rgn_master.csv,
             rgn_synonyms  = rgn_synonyms.csv,
             keep_fld_name = TRUE)
  
  df_collapsed <- df_named %>% 
    collapse2rgn(fld_value = fld_value,
                 fld_id    = 'rgn_id', 
                 fld_keep  = setdiff(flds_unique, fld_name),
                 collapse_fxn = switch(collapse_fxn,
                                       'sum_na' = 'sum', 
                                       'mean'   = 'mean',
                                       'weighted.mean' = 'weighted_mean',
                                       collapse_fxn),
                 collapse_wts = collapse_csv)
  
  fld_keep_rgn_id <- c('rgn_id', setdiff(c(flds_unique), c('rgn_id', fld_name)))
  
  df_out = df_collapsed[, c(fld_keep_rgn_id, fld_value)]
  
  if(add_rgn_type | add_rgn_name) {
    rgns <- read.csv(rgn_master.csv, na = "", stringsAsFactors = FALSE) %>% 
      select(rgn_id = rgn_id_2013, rgn_name = rgn_nam_2013, rgn_type = rgn_typ) %>% 
      arrange(rgn_type, rgn_id, rgn_name) %>% 
      group_by(rgn_id) %>% 
      summarize(rgn_name = first(rgn_name), rgn_type = first(rgn_type)) %>% 
      ungroup()
  }
  if (add_rgn_type) {
    df_out <- df_out %>% 
      left_join(rgns %>% 
                  select(rgn_id, rgn_type), 
                by = "rgn_id")
  }
  if (add_rgn_name) {
    df_out <- df_out %>% 
      left_join(rgns %>% 
                  select(rgn_id, rgn_name), 
                by = "rgn_id")
  }
  stopifnot(duplicated(df_out[, c(fld_keep_rgn_id)]) == 0)
  
  return(as.data.frame(df_out))
}

