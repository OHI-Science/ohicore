#' Collapse region
#' 
#' Collapse data for duplicate regions.
#' 
#' @param df_in dataset
#' @param fld_id 
#' @param fld_value field with value, defaults to 'value'
#' @param collapse_fxn function to collapse duplicate regions into one (example: China, Macau, Hong Kong)
#' @param collapse_wts table with weights if weighted mean function is chosen
#' 
#' @details This function collapses duplicate regions (example: China, Macau, Hong Kong)
#'  
#' @keywords ohi
#' @export
collapse_2_rgn <- function(df_in, 
                         fld_value,
                         fld_id = 'rgn_id',
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
        dplyr::filter(!is.na(tmp_value)) %>%
        dplyr::group_by_(.dots = as.list(fld_keep_rgn_id)) %>% 
        dplyr::summarize(tmp_value = sum(tmp_value)) 
      ### NOTE: since NAs removed above, no need here... similar below
    } else if(collapse_fxn == "mean") {
      df_in_collapsed <- df_in_dup %>% 
        dplyr::filter(!is.na(tmp_value)) %>% 
        dplyr::group_by_(.dots=as.list(fld_keep_rgn_id)) %>% 
        dplyr::summarize(tmp_value = mean(tmp_value))
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
        dplyr::left_join(wts_df, by = flds_matched) %>% 
        dplyr::filter(!is.na(tmp_value) & !is.na(tmp_weight)) %>% 
        dplyr::group_by_(.dots = as.list(fld_keep_rgn_id)) %>% 
        dplyr::summarize(tmp_value = sum(tmp_value * tmp_weight)/sum(tmp_weight))
    } else {
      stop("collapse_fxn needs to be a string of one of the following: sum, mean, weighted_mean.")
    }
    
    ### clean up tmp_value field, quick check
    df_in_collapsed <- df_in_collapsed %>%
      dplyr::rename_(.dots = setNames('tmp_value', fld_value))
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
