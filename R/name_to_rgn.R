#' Get scenarios
#' 
#' Get scenarios from Github.
#' 
#' @param d dataset
#' @param fld_name field name of the region from the dataset
#' @param flds_unique field name for the dataset
#' @param fld_value field with value, defaults to 'value'
#' 
#' @details This function translates name to region id with a lookup.
#' 
#' TEMPORARY. More notes here on documenting and including new functions in ohicore.
#' See http://r-pkgs.had.co.nz/man.html for function documentation details.
#' TODO: finish out parameters above.
#' FINISH: After any documentation edits or a new function file added, you'll need to do the following:
#' library(devtools)
#' document() # writes R documentation file (man/name_to_rgn.Rd), updates DESCRIPTION and NAMESPACE files of ohicore package
#' build() # to ensure works
#' install() # to ensure installs
#' 
#' finally git it to github: commit, push, done.
#' 
#' @keywords ohi
#' 
#' @export

name_to_rgn = function(
  d, fld_name = 'country', flds_unique=fld_name, fld_value='value', 
  collapse_fxn = c('sum_na','mean','weighted.mean')[1],
  collapse_csv = NULL,
  collapse_flds_join = NULL,
  dir_lookup = '../ohiprep/src/LookupTables',
  rgn_master.csv   = file.path(dir_lookup, 'eez_rgn_2013master.csv'),
  rgn_synonyms.csv = file.path(dir_lookup, 'rgn_eez_v2013a_synonyms.csv'),
  add_rgn_name=F, add_rgn_type=F) {
  # DETAIL. Return a data.frame (vs add_rgn_id which writes to a csv) and perform extra checks, including collapsing on duplicates.
  #  Note: The original fld_name lost because possible to collapse multiple countries into a single region.
  #
  # debug: fld_name='country'; flds_unique=c('country','commodity','year'); fld_value='value'; collapse_fxn=function(x) sum(x, na.rm=T); dpath = '../ohiprep/src/LookupTables'; rgn_master.csv   = file.path(dpath, 'eez_rgn_2013master.csv'); rgn_synonyms.csv = file.path(dpath, 'rgn_eez_v2013a_synonyms.csv')
  
  #   # ensure dplyr's summarize overrides plyr's summarize by loading in succession
  #   if ('package:reshape2'  %in% search()) detach('package:reshape2')
  #   if ('package:plyr'      %in% search()) detach('package:plyr')
  #   if ('package:dplyr'     %in% search()) detach('package:dplyr')
  #   library(reshape2)
  #   library(plyr)
  #   library(dplyr)
  
  # check for valid arguments
  stopifnot(fld_name %in% names(d))
  stopifnot(fld_value %in% names(d))
  stopifnot(all(flds_unique %in% names(d)))  
  stopifnot( sum( duplicated(d[,flds_unique]) ) == 0 )
  
  # get authoritative rgn_id and rgn_name
  rgns = read.csv(rgn_master.csv, na='', stringsAsFactors=F) %.% 
    select(rgn_id=rgn_id_2013, rgn_name=rgn_nam_2013, rgn_type=rgn_typ) %.% 
    arrange(rgn_type, rgn_id, rgn_name) %.% 
    group_by(rgn_id) %.% 
    summarize(
      rgn_name = first(rgn_name),
      rgn_type = first(rgn_type)) %.%
    ungroup()
  
  #browser()
  #filter(d, country=='Albania' & year==1990)
  
  # combine to have a unique tmp_name to rgn_id lookup
  r = rbind_list(
    rgns %.%
      select(rgn_id, tmp_name = rgn_name, tmp_type = rgn_type),
    read.csv(rgn_synonyms.csv, na='') %.%
      select(rgn_id=rgn_id_2013, tmp_name=rgn_nam_2013, tmp_type=rgn_typ)) %.%
    group_by(tmp_name) %.%
    summarize(
      rgn_id = first(rgn_id),
      tmp_type = first(tmp_type))
  
  # remove accents from data
  d['tmp_name'] = d[fld_name]
  d = d %.%
    mutate(
      tmp_name = str_replace(tmp_name, "^'"           , ''),                      # get rid of beginning quote
      tmp_name = str_replace(tmp_name, '.+voire'      , 'Ivory Coast'),           # Ivory Coast
      tmp_name = str_replace(tmp_name, '.+union'      , 'Reunion'),               # Reunion
      tmp_name = str_replace(tmp_name, '.+publique du', 'Republic of'),           # Congo
      tmp_name = str_replace(tmp_name, 'Cura.+'       , 'Curacao'),               # Curacao 
      tmp_name = str_replace(tmp_name, 'Saint Barth.+', 'Saint Barthelemy'),      # Saint Barthelemy 
      tmp_name = str_replace(tmp_name, '.+Principe'   , 'Sao Tome and Principe')) # Sao Tome and Principe
  
  # merge data and regions
  m = merge(d, r, by='tmp_name') %.%
    filter(tmp_type %in% c('eez','ohi_region')) # exclude: disputed, fao, landlocked, largescale
  
  # check the regions removed
  m_r = left_join(d, r, by='tmp_name') %.%
    filter(!tmp_type %in% c('eez','ohi_region')) %.%
    mutate(tmp_name = factor(as.character(tmp_name)))
  
  # if any rgn_type is NA, then presume not matched in lookups and error out
  if (sum(is.na(m_r$tmp_type)) > 0){
    cat('\nThese data were removed for not having any match in the lookup tables:\n')  
    print(table(subset(m_r, is.na(tmp_type), tmp_name)))
    # stop('FIX region lookups.') # commented out because larger regions (eg Middle East) would cause this to stop with an error. Check printed table instead. 
  }
  
  # show table of others filtered out
  if (nrow(m_r) > 0){
    cat('\nThese data were removed for not being of the proper rgn_type (eez,ohi_region) or mismatching region names in the lookup tables:\n')  
    print(table(select(m_r, tmp_name, tmp_type), useNA='ifany'))
  }  
  
  # stop if any still need to be assigned rgn_id
  m_na = filter(m, is.na(rgn_id))
  if (nrow(m_na) > 0){
    cat('\nThese data have a valid tmp_type but no rgn_id:\n')  
    print(table(m_na[,c(fld_name, 'tmp_type')], useNA='ifany'))
    stop('FIX region lookups.')
  }
  
  # collapse duplicates
  flds_unique_rgn_id = c('rgn_id', setdiff(c(flds_unique), fld_name))
  i_dup = duplicated(m[,flds_unique_rgn_id], fromLast=F) | duplicated(m[,flds_unique_rgn_id], fromLast=T)  
  
  if (sum(i_dup) > 0){
    
    cat(sprintf('\nDUPLICATES found. Resolving by collapsing rgn_id with collapse_fxn: %s after first removing all NAs from duplicates...\n', collapse_fxn))
    
    # get duplicates    
    m_dup = m[i_dup,]    
    m_dup$tmp_value = m_dup[[fld_value]]                                    
    m_dup$fld_name  = m_dup[[fld_name]]
    m_dup =  mutate(m_dup, fld_name = factor(as.character(fld_name)) )
    print(table(select(m_dup, fld_name, rgn_id)))

    sum_na = function(x){
      # sum with na.rm=T, but if all NAs return NA and not 0
      if (sum(is.na(x)) == length(x)) return(NA)
      return(sum(x, na.rm=T))    
    }

    weighted_mean = function(x, collapse_csv){
      # sum with na.rm=T, but if all NAs return NA and not 0
      # eg weighted_avg(x, csv='~/github/ohiprep/Global/WorldBank-Statistics_v2012/data/country_total_pop.csv')
      #print(names(list(...)))
      browser()
      
      #w = read.csv(collapse_csv)
      #  inner_join(
      #names(intersect(
      #w = match(x, table, nomatch=1)
    #   %>%
    #    left_join(p)
      
      #weighted.mean(x, w)
      if (sum(is.na(x)) == length(x)) return(NA)
      #p = read.csv()
      #x2 = x 
      return(sum(x, na.rm=T))    
    }
    
    # handle duplicates differentially based on collapse_fxn
    #collapse_fxn='weighted_avg', collapse_csv=pop_csv, collapse_flds_join
    if (collapse_fxn=='sum_na'){
      
      m_dup = m_dup %.%
        filter(!is.na(tmp_value)) %.%
        regroup(as.list(flds_unique_rgn_id)) %.% 
        summarize(tmp_value = sum_na(tmp_value)) %.%            
        rename(setNames(fld_value, 'tmp_value')); head(m_dup)         

    } else if (collapse_fxn=='mean'){
      
      m_dup = m_dup %.%
        filter(!is.na(tmp_value)) %.%
        regroup(as.list(flds_unique_rgn_id)) %.% 
        summarize(tmp_value = mean(tmp_value, na.rm=T)) %.%            
        rename(setNames(fld_value, 'tmp_value')); head(m_dup)               
      
    } else if (collapse_fxn=='weighted.mean'){
      
      w = read.csv(collapse_csv)
      flds = intersect(names(w), names(m_dup))
      fld  = setdiff(names(w), names(m_dup))
      stopifnot(length(fld)==1)
      stopifnot(length(flds)>1)
      w['tmp_weight'] = w[fld]
      w = w[, c(flds, 'tmp_weight')]
      
      m_dup = m_dup %.%
        filter(!is.na(tmp_value)) %.%
        regroup(as.list(flds_unique_rgn_id)) %.% 
        left_join(w, by=flds) %>%
        summarize(tmp_value = weighted.mean(tmp_value, tmp_weight, na.rm=T)) %.%            
        rename(setNames(fld_value, 'tmp_value')); head(m_dup)               

    } else {
      stop('collapse_fxn needs to be a string of one of the following: sum_na, mean, weighted.mean.')
    }

    # bind summarized duplicates back   
    m_d = rbind_list(
      m[ !i_dup, c(flds_unique_rgn_id, fld_value) ], 
      m_dup)            
    
  } else {
    
    m_d = m
    
  }
  # limit to same subset of fields for consistent behavior regardless of duplicates presents
  m_d = m_d[, c(flds_unique_rgn_id, fld_value)]
  
  # add fields if explicitly requested
  if (add_rgn_type) m_d = left_join(m_d, rgns %.% select(rgn_id, rgn_type), by='rgn_id')
  if (add_rgn_name) m_d = left_join(m_d, rgns %.% select(rgn_id, rgn_name), by='rgn_id')
  
  # check to ensure no duplicates
  stopifnot(duplicated(m_d[, c(flds_unique_rgn_id)]) == 0) 
  
  # return data.fram    
  return(as.data.frame(m_d))
  
}