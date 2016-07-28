#' Gapfill using georegional means - NOTE THIS IS DOCUMENTATION FROM OLD VERSION
#' 
#' Gapfill using georegional means, providing the finest possible resolution from 3 hierarchies (r2 > r1 > r0) derived from \href{http://en.wikipedia.org/wiki/United_Nations_geoscheme}{United Nations geoscheme}.
#' 
#' @param data data.frame to gapfill having at least fields: \code{fld_id} and \code{fld_value}, and optionally \code{fld_weight}
#' @param georegions data.frame having at least fields: \code{fld_id} and \code{r0}, \code{r1}, and \code{r2} with georegion id values
#' @param fld_id common spatial id field (eg region_id or country_key) between \code{data} and \code{georegions}
#' @param fld_weight optional weighting field in \code{data}
#' @param rgn_weights data frame of weights, expecting rgn_id in first column and weight in second
#' @param ratio_weights if TRUE, multiply the gapfilled value by the ratio of the region's weight to the regional average weight. Defaults to FALSE. IMPORTANT to set to TRUE if dealing with values that SUM!
#' @param fld_year optional year field in \code{data}
#' @param fld_value value to gapfill in \code{data}
#' @param georegion_labels with same dimensions as georegions having fields: \code{r0_label}, \code{r1_label}, \code{r2_label} and \code{v_label}
#' @param gapfill_scoring_weights used to determine gapfilling scoreset. should range 0 to 1. defaults to \code{c('r0'=1, 'r1'=0.8, 'r2'=0.5, 'v'=0)}
#' @param r0_to_NA assign value of NA if only georegional average availabe at the global level (r0). defaults to True.
#' @param attributes_csv optional path and filename to save attribute table. defaults to NULL
#' 
#' @return Returns a data.frame of having all the \code{fld_id} from georegions filled in the following columns:
#' \itemize{
#'   \item \code{fld_id} - spatial id (eg region_id or country_key).
#'   \item \code{fld_value} - the gapfilled value (eg score).
#' }
#' The returned data.frame also has an attribute "gapfill_georegions" which shows the calculated georegional means and which levels were chosen:
#' \itemize{
#'   \item \code{r0} - georegional id for level 0, ie global.
#'   \item \code{r1} - georegional id for level 1.
#'   \item \code{r2} - georegional id for level 2, the finest resolution of georegions.
#'   \item \code{id} - spatial id (eg region_id or country_key).
#'   \item \code{w} - weight used to apply \code{\link{weighted.mean}}. Defaults to 1 if not supplied as \code{fld_weight} parameter.
#'   \item \code{v} - original \code{fld_value} in \code{data}
#'   \item \code{r2_v} - weighted.mean for level 2
#'   \item \code{r1_v} - weighted.mean for level 1
#'   \item \code{r0_v} - weighted.mean for level 0 (global)
#'   \item \code{r2_n} - count of regions available for level 2
#'   \item \code{r1_n} - count of regions available for level 1
#'   \item \code{r0_n} - count of regions available for level 0
#'   \item \code{r2_n_notna} - count of region values that are not NA for level 2
#'   \item \code{r1_n_notna} - count of region values that are not NA for level 1
#'   \item \code{r0_n_notna} - count of region values that are not NA for level 0
#'   \item \code{z_level} - finest level available
#'   \item \code{z_ids} - ids for regions that are not NA which contributed to the score
#'   \item \code{z_n} - count of input values for finest level available
#'   \item \code{z_n_pct} - percent of region values that are not NA over all possible [0 to 1]
#'   \item \code{z_g_score} - gapfilling score (see details)
#'   \item \code{z} - weighted.mean for finest level available
#' }
#' 
#' @details
#' Gapfill using georegional means, providing the finest possible resolution from 3 hierarchies (r2 > r1 > r0).
#' 
#' The gapfill score (z_g_score) in the attribute table is formulated such that the higher the score, the 
#' more gapfilling performed. The maximal gapfill score is based on gapfilling at the global level (r0=1) and least
#' if no gapfilling performed (ie z = v). But then some regional averages are applied with only a few regional values 
#' while others might have all but the gapfilled region available. To account for this aspect, the difference between the next
#' finer level's weight is multiplied by the percent regions and subtracted from the level's weight, like so:
#'
#' \code{gapfill_scoring_weights[z_level] - z_n_pct * diff(gapfill_scoring_weights[z_level, z_level_finer])}
#' 
#' @keywords ohi
#' @examples
#' 
#' \dontrun{
#' ## setup
#' require(ohicore)
#' 
#' # gapfill
#' g = gapfill_georegions(data, georegions, fld_weight='w_sum')
#' 
#' # show result and table
#' head(g)
#' head(attr(g, 'gapfill_georegions'))
#' }
#' @import dplyr
#' 
#' @export

gapfill2georegion <- function(
  data,                       ### data.frame to be gapfilled
  fld_value,                  ### field name of value to be gapfilled
                              ### removed default: this is important for the user to pay attention to
                              ###   was: fld_value = setdiff(names(data), c(fld_id, fld_weight, 'year'))
  fld_id            = 'rgn_id', ### was intersect(names(data), names(georegions)); just hard code it!
  fld_year          = ifelse('year' %in% names(data), 'year', NA),
  georegions        = 'std',  ### added default: if 'std', load the layers/rgn_georegions.csv and process accordingly
                              ###   allowed values: 'std' (default georegions), 'sovereign' or 'sov' (use sovereign regions), 
                              ###   data.frame (to override default georegions)
  georegion_labels  = FALSE,  ### keep default; if georegions is NULL and this is TRUE, load layers/rgn_georegion_labels.csv
                              ###   allowed values: TRUE (only if georegions default), FALSE (no labels), 
                              ###   data.frame (required if labeling override georegions)
  fld_weight        = NULL,   ### accepts NULL (no weights), a character string (for field name) or data.frame (rgn_id and weight field)
  ratio_weights     = FALSE,  ### multiply the gapfilled value by the ratio of the region's weight to the regional average weight.
  r0_to_NA          = TRUE,   ### gapfill using only levels r2 and r1
  attributes_csv    = NULL) {
  
  ### check input arguments -----
  if(length(fld_id) != 1 || !fld_id %in% names(data) || fld_id %in% c('r0','r1','r2'))
    stop('"fld_id" must be a single column name that is present in the data set, and cannot be "r0", "r1", or "r2"')
  if(length(fld_value) != 1 || !fld_value %in% names(data))
    stop('"fld_value" must be a single column name that is present in the data set')
  if(!is.na(fld_year) && !fld_year %in% names(data))
    stop('"fld_year" must be a column name that is present in the data set')
  if(ratio_weights == T & is.null(fld_weight))
     stop('If using "ratio_weights", a "fld_weight" column or data frame must be provided')

  ### set up functions to get georegions and labels for default cases
  get_georegions <- function(sov = FALSE) {
    ### quick function to get and arrange dataframes for georegions and 
    ### georegion labels.  Sovereign regions code taken from WGI data_prep
    if(!sov) {
      # georegions <- read.csv('~/github/ohi-global/eez2013/layers/rgn_georegions.csv', na.strings='') %>%
      #   spread(level, georgn_id) 
      # return(georegions)
      
      return(read.csv('georegions.csv', na.strings=''))
      ### !!!NOTE: local path name... when packaging, convert this to package-specific path
      
    } else {
      # sovregions <- read.csv('~/github/ohiprep/src/LookupTables/eez_rgn_2013master.csv', na.strings='') %>%
      #   select(rgn_id = rgn_id_2013,
      #          r2 = sov_id) %>%               # r2 is actually rgn_ids of sovereign regions
      #   group_by(rgn_id) %>%                  # remove duplicated countries from this rgn_id list                    
      #   summarize(r2 = mean(r2, na.rm=T)) %>% # duplicates always have the same sov_id (r2 value)
      #   mutate(r1 = r2, 
      #          r0 = r2,
      #          fld_wt = as.integer(rgn_id == r2)) %>%  # flag the 'parent' rgn_id with 1, others with 0 
      #   filter(rgn_id < 255, rgn_id != 213)
      # return(sovregions)

      return(read.csv('sovregions.csv', na.strings=''))
      ### !!!NOTE: local path name... when packaging, convert this to package-specific path
    }
  }
  
  get_georegion_labels <- function(sov = FALSE) {
    if(!sov) {
      # georgn_lbls <- read.csv('~/github/ohi-global/eez2013/layers/rgn_georegion_labels.csv') %>%    
      #   mutate(level_label = sprintf('%s_label', level)) %>%
      #   select(-level) %>%
      #   spread(level_label, label) %>%
      #   left_join(
      #     read.csv('~/github/ohi-global/eez2013/layers/rgn_labels.csv') %>%
      #       select(rgn_id, rgn_label = label),
      #     by='rgn_id') %>%
      #   arrange(r0_label, r1_label, r2_label, rgn_id)
      # return(georgn_lbls)
      
      return(read.csv('georegion_labels.csv', na.strings=''))
      ### !!!NOTE: local path name... when packaging, convert this to package-specific path
    } else {
      return(read.csv('sovregion_labels.csv', na.strings=''))
      ### !!!NOTE: local path name... when packaging, convert this to package-specific path
    }
  }
  
  ### Get georegions and georegion labels based on arguments -----
  ### * 'std'       = default georegions
  ### * 'sovereign' = use sovereign country definitions
  ### * data.frame  = override defaults for custom georegions
  
  message('Establishing georegions and labels...')
  
  if(is.character(georegions)) {
    if(tolower(georegions) %in% c('std', 'standard', 'default')) {
      georgn <- get_georegions()
      message('  Using default georegion definitions.')
      
      if(georegion_labels == TRUE) {
        georgn_lbls <- get_georegion_labels()
        message('  Using default georegion labels.')
      } else georgn_lbls <- NULL
      
    } else if(tolower(georegions) %in% c('sovereign', 'sov')) {
      georgn <- get_georegions(sov = TRUE)
      message('  Using sovereign country definitions as georegions.')
      
      ### The following adjustments to data and georgn emulate commands from data_prep_wgi.R...
      data <- data %>% 
        left_join(georgn %>% select(rgn_id, fld_wt), by = 'rgn_id')
      georgn <- georgn %>% select(-fld_wt)
      
      if(georegion_labels == TRUE) {
        georgn_lbls <- get_georegion_labels(sov = TRUE)
        message('  Using sovereign georegion labels.')
      } else georgn_lbls <- NULL
      
    } else {
      stop('Unrecognized character string argument to "georegions"; use "std", "sovereign", or a data.frame')
    }
  } else if(is.data.frame(georegions)) {
    ### georegions from user-defined data frame
    georgn      <- georegions
    message(sprintf('  Default georegions overwritten by user-defined "georegions" object of class %s.', class(georegions)))

    if (is.data.frame(georegion_labels)) {
      georgn_lbls <- georegion_labels
      message('  Using georegions labels from the user-provided data.frame object.')
    } else if(georegion_labels == FALSE) {
      georgn_lbls <- NULL
    } else stop ('To add labels to a non-default georegion, please pass an object of class data.frame (or set georegion_labels == FALSE)')
    
  } else {
    ### georegions is neither character nor data.frame
    stop('Unrecognized format for argument "georegions": should be "std" (for default), "sovereign", or a data.frame object')
  }

  ### check for possible errors with georgn dataframe -----
  if(!fld_id %in% names(georgn) | fld_id %in% c('r0','r1','r2')) {
    stop(sprintf('"%s" must be found in the georegions dataframe, and cannot be any of: "r0", "r1", "r2".\n', fld_id))
    ### unique region ID field must be in georegions dataframe
  }
  if(!all(c('r0', 'r1', 'r2') %in% names(georgn))) {
    stop('georegions dataframe must contain columns designating "r0", "r1", "r2"')
    ### make sure all groupings are present in georegions df
  }
  if(!all(data[[fld_id]] %in% georgn[[fld_id]])) {
    id_missing <- data[[fld_id]][!(data[[fld_id]] %in% georgn[[fld_id]])]
    message(sprintf('In data$%s, %d values (%d instances) are not found in georegions dataframe; %d rows will be removed.\n  Missing in georegions$%s: %s',
                    fld_id, length(unique(id_missing)), length(id_missing), length(id_missing), 
                    fld_id, paste(unique(id_missing), collapse = ', ') ))
    data <- data[data[[fld_id]] %in% georgn[[fld_id]], ]
    ### Provide message if some of the fld_id instances in the dataframe don't show up in georegions,
    ### and then delete the offending ones (things like disputed regions etc.)
  }
  
  ### check for possible errors with georgn_lbls dataframe -----
  if (!is.null(georgn_lbls)){
    if (!fld_id %in% names(georgn_lbls)) 
      stop(sprintf('Georegion labels dataframe does not contain region ID field %s.\n', fld_id))
    if (!all(c('r0_label', 'r1_label', 'r2_label') %in% names(georgn_lbls)))
      stop('Georegion labels dataframe does not contain all of the fields: "r0_label", "r1_label", "r2_label"')
    if(nrow(georgn_lbls) != nrow(georgn))
      stop(sprintf('Georegion labels dataframe (%s rows) and georegions dataframe (%s rows) must contain same number of rows.\n',
                   nrow(georgn_lbls), nrow(georgn)))
    
    ### rename the region ID field to just 'id', to match other data frames
    georgn_lbls <- georgn_lbls %>% 
      dplyr::rename_(.dots = setNames(fld_id, 'id'))
    stopifnot(anyDuplicated(georgn_lbls$id) == 0)
  }
  
  ### rename fields for easy manipulation (using dplyr alternative evaluation, rename_())
  georgn <- georgn %>% 
    dplyr::rename_(.dots = setNames(fld_id, 'id'))
  df_in <- data %>%
    dplyr::rename_(.dots = setNames(c(fld_id, fld_value), c('id', 'val')))
  
  ### check for duplicate georegion id entries
  if(anyDuplicated(georgn$id)) stop('Georegions dataframe contains duplicate region ID entries')

  ### get n regions per georegion for later calculating gapfill score
  georgn <- georgn %>%
    group_by(r0) %>%
    mutate(r0_n = n()) %>%  
    group_by(r1) %>%
    mutate(r1_n = n()) %>%
    group_by(r2) %>%
    mutate(r2_n = n())
  
  ### add weights to data -----
  ### options:
  ### * no fld_weight, so use default of unweighted (wts = 1)
  ### * fld_weight is character; use weights included named in dataframe column
  ### * fld_weight is data.frame; use weights included in new data.frame

  message('Establishing weighting values...')
  if (is.null(fld_weight)){
    ### assign equal weights to all observations
    
    message('  No weighting info provided; defaulting to equal weighting.')
    df_in$wts <- 1
    df_working <- df_in ### protect input data frame from filters
    
  } else if (is.character(fld_weight)){
    ### use weights in data frame fld_weight column
    
    if (!fld_weight %in% names(df_in)) stop(sprintf('Weights field %s not found in data set', fld_weight))

    message(sprintf('  Weighting by user-defined field; using "%s" for weights field', fld_weight))
    
    df_in <- df_in %>% 
      dplyr::rename_(.dots = setNames(fld_weight, 'wts'))
    df_working <- df_in ### protect input data frame
    
    if (sum(is.na(df_working$wts)) > 0){
      ### weights variable contains NAs; remove NA occurrences
      message(sprintf('  Found %d instances of data$%s == NA so removed: %d of %d rows', 
                      sum(is.na(df_working$wts)), fld_weight, sum(is.na(df_working$wts)), nrow(df_working) ))
      df_working <- df_working %>%
        filter(!is.na(wts))      
    }    
  } else if(is.data.frame(fld_weight)){
    ### use user-assigned data frame for weights; no weights in original data set
    df_working <- df_in ### protect input data frame
    
    if (!ncol(fld_weight) == 2 & !names(fld_weight)[1] == 'rgn_id')
      stop('If using a data.frame for weights, please include only two columns, with first column named "rgn_id."')
    ### use fld_weight object
    message(sprintf('  Using user-provided data.frame for weighting; field "%s" will be used for weighting factors.', 
                    names(fld_weight)[1]))

    georgn <- georgn %>%
      left_join(
        fld_weight %>%
          dplyr::rename_(.dots = setNames(names(fld_weight)[2], 'wts')) %>%
          select(id=rgn_id, wts),
        by='id')
    if (sum(is.na(georgn$wts)) > 0){
      message(sprintf('  In weights data.frame, found %d instances of weights == NA (out of %d regions)\n  region ID: %s', 
                      sum(is.na(georgn$wts)), nrow(georgn), 
                      paste(unique(georgn$id[is.na(georgn$wts)]), collapse=', ') ))
      georgn <- subset(georgn, !is.na(georgn))
    }
  } else {
    stop('Unrecognized format for argument "fld_weights"; please use one of the following: \n  NULL (default: unweighted) \n  character string (for name of column in current data set) \n  data.frame (two columns: "rgn_id" and weight)')
  }
  
  ### remove NAs from value field
  if (sum(is.na(df_working$val)) > 0) {
    message(sprintf('Removing NAs from data: \n  Removing %d of %d rows, with data$%s == NA', 
                    sum(is.na(df_working$val)), nrow(df_working), fld_value))
    df_working <- df_working %>%
      filter(!is.na(val))
  }
  
  ### Create dataframe of georegion summaries, with no year -----
  if (is.na(fld_year)){
    ### No year field, so should not be duplicate rgn IDs: check for duplicates
    stopifnot(anyDuplicated(df_working$id) == 0)
    
    ### merge georegions with data
    ###   x is the basic df_working with georegions attached.
    x <- georgn %>%
      left_join(df_working, by = 'id') %>%
      arrange(id)

    ###   y is x, NAs filtered out, for grouping and summarizing at each georegion level
    y <- x %>%
      filter(!is.na(val), !is.na(wts))

    ###   z is x, with summarized y for each georegion.  z is the dataframe used for gapfilling.
    z <- x %>%
      left_join(
        y %>% 
          group_by(r2) %>%
          summarise(
            r2_val     = weighted.mean(val, wts),
            r2_wts_avg = mean(wts),
            r2_n_notna = n(),
            r2_ids     = paste(id, collapse=',')),
        by='r2') %>%
      left_join(
        y %>% 
          group_by(r1) %>%
          summarise(
            r1_val     = weighted.mean(val, wts),
            r1_wts_avg = mean(wts),
            r1_n_notna = n(),
            r1_ids     = paste(id, collapse=',')),
        by='r1') %>%
      left_join(
        y %>% 
          group_by(r0) %>%
          summarise(
            r0_val     = weighted.mean(val, wts),
            r0_wts_avg = mean(wts),
            r0_n_notna = n(),
            r0_ids     = paste(id, collapse=',')),
        by='r0') %>%
      arrange(r0, r1, r2, id) %>%
      select(r0, r1, r2, id, wts, val, r2_val, r1_val, r0_val, 
             r2_wts_avg, r1_wts_avg, r0_wts_avg, 
             r2_n,       r1_n,       r0_n, 
             r2_n_notna, r1_n_notna, r0_n_notna, 
             r2_ids,     r1_ids,     r0_ids)    
  } else {    
    ### Create dataframe of georegion summaries, *with* year -----
    df_working <- df_working %>%
      dplyr::rename_(.dots = setNames(fld_year, 'yr'))
    
    ### check for duplicates of rgn id and year
    stopifnot(anyDuplicated(df_working[,c('id','yr')]) == 0)
    
    ### expand georegions to every possible year in data
    georgn_yr <- 
      expand.grid(list(
        yr = sort(unique(df_working$yr)),
        id = georgn$id)) %>%
      inner_join(georgn, by = 'id') %>%
      arrange(yr, id)
    
    ### merge georegions with data, incl years
    ###   x is the basic df_working with georegions attached.
    x <- georgn_yr %>%
      left_join(df_working, by = c('yr', 'id')) %>%
      arrange(yr, id) %>%
      select(yr, id, r0, r1, r2, r2_n, r1_n, r0_n, val, wts)

    ###   y is x, NAs filtered out, for grouping and summarizing at each georegion level
    y <- x %>%
      filter(!is.na(val), !is.na(wts))
    
    ###   z is x, with summarized y for each georegion/yr.  z is the dataframe used for gapfilling.
    z <- x %>%
      left_join(
        y %>% 
          group_by(yr, r2) %>%
          summarise(
            r2_val     = weighted.mean(val, wts),
            r2_wts_avg = mean(wts),
            r2_n_notna = n(),
            r2_ids     = paste(id, collapse=',')),
        by=c('yr','r2')) %>%
      left_join(
        y %>% 
          group_by(yr, r1) %>%
          summarise(
            r1_val     = weighted.mean(val, wts),
            r1_wts_avg = mean(wts),
            r1_n_notna = n(),
            r1_ids     = paste(id, collapse=',')),
        by=c('yr','r1')) %>%
      left_join(
        y %>% 
          group_by(yr, r0) %>%
          summarise(
            r0_val     = weighted.mean(val, wts),
            r0_wts_avg = mean(wts),
            r0_n_notna = n(),
            r0_ids     = paste(id, collapse=',')),
        by=c('yr','r0')) %>%
      arrange(yr, r0, r1, r2, id) %>%
      select(yr, r0, r1, r2, id, wts, val, 
             r2_val,     r1_val,     r0_val, 
             r2_wts_avg, r1_wts_avg, r0_wts_avg, 
             r2_n,       r1_n,       r0_n, 
             r2_n_notna, r1_n_notna, r0_n_notna, 
             r2_ids,     r1_ids,     r0_ids)    
  }
  
  # select best available value and calculate gapfilling score  
  z <- z %>%
    mutate(z_level = ifelse(!is.na(val), 'val',
                       ifelse(!is.na(r2_val), 'r2',
                              ifelse(!is.na(r1_val), 'r1',
                                     ifelse(!is.na(r0_val), 'r0', NA)))))
  
  # assign attributes by georegion level (r#)
  z  <- 
  dplyr::bind_rows()(
    ### rgn has value, so no gapfill: filter and assign values
    z %>%
      filter(z_level == 'val') %>%
      mutate(
        z_ids     = as.character(id),
        z_wts_avg = wts,
        z_n       = 1,
        z_n_pct   = 1,
        z         = val),
    ### gapfill using r2 average
    z %>%
      filter(z_level == 'r2') %>%
      mutate(
        z_ids     = r2_ids,
        z_wts_avg = r2_wts_avg,
        z_n       = r2_n_notna,
        z_n_pct   = r2_n_notna/r2_n,
        z         = r2_val),
    ### gapfill using r1 average
    z %>%
      filter(z_level == 'r1') %>%
      mutate(
        z_ids     = r1_ids,
        z_wts_avg = r1_wts_avg,
        z_n       = r1_n_notna,
        z_n_pct   = r1_n_notna/r1_n,
        z         = r1_val),
    ### gapfill using r0 average
    z %>%
      filter(z_level == 'r0') %>%
      mutate(
        z_ids     = r0_ids,
        z_wts_avg = r0_wts_avg,
        z_n       = r0_n_notna,
        z_n_pct   = r0_n_notna/r0_n,
        z         = r0_val)
  ) %>%
  select(-r2_ids, -r1_ids, -r0_ids)
  
  ### multiply by ratio if argument
  if (ratio_weights){  
    z <- z %>% mutate(
      z_orig = z,
      z = z * wts / z_wts_avg)    
  }


  ### if r0_to_NA, assign value of NA if only georegional average available at the global level (r0)
  if (r0_to_NA) z$z <- ifelse(z$z_level == 'r0', NA, z$z)
  
  ### add georegion labels if provided
  if (!is.null(georgn_lbls)){
    z <- z %>%
      left_join(
        georgn_lbls %>%
          select(id, r0_label, r1_label, r2_label, rgn_label),
        by='id') %>%
      arrange(r0_label, r1_label, r2_label, rgn_label) 
    if (is.na(fld_year)){
      z <- z %>%
        select(r0_label, r1_label, r2_label, rgn_label, 
               r0, r1, r2, id, wts, val, r2_val, r1_val, r0_val, r2_n, r1_n, r0_n, 
               r2_n_notna, r1_n_notna, r0_n_notna, z_level, z_ids, z_n, z_n_pct, z)
    } else {
      z <- z %>%
        select(r0_label, r1_label, r2_label, rgn_label, yr, 
               r0, r1, r2, id, wts, val, r2_val, r1_val, r0_val, r2_n, r1_n, r0_n, 
               r2_n_notna, r1_n_notna, r0_n_notna, z_level, z_ids, z_n, z_n_pct, z)
    }
  }
  
  ### Reattach unmodified columns, and rename columns back to original names
  df_unmod <- ifelse('wts' %in% names(df_in),
                     df_in %>% select(-val, -wts),
                     df_in %>% select(-val))
  if (is.na(fld_year)){
    df_out <- z %>%
      select(id, z) %>%
      left_join(df_unmod, by = 'id') %>%
      arrange(id) %>%
      dplyr::rename_(.dots = setNames(c('id','z'), c(fld_id, fld_value)))
  } else {
    df_out <- z %>%
      select(yr, id, z) %>%
      left_join(df_unmod, by = c('id', 'yr')) %>%
      arrange(yr, id) %>%
      dplyr::rename_(.dots = setNames(c('yr', 'id', 'z'), c(fld_year, fld_id, fld_value)))
  }
  
  ### store attributes, with option to save as .csv
  attr(df_out, 'gapfill_georegions') <- z
  
  if (!is.null(attributes_csv)){
    write.csv(z, attributes_csv, na = '', row.names=FALSE)   
  }
  
  return(df_out)
}

