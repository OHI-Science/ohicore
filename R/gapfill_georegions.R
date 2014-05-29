#' Gapfill using georegional means
#' 
#' Gapfill using georegional means, providing the finest possible resolution from 3 hierarchies (r2 > r1 > r0).
#' 
#' @param data data.frame to gapfill having at least fields: \code{fld_id} and \code{fld_value}, and optionally \code{fld_weight}
#' @param georegions data.frame having at least fields: \code{fld_id} and \code{r0}, \code{r1}, and \code{r2} with georegion id values
#' @param fld_id common spatial id field (eg region_id or country_key) between \code{data} and \code{georegions}
#' @param fld_weight optional weighting field in \code{data}
#' @param fld_year optional year field in \code{data}
#' @param fld_value value to gapfill in \code{data}
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
#'   \item \code{r0_v} - weighted.mean for level 0
#'   \item \code{r2_n} - count of input values for level 2
#'   \item \code{r1_n} - count of input values for level 1
#'   \item \code{r0_n} - count of input values for level 0
#'   \item \code{z_n} - count of input values for finest level available
#'   \item \code{z_level} - finest level available
#'   \item \code{z} - weighted.mean for finest level available
#' }
#' 
#' @details
#' Gapfill using georegional means, providing the finest possible resolution from 3 hierarchies (r2 > r1 > r0).
#' 
#' @keywords ohi
#' @examples
#' 
#' \dontrun{
#' ## setup
#' require(ohicore)
#' 
#' # gapfill
#' g = gapfill_georegions(data, georegions, fld_weight='w_sum') # fld_id='rgn_id', fld_weight='w_sum', fld_value='score'
#' 
#' # show result and table
#' head(g)
#' head(attr(g, 'gapfill_georegions'))
#' }
#' @import dplyr
#' 
#' @export
gapfill_georegions = function(
  data, georegions, 
  fld_id =intersect(names(data), names(georegions)),
  fld_weight = NULL,
  fld_year = ifelse('year' %in% names(data), 'year', NULL),
  fld_value = setdiff(names(data), c(fld_id, fld_weight, 'year'))
){
  # TODO: provide aggregate_by_country_year() functionality of old [AggregateLayers.R](https://github.com/OHI-Science/ohicore/blob/88b136a6f32dd20160b3b3d28e30794ac66f69c5/R/AggregateLayers.R)
     
  # check arguments
  stopifnot(length(fld_id) == 1, fld_id %in% names(data), fld_id %in% names(georegions), !fld_id %in% c('r0','r1','r2'))
  stopifnot( is.null(fld_weight) || (!is.null(fld_weight) && fld_weight %in% names(data)) )
  stopifnot(length(fld_value) == 1, fld_value %in% names(data))
  stopifnot(all(c('r0','r1','r2') %in% names(georegions)))
  stopifnot(all(data[[fld_id]] %in% georegions[[fld_id]]))
  stopifnot( is.null(fld_year) || (!is.null(fld_year) && fld_year %in% names(data)) )
     
  # rename fields
  g = rename(georegions, setNames('id', fld_id))
  d = rename(data        , setNames(c('id','v'),  c(fld_id, fld_value)))
  
  # add weight
  if (is.null(fld_weight)){
    d[['w']] = 1
  } else {
    d = rename(d, setNames('w', fld_weight))
  }
     
  # remove NAs
  if (sum(is.na(d$v))>0){
    warning(sprintf('\n  data values are NA so removed: %d of %d rows', sum(is.na(d$v)), nrow(d) ))
    d = subset(d, !is.na(v))
  }
  if (sum(is.na(d$w))>0){
    warning(sprintf('\n  weights are NA (where values reported) so removed: %d of %d rows\n    %s', sum(is.na(d$w)), nrow(d), paste(unique(d$id[is.na(d$w)]), collapse=',') ))
    d = subset(d, !is.na(w))
  }

  if (is.null(fld_year)){
    # merge georegions with data
    x = merge(g, d, by='id', all.x=T) %.%
      arrange(id)
    
    # georegion means
    y = x %.%
      filter(!is.na(v), !is.na(w))
    z = x %.%
      left_join(
        y %.% 
          group_by(r2) %.%
          summarise(
            r2_v = weighted.mean(v, w),
            r2_n = n()),
        by='r2') %.%
      left_join(
        y %.% 
          group_by(r1) %.%
          summarise(
            r1_v = weighted.mean(v, w),
            r1_n = n()),
        by='r1') %.%
      left_join(
        y %.% 
          group_by(r0) %.%
          summarise(
            r0_v = weighted.mean(v, w),
            r0_n = n()),
        by='r0') %.%
      arrange(r0, r1, r2, id) %.%
      select(r0, r1, r2, id, w, v, r2_v, r1_v, r0_v, r2_n, r1_n, r0_n, z_n, z_level, z)    
  } else {
    # using year
    d = rename(d, setNames('yr', fld_year))
    
    # expand georegions to every possible year in data
    gy = expand.grid(list(
      yr = sort(unique(d$yr)),
      id = g$id)) %.%
      merge(g, by='id') %.%
      select(yr, id, r0, r1, r2) %.%
      arrange(yr, id)
    
    x = merge(gy, d, by=c('yr','id'), all.x=T) %.%
      arrange(yr, id) %.%
      select(yr, id, r0, r1, r2, v, w)
    
    # georegion means
    y = x %.%
      filter(!is.na(v), !is.na(w))
    z = x %.%
      left_join(
        y %.% 
          group_by(yr, r2) %.%
          summarise(
            r2_v = weighted.mean(v, w),
            r2_n = n()),
        by=c('yr','r2')) %.%
      left_join(
        y %.% 
          group_by(yr, r1) %.%
          summarise(
            r1_v = weighted.mean(v, w),
            r1_n = n()),
        by=c('yr','r1')) %.%
      left_join(
        y %.% 
          group_by(yr, r0) %.%
          summarise(
            r0_v = weighted.mean(v, w),
            r0_n = n()),
        by=c('yr','r0')) %.%
      arrange(yr, r0, r1, r2, id) %.%
      select(yr, r0, r1, r2, id, w, v, r2_v, r1_v, r0_v, r2_n, r1_n, r0_n)    
  }
  
  # select best available value
  z = z %.%
    mutate(
      z_level = ifelse(!is.na(v), 'v',
                       ifelse(!is.na(r2_v), 'r2',
                              ifelse(!is.na(r1_v), 'r1',
                                     ifelse(!is.na(r0_v), 'r0', NA)))),
      z_n = ifelse(!is.na(v), 1,
                   ifelse(!is.na(r2_v), r2_n,
                          ifelse(!is.na(r1_v), r1_n,
                                 ifelse(!is.na(r0_v), r0_n, NA)))),
      z = ifelse(!is.na(v), v,
                 ifelse(!is.na(r2_v), r2_v,
                        ifelse(!is.na(r1_v), r1_v,
                               ifelse(!is.na(r0_v), r0_v, NA)))))
  
  # return result
  if (is.null(fld_year)){
    r = z %.%
      select(id, z) %.%
      arrange(id) %.%
      rename(setNames(c(fld_id, fld_value), c('id','z')))
  } else {
    r = z %.%
      select(yr, id, z) %.%
      arrange(yr, id) %.%
      rename(setNames(c(fld_year, fld_id, fld_value), c('yr', 'id', 'z')))
  }
    
  attr(r, 'gapfill_georegions') = z
  return(r)
}