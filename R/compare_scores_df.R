#' Compares scores
#' 
#' Combine two scores.csv files and calculate difference.
#' 
#' @param a_csv scores.csv for A
#' @param b_csv scores.csv for B
#' @param r_csv region labels, ie layers/rgn_labels.csv
#' @param g_csv goals, ie conf/goals.csv
#' 
#' @details Returns a data frame with calculated differences sorted by global (region_id=0), Index, score, goal, dimension, absolute score, is.na(a), is.na(b).
#' 
#' @export

compare_scores_df = function(a_csv, b_csv, r_csv, g_csv){
  
  # read in data
  a = read.csv(a_csv)
  b = read.csv(b_csv)
  r = read.csv(r_csv, stringsAsFactors=F)
  g = read.csv(g_csv, stringsAsFactors=F) %>% arrange(order_color)
  ohi_dimensions = c('score','status','trend','pressures','resilience','future')
  
  # check column names
  stopifnot(names(a) == c('goal','dimension','region_id','score'))
  stopifnot(names(b) == c('goal','dimension','region_id','score'))
  stopifnot(c('rgn_id','label') %in% names(r))
  
  # merge
  d = a %>%
    base::merge(
      b, 
      by=c('goal','dimension','region_id'), 
      suffixes=c('.a','.b')) %>%        
    dplyr::rename(rgn_id=region_id) %>%
    mutate(
      score.dif = score.a - score.b,
      score.na = is.na(score.a)!=is.na(score.b)) %>%      
    left_join(
      rbind_list(
        r %>%
          select(rgn_id, rgn_name=label),
        data.frame(rgn_id=0, rgn_name='GLOBAL')),
      by='rgn_id') %>%
    # filter(abs(score_dif) > 0.01 | score_na == T) %>%
    arrange(rgn_id!=0, goal!='Index', dimension!='score', goal, desc(dimension), desc(abs(score.dif)), is.na(score.a), is.na(score.b)) %>%
    select(goal, dimension, rgn_id, rgn_name, score.a, score.b, score.dif) %>%
    mutate(
      goal      = factor(goal, c('Index', g$goal)),
      dimension = factor(dimension, ohi_dimensions),
      id        = row_number())

  return(d)
}