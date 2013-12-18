#' Calculate All
#' 
#' Calculate all scores, given layers and configuration.
#' 
#' @param conf of class \code{\link{Conf}}
#' @param layers of class \code{\link{Layers}}
#' @param debug print debug messages (default=FALSE)
#' @return Returns a data.frame of scores having the following columns:
#' \itemize{
#'   \item{\emph{region_id} - unique numeric region identifier, reserving 0 as the region_id for the area-weighted average of the entire study area}
#'   \item{\emph{goal} - the goal code or Index}
#'   \item{\emph{dimension} - the dimension code, one of: status, trend, pressures, resilience, future, score}
#'   \item{\emph{score} - the numeric score: 0-100 for all dimensions, except trend (-1 to 1)}
#' }
#' @keywords ohi
#' @examples
#' 
#' \dontrun{
#' ## run a model with 50 regions using random data,
#' ## using 5 year 1-percent discount rate and beta=0.67
#' require(ohi)
#' d <- ohi.model.goal(id=1:50, 
#'                     status=runif(50, 0, 1), 
#'                     trend=runif(50, -1, 1), 
#'                     resilience=runif(50, 0, 1), 
#'                     pressure=runif(50, 0, 1), 
#'                     DISCOUNT = (1 + 0.01)^-5,
#'                     BETA = 0.67,
#'                     default_trend = 0.0) 
#' ## view model output
#' names(d)
#' d[,c('id','score','xF')]
#' }
#' 
#' @export
CalculateAll = function(conf, layers, debug=F){

  # remove global scores
  if (exists('scores')) rm(scores)
  
  # Pressures, all goals
  cat(sprintf('Calculating Pressures...\n'))
  scores_P = CalculatePressuresAll(layers, conf, gamma=conf$config$pressures_gamma)
  #write.csv(scores_P, '~/Downloads/scores_P.csv', na='', row.names=F)
  #scores_P = read.csv('~/Downloads/scores_P.csv', na.strings='', stringsAsFactors=F)
  scores = scores_P
  
  # Resilience, all goals
  cat(sprintf('Calculating Resilience...\n'))
  scores_R = CalculateResilienceAll(layers, conf)
  #write.csv(scores_R, '~/Downloads/scores_R.csv', na='', row.names=F)
  #scores_R = read.csv('~/Downloads/scores_R.csv', na.strings='', stringsAsFactors=F)
  scores = rbind(scores, scores_R)
  
  # pre-Index functions: Status and Trend, by goal  
  goals_X = subset(conf$goals, !is.na(preindex_function))
  goals_X = goals_X[order(nchar(goals_X$goal), decreasing=T),] # order by length of goal id so subgoals first
  for (i in 1:nrow(goals_X)){ # i=2
    cat(sprintf('Calculating Status and Trend for %s...\n', goals_X$goal[i]))
    scores_X = eval(parse(text=goals_X$preindex_function[i]), envir=conf$functions)[,c('goal','dimension','region_id','score')]
    scores = rbind(scores, scores_X)    
  }

  # Goal Score and Likely Future
  goals_G = as.character(unique(subset(scores, dimension=='status', goal, drop=T)))
  for (g in goals_G){ # g = goals_G[9]
    cat(sprintf('Calculating Goal Score and Likely Future for %s...\n', g))
    
    # cast data
    v = dcast(
      scores, 
      region_id ~ dimension, 
      subset = .(goal==g),
      value.var='score')
  
    # calculate Goal Score and Likely Future
    x = CalculateGoalIndex(
      id         = v$region_id,
      status     = v$status/100,
      trend      = v$trend,
      resilience = v$resilience/100,
      pressure   = v$pressure/100,
      DISCOUNT      = conf$config$goal_discount, 
      BETA          = conf$config$goal_beta, 
      default_trend = conf$config$default_trend)
    x$score = x$score * 100
    x$xF    = x$xF * 100
    
    # melt to scores format
    scores_G = melt(rename(x[,c('id','xF','score')], 
                    c('id'='region_id', 
                      'xF'='future')), 
             id.vars='region_id', 
             variable.name='dimension',
             value.name='score')
    scores_G$goal = g
    
    # bind to other scores
    scores = rbind(scores, scores_G)
  }

  # post-Index functions: supragoals
  goals_Y = subset(conf$goals, !is.na(postindex_function))
  assign('layers', layers, envir=conf$functions)
  for (i in 1:nrow(goals_Y)){ # i=2
    
    cat(sprintf('Calculating post-Index function for %s...\n', goals_Y$goal[i]))
    
    # load environment and run function    
    assign('scores', scores, envir=conf$functions)
    scores = eval(parse(text=goals_Y$postindex_function[i]), envir=conf$functions)    
  }

  # regional Index score by goal weights
  cat(sprintf('Calculating regional Index score by goal weights...\n'))
  supragoals = subset(conf$goals, is.na(parent), goal, drop=T); supragoals
  scores_I = ddply(
    merge(subset(scores, dimension=='score' & goal %in% supragoals),
      conf$goals[,c('goal','weight')]), 
    .(region_id), summarize,
    goal='Index',
    dimension='score',
    score = weighted.mean(score, weight, na.rm=T))
  scores = rbind(scores, scores_I)

  # regional Future score by goal weights
  cat(sprintf('Calculating regional Future score by goal weights...\n'))
  scores_F = ddply(
    merge(subset(scores, dimension=='future' & goal %in% supragoals),
          conf$goals[,c('goal','weight')]), 
    .(region_id), summarize,
    goal='Index',
    dimension='future',
    score = weighted.mean(score, weight, na.rm=T))
  scores = rbind(scores, scores_F)

  # post-process scores, but pre-global calculation
  cat(sprintf('Calculating post-process function...\n'))
  scores = conf$functions$PreGlobalScores(layers, conf, scores)

  # global (region_id-0) scores by area weighting
  cat(sprintf('Calculating GLOBAL (region_id=0) scores by area weighting...\n'))
  # NOTE: Index.future.0 (goal.dimension.region_id) is now 65.61 vs previous 64.71 which was just the mean, not area weighted
  region_areas = rename(SelectLayersData(layers, layers=conf$config$layer_region_areas, narrow=T), 
                        c('id_num'='region_id','val_num'='area')); head(region_areas); subset(region_areas, region_id==213)
  data_0 = merge(subset(scores, dimension %in% c('score','status','future')), 
                 region_areas); head(data_0)
  scores_0 = ddply(
    data_0, .(goal, dimension), summarize,
    region_id = 0,
    score = weighted.mean(score, area, na.rm=T))
  scores = rbind(scores, scores_0)

  # post-process
  cat(sprintf('Calculating FinalizeScores function...\n'))
  scores = conf$functions$FinalizeScores(layers, conf, scores)
  
  # return scores
  return(scores)
}
