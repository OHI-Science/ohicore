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
#' @details
#' Performs the following sequence of functions, some of which are [optional]:
#' \enumerate{
#'   \item [functions.R:\code{Setup}()] - execute function \code{Setup}() if defined in file functions.R. 
#'   This function typically installs extra packages upon which the other functions in functions.R depend.
#'   \item \code{\link{CalculatePressuresAll}()} - calculate pressures across all goals using pressures_matrix.csv.
#'   \item \code{\link{CalculateResilienceAll}()} - calculate resilience across all goals using resilience_matrix.csv and resilience_weights.csv.
#'   \item goals.csv:\code{preindex_functions} - execute code in the \code{preindex_function} column of the goals.csv file 
#'   based on \code{order_calculate} using functions defined in functions.R. 
#'   These funcutions are usually for calculating the goal's status and trend dimensions, ie 
#'   the additional dimensions beyond pressures and resilience needed to calculate a goal index score.
#'   \item \code{\link{CalculateGoalIndex}()} - run function for every goal having a status dimension assigned from the \code{preindex_functions}.
#'   \item goals.csv:\code{postindex_functions} - execute code in the \code{postindex_function} column of the goals.csv file based on \code{order_calculate}
#'   using functions defined in functions.R. These functions are usually for goals containing subgoals, ie those without their own directly calculated 
#'   index scores, but rather scores representing averages of subgoals.
#'   \item regional index - calculate regional index score as weighted mean using goals.csv:\code{weight}.
#'   \item regional likely future - calculate regional likely future score (ie goal='Index' and dimension='future') across supragoals (ie goals without a \code{parent} in goals.csv).
#'   \item [functions.R:\code{PreGlobalScores}()] - execute function \code{PreGlobalScores}() if defined in file functions.R. 
#'   This function could perform a variety of operations on the regional scores, strategically before calculating the global scores.
#'   \item global (region_id=0) scores - calculate scores for global (region_id=0) with regional values weighted by config.R:\code{layer_region_areas}.
#'   \item [functions.R:\code{FinalizeScores}()] - execute function \code{FinalizeScores}() if defined in file functions.R. 
#'   This function could perform a variety of operations on the regional and global scores.
#' }
#' @keywords ohi
#' @examples
#' 
#' \dontrun{
#' ## run a scenario assuming setwd() to directory containing default names for directories and files
#' ## setup
#' require(ohi)
#' conf       = Conf('conf')
#' layers     = Layers(layers.csv = 'layers.csv', 
#'                     layers.dir = 'layers')
#' ## calculate
#' scores = CalculateAll(conf, layers, debug=T)
#' 
#' ## write
#' write.csv(scores, 'scores.csv', na='', row.names=F)
#' }
#' @import plyr dplyr
#' 
#' @export
CalculateAll = function(conf, layers, debug=F){

  # remove global scores
  if (exists('scores', envir=.GlobalEnv)) rm(scores, envir=.GlobalEnv)
  
  # Run Setup, all goals
  if ('Setup' %in% ls(conf$functions)){
    cat('Running Setup()...\n')
    conf$functions$Setup()
  }
  
  cat(sprintf('\n  getwd(): %s\n', getwd()))
  
  # Pressures, all goals
  cat(sprintf('Calculating Pressures...\n'))
  scores = CalculatePressuresAll(layers, conf, gamma=conf$config$pressures_gamma, debug)
  
  cat(sprintf('\n  getwd(): %s\n', getwd()))
  
  # Resilience, all goals
  cat(sprintf('Calculating Resilience...\n'))
  scores = rbind(scores, CalculateResilienceAll(layers, conf, debug))
  
  cat(sprintf('\n  getwd(): %s\n', getwd()))
  
  # pre-Index functions: Status and Trend, by goal  
  goals_X = conf$goals %.%
    filter(!is.na(preindex_function)) %.%
    arrange(order_calculate)
  for (i in 1:nrow(goals_X)){ # i=1
    g = goals_X$goal[i]
    cat(sprintf('Calculating Status and Trend for %s...\n', g))
    assign('scores', scores, envir=conf$functions)
    if (nrow(subset(scores, goal==g & dimension %in% c('status','trend')))!=0) stop(sprintf('Scores were assigned to goal %s by previous goal function.', g))    
    scores = rbind(scores, eval(parse(text=goals_X$preindex_function[i]), envir=conf$functions)[,c('goal','dimension','region_id','score')])
    cat(sprintf('\n  getwd(): %s\n', getwd()))
    
  }

  # Goal Score and Likely Future
  goals_G = as.character(unique(subset(scores, dimension=='status', goal, drop=T)))
  for (g in goals_G){ # g = goals_G[9]
    cat(sprintf('Calculating Goal Score and Likely Future for %s...\n', g))
    
    # cast data
    v = dcast(scores, region_id ~ dimension, subset = .(goal==g), value.var='score')
    
    # calculate Goal Score and Likely Future
    x = CalculateGoalIndex(
      id         = v$region_id,
      status     = v$status/100,
      trend      = v$trend,
      resilience = v$resilience/100,
      pressure   = v$pressures/100,
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
  for (i in 1:nrow(goals_Y)){ # i=2
    
    cat(sprintf('Calculating post-Index function for %s...\n', goals_Y$goal[i]))
    
    # load environment and run function    
    assign('scores', scores, envir=conf$functions)
    scores = eval(parse(text=goals_Y$postindex_function[i]), envir=conf$functions)    
  }

  # regional Index score by goal weights
  cat(sprintf('Calculating regional Index score by goal weights...\n'))
  supragoals = subset(conf$goals, is.na(parent), goal, drop=T); supragoals
  scores = rbind(scores, ddply(
    merge(subset(scores, dimension=='score' & goal %in% supragoals),
      conf$goals[,c('goal','weight')]), 
    .(region_id), summarize,
    goal='Index',
    dimension='score',
    score = weighted.mean(score, weight, na.rm=T)))

  # regional Future score by goal weights
  cat(sprintf('Calculating regional Future score by goal weights for supragoals...\n'))
  scores = rbind(
    scores, 
    ddply(
      merge(
        subset(scores, dimension=='future' & goal %in% supragoals),
        conf$goals[,c('goal','weight')]), 
      .(region_id), summarize,
      goal='Index',
      dimension='future',
      score = weighted.mean(score, weight, na.rm=T)))

  # post-process scores, but pre-global calculation
  if ('PreGlobalScores' %in% ls(conf$functions)){
    cat(sprintf('Calculating post-process PreGlobalScores() function...\n'))
    scores = conf$functions$PreGlobalScores(layers, conf, scores)
  }
  
  # global (region_id-0) scores by area weighting
  cat(sprintf('Calculating GLOBAL (region_id=0) scores by area weighting...\n'))
  # NOTE: Index.future.0 (goal.dimension.region_id) is now 65.61 vs previous 64.71 which was just the mean, not area weighted
  region_areas = rename(SelectLayersData(layers, layers=conf$config$layer_region_areas, narrow=T), 
                        c('id_num'='region_id','val_num'='area')); head(region_areas); subset(region_areas, region_id==213)
  data_0 = merge(subset(scores, dimension %in% c('score','status','future')), 
                 region_areas); head(data_0)
  scores = rbind(scores, ddply(
    data_0, .(goal, dimension), summarize,
    region_id = 0,
    score = weighted.mean(score, area, na.rm=T)))

  # post-process
  if ('FinalizeScores' %in% ls(conf$functions)){
    cat(sprintf('Calculating FinalizeScores function...\n'))
    scores = conf$functions$FinalizeScores(layers, conf, scores)
  }
  
  # check that scores are not duplicated
  stopifnot(sum(duplicated(scores[,c('region_id','goal','dimension')]))==0)
  
  # return scores
  return(scores)
}
