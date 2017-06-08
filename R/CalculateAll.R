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
#'
#' @export
CalculateAll = function(conf, layers){

  ## Remove global scores
  if (exists('scores', envir=.GlobalEnv)) rm(scores, envir=.GlobalEnv)

 ## Run Setup, all goals
    if ('Setup' %in% ls(conf$functions)){
      cat('Running Setup()...\n')
      conf$functions$Setup()
    }

  ## Access Pre-Index functions: Status and Trend, by goal
  goals_X = conf$goals %>%
    dplyr::filter(!is.na(preindex_function)) %>%
    dplyr::arrange(order_calculate)

  ## Setup scores variable; many rbinds to follow
  scores = data.frame(
    goal      = character(0),
    dimension = character(0),
    region_id = integer(0),
    score     = numeric())

  ## Calculate Status and Trend, all goals
  for (i in 1:nrow(goals_X)){ # i=5
    g = goals_X$goal[i]
    cat(sprintf('Calculating Status and Trend for each region for %s...\n', g))

    assign('scores', scores, envir=conf$functions)
    if (nrow(subset(scores, goal==g & dimension %in% c('status','trend')))!=0) stop(sprintf('Scores were assigned to goal %s by previous goal function.', g))
    scores_g = eval(parse(text=goals_X$preindex_function[i]), envir=conf$functions)
    
    # error if 'status' or 'trend' are missing
    if ( !all( c('status', 'trend') %in% unique(scores_g$dimension)) ){
      stop(sprintf('Missing "status" or "trend" dimension in %s goal model\n', g))
    }
    # error if something other than 'status' or 'trend' as dimension
    if ( !all(unique(scores_g$dimension) %in% c('status', 'trend')) ){
      stop(sprintf('"status" and "trend" should be the only dimensions in %s goal model\n', g))
    }
    
    if (nrow(scores_g) > 0){
      scores = rbind(scores, scores_g[,c('goal','dimension','region_id','score')])
    }
  }

  ## Calculate Pressures, all goals
  layers = Layers(layers.csv = 'layers.csv', layers.dir = 'layers')
  scores_P = CalculatePressuresAll(layers, conf)
  scores = rbind(scores, scores_P)

  ## Calculate Resilience, all goals
  scores_R = CalculateResilienceAll(layers, conf)
  scores = rbind(scores, scores_R)
  scores = data.frame(scores)

  ## Calculate Goal Score and Likely Future, all goals
  goals_G = as.character(unique(subset(scores, dimension=='status', goal, drop=T)))
  for (g in goals_G){ # g = 'FIS'
    cat(sprintf('Calculating Goal Score and Likely Future for each region for %s...\n', g))

    ## spread the scores by dimension
    v = scores %>%
      dplyr::filter(goal == g) %>%
      tidyr::spread(dimension, score)

   ## message if missing dimension, assign NA
    for (col in c('status','trend','pressures','resilience')){
      if (!col %in% names(v)){
        cat(sprintf('  missing %s dimension, assigning NA!\n', col))
        v[col] = NA
      }
    }

    ## Calculations and Scaling
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

    ## Gather to scores format: goal, dimension, region_id, score
    scores_G = x %>%
        dplyr::select(region_id = id,
                      future    = xF,
                      score) %>%
      tidyr::gather(dimension, score, -region_id) %>%
      dplyr::mutate(goal = g) %>%
      dplyr::select(goal, dimension, region_id, score)

    ## bind to other scores
    scores = rbind(scores, scores_G)
  }

  ## Post-Index functions: Calculate Status, Trend, Likely Future State and Scores for 'Supragoals'
  goals_Y = subset(conf$goals, !is.na(postindex_function))
  supragoals = subset(conf$goals, is.na(parent), goal, drop=T); supragoals

  for (i in 1:nrow(goals_Y)){ # i = 1

    cat(sprintf('Calculating post-Index function for each region for %s...\n', goals_Y$goal[i]))

    ## load environment and run function from functions.r
    assign('scores', scores, envir=conf$functions)
    scores = eval(parse(text=goals_Y$postindex_function[i]), envir=conf$functions)
  }

  ## Calculate Overall Index Scores for each region using goal weights
  cat(sprintf('Calculating Index Score for each region using goal weights to combine goal scores...\n'))

  # calculate weighted-mean Index scores from goal scores and rbind to 'scores' variable
  scores =
    rbind(scores,
        scores %>%

            # filter only supragoal scores, merge with supragoal weightings
            dplyr::filter(dimension=='score',  goal %in% supragoals) %>%
            merge(conf$goals %>%
                    dplyr::select(goal, weight)) %>%
            dplyr::mutate(weight = as.numeric(weight)) %>%

            # calculate the weighted mean of supragoals, add goal and dimension column
            dplyr::group_by(region_id) %>%
            dplyr::summarise(score = weighted.mean(score, weight, na.rm=T)) %>%
            dplyr::mutate(goal      = 'Index',
                   dimension = 'score') %>%
            data.frame())


  ## Calculate Overall Index Likely Future State for each region
  cat(sprintf('Calculating Index Likely Future State for each region...\n'))

  # calculate weighted-mean Likely Future State scores and rbind to 'scores' variable
  scores =
    rbind(scores,
          scores %>%

            # filter only supragoal scores, merge with supragoal weightings
            dplyr::filter(dimension=='future',  goal %in% supragoals) %>%
            merge(conf$goals %>%
                    dplyr::select(goal, weight)) %>%

            # calculate the weighted mean of supragoals, add goal and dimension column
            dplyr::group_by(region_id) %>%
            dplyr::summarise(score = weighted.mean(score, weight, na.rm=T)) %>%
            dplyr::mutate(goal      = 'Index',
                   dimension = 'future') %>%
            data.frame())

  ## Post-process scores, but pre-global calculation: for global assessment only
  if ('PreGlobalScores' %in% ls(conf$functions)){
    cat(sprintf('Calculating Post-process PreGlobalScores() function for each region...\n'))
    scores = conf$functions$PreGlobalScores(layers, conf, scores)
  }

  ## Assessment Areas (sometimes known as 'global', region_id-0) scores by area weighting
  cat(sprintf('Calculating scores for ASSESSMENT AREA (region_id=0) by area weighting...\n'))

  ## Calculate area-weighted Assessment Area scores and rbind to all scores
  scores = rbind(
    scores,
    scores %>%

      # filter only score, status, future dimensions, merge to the area (km2) of each region
      dplyr::filter(dimension %in% c('score','status','future')) %>%
      merge(SelectLayersData(layers, layers=conf$config$layer_region_areas, narrow=T) %>%
              dplyr::select(region_id = id_num,
                            area      = val_num)) %>%

      # calculate weighted mean by area
      dplyr::group_by(goal, dimension) %>%
      dplyr::summarise(score = weighted.mean(score, area, na.rm=T),
                region_id = 0) %>%
      ungroup())

  ## post-process
  if ('FinalizeScores' %in% ls(conf$functions)){
    cat(sprintf('Calculating FinalizeScores function...\n'))
    scores = conf$functions$FinalizeScores(layers, conf, scores)
  }

  ## check that scores are not duplicated
  stopifnot(sum(duplicated(scores[,c('region_id','goal','dimension')]))==0)

  # return scores
  return(scores)
}
