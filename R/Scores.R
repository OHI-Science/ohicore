#' Scores reference class.
#' 
#' @param results.csv path to comma-seperated value results file, long style
#' @return object (non-instantiated) reference class of Layers containing
#' \itemize{
#'  \item{\emph{long} - long view (many rows) of score results with columns: region, goal, dimension, score}
#'  \item{\emph{wide} - wide view (many columns) with one row per region and columns having combination of goal and dimension}
#' }
#' @details To instantiate this object, \code{Scores(results.csv)} is used. The \code{results.csv} is expected to have the following columns:
#' \itemize{
#'   \item{\emph{region_id} - unique region identifier}
#'   \item{\emph{goal} - the goal code}
#'   \item{\emph{dimension} - the dimension code}
#'   \item{\emph{score} - the numeric score: 0-100 for all dimensions, except trend (-1 to 1)}
#' }
#' @export
# just a placeholder for now. See Layers.R
Scores = setRefClass(
  'Scores', fields = list(
    long = 'data.frame',
    wide = 'data.frame'
    ),
  methods = list(
    initialize = function(results.csv) {
      # TODO: validate fields of results.csv
     .self$long = read.csv(results.csv, header = T)
     .self$wide = reshape2::dcast(.self$long, as.formula('region_id ~ .'), value.var = 'score')
     },
    show = function () {
      print(summary(long))
    })
)