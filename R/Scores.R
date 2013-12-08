#' Scores reference class.
#' 
#' @param scores.csv path to comma-seperated results file, long style
#' @return object reference class of Layers containing
#' \itemize{
#'  \item{\emph{data} - long view (many rows) of score results with columns: region_id, goal, dimension, score}
#' }
#' @details To instantiate this object, \code{Scores(results.csv)} is used. The \code{results.csv} is expected to have the following columns:
#' \itemize{
#'   \item{\emph{region_id} - unique numeric region identifier, reserving 0 as the region_id for the area-weighted average of the entire study area}
#'   \item{\emph{goal} - the goal code or Index}
#'   \item{\emph{dimension} - the dimension code, one of: status, trend, pressures, resilience, future, score}
#'   \item{\emph{score} - the numeric score: 0-100 for all dimensions, except trend (-1 to 1)}
#' }
#' To get the wide view (many columns, with one row per region and columns having combination of goal and dimension), 
#' use something like: \code{reshape2::dcast(.self$long, region_id ~ goal + dimension, value.var='score')}.
#' @export
Scores = setRefClass(
  'Scores', fields = list(
    data = 'data.frame'),
  methods = list(
    initialize = function(scores.csv) {
      # TODO: validate fields of results.csv
     .self$data = read.csv(scores.csv, header = T)
     },
    write = function(scores.csv){
      write.csv(.self$data, scores.csv, row.names=F, na='')
    },
    show = function () {
      print(summary(.self$data))
    })
)