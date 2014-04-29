##' Score Scaling Functions
##' 
##' Scoring functions
##' 
##' @aliases score.rescale score.max score.clamp
##' @param x A numeric vector of data.
##' @param xlim The scoring range. If null, derives range from data.
##' @param p A percentage buffer to add to the maximum value.
##' @param method Only 'linear' is supported.
##' @param ... Arguments for min, max, pmin, pmax.
##' @return Returns scores.
##' @seealso min, max, pmin, pmax
##' @keywords ohi
##' @export score.rescale score.max score.clamp
##' @examples
##' 
##' score.max(c(0.5, 1, 2))
##' score.max(c(0.5, 1, 2), p=0.25)
##' score.rescale(c(0.5, 1, 2))
##' score.clamp(c(-0.5, 1, 2))
##' score.clamp(c(-0.5, 1, 2), xlim=c(-1, 1))
##' @name ScoreScaling

# scoring functions -------------------------------------------------------
#
# > score.max(c(0.5, 1, 2))
# [1] 0.25 0.50 1.00
# > score.rescale(c(0.5, 1, 2))
# [1] 0.00 0.33 1.00
# > score.clamp(c(-0.5, 1, 2))
# [1] 0.00 1.00 1.00
#
score.rescale <- function(x, xlim=NULL, method='linear', ...) {
  if (is.null(xlim)) {
    xlim <- c(min(x, ...), max(x, ...))
  }
  if (getOption('debug', FALSE)) {
    stopifnot(method == 'linear')
    stopifnot(length(xlim) == 2)
    stopifnot(xlim[1] == min(xlim))
    stopifnot(xlim[2] == max(xlim))
  }
  
  (x - xlim[1])/(xlim[2]-xlim[1])
}

score.max <- function(x, p=0.0, ...) {
  score.rescale(x, xlim=c(0, (max(x, ...)*(1.0 + p))))
}

score.clamp <- function(x, xlim=c(0,1), ...) {
  if (getOption('debug', FALSE)) {
    stopifnot(length(xlim) == 2)
    stopifnot(xlim[1] == min(xlim))
    stopifnot(xlim[2] == max(xlim))
  }
  
  # x must be first since its class is used in mismatches
  pmin(pmax(x, xlim[1]), xlim[2]) 
}

if (getOption('debug', FALSE) && getOption('unit.test', FALSE)) {
  stopifnot(score.rescale(c(-0.5, 0.0, 0.5, 1.0, 1.5)) == c(0.00, 0.25, 0.5, 0.75, 1.00))
  stopifnot(score.max(c(-0.5, 0.0, 0.5, 1.0, 1.5)) == c(-1/3, 0, 1/3, 2/3, 1))
  stopifnot(score.clamp(c(-0.5, 0.0, 0.5, 1.0, 1.5)) == c(0, 0, 0.5, 1, 1))
}