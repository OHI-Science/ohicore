#' Calculate Goal Index
#' 
#' Goal-level computation function to goal score ("component indicators for
#' public goals") based on status, trend, resilience, pressure
#' 
#' Parameters:
#' @param id is the subregion identifier
#' @param status (x) score
#' @param trend (t) score for 5 year outloook
#' @param resilience (r) score
#' @param pressure (p) score
#' 
#' Constants: 
#' @param DISCOUNT is the discount multiplier (i.e., df = 1 - rate)
#' @param BETA is the trend dampening multiplier used in likely future status calculation
#' @param default_trend The default trend value (0) if region has NA.
#'
#' @return Returns a data.frame with the input data, a likely future
#' status and OHI score, containing columns: status (x), trend (t), resilience (r), 
#' pressure (p), future status (xF) and goal score (score).
#' @keywords ohi
#' @examples
#' 
#' \dontrun{
#' ## run a model with 50 regions using random data,
#' ## using 5 year 1-percent discount rate and beta = 0.67
#' require(ohi)
#' d <- ohi.model.goal(id = 1:50, 
#'                     status = runif(50, 0, 1), 
#'                     trend = runif(50, -1, 1), 
#'                     resilience = runif(50, 0, 1), 
#'                     pressure = runif(50, 0, 1), 
#'                     DISCOUNT = (1 + 0.01)^-5,
#'                     BETA = 0.67,
#'                     default_trend = 0.0) 
#' ## view model output
#' names(d)
#' d[,c('id','score','xF')]
#' }
#' 
#' @export
CalculateGoalIndex <- function(id, status, trend, resilience, pressure, 
                               DISCOUNT = 1.0, BETA = 0.67, default_trend = 0.0, xlim = c(0, 1)) {
     
     # verify parameters
     #if (getOption('debug', FALSE)) {
     if(!(BETA >= 0 && BETA <= 1))
       stop('Beta parameter must be between 0 and 1; check it in config.R')
     if(!DISCOUNT >= 0)
       stop('Discount parameter must be greater than 0 (1.0 = no discounting); check it in config.R')
  #}
     
     # Simplify symbols based on math writeup
     d <- data.frame(id = id, x = status, t = trend, r = resilience, p = pressure)
     
     # replace a trend of NA with a default (0)
     if (!is.null(default_trend) && is.numeric(default_trend) && any(is.na(d$t))) {
       d$t[is.na(d$t)] <- default_trend
     }
     
     # enforce domains
     if(!(min(d$x, na.rm = T) >= 0  && max(d$x, na.rm = T) <= xlim[2]))   #  [ 0, 1]
       stop('one or more status scores exceed bounds of 0 to ', xlim[2])
     #if(! (min(d$t, na.rm = T) >= -1 && max(d$t, na.rm = T) <= 1) ) browser()
     if(!(min(d$t, na.rm = T) >= -1 && max(d$t, na.rm = T) <= 1))         #  [-1, 1]
       stop('one or more trend scores exceed bounds of -1 to +1')
     if(!(min(d$r, na.rm = T) >= 0  && max(d$r, na.rm = T) <= xlim[2]))   #  [ 0, 1]
       stop('one or more resilience scores exceed bounds of 0 to ', xlim[2])
     if(!(min(d$p, na.rm = T) >= 0  && max(d$p, na.rm = T) <= xlim[2]))   #  [ 0, 1]
       stop('one or more pressure scores exceed bounds of 0 to ', xlim[2])
     
     ## manage when resilience or pressure is NA
     d$p <- ifelse(is.na(d$p), 0, d$p)
     d$r <- ifelse(is.na(d$r), 0, d$r)
     
     # compute "future" status, using all dimensions
     
     #handle cases with NA for resilience or pressures, converts NA to zero so it drops out of equation
     d$r <- ifelse(is.na(d$r), 0, d$r)
     d$p <- ifelse(is.na(d$p), 0, d$p)
     
     ## cap the resilience so that it is no more than the pressures. 
     d$r <- ifelse(d$r > d$p, d$p, d$r)
     
     d$xF <- with(d, (DISCOUNT * (1 + (BETA * t) + ((1-BETA) * (r - p)))) * x)
     # clamp status domain to [0, 1]
     d$xF <- with(d, score.clamp(xF, xlim = c(0,1)))
     # compute score using formula for individual goal component indicator
     d$score <- with(d, (x + xF)/2)
     
     # return results, rescaling if needed
     #if (identical(xlim, c(0,100))){
     #   d[,c('x','r','p','xF','score')] = d[,c('x','r','p','xF','score')]*100
     #}
     return(d)
}