#' Compute a single subgoal.
#' 
#' @param current.data data.frame containing columns 'region', 'value'
#' @param fun (optional) function for calculating the subgoal value, if not specified it will default to a weighted average
#' @return stuff
#' @export
#' 
CalculateSubgoal <- function (current.data, eco.pressures, social.pressures, 
                              gs.regulations, social.integrity, eco.integrity, 
                              fun=stats::weighted.mean, trend.Years=5) {

  components = CalculateStatusComponent(current.data, fun, trend.Years)
  components$pressures = CalculatePressuresComponent(eco.pressures, 
                                                     social.pressures)
  components$resilience = CalculateResilienceComponent(gs.regulations, 
                                                       social.integrity, 
                                                       eco.integrity)

  dell = 0
  beta = 0.67

  x.hat = (components$status * (1 + (beta * components$trend) + ((1 - beta) * 
    (components$resilience - components$pressures)))) / (1 + dell)

  out = ((components$status + x.hat) / 2.0)

  return (out)
}
