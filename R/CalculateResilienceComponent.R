#' Calculate the Resilience component of each (sub)goal.
#' 
#' @param goal.specific.regulations (data.frame) contains columns 'region', 'weight', and 'value'
#' @param gamma (numeric) represents the weighting between ecological and social aspects of resilience, defaults to 0.5 (equal weights)
#' @return (data.frame) 
#' @export
#' 

# Needs work
CalculateResilienceComponent <- function (data,
                                          goal.specific.regulations.columns,
                                          ecological.integrity.columns,
                                          social.integrity.columns,
                                          gamma = 0.5) {

  goal.specific.regulations <- plyr::ddply(goal.specific.regulations.data,
                                c('spatial'),
                                plyr::splat(function (value, weight, ...) {
                                  return (sum(weight * value) / sum(weight))
                                }))
  names(goal.specific.regulations) <- gsub("V1", "goal.specific.regulations",
                                names(goal.specific.regulations))
  
  social.integrity <- plyr::ddply(social.integrity.data,
                                  c('spatial'),
                                  plyr::splat(function (value, ...) {
                                    return (mean(value))
                                  }))
  names(social.integrity) <- gsub("V1", "social.integrity",
                                  names(social.integrity))
  
  ecological.integrity <- plyr::ddply(ecological.integrity.data,
                                      c('spatial'),
                                      plyr::splat(function (value, ...) {
                                        return (mean(value))
                                      }))
  names(ecological.integrity) <- gsub("V1", "ecological.integrity",
                                      names(ecological.integrity))
  
  resilience.data <- plyr::join(goal.specific.regulations, social.integrity,
                                by = c('spatial'))
  resilience.data <- plyr::join(resilience.data, ecological.integrity,
                                by = c('spatial'))
  
  resilience.integrity <- (resilience.data$ecological.integrity +
    resilience.data$social.integrity) / 2
  
  resilience <- (gamma * resilience.integrity) +
    ((1 - gamma) * resilience.data$goal.specific.regulations)
  
  return (resilience)
}
