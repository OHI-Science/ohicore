#' Calculate the Resilience component of each (sub)goal.
#' 
#' @param goal.specific.regulations (data.frame) contains columns 'region', 'weight', and 'value'
#' @param gamma (numeric) represents the weighting between ecological and social aspects of resilience, defaults to 0.5 (equal weights)
#' @return (data.frame) 
#' @export
#' 
CalculateResilienceComponent = function (goal.specific.regulations,
                                         ecological.integrity,
                                         social.integrity,
                                         c.name = 'category', s.name = 'region',
                                         gamma = 0.5) {

    DoSingle = function (gsr, ei, si) {
        # Calculates a sub-component of the resilience component of the subgoal,
        # if the subgoal has only one sub-component, then this is equivalent 
        # to the overall resilience component

        gs.regulations.averaged = plyr::ddply(gsr,
            c(s.name),
            plyr::splat(function (value, weight, ...) {
                c('gs.regulations' = sum(weight * value) / sum(weight))
            }))

        social.integrity.averaged = plyr::ddply(si,
            c(s.name),
            plyr::splat(function (value, ...) {
                c('social.integrity' = mean(value))
            }))

        eco.integrity.averaged = plyr::ddply(ei,
            c(s.name),
            plyr::splat(function (value, ...) {
                c('ecological.integrity' = mean(value))
            }))


        resilience.data = plyr::join(gs.regulations.averaged, 
            social.integrity.averaged, by = c(s.name))
        resilience.data = plyr::join(resilience.data, 
            eco.integrity.averaged, by = c(s.name))

        resilience.integrity = (resilience.data$ecological.integrity +
            resilience.data$social.integrity) / 2

        resilience.data$resilience = (gamma * resilience.integrity) +
            ((1 - gamma) * resilience.data$gs.regulations)

        return (resilience.data)
    }

    if (!is.data.frame(goal.specific.regulations)) {
        full.resilience = data.frame(gs.regulations=c(),
            social.integrity=c(), ecological.integrity=c(), resilience=c())
        full.resilience[s.name] = c()
        for (i in c(1:length(goal.specific.regulations))) {
            full.resilience = rbind(full.resilience, 
                DoSingle(goal.specific.regulations[[i]], 
                    ecological.integrity[[i]],
                    social.integrity[[i]]
                )
            )
        }

        reduced.resilience = plyr::ddply(full.resilience, s.name,
            plyr::splat(function (gs.regulations, ecological.integrity,
                                  social.integrity, resilience, ...) {
                c('gs.regulations' = mean(gs.regulations),
                    'ecological.integrity' = mean(ecological.integrity),
                    'social.integrity' = mean(social.integrity),
                    'resilience' = mean(resilience)
                )
            }))

        return (reduced.resilience)
    } else {
        return (DoSingle(goal.specific.regulations, ecological.integrity,
            social.integrity))
    }
}
