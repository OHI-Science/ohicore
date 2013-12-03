#' Calculate the pressures component of each (sub)goal.
#' 
#' @param eco.pressures data.frame containing columns 'region', 
#'   'category', 'weight', and 'value'
#' @param social.pressures data.frame containing columns 'region', and 'value'
#' @param gamma (optional) if not specified defaults to 0.5
#' @return data.frame containing columns 'region', 'p_E', 'p_S', and 'p_x'
#' @export
#' 
CalculatePressuresComponent <- function (eco.pressures, social.pressures, 
                                         c.name = 'category', s.name = 'region',
                                         gamma = 0.5) {

    DoSingle = function (eco.p, social.p) {
        # Calculates a sub-component of the pressures component of the subgoal,
        # if the subgoal has only one sub-component, then this is equivalent 
        # to the overall pressures component

        pressure.i = plyr::ddply(eco.p, c(s.name, c.name),
            plyr::splat(function (value, weight, ...) {
                c('pressure_i' = sum(weight * value) / 3.0)
            }))

        pressure.i$pressure_i[pressure.i$pressure_i > 1.0] = 1.0

        w.max = plyr::ddply(eco.p, c(s.name, c.name),
            plyr::splat(function (weight, ...) {
                c('max_weight' = max(weight))
            }))

        eco.p = plyr::join(eco.p, pressure.i, by = c(s.name, c.name))
        eco.p = plyr::join(eco.p, w.max, by = c(s.name, c.name))

        ecological = plyr::ddply(eco.p, c(s.name), 
            plyr::splat(function (pressure_i, max_weight, ...) {
                c('ecological' = sum(max_weight * pressure_i) / sum(max_weight))
            }))

        social = plyr::ddply(social.p, c(s.name), 
            plyr::splat(function (value, ...) {
                c('social' = mean(value))
            }))

        full.pressures = plyr::join(ecological, social, by = c(s.name))
        full.pressures$combined = (gamma * full.pressures$ecological) +
            ((1 - gamma) * full.pressures$social)
        
        return (full.pressures)
    }


    if (!is.data.frame(eco.pressures)) {
        full.pressures = data.frame(ecological=c(), social=c(), combined=c())
        full.pressures[s.name] = c()
        for (i in c(1:length(eco.pressures))) {
            full.pressures = rbind(full.pressures, 
                DoSingle(eco.pressures[[i]], social.pressures[[i]]))
        }

        reduced.pressures = plyr::ddply(full.pressures, s.name,
            plyr::splat(function (ecological, social, combined, ...) {
                c('ecological' = mean(ecological),
                    'social' = mean(social),
                    'combined' = mean(combined)
                )
            }))

        return (reduced.pressures)
    } else {
        return (DoSingle(eco.pressures, social.pressures))
    }
}
