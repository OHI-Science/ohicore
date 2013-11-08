#' Calculate the pressures component of each (sub)goal.
#' 
#' @param eco.pressures data.frame containing columns 'region', 'category', 'weight', and 'value'
#' @param social.pressures data.frame containing columns 'region', and 'value'
#' @param gamma (optional) if not specified defaults to 0.5
#' @return data.frame containing columns 'region', 'p_E', 'p_S', and 'p_x'
#' @export
#' 
subcalc.Pressures <- function (eco.pressures, social.pressures, gamma=0.5) {


    p_i = plyr::ddply(          # Intermediate calculation of stressor intensity within Pressure categories
        eco.pressures,
        c('region', 'category'),
        plyr::splat(function (value, weight, ...) {
            return ( sum( weight * value ) / 3.0 )
        })
    )
    names(p_i) = gsub("V1", "p_i", names(p_i))
    p_i$p_i[p_i$p_i > 1.0] = 1.0


    w_max = plyr::ddply(        # Intermediate calculation of the maximum weight within each category
        eco.pressures,
        c('region', 'category'),
        plyr::splat(function (weight, ...) {
            return( max( weight ) )
        })
    )
    names(w_max) = gsub("V1", "w_max", names(w_max))

    
    eco.pressures = plyr::join(eco.pressures, p_i, by = c('region', 'category'))
    eco.pressures = plyr::join(eco.pressures, w_max, by = c('region', 'category'))

    
    p_E = plyr::ddply(          # Ecological Pressure component calculation
        eco.pressures,
        c('region'),
        plyr::splat(function (p_i, w_max, ...) {
            return ( sum ( w_max * p_i ) / sum ( w_max ) )
        })
    )
    names(p_E) = gsub("V1", "p_E", names(p_E))

    
    p_S = plyr::ddply(          # Social Pressure component calculation
        social.pressures,
        c("region"),
        plyr::splat(function (value, ...) {
            return ( mean ( value ) )
        })
    )
    names(p_S) = gsub("V1", "p_S", names(p_S))

    
    full.Pressures = plyr::join(p_E, p_S, by = c('region'))
    full.Pressures$p_x = ( gamma * full.Pressures$p_E ) + ( (1 - gamma) * full.Pressures$p_S )


    return ( full.Pressures )
    
    
}



