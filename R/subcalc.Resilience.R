#' Calculate the Resilience component of each (sub)goal.
#' 
#' @param gs.regulations (data.frame) contains columns 'region', 'weight', and 'value'
#' @param social.integrity (data.frame) contains columns 'region', and 'value'
#' @param eco.integrity (data.frame) contains columns 'region', and 'value'
#' @param gamma (numeric) represents the weighting between ecological and social aspects of resilience, defaults to 0.5 (equal weights)
#' @return (data.frame) contains columns 'region', 'G', 'Y_S', 'Y_E', and 'r_x'
#' @export
#' 
subcalc.Resilience = function (gs.regulations, eco.integrity, social.integrity, gamma=0.5) {

    G = plyr::ddply(        # Goal-specific regulations component
        gs.regulations,
        c('region'),
        plyr::splat(function (value, weight, ...) {
            return ( sum ( weight * value ) / sum ( weight ) )
        })
    )
    names(G) = gsub("V1", "G", names(G))
    
    
    Y_S = plyr::ddply(      # Social integrity component
        social.integrity,
        c('region'),
        plyr::splat(function (value, ...) {
            return ( mean ( value ) )
        })
    )
    names(Y_S) = gsub("V1", "Y_S", names(Y_S))
    
    Y_E = plyr::ddply(      # Ecological integrity component
        eco.integrity,
        c('region'),
        plyr::splat(function (value, ...) {
            return ( mean ( value ) )
        })
    )
    names(Y_E) = gsub("V1", "Y_E", names(Y_E))
    
    
    full.resilience = plyr::join(G, Y_S, by = c('region'))
    full.resilience = plyr::join(full.resilience, Y_E, by = c('region'))
    
    full.resilience$r_x = ( gamma * ( ( full.resilience$Y_E + full.resilience$G ) / 2.0 ) ) + ( ( 1 - gamma ) * full.resilience$Y_S )
    
    return ( full.resilience )
}
