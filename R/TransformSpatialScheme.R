#' Transform data
#' 
#' @param object instance of SpatialSchemes class
#' @param data data.frame such as returned from 'SelectLayers' function
#' @param target single spatial scheme to which data should be transformed
#' @param origin spatial schemes from which to transform, can be vector
#' @param categories layers for which transformation should be done (to be safe, for now this should be all the layers in param data)
#' @return data.frame transformed data
#' @export
TransformSpatialScheme = function (object, data, target, origin, categories) {
    
    # grab the data that isn't going to be transformed
    uninteresting.data = data[!is.na(data[target]), ]
    
    # find the index of the specific transformation we're going to use here
    map.index = which(object$schemes.navigation$target_scheme == target & 
        object$schemes.navigation$origin_scheme %in% origin)
    
    
    maps = list(origin = object$schemes.navigation$origin_scheme[map.index],
        map = object$scheme.maps[map.index])
    
    # ...and grab the method, map
    method = object$schemes.navigation$method[map.index]
    
    if (method == 'data.frame') {
        transformed.data = plyr::ldply(origin,
            function (X) {
                
                map = maps$map[[which(maps$origin == ORIGIN)]]
            
                focus.data = data[!is.na(data[X]), ]
                
                mapped = plyr::join(focus.data[, 
                    -which(names(focus.data) == target)], map, by = X)
                
                reduced = plyr::ddply(mapped,
                    c(target, 'year'),
                    function (x) {
                        sapply(x[, which(names(x) %in% categories)],
                            stats::weighted.mean,
                            x$w)
                    })
                
                return (reduced)
            }
        )
    
    } else if (method == 'matrix') {
        transformed.data = plyr::ldply(origin,
            function (ORIGIN) {
            
                map = maps$map[[which(maps$origin == ORIGIN)]]
            
                focus.data = data[!is.na(data[ORIGIN]), ]
                
                plyr::ddply(focus.data, c('year'), function (YEAR) {
                    
                    ordered.df = plyr::join(setNames(data.frame(colnames(map)),
                        ORIGIN), YEAR[, -which(names(YEAR) == target)], 
                        by = ORIGIN)
                    
                    mapped = as.data.frame(apply(
                        ordered.df[, which(names(ordered.df) %in% categories)],
                        2,
                        function (x) {
                            
                            # if any are defined, convert non-defineds to zero
                            #   and run analysis
                            if (Reduce('|', !is.na(x) & !is.nan(x))) {
                                map %*% replace(x, is.na(x) | is.nan(x), 0)
                            # else make NA
                            } else {
                                rep(NA, nrow(map))
                            }
                        }
                    ))
                    
                    mapped$region = rownames(map)
                    return (mapped)

                })
            }
        )
        
    } else if (method == 'function') {
        transformed.data = plyr::ldply(origin,
            function (ORIGIN) {
                map = maps$map[[which(maps$origin == ORIGIN)]]
            
                focus.data <<- data[!is.na(data[ORIGIN]), ]
                plyr::ddply(focus.data, c('year'), map)
            }
        )
    
    } else {
        stop ('method not known')
    }
    
    combined.data = plyr::rbind.fill(uninteresting.data, transformed.data)
    return (combined.data[, -which(names(combined.data) %in% origin)])
}
