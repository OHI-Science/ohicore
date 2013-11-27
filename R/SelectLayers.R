#' Select a set of layers.
#' 
#' @param object instance of Layers class
#' @param mode {all | target | layers} defines how to select layers
#' @param target only needed if mode='target', specifies the target (from
#'   layers.navigation) which should be selected
#' @param layers only needed if mode='layers', specifies the layers which should
#'   be selected
#' @param alternate.layer.names aliases for layer names
#' @param expand.time.invariant for layers without a year column, populate the
#'   same value throughout all years where available in other layer(s)
#' @param cast {T|F} whether to cast the resulting dataset, or leave it melted,
#'   defaults to TRUE
#' @return data.frame with data from selected layers
#' @export
SelectLayers = function (object, mode = "all", cast = T,
                         target = NULL, layers = NULL,
                         expand.time.invariant = F,
                         alternate.layer.names = NULL) {
  

    if (mode == "layers") {
        focus.data = plyr::rbind.fill(
          object$data[names(object$data) %in% layers]
        )
    } else if (mode == "target") {
        browser()
        layers.with.target = names(which(sapply(object$targets, function(x){ target %in% x }) == T))
        focus.data = plyr::rbind.fill(
            object$data[names(object$data) %in% layers.with.target]
        )
    } else if (mode == "all") {
        focus.data = plyr::rbind.fill(
            object$data
        )
    } else {
        stop ("mode not understood")
    }
    if (cast) {
        stationary.columns = which(names(focus.data) %in% 
            c('value', 'layer_id'))
        formula.text = paste(
            paste(names(focus.data)[-stationary.columns], 
            collapse = '+'), '~layer_id')
            

        recasted.data = reshape2::dcast(focus.data, as.formula(formula.text),
            value.var = 'value', fun.aggregate=mean)

        if (expand.time.invariant) {
            ti.logical = plyr::ldply(recasted.data[, layers], function(X) {
                Reduce('|', !is.na(recasted.data$year) & !is.nan(X))
            })
            
            spatial = names(recasted.data)[-which(names(recasted.data) %in% 
                c('year', layers))]
            time.invariants = ti.logical$.id[!ti.logical$V1] # DOH!

            base = recasted.data[!is.na(recasted.data$year),
                -which(names(recasted.data) %in% time.invariants)]

            for (TI in time.invariants) {
                base = plyr::join(base, 
                    recasted.data[!is.nan(recasted.data[,TI]), c(spatial, TI)],
                    by = spatial)
            }

            recasted.data = base

        }

        if (!is.null(alternate.layer.names)) {
            names(recasted.data)[names(recasted.data) %in% layers] = alternate.layer.names
        }

        return (recasted.data)

    } else {
        return (focus.data)
    }
}