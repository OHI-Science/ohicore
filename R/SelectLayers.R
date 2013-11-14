#' Select a set of layers.
#' 
#' @param object instance of Layers class
#' @param mode {all|target|layers} defines how to select layers
#' @param target only needed if mode='target', specifies the target (from layers.navigation) which should be selected
#' @param layers only needed if mode='layers', specifies the layers which should be selected
#' @param cast {T|F} whether to cast the resulting dataset, or leave it melted, defaults to TRUE
#' @return data.frame with data from selected layers
#' @export
SelectLayers = function (object, mode = "all", cast = T,
                         target = NULL, layers = NULL) {
    if (mode == "layers") {
        focus.data = plyr::rbind.fill(
            object$layer.data[object$layer.id %in% layers]
        )
    } else if (mode == "target") {
        focus.data = plyr::rbind.fill(
            object$layer.data[object$layers.navigation$target == target]
        )
    } else if (mode == "all") {
        focus.data = plyr::rbind.fill(
            object$layer.data
        )
    } else {
        stop ("mode not understood")
    }
    if (cast) {
        stationary.columns = which(names(focus.data) %in% 
            c('value', 'layer.id'))
        formula.text = paste(
            paste(names(focus.data)[-stationary.columns], 
            collapse = '+'), '~layer.id')
        return (reshape2::dcast(focus.data, as.formula(formula.text),
            value.var = 'value', fun.aggregate=mean))
    } else {
        return (focus.data)
    }
}




