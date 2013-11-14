
Layers = setRefClass(
    'Layers',
    fields = list(
        layer.data = 'list',
        layers.navigation = 'data.frame'
    ),
    methods = list(
        initialize = function (file) {
            .self$layers.navigation = read.csv(file, header = T)
            .self$layer.data = apply(
                .self$layers.navigation,
                1,
                function (X) {
                    data = read.csv(X['filepath'], header = T)
                    data$layer.id = X['layer_id']
                    return (data)
                })
        },
        show = function () {
            print (layers.navigation[-which(names(layers.navigation) == 'filepath')])
        }
    )
)


setGeneric("SelectLayers", SelectLayers)

setMethod("names", 'Layers', 
    function (x) {
        names(x$layers.navigation[
            -which(names(x$layers.navigation) == 'filepath')])
    }
)
