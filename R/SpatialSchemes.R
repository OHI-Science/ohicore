

SpatialSchemes = setRefClass(
    'SpatialSchemes',
    fields = list(
        scheme.maps = 'list',
        schemes.navigation = 'data.frame'
    ),
    methods = list(
        initialize = function (file) {
            .self$schemes.navigation = read.csv(file, header = T)
            .self$scheme.maps = apply(
                .self$schemes.navigation,
                1,
                function (X) {
                    if (X['method'] == 'data.frame') {
                        data = read.csv(X['filepath'], header = T)
                        return (data)
                    } else if (X['method'] == 'matrix') {
                        data = read.csv(X['filepath'], header = T)
                        
                        mat = as.matrix(data[, -1])
                        rownames(mat) = as.vector(data[,1])
                        
                        return (mat)
                    } else {
                        return (NULL)
                    }
                })
        },
        show = function () {
            print (schemes.navigation[-which(names(schemes.navigation) == 'filepath')])
        }
    )
)


setGeneric("SelectLayers", SelectLayers)

setMethod("names", 'SpatialSchemes', 
    function (x) {
        names(x$schemes.navigation[
            -which(names(x$schemes.navigation) == 'filepath')])
    }
)
