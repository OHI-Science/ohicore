#' Transform layer data from one spatial partitioning scheme to another using arbitrary scheme-to-scheme mapping.
#' 
#' @param d (data.frame) contains data with columns corresponding to spatial partitioning schemes, layer categories, and layer values
#' @param main.scheme (string) name of column which describes the spatial partitioning scheme to which layer data should be converted
#' @param map (list) 
#' @param value.var (string) name of column which contains the layer data
#' @param category.var (string) name of column which contains the layer category
#' @return (data.frame) 
#' @export
#' 
ConvertSpatialScheme <- function(d, main.scheme, map, ..., 
                                 value.var='value', category.var='category') {

  DoConversion <- function (s) {
    # Used exclusively in plyr::ldply below
    # Checks form of input data (an element of map)
    # if form is acceptable (ie is a matrix, data.frame, or function)
    # computes the map and returns 
    focus.data <- mutable.data[!is.na(mutable.data[s]), ]

    if (is.data.frame(map[[s]])) {
      mapped <- plyr::ddply(focus.data,
                            category.var,
                            function (x) {
                              plyr::join(x, map[[s]], by = s)
                            })
      
      reduced <- plyr::ddply(mapped,
                             c(main.scheme, 'category'),
                             function (x) {
                               c('value' = stats::weighted.mean(x$value, x$w))
                             })
  
      return (reduced)
    } else if (is.matrix(map[[s]])) {
      out <- plyr::ddply(focus.data,
                         category.var,
                         function (y) {
                           # check to make sure names are equal
                           if (!(Reduce('&', 
                                        as.vector(y[[s]]) %in%
                                          colnames(map[[s]]))
                              && Reduce('&',
                                        colnames(map[[s]]) %in%
                                          as.vector(y[[s]])))) {
                             stop('Matrix colnames != origin names')
                           }
                           
                           x <- plyr::join(setNames(
                             data.frame(s = colnames(map[[s]])), s), y, by=s)
                           
                           B <- map[[s]] %*% x[, value.var]
                           
                           out <- setNames(data.frame(rownames(map[[s]]), B),
                                           c(main.scheme, value.var))
                           
                           return (out)
                         })
      return (out)
    } else if (is.function(map[[s]])) {
      return ( map[[s]](focus.data) )
    } else {
      stop('An item in list "map" is not an acceptable data type.')
    }
  }


  
  # Allow for exception handling
  tryCatch({
    
    # Make sure 'd' is a data.frame
    if (!is.data.frame(d)) {
      stop('Input "d" is not a data.frame.')
    }
    
    # Make sure value.var, category.var in 'd'
    if (!Reduce('&', c(value.var, category.var) %in% names(d))) {
      stop('value.var and/or category.var not a column in d')
    }
    
    # 
    spatial.schemes <- names(map)

    # Make sure all 'origin' schemes correspond to columns in 'd'
    if (!Reduce('&', spatial.schemes %in% names(d))) {
      stop('not all origin schemes accounted for in d')
    }

    # Initially the input data.frame will be separated into rows
    mutable.data <- d[Reduce('|',
                             as.data.frame(!is.na(d[, which(names(d) %in% 
                               spatial.schemes)]))), ]
    if (main.scheme %in% names(mutable.data)) {
      mutable.data <- mutable.data[, -which(names(mutable.data) == main.scheme)]
    }
    
  
    # ... and rows that will be left alone
    immutable.data <- d[ Reduce('&',
                                as.data.frame(is.na(d[, which(names(d) %in% 
                                  spatial.schemes)]))), ]
    
    converted.data <- plyr::ldply(spatial.schemes, DoConversion)
  
    output <- plyr::rbind.fill(immutable.data, converted.data)

    return (output[, -which(names(output) %in% spatial.schemes)])
  },
  error <- function(e) {
    return (e)
  })
}
