#' Layers reference class.
#' 
#' @param layers.csv path to comma-seperated value file with row of metadata for each dataset used in OHI analysis.
#' @param layers.dir full path to the directory containing the layers files (csv files that correspond to each entry in layers.csv).
#' @return object (non-instantiated) reference class of Layers containing
#' \itemize{
#'  \item{\emph{meta} - metadata data frame of original layers.csv}
#'  \item{\emph{data} - named list of data frames, one per layer}
#'  \item{\emph{targets} - named list of character vector indicating a layer's targets, goal (status, trend) or dimension (pressures, resilience)}
#' }
#' @details This function creates an R object that combines into a single object all the information 
#' from the layers files and the layers.csv metadata. Individual layers can be accessed as: layer_object_name$data$layer_name
#' To create this object, \code{Conf(dir)}. The \code{dir} is expected to have the following files:
#' \itemize{
#'   \item{\emph{layer} - unique layer identifier (no spaces or special characters)}
#'   \item{\emph{targets} - a space delimited list of targets (goal code, 'Pressures', 'Resilience' or 'Regions') for which this layer is applied}
#'   \item{\emph{name} - name of the variable}
#'   \item{\emph{description} - detailed description}
#'   \item{\emph{units} - units of the value}
#'   \item{\emph{citation} - reference for documentation, typically a heading code for a supplemental document}
#'   \item{\emph{filename} - the csv data file for the layer}   
#'   \item{\emph{fld_value} - required field in the layer csv file containing the value, which is often best named as a shorthand for the units without spaces or special characters}
#' }
#' The layers.dir directory should contain all the csv filenames listed in the layers.csv file.
#' @export Layers
#' @exportClass Layers

Layers = methods::setRefClass(
  'Layers',
  fields = list(
    data = 'list',
    meta = 'data.frame',
    targets = 'list'
    ),
  methods = list(
    initialize = function (layers.csv, layers.dir) {
      
      .self$meta = read.csv(layers.csv, header = T, na='')
      row.names(.self$meta) = .self$meta$layer
      
      .self$data = plyr::dlply(meta, 'layer', function (m) {
        d = read.csv(file.path(layers.dir, m[['filename']]), header = T)
        if (nrow(d)>0){
          d$layer = m[['layer']]
        } else {
          d[1,] = rep(NA, ncol(d))
          d$layer = m[['layer']]
          message(sprintf('Layer %s has no rows of data.', m[['layer']]))
        }
        return(d)})
    
      .self$targets = plyr::dlply(meta, 'layer', function(m){
        return(strsplit(as.character(m[['targets']]), ' ', fixed=T)[[1]])})
      },
    write = function(layers.csv, layers.dir){
      write.csv(.self$meta, layers.csv, row.names=F, na='')
      if (!file.exists(layers.dir)) dir.create(layers.dir)
      for (i in 1:nrow(.self$meta)){
        lyr = .self$meta$layer[i]
        csv = .self$meta$filename[i]
        d = .self$data[[lyr]]
        d = d[,names(d) != 'layer']
        write.csv(d, file.path(layers.dir, csv), row.names=F, na='')
      }},
    show = function(){
      print(meta[-which(names(.self$meta) == 'filename')])}
    )
)

#setGeneric("SelectLayers", SelectLayers)

setMethod(
  'names','Layers', 
  function (x) {
    names(x$data)}
)

#layers.Global2012.www2013$write('~/myohi/layers.csv')