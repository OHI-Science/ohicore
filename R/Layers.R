#' Layers reference class.
#' 
#' @param layers.csv path to comma-seperated value file with row of metadata per layer
#' @param layers.dir path of directory containing individual layer files
#' @return object (non-instantiated) reference class of Layers containing
#' \itemize{
#'  \item{\emph{meta} - metadata data frame of original layers.csv}
#'  \item{\emph{data} - named list of data frames, one per layer}
#'  \item{\emph{targets} - named list of character vector indicating a layer's targets, goal (status, trend) or dimension (pressures, resilience)}
#' }
#' @details To instantiate this object, \code{Layers(layers.csv, layers.dir)} is used. The \code{layers.csv} is expected to have the following columns:
#' \itemize{
#'   \item{\emph{layer} - unique identifier (no spaces or special characters)}
#'   \item{\emph{targets} - the pipe and space (' | ') delimited list of targets (goal name, 'Pressures' or 'Resilience') to feed this data layer}
#'   \item{\emph{title} - full title of the variable}
#'   \item{\emph{description} detailed description}
#'   \item{\emph{citation} - reference for documentation}
#'   \item{\emph{units} - indicating units and required column name in the layer csv file}
#'   \item{\emph{filename} - the csv data file for the layer}   
#' }
#' The layers.dir directory should contain all the csv filenames listed in the layers.csv file.
#' @export
#' @include SelectLayers.R
Layers = setRefClass(
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
        d$layer = m[['layer']]
        return(d)})
    
      .self$targets = plyr::dlply(meta, 'layer', function(m){
        return(strsplit(as.character(m[['targets']]), ' | ', fixed=T)[[1]])})
      },
    write = function(layers.csv, layers.dir){
      write.csv(.self$meta, layers.csv, row.names=F, na='')
      dir.create(layers.dir)
      for (i in 1:nrow(.self$meta)){
        lyr = .self$meta$layer[i]
        csv = .self$meta$filename[i]
        write.csv(.self$data[[lyr]], file.path(layers.dir, csv), row.names=F, na='')
      }},
    show = function(){
      print(meta[-which(names(.self$meta) == 'filename')])}
    )
)

setGeneric('SelectLayers', SelectLayers)

setMethod(
  'names','Layers', 
  function (x) {
    names(x$data)}
)

#layers.Global2012.www2013$write('~/myohi/layers.csv')