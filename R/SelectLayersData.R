#' Select Layers to Data
#' 
#' @param object instance of Layers class
#' @param targets specifies the targets of layers to be selected, defaulting to \code{c('regions')}
#' @param layers specifies the layers to be selected. If given as a named character vector, then layers get renamed
#'   with new names as values, and old names as names per
#'   {\code{\link{plyr::rename}}}
#' @param narrow narrow the resulting data frame to just the fields containing data (as described by \emph{flds} in the default wide result)
#' #@param expand.time.invariant for layers without a year column, populate the 
#'   same value throughout all years where available in other layer(s)
#' #@param cast whether to cast the resulting dataset, or leave it melted, 
#'   defaults to TRUE
#' @return data.frame with the merged data of selected layers having the following fields:
#' \itemize{
#'  \item{\emph{layer} - layer name, possibly renamed}
#'  \item{\emph{layer0} - original layer name, if fed a named character vector to layers}  
#'  \item{\emph{id_num} - numeric id}
#'  \item{\emph{id_chr} - character id}
#'  \item{\emph{id_name} - fieldname of id in original layer csv file}
#'  \item{\emph{category} - category}
#'  \item{\emph{category_name} - fieldname of character in original layer csv file}
#'  \item{\emph{year} - year}
#'  \item{\emph{val_num} - numeric value}
#'  \item{\emph{val_chr} - character value}
#'  \item{\emph{val_name} - fieldname of value in original layer csv file}
#'  \item{\emph{flds} - data fields used for the layer}
#' }
#' @details If neither targets or layers are specified then all layers are returned. If targets and layers are specified, then the union of the two sets of layers are returned, with any renamed layers renamed.
#' @export
SelectLayersData = function(object, targets=NULL, layers=NULL, cast=TRUE, narrow=FALSE, expand.time.invariant=FALSE){
  
  # determine if renaming (some) layers
  layer.newnames = NULL
  if (!is.null(names(layers))) {
    layer.newnames = layers
    layers = names(layers)
  }

  # get layers by targets
  if (!is.null(targets)) {
    layers.targets = names(which(sapply(object$targets, function(x){ any(targets %in% x) }) == T))

    # merge with other layers specified, some of which may get new names
    if (!is.null(layers)){
      layers = union(layers, layers.targets)
    } else {
      layers = layers.targets
    }
  }
  
  # all layers
  if (is.null(layers) & is.null(targets)) {
    layers = names(object)
  }
   
# # DEBUG tmp fixes
# #paste(names(d), collapse="','")
# # 'layer','id_num','id_chr','id_nam','category','category_name','year','val_num','val_chr','val_units'
# row.names(object$meta) = object$meta$layer
# object$meta[object$meta == ''] = NA
# #paste(names(object$meta), collapse="','")
  
  # merge into single data.frame
  focus.data = plyr::rbind.fill(
    plyr::llply(object$data[names(object) %in% layers], function(x) { #x = object$data[names(object) %in% layers][[1]]
      
      # get field names from layers metadata
      layer = unique(as.character(x$layer))
      flds.df = na.omit(t(object$meta[layer, c('fld_id_num','fld_id_chr','fld_category','fld_year','fld_val_num','fld_val_chr')]))
      flds = setNames(flds.df[,1], sub('fld_', '', rownames(flds.df)))
      
      # rename data.frame
      x = plyr::rename(x, setNames(names(flds), flds))
      
      # assign original field names
      x$id_name       = ifelse(length(intersect(c('id_num' ,'id_chr' ), names(x)))==1, as.character(na.omit(flds[c('id_num','id_chr')])), NA)
      x$val_name      = ifelse(length(intersect(c('val_num','val_chr'), names(x)))==1, as.character(na.omit(flds[c('val_num','val_chr')])), NA)
      x$category_name = ifelse('category' %in% names(x), as.character(flds['category']), NA)
      x$flds = paste(names(flds), collapse=' | ')
      x$layer = layer
      
      return(x)
      
      }))
  #plyr::ddply(focus.data, .(layer), function(x) unique(x$flds))
  
  # rename layers
  if (!is.null(layer.newnames)) {
    focus.data$layer0 = focus.data$layer
    focus.data$layer = plyr::revalue(focus.data$layer, layer.newnames)
  }    
  
  if (narrow) {
    flds = unique(strsplit(paste(unique(focus.data$flds), collapse=' | '), ' | ', fixed=T)[[1]])
    #browser()
    if (length(unique(focus.data$layer))==1){
      focus.data = focus.data[flds]
    } else {
      focus.data = focus.data[c(flds, 'layer')]
    }
  }
  
  return (focus.data)
  
# TODO: redo cast() and expand.time.invariant() now with id_num / id_chr, val_num / val_chr
#   if (cast) {
#     stationary.columns = which(names(focus.data) %in% c('val_num', 'layer'))
#     formula.text = paste(paste(names(focus.data)[-stationary.columns], 
#                                collapse = '+'), '~layer')
#     recasted.data = reshape2::dcast(focus.data, as.formula(formula.text),
#                                     value.var = 'value', fun.aggregate=mean)
#     
#     if (expand.time.invariant) {    
#       ti.logical = plyr::ldply(recasted.data[, layers], function(X) {
#           Reduce('|', !is.na(recasted.data$year) & !is.nan(X))
#       })
#       
#       spatial = names(recasted.data)[-which(names(recasted.data) %in% c('year', layers))]
#       time.invariants = ti.logical$.id[!ti.logical$V1] # DOH!
# 
#       base = recasted.data[!is.na(recasted.data$year), -which(names(recasted.data) %in% time.invariants)]
# 
#       for (TI in time.invariants) {
#         base = plyr::join(base, recasted.data[!is.nan(recasted.data[,TI]), c(spatial, TI)], by = spatial)
#       }
#       recasted.data = base
#     }
#     return (recasted.data)
#   } else {
#     return (focus.data)
#   }
}
setGeneric('SelectLayersData', SelectLayersData)