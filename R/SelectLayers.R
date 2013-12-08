#' Select a set of layers.
#' 
#' @param object instance of Layers class
#' @param target specifies the target of layers to be selected
#' @param layers specifies the layers to be selected. If given as a named character vector, then layers get renamed
#'   with new names as values, and old names as names per
#'   {\code{\link{plyr::rename}}}
#' #@param expand.time.invariant for layers without a year column, populate the 
#'   same value throughout all years where available in other layer(s)
#' #@param cast {T|F} whether to cast the resulting dataset, or leave it melted, 
#'   defaults to TRUE
#' @return data.frame with data of selected layers with the following fields:
#' \itemize{
#'  \item{\emph{layer} - layer name, possibly renamed}
#'  \item{\emph{layer0} - original layer name, if fed a named character vector to layers}  
#'  \item{\emph{id_num} - numeric id}
#'  \item{\emph{id_chr} - character id}
#'  \item{\emph{id_name} - fieldname of id}
#'  \item{\emph{category} - category}
#'  \item{\emph{category_name} - fieldname of character}
#'  \item{\emph{year} - year}
#'  \item{\emph{val_num} - numeric value}
#'  \item{\emph{val_chr} - character value}
#'  \item{\emph{val_name} - fieldname of value, usually in units as specified in Layers}
#' }
#' @details If neither target or layers are specified then all layers are returned. If target and layers are specified, then the union of the two sets of layers are returned, with any renamed layers renamed.
#' @export
SelectLayers = function(object, target=NULL, layers=NULL, cast=T, expand.time.invariant=F){
  
  # determine if renaming (some) layers
  layer.newnames = NULL
  if (!is.null(names(layers))) {
    layer.newnames = layers
    layers = names(layers)
  }

  # get layers by target
  if (!is.null(target)) {
    layers.target = names(which(sapply(object$targets, function(x){ target %in% x }) == T))

    # merge with other layers specified, some of which may get new names
    if (!is.null(layers)){
      layers = union(layers, layers.target)
    } else {
      layers = layers.target
    }
  }
  
  # all layers
  if (is.null(layers) & is.null(target)) {
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
      flds = na.omit(unlist(object$meta[layer, c('fld_id_num','fld_id_chr','fld_category','fld_year','fld_val_num','fld_val_chr')]))
      names(flds) = sub('fld_', '', names(flds))
      
      # rename data.frame
      x = plyr::rename(x, setNames(names(flds), flds))
      
      # assign original field names
      x$id_name       = ifelse(length(intersect(c('id_num' ,'id_chr' ), names(x)))==1, as.character(na.omit(flds[c('id_num','id_chr')])), NA)
      x$val_name      = ifelse(length(intersect(c('val_num','val_chr'), names(x)))==1, as.character(na.omit(flds[c('val_num','val_chr')])), NA)
      x$category_name = ifelse('category' %in% names(x), flds['category'], NA)
      x$flds = paste(names(flds), collapse=' | ')
      
      return(x)
      }))
  #plyr::ddply(focus.data, .(layer), function(x) unique(x$flds))
  
  # rename layers
  if (!is.null(layers_rename)) {
    focus.data$layer0 = focus.data$layer
    focus.data = plyr::revalue(focus.data, layer.newnames)
  }    

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