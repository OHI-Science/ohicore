#' mapvalues
#' 
#' Replace specified values with new values, in a vector or factor. This is copied from plyr. 
#' 
#' @param x  the factor or vector to modify
#' @param from  a vector of the items to replace
#' @param to  a vector of replacement values
#' @param warn_missing  print a message if any of the old values are not actually present in x
#'
#' @return Returns a vector with new values.
#'
#' @details #' If x is a factor, the matching levels of the factor will be replaced with the new values.
#' 
#' The related revalue function works only on character vectors and factors, but this function works 
#' on vectors of any type and factors.
#' @keywords ohicore
#' @examples
#' \dontrun{
#' } 
#' @export

mapvalues <- function (x, from, to, warn_missing = TRUE) 
{
  if (length(from) != length(to)) {
    stop("`from` and `to` vectors are not the same length.")
  }
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector.")
  }
  if (is.factor(x)) {
    levels(x) <- mapvalues(levels(x), from, to, warn_missing)
    return(x)
  }
  mapidx <- match(x, from)
  mapidxNA <- is.na(mapidx)
  from_found <- sort(unique(mapidx))
  if (warn_missing && length(from_found) != length(from)) {
    message("The following `from` values were not present in `x`: ", 
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
  }
  x[!mapidxNA] <- to[mapidx[!mapidxNA]]
  x
}