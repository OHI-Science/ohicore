#' Layers accompanying Nature 2012 publication on the FTP site for Global 2012 analysis.
#'
#' These layers get used to calculate the Ocean Health Index.
#'
#' @docType data
#' @name layers.Global2012.Nature2012ftp
#' @usage layers.Global2012.Nature2012ftp
#' @format a \code{\link{Layers}} object
#' @references \url{http://ohi-science.org}
#' @keywords datasets
NULL

#' Layers used for the 2013 web launch applied to Global 2012 analysis.
#'
#' These layers get used to calculate the Ocean Health Index.
#'
#' @docType data
#' @name layers.Global2012.www2013
#' @usage layers.Global2012.www2013
#' @format a \code{\link{Layers}} object
#' @references \url{http://ohi-science.org}
#' @keywords datasets
NULL

#' Layers used for the 2013 web launch applied to Global 2013 analysis.
#'
#' These layers get used to calculate the Ocean Health Index.
#'
#' @docType data
#' @name layers.Global2013.www2013
#' @usage layers.Global2013.www2013
#' @format a \code{\link{Layers}} object
#' @references \url{http://ohi-science.org}
#' @keywords datasets
NULL

#' Scores resulting from the 2013 web launch applied to Global 2012 analysis.
#'
#' These scores are the results of the Ocean Health Index.
#'
#' @docType data
#' @name scores.Global2012.www2013
#' @usage scores.Global2012.www2013
#' @format a \code{\link{Scores}} object
#' @references \url{http://ohi-science.org}
#' @keywords datasets
NULL

#' Scores resulting from the 2013 web launch applied to Global 2013 analysis.
#'
#' These scores are the results of the Ocean Health Index.
#'
#' @docType data
#' @name scores.Global2013.www2013
#' @usage scores.Global2013.www2013
#' @format a \code{\link{Scores}} object
#' @references \url{http://ohi-science.org}
#' @keywords datasets
NULL

#' scores data.frame format
#' 
#' Expected data frame format for scores. 
#' 
#' @name scores
#' @details The expected scores format is the following columns:
#' \itemize{
#'   \item{\emph{region_id} - unique numeric region identifier, reserving 0 as the region_id for the area-weighted average of the entire study area}
#'   \item{\emph{goal} - the goal code or Index}
#'   \item{\emph{dimension} - the dimension code, one of: status, trend, pressures, resilience, future, score}
#'   \item{\emph{score} - the numeric score: 0-100 for all dimensions, except trend (-1 to 1)}
#' }
#' To get the wide view (many columns, with one row per region and columns having combination of goal and dimension), 
#' use something like: \code{reshape2::dcast(.self$long, region_id ~ goal + dimension, value.var='score')}.
#' @keywords data
#' @export scores
NULL