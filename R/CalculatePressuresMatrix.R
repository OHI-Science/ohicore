##' Calculate Pressures Matrix
##' 
##' The pressures matrix model function computes a pressures weighting matrix
##' based on regional attributes per category.
##' 
##' 
##' Given:
##' 
##' \itemize{
##' 
##' \item \emph{g} is the goal or subgoal (e.g., AO, CW, LIV, ECO, ...), \item
##' \emph{i} is the region (e.g., 1, 2, 3, ...), \item \emph{j} is the
##' pressures layer or stressor (e.g., \code{cc_acid}, \code{fp_art_lb}, etc.).
##' \item \emph{k} is the category (e.g., habitat, sector, product, etc.)
##' 
##' }
##' 
##' There may be a component \emph{k} for a given goal \emph{g} such that
##' \eqn{p_w(g,i,j,k)} and \eqn{w(g,i,j,k)}.
##' 
##' \deqn{p_{w}(g,i,j,k) = w(g,i,j,k) * p(i,j)}
##' 
##' In these cases where there is a component \emph{k} for goal \emph{g},
##' there's an additional aggregation or formula to calculate \eqn{w(g,i,j)}
##' based on the core rank weight \eqn{\alpha(g,j,k)} from the original
##' pressures matrix (as written in Halpern et al. (2012)) and some
##' region-specific data for each category \code{k} \eqn{\beta(i,k)}.
##' 
##' This function \code{CalculatePressuresMatrix} will aggregate a
##' category-specific weighting matrix \eqn{\alpha(g,j,k)} [category x
##' pressure] using region-specific data \eqn{\beta(g,i,k)} into a [region_id x
##' pressure] matrix \eqn{w(g,i,j)} used in \code{CalculatePressuresScore}, such
##' that:
##' 
##' \deqn{w(g,i,j) = \frac{\sum_k \alpha({g,j,k}) * \beta({g,i,k})}{\sum_k
##' \beta({g,i,k})}}
##' 
##' \enumerate{
##' 
##' \item For the CP, CS goals, the weight depends on the extent \emph{A} of
##' habitat \emph{k} in region \emph{i}:
##' 
##' \deqn{\beta(i,k) = A(i,k)}
##' 
##' \item For the HAB goal, the weight depends on the presence of habitat
##' \emph{k} (i.e., if \eqn{A(i,k)>0}) in region \emph{i}:
##' 
##' \deqn{\beta(i,k) = hasHabitat(i,k)}
##' 
##' \item For the LIV and ECO goals, the weight depends on the presence of
##' sector \emph{k} if data available for region \emph{i} and sector \emph{k}:
##' 
##' \deqn{\beta(i,k) = hasSector(i,k)}
##' 
##' \item For the NP goal, the weight depends on the peak dollar value of each
##' product \emph{k} across all years (see \eqn{w_p} from SI Equation S27) if
##' data available for region \emph{i} and product \emph{k}:
##' 
##' \deqn{\beta(i,k) = w_p(i,k)} }
##' 
##' @param alpha the weighting matrix of the form [category x pressure]. Each
##' rank weight must be an integer between 0 and 3 inclusive, or NA.
##' @param beta the aggregation matrix of the form [region_id x category] to
##' collapse across each category.
##' @param calc type of calculation, whether avg (default), mean (diff't from
##' avg?) or presence (results in 1 or 0).
##' @return Returns a weight matrix \emph{w} [region_id x pressure] suitable
##' for \code{\link{CalculatePressuresScore}}.
##' @seealso \code{\link{CalculatePressuresScore}}
##' @keywords ohi
##' @export
CalculatePressuresMatrix <- function(alpha, beta, calc='avg') {
  #' Parameters:
  #' @param alpha weighting matrix of the form [category x pressure]
     #' @param beta aggregation matrix of the form [region_id x category] to collapse across each category
     #' @param calc type of calculation, whether avg (default), mean (diff't from avg?) or presence (results in 1 or 0)
     
     w <- matrix(NA, nrow=dim(beta)[[1]], ncol=dim(alpha)[[2]], 
                 dimnames=list(region_id=dimnames(beta)[[1]], pressure=dimnames(alpha)[[2]]))
     for (i in dimnames(w)$region_id) { # i=dimnames(w)$region_id[1]
       for (j in dimnames(w)$pressure) { # j=dimnames(w)$pressure[1]
         if (calc=='avg'){
           w[i,j] <- sum(t(alpha)[j,] * beta[i,], na.rm=T) / sum(beta[i,], na.rm=T)        
         } else if (calc=='mean') {
           # eg HAB (see /var/data/ohi/model/GL-NCEAS-Pressures_Matrix/report7.R)
           # TODO: check whether bug in HAB calculation or intentional since beta [region_id x category] only ever 1 or NA
           w[i,j] <- mean(t(alpha)[j,] * beta[i,], na.rm=T)
         } else if (calc=='presence') {
           w[i,j] <- mean(t(alpha)[j,] * beta[i,], na.rm=T)
         } else {
           stop("CalculatePressuresMatrix() calc argument not one of required: 'avg','mean','presence'")
         }
       }
     }
     # convert NaN to NA (which happens when dividing by 0, ie no category in given region_id)
     w[is.nan(w)] <- NA
     w
}