#' Calculate Pressures Score
#' 
#' The pressures score is calculated for each region
#' given a weighting matrix for a goal and the individual pressures values.
#' 
#' Each pressure layer \eqn{p(i,j)} is either environmental or social, belongs
#' to a pressures category \eqn{K \in \{cc,fp,hd,po,sp,ss\}}, and has a value
#' \eqn{(0..1)} for each region \emph{i} and pressures layer \emph{j}. Each
#' goal has a weight matrix \eqn{w} that has a rank weight between 0 and 3
#' inclusive, or NoData, for each region \emph{i} and each pressure layer
#' \emph{j} on a per goal \emph{g} basis.
#' 
#' The pressures scores calculations go through 5 steps, using a complex
#' weighting scheme that varies across goals, subgoals, pressures categories,
#' and regions:
#' 
#' \itemize{
#' 
#' \item \emph{g} is the goal or subgoal (e.g., AO, CW, LIV, ECO, ...), \item
#' \emph{i} is the region (e.g., 1, 2, 3, ...), \item \emph{j} is the
#' pressures layer or stressor (e.g., \code{cc_acid}, \code{fp_art_lb}, etc.).
#' 
#' }
#' 
#' Calculations
#' 
#' \enumerate{
#' 
#' \item Apply weights for each goal \emph{g}, region \emph{i}, and pressure
#' layer \emph{j}: Each weighted pressure \eqn{p_w(g,i,j)} is the pressure
#' layer value \eqn{p(i,j)} per region \emph{i} and pressure layer \emph{j}
#' multiplied by the rank weight \eqn{w(g,i,j)} for that goal \emph{g}, region
#' \emph{i}, and pressure layer \emph{j}. If the \eqn{w(g,i,j)} is NoData or
#' 0, the weighted pressure \eqn{p_w(g,i,j)} is NoData.
#' 
#' \deqn{p_{w}(g,i,j) = w(g,i,j) * p(i,j)}
#' 
#' \item Category-level aggregation: The pressures category score \eqn{p_K} is
#' the sum of all \eqn{p_w} within each category, then rescaled to 0..1 using
#' a linear scale range transformation (from 0..3 to 0..1). Any score
#' \eqn{p_K} greater than 1 is capped to 1:
#' 
#' \deqn{p_K(g,i) = \frac{\min(\sum_{j \in K} p_w(g,i,j), 3)}{3}}
#' 
#' \item Environmental aggregation: The environmental pressures score
#' \eqn{p_E(g,i)} is the weighted sum of \eqn{p_K}(g,i), where each weight is
#' the maximum weight in the pressure category \eqn{K}, and then divided by
#' the sum of the maximum weights:
#' 
#' \deqn{w_{K,max}(g,i) = max(\{\forall_j \in K | w(g,i,j)\})}
#' 
#' \deqn{p_E(g,i) = \frac{\sum_K w_{K,max}(g,i) p_K(g,i)}{\sum_K
#' w_{K,max}(g,i)}}
#' 
#' \item Social aggregation: The social pressures score \eqn{p_S(g,i)} is the
#' mean of the \emph{unweighted} social pressure scores \eqn{p(i,j)}:
#' 
#' \deqn{p_S(g,i) = \frac{\sum_{j \in S}^{} p(i,j)}{N}}
#' 
#' \item Gamma combination: The pressures score \eqn{p_X(g,i)}:
#' 
#' \deqn{p_X(g,i) = \gamma p_E(g,i) + (1-\gamma)p_S(g,i)} }
#' 
#' @param p the pressures value matrix [region_id x pressure]. Each score must
#' be a real number between 0 and 1 inclusive, or NA. The pressure names must
#' be of the form \emph{category}\emph{_pressure} where \emph{category} is one
#' of the categories listed in \code{ohi.pressure.category}. Use \code{ss} to
#' denote the social category.
#' 
#' \preformatted{ pressure region_id cc_acid cc_sst cc_uv fp_art_hb 1 0.879
#' 0.360 0.764 NA 2 0.579 0.396 0.531 NA 3 0.926 0.235 0.769 NA 4 0.914 0.554
#' 0.795 NA 5 0.860 0.609 0.802 0.001 6 0.871 0.325 0.788 0.001 7 0.846 0.410
#' 0.677 0.000 8 0.806 0.671 0.752 NA 9 0.844 0.595 0.678 NA 10 0.860 0.575
#' 0.781 0.109 }
#' @param w the weighting matrix of the form [region_id x pressure]. Each rank
#' weight must be a real number between 0 and 3 inclusive, or NA.
#' \preformatted{ pressure region_id cc_acid cc_sst cc_uv fp_art_hb 1 2 1 0.6
#' NA 2 2 1 0.5 NA 3 2 1 2.1 NA 4 2 1 3.0 NA 5 2 1 2.8 1 6 2 1 2.2 1 7 2 1 1.3
#' 1 8 2 1 1.7 NA 9 2 1 3.0 NA 10 2 1 1.2 1 }
#' @param GAMMA Multiplier used to combine environmental and social pressures.
#' @return Returns a named vector with the pressures score for each named
#' region.
#' @seealso \code{\link{CalculatePressuresMatrix}}
#' @keywords ohi
#' @examples
#' \dontrun{
#'   > conf$config$pressures_categories
#' 	$environmental
#' 	[1] "po" "hd" "fp" "sp" "cc"
#' 
#' 	$social
#' 	[1] "ss"
#' 	> p
#' 	         pressure
#' 	region_id fp_art_hb fp_art_lb fp_com_hb fp_com_lb hd_intertidal
#' 	       1      0.122      0.25      0.35     0.395         0.954
#' 	       2      0.096      0.94      0.85     0.252         0.649
#' 	       3      0.858      0.46      0.84     0.097         0.425
#' 	       4      0.814      0.63      0.60     0.672         0.659
#' 	       5      0.247      0.51      0.58     0.941         0.046
#' 	       6      0.853      0.34      0.15     0.370         0.385
#' 	       7      0.601      0.31      0.39     0.873         0.064
#' 	       8      0.355      0.89      0.74     0.159         0.273
#' 	       9      0.289      0.94      0.52     0.743         0.094
#' 	       10     0.887      0.89      0.87     0.660         0.746
#' 	         pressure
#' 	region_id hd_subtidal_hb hd_subtidal_sb po_chemicals po_nutrients
#' 	       1           0.535          0.651        0.042        0.931
#' 	       2           0.454          0.069        0.234        0.025
#' 	       3           0.297          0.428        0.970        0.679
#' 	       4           0.953          0.485        0.063        0.565
#' 	       5           0.963          0.045        0.552        0.828
#' 	       6           0.598          0.213        0.907        0.220
#' 	       7           0.476          0.641        0.980        0.214
#' 	       8           0.285          0.858        0.447        0.793
#' 	       9           0.591          0.702        0.719        0.472
#' 	       10          0.072          0.431        0.685        0.102
#' 	         pressure
#' 	region_id sp_alien sp_genetic ss_wgi
#' 	       1     0.979      0.761  0.181
#' 	       2     0.345      0.091  0.631
#' 	       3     0.223      0.986  0.646
#' 	       4     0.035      0.078  0.559
#' 	       5     0.992      0.643  0.432
#' 	       6     0.963      0.416  0.221
#' 	       7     0.752      0.627  0.257
#' 	       8     0.100      0.245  0.333
#' 	       9     0.316      0.373  0.347
#' 	       10    0.283      0.224  0.031
#' 	> w
#' 	         pressure
#' 	region_id fp_art_hb fp_art_lb fp_com_hb fp_com_lb hd_intertidal
#' 	       1          2         1      0.92         1             1
#' 	       2          2         1      0.48         1             1
#' 	       3          2         1      2.81         1             1
#' 	       4          2         1      1.19         1             1
#' 	       5          2         1      2.82         1             1
#' 	       6          2         1      1.07         1             1
#' 	       7          2         1      1.48         1             1
#' 	       8          2         1      0.46         1             1
#' 	       9          2         1      0.56         1             1
#' 	       10         2         1      0.90         1             1
#' 	         pressure
#' 	region_id hd_subtidal_hb hd_subtidal_sb po_chemicals po_nutrients
#' 	       1               2              2         1.00            1
#' 	       2               2              2         0.79            1
#' 	       3               2              2         0.37            1
#' 	       4               2              2         0.91            1
#' 	       5               2              2         1.06            1
#' 	       6               2              2         0.72            1
#' 	       7               2              2         0.49            1
#' 	       8               2              2         1.18            1
#' 	       9               2              2         0.18            1
#' 	       10              2              2         0.28            1
#' 	         pressure
#' 	region_id sp_alien sp_genetic ss_wgi
#' 	       1         1          1      1
#' 	       2         1          1      1
#' 	       3         1          1      1
#' 	       4         1          1      1
#' 	       5         1          1      1
#' 	       6         1          1      1
#' 	       7         1          1      1
#' 	       8         1          1      1
#' 	       9         1          1      1
#' 	       10        1          1      1
#' 	> p_x <- CalculatePressuresScore(p, w)
#' 	> p_x
#' 	   1    2    3    4    5    6    7    8    9   10 
#' 	0.40 0.53 0.68 0.63 0.60 0.43 0.48 0.47 0.50 0.30 
#' 	> data.frame(region_id=names(p_x), pressure=p_x)
#' 	   region_id pressure
#' 	1          1     0.40
#' 	2          2     0.53
#' 	3          3     0.68
#' 	4          4     0.63
#' 	5          5     0.60
#' 	6          6     0.43
#' 	7          7     0.48
#' 	8          8     0.47
#' 	9          9     0.50
#' 	10        10     0.30
#' 	> 
#' 	> 
#' 
#' }
#' 
#' @export 
CalculatePressuresScore <- function(p, w, GAMMA=0.5, browse=F, pressures_categories=list(environmental=c('po','hd','fp','sp','cc'), social='ss')) {
  # Computation of pressure
  #
  # The weighting matrix and the pressure scores matrix are of the form
  # [region_id] x [pressure]
  #
  # The pressure names must be of the form "category"_"pressure". 
  # Use "ss" to denote the social category.
  #    
  # Parameters:
  #' @param p is the pressures value matrix [region_id x pressure]
     #' @param w is the weighting matrix of the form [region_id x pressure]
     #'
     #' @return pressures scores as a named vector.
     # verify parameters
     if (getOption('debug', FALSE)) {
       stopifnot(is.array(w) && is.array(p))
       stopifnot(min(p, na.rm=T) >= 0  && max(p, na.rm=T) <= 1)   #  [0, 1]
       stopifnot(min(w, na.rm=T) >= 0  && max(w, na.rm=T) <= 3)   #  [0, 3]
     }
     # normalize dimension handles
     stopifnot(all(names(dimnames(p)) == c('region_id', 'pressure')))
     stopifnot(all(names(dimnames(w)) == c('region_id', 'pressure')))
     stopifnot(all(dimnames(w)$region_id %in% dimnames(p)$region_id))
     stopifnot(all(dimnames(w)$pressure %in% dimnames(p)$pressure))
     # align
     #browser()
     w <- with(dimnames(p), w[region_id,pressure,drop=F])
     # create tree for hierarchy of pressures categories
     stopifnot(all(grepl('_', dimnames(p)$pressure)))
     pcat <- data.frame(pressure=unique(dimnames(p)$pressure))
     pcat <- within(pcat, {
       category <- gsub('^([a-z]+)_.*$', '\\1', tolower(pressure))
     })
     # all.cats <- unlist(pressures_categories, use.names=F)
     # stopifnot(all(pcat$category %in% all.cats))
     # Step 1: apply rank weights
     w <- ifelse(w == 0, NA, w) # exclude any 0 or NoData weights
     p_w <- p * w
     p_w <- merge(melt(p_w), pcat, all.x=T, by='pressure')
     p_w <- acast(p_w, region_id ~ category ~ pressure)
     # Step 2: rescale and save the max rank weight per category for later
     p_w_max <- array(NA, 
                      dim=c(dim(p_w)[[1]], length(pressures_categories$environmental)), 
                      dimnames=list(region_id=dimnames(p)[[1]], 
                                    category=pressures_categories$environmental))
     p_k <- p_w_max
     for (k in dimnames(p_k)$category) { # k='po'
       j <- grep(paste('^', k, '_', sep=''), dimnames(w)[[2]], value=T)
       wj <- w[,j, drop=F]
       pj <- p[,j, drop=F]
       p_w_max[,k] <- apply(wj, 1, function(x) { 
         if (all(is.na(x))) {
           NA
         } else {
           max(x, na.rm=T)
         }
       })
       # Eq (8) from Nature 2012
       p_k[,k] <- apply(pj * wj, 1, function(x) { # Refs #26 - fix problem when all stressors within a category are NA
         if (all(is.na(x))) {
           NA
         } else {
           sum(x, na.rm=T)
         }
       })  # sum over all pressures
       # DEBUG
       if (browse & k=='po') browser(); range(p_k[,k])      
       p_k[,k] <- score.rescale(p_k[,k], xlim=c(0,3)) # rescale from rank weight max
       p_k[,k] <- score.clamp(p_k[,k]) # clamp to [0,1]
       # BB quick fix, since example of Ascension 2012a (id 85) was getting a po of 1. NOPE: getting value > 1.
       #p_k[,k] <- score.rescale(p_k[,k], xlim=c(0,1)) # rescale from rank weight max      
     }
     # Step 3: compute environmental pressures score using weights from max ranks
     k <- pressures_categories$environmental
     p_e <- rep(NA, nrow(p_k))
     for (i in 1:nrow(p_k)) {
       # Eq (9) from Nature 2012
       p_e[i] <- sum(p_k[i,k] * p_w_max[i,k], na.rm=T) / sum(ifelse(is.na(p_k[i,k]),NA,1) * p_w_max[i,k], na.rm=T)
     }
     names(p_e) <- dimnames(p_k)$region_id
     # Step 4: compute social pressures score using unweighted pressures
     # Eq (10) from Nature 2012
     stopifnot(length(pressures_categories$social) == 1) # supports only a single social category
     k <- pressures_categories$social[[1]]
     j <- grep(paste('^', k, '_', sep=''), dimnames(p)[[2]], value=T)
     p_s <- score.clamp(apply(p[,j, drop=F], 1, mean, na.rm=T))
     names(p_s) <- rownames(p)
     # Step 5: apply gamma to environmental and social to yield score per region     
     if (all(is.nan(p_s))){
       #warning('Skipping social pressures since none found in pressures matrix.')
       p_x <- p_e
     } else {
       p_x <- (GAMMA * p_e) + ((1-GAMMA) * p_s)
     }
     round(p_x * 100, 2)
}
