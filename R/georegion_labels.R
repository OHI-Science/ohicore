#' United Nations Regional Groups.
#' 
#' United Nations geopolitical regions.
#' In some cases, when data were not available for a country, 
#' we estimated the values using georegional averages at the smallest 
#' possible spatial scale. There are three spatial scales in these data
#' (general to specific regions: r0_label, r1_label, r2_label).
#' 
#' @details A data frame with 220 rows and 5 variables:
#' \enumerate{
#'    \item{rgn_id}{numeric region id used for Ocean Health Index (1-250)}
#'    \item{r0_label}{general regional groups (1 group: World)}
#'    \item{r1_label}{more specific regional groups (7 groups: Africa, Americas, etc.)}
#'    \item{r2_label}{most specific regional groups (22 groups: Caribbean, Central America, etc.)}
#'    \item{rgn_label}{region name used for Ocean Health Index (Albania, Angola, etc.)}
#' @format A data frame with 220 rows and 5 variables.
#' @source <https://unstats.un.org/unsd/methodology/m49/> ??
#' "georegion_labels"
#> [1] "georegion_labels"