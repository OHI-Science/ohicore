#' United Nations Regional Groups
#'
#' United Nations geopolitical regions.
#' In some cases, when data were not available for a country, 
#' we estimated the values using georegional averages at the smallest 
#' possible spatial scale. There are three spatial scales in these data
#' (general to specific regions: r0_label, r1_label, r2_label).
#'
#' @format A data frame with 220 rows and 5 variables:
#' \itemize{
#'   \item rgn_id: numeric region id used for Ocean Health Index (1-250)
#'   \item r0_label: general regional groups (1 group: World)
#'   \item r1_label: more specific regional groups (7 groups: Africa, Americas, etc.)
#'   \item r2_label: most specific regional groups (22 groups: Caribbean, Central America, etc.)
#'   \item rgn_label: region name used for Ocean Health Index (Albania, Angola, etc.)
#' }
"georegion_labels"

#' United Nations Regional Groups (streamlined)
#'
#' United Nations geopolitical regions (version of the georegion_labels data).
#' In some cases, when data were not available for a country, 
#' we estimated the values using georegional averages at the smallest 
#' possible spatial scale. There are three spatial scales in these data
#' (general to specific regions: r0_label, r1_label, r2_label).
#'
#' @format A data frame with 220 rows and 5 variables:
#' \itemize{
#'   \item rgn_id: numeric region id used for Ocean Health Index (1-250)
#'   \item r0: general regional groups (1 group: World)
#'   \item r1: more specific regional groups (7 groups: Africa, Americas, etc.)
#'   \item r2: most specific regional groups (22 groups: Caribbean, Central America, etc.)
#' }
"georegions"

#' Territories 
#'
#' Identifies a country's territories.
#'
#' This information is used to assign data to a country's territories.
#'
#' @format A data frame with 220 rows and 5 variables:
#' \itemize{
#'   \item rgn_id: numeric region id used for Ocean Health Index (1-250)
#'   \item r0_label: sovreign country name
#'   \item r1_label: sovreign country name
#'   \item r2_label: sovreign country name
#'   \item rgn_label: region name used for Ocean Health Index (Albania, Angola, etc.)
#' }
"sovregion_labels"

#' Territories (streamlined)
#'
#' This information is used to assign data to a country's territories.
#' (This is a version of the sovregion_labels data)
#'
#'
#' @format A data frame with 220 rows and 5 variables:
#' \itemize{
#'   \item rgn_id: region id used for Ocean Health Index (1-250)
#'   \item r0: sovreign country region id
#'   \item r1: sovreign country region id
#'   \item r2: sovreign country region id
#'   \item fld_wt: identifies territory countries (0) and sovreign regions (1) 
#' }
"sovregions"

#' Region synonyms
#'
#' Synonyms for countries included in Ocean Health Index.
#' These data are used to translate region names to region ids.'
#'
#' @format A data frame with 262 rows and 6 variables:
#' \itemize{
#'   \item region_id_2012: numeric region id used for 2012 Ocean Health Index (1-187)
#'   \item rgn_id_2013: numeric region id used for Ocean Health Index after 2012 (1-255) 
#'   \item rgn_nam_2013: country names and synonyms (e.g., 'Federated State of Micronesia', 'Micronesia, FS')
#'   \item rgn_key_2013: 2 letter key for countries (e.g., US, BA)
#'   \item eez_iso3: 3 letter key for countries (e.g., USA, FSM)
#'   \item rgn_typ: identifies whether a region is an Ocean Health Index region ('ohi_region'),
#'                  'landlocked', 'disputed', or 'largescale' (includes regions: Total and World)
#' }
"rgn_synonyms"