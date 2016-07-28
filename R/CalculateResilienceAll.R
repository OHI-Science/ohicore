#' Calculate all the resilience score for each (sub)goal.
#'
#' @param layers object \code{\link{Layers}}
#' @param conf object \code{\link{Conf}}
#' @return data.frame containing columns 'region_id' and per subgoal resilience score
#' @import tidyr
#' @import dplyr
#' @export
CalculateResilienceAll = function(layers, conf){

  # reporting 1
  cat(sprintf('Calculating Resilience for each region...\n'))

  ## get resilience matrix, goal elements, weights, categories, layers
  r_element = conf$config$resilience_element                              # weighting data for goals with elements
  if (length(r_element) >= 1) { # only if there are any goals that have elements
    r_element = plyr::ldply(r_element)
    names(r_element) <- c('goal', 'layer')
  }

  r_gamma = conf$config$resilience_gamma                                        # gamma weighting for social vs. ecological resilience categories

  r_matrix = conf$resilience_matrix
  r_matrix = within(r_matrix, {element[is.na(element)] = ''})               # resilience matrix
  r_matrix <- tidyr::gather(r_matrix, layer, included, 3:ncol(r_matrix)) %>%    # format the resilience matrix so it is a dataframe
    dplyr::filter(!is.na(included)) %>%
    dplyr::select(goal, element, layer)

  r_categories = conf$resilience_categories                                           # resilience weights table

  r_layers = setdiff(names(conf$resilience_matrix), c('goal','element','element_name'))   # list of resilience layers from matrix

  # reporting 2
  cat(sprintf('There are %s Resilience subcategories: %s \n',
              length(unique(r_categories$subcategory)),
              paste(unique(r_categories$subcategory), collapse=', ')))

  
  ## error if the config.R weighting files are not actually included in the the data
  obs_data <- dplyr::select(SelectLayersData(layers, layers=r_element$layer), layer)
  obs_data <- unique(obs_data$layer)
  exp_data <- unique(r_element$layer)
  dif <- setdiff(exp_data, obs_data)
  if (length(dif) > 0) {
    stop(sprintf('weighting data layers identified in config.r do not exist; please update layers.csv and layers folder to include: %s',
                 paste(dif, collapse=', ')))
  }
  
  
  # error if resilience categories deviate from "ecological" and "social"
  check <- setdiff(c("ecological", "social"), unique(r_categories$category))
  if (length(check) > 0){
    stop(sprintf('In resilience_categories.csv, the "category" variable does not include %s', paste(check, collapse=', ')))
  }

  check <- setdiff(unique(r_categories$category), c("ecological", "social"))
  if (length(check) > 0){
    stop(sprintf('In resilience_categories.csv, the "category" variable includes %s', paste(check, collapse=', ')))
  }


  ## error unless layer value range is correct
  if (!all(subset(layers$meta, layer %in% r_layers, val_0to1, drop=T))){
    stop(sprintf('These resilience layers do not range in value from 0 to 1:\n%s',
                 paste(
                   unlist(
                     layers$meta %>%
                       filter(layer %in% r_layers & val_0to1==F) %>%
                       select(layer)),
                   collapse = ', ')))
  }

  ## error check: that matrix and categories table include the same resilience factors
  check <- setdiff(r_layers, r_categories$layer)
  if (length(check) >= 1) {
    message(sprintf('These resilience layers are in the resilience_matrix.csv but not in resilience_categories.csv:\n%s',
                    paste(check, collapse=', ')))
  }

  check <- setdiff(r_categories$layer, r_layers)
  if (length(check) >= 1) {
    message(sprintf('These resilience layers are in the resilience_categories.csv but not in the resilience_matrix.csv:\n%s',
                    paste(check, collapse=', ')))
  }


  ## setup initial data.frame for column binding results by region
  regions_dataframe = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T) %>%
    dplyr::select(region_id = id_num)
  regions_vector = regions_dataframe[['region_id']]

  ## create the weighting scheme
  eco_soc_weight <- data.frame(category = c("ecological", "social"),
                                  weight = c(r_gamma, 1-r_gamma))
  eco_soc_weight$category <- as.character(eco_soc_weight$category)


  ### get the regional data layer associated with each resilience data layer:
  r_rgn_layers <- SelectLayersData(layers, layers=r_layers) %>%
    dplyr::filter(id_num %in% regions_vector) %>%
    dplyr::select(region_id = id_num,
                  val_num,
                  layer) %>%
    dplyr::filter(!is.na(val_num))


  ## error check: matrix and region data layers include the same resilience factors
  check <- setdiff(r_layers, r_rgn_layers$layer)
  if (length(check) >= 1) {
    message(sprintf('These resilience layers are in the resilience_matrix.csv, but there are no associated data layers:\n%s',
                    paste(check, collapse=', ')))
  }

  check <- setdiff(r_rgn_layers$layer, r_layers)
  if (length(check) >= 1) {
    message(sprintf('These resilience layers have data layers, but are not included in the resilience_matrix.csv:\n%s',
                    paste(check, collapse=', ')))
  }



  # merge the region data layers and the resilience matrix
  rgn_matrix <- dplyr::left_join(r_matrix, r_rgn_layers, by="layer")

  # merge rgn_and_matrix data with the information in the resilience_categories.csv
  rgn_matrix_weights <- dplyr::left_join(rgn_matrix, r_categories, by="layer")

  ## average subcategories of resilience layers
  calc_resil <- rgn_matrix_weights %>%
    dplyr::group_by(goal, element, region_id, category, category_type, subcategory) %>%
    dplyr::summarize(max_subcategory = max(weight),
                     val_num = weighted.mean(val_num, weight)) %>%
    data.frame()

  ## average category types of resilience layers (weight by max weight in each subcategory)
  calc_resil <- calc_resil %>%
    dplyr::group_by(goal, element, region_id, category, category_type) %>%
    dplyr::summarize(val_num = weighted.mean(val_num, max_subcategory)) %>%
    data.frame()

  ## average ecological element (ecosystem and regulatory)
  calc_resil <- calc_resil %>%
    dplyr::group_by(goal, element, region_id, category) %>%
    dplyr::summarize(val_num = mean(val_num)) %>%
    data.frame()

  ## combine ecological and social based on resilience gamma weighting
  calc_resil <- calc_resil %>%
    dplyr::left_join(eco_soc_weight, by="category") %>%
    dplyr::group_by(goal, element, region_id) %>%
    dplyr::summarise(val_num = weighted.mean(val_num, weight)) %>%
    data.frame()

  ## For goals with elements, get the relevant data layers used for weights
  if (length(r_element) >= 1) { # only if there are any goals that have elements
    r_element_layers <- SelectLayersData(layers, layers=r_element$layer) %>%
      dplyr::filter(id_num %in% regions_vector) %>%
      dplyr::select(region_id = id_num,
                    element = category,
                    element_wt = val_num,
                    layer) %>%
      dplyr::filter(!is.na(element)) %>%
      dplyr::filter(!is.na(element_wt)) %>%
      dplyr::left_join(r_element, by="layer") %>%
      dplyr::select(region_id, goal, element, element_wt) %>%
      dplyr::mutate(element = as.character(element))

    ## data check:  Make sure elements for each goal are included in the resilience_matrix.R
    check <- setdiff(paste(r_element_layers$goal, r_element_layers$element, sep= "-"),
                     paste(r_matrix$goal[r_matrix$goal %in% r_element$goal], r_matrix$element[r_matrix$goal %in% r_element$goal], sep= "-"))
    if (length(check) >= 1) {
      message(sprintf('These goal-elements are in the weighting data layers, but not included in the resilience_matrix.csv:\n%s',
                      paste(check, collapse=', ')))
    }

    check <- setdiff(paste(r_matrix$goal[r_matrix$goal %in% r_element$goal], r_matrix$element[r_matrix$goal %in% r_element$goal], sep= "-"),
                     paste(r_element_layers$goal, r_element_layers$element, sep= "-"))
    if (length(check) >= 1) {
      message(sprintf('These goal-elements are in the resilience_matrix.csv, but not included in the weighting data layers:\n%s',
                      paste(check, collapse=', ')))
    }

    ## A weighted average of the elements:
    calc_resil <- calc_resil %>%
      dplyr::left_join(r_element_layers, by=c('region_id', 'goal', 'element')) %>%
      dplyr::filter(!(is.na(element_wt) & goal %in% r_element$goal))  %>%
      dplyr::mutate(element_wt = ifelse(is.na(element_wt), 1, element_wt)) %>%
      dplyr::group_by(goal, region_id) %>%
      dplyr::summarize(val_num = weighted.mean(val_num, element_wt))

  } # end if(length(r_element) >= 1) for goals with elements

  # return scores
  scores <- regions_dataframe %>%
    dplyr::left_join(calc_resil, by="region_id") %>%
    dplyr::mutate(dimension="resilience") %>%
    dplyr::select(goal, dimension, region_id, score=val_num) %>%
    dplyr::mutate(score = round(score*100, 2))
  return(scores)

}
