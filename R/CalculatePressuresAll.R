#' Calculate all the pressures score for each (sub)goal.
#'
#' @param layers object \code{\link{Layers}}
#' @param conf object \code{\link{Conf}}
#' @param gamma (optional) if not specified defaults to 0.5
#' @return data.frame containing columns 'region_id' and per subgoal pressures score
#' @import dplyr
#' @import tidyr
#' @export
CalculatePressuresAll = function(layers, conf, gamma=0.5, debug=FALSE){

  ## setup initial data.frame for column binding results by region
  D = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T) %>%
    dplyr::select(region_id = id_num)
  regions = D[['region_id']]

  ## get pressures matrix, components, categories, layers
  pm = conf$pressures_matrix
  pc = conf$config$pressures_components
  pk = conf$config$pressures_categories
  p.layers = sort(names(pm)[!names(pm) %in% c('goal','component','component_name')])

  # error if layer value range is incorrect
  if (!all(subset(layers$meta, layer %in% p.layers, val_0to1, drop=T))){
    stop(sprintf('These pressures layers must range in value from 0 to 1:\n%s',
                 paste(
                   unlist(
                     layers$meta %>%
                       dplyr::filter(layer %in% p.layers & val_0to1==F) %>%
                       dplyr::select(layer)),
                   collapse = ', ')))
  }

  ## spread pressures layers data
  d.p = SelectLayersData(layers, layers=p.layers) %>%
           dplyr::select(region_id = id_num,
                  val_num,
                  layer) %>%
             tidyr::spread(layer, val_num) %>%
             dplyr::filter(region_id %in% regions)

  ## identify number of regions and pressures layers
  nr = length(regions)
  np = length(p.layers)

  ## iterate goals to calculate pressures scores by region by goal
  subgoals = subset(conf$goals, !goal %in% unique(conf$goals$parent), goal, drop=T)
  for (g in subgoals) { # g="LIV"
    if (debug) cat(sprintf('goal: %s\n', g))

    ## reset components for so when debug==TRUE and saving, is per goal
    P = w = p = alpha = beta = NA

    ## p: pressures value matrix [region_id x pressure: values]
    p = matrix(as.matrix(d.p[,-1]), nrow=nr, ncol=np,
               dimnames = list(region_id=d.p[[1]], pressure=names(d.p)[-1]))

    ## components
    p.components = pm$component[pm$goal==g]
    if (length(p.components) > 1) {
      message(sprintf('These %s components are registered in pressures_matrix.csv:\n%s.',
                      g,
                      paste(p.components, collapse=', ')))
    }

    ########################################################################################
    ## Case 1: simple single component goal has 1 row (Case 1b else statement follows) ----
    ########################################################################################

        if (length(p.components)==1){
      if (debug) cat('  no components\n')

      ## pressure weighting matrix applied to all regions [region_id x pressure: weights]
      w <- matrix(rep(unlist(pm[pm$goal==g, p.layers]), nr*np),
                  byrow=T, nrow=nr, ncol=np,
                  dimnames = list(region_id=regions, pressure=p.layers))

      ## calculate pressures per region
      P = CalculatePressuresScore(p, w, pressures_categories=pk, GAMMA=gamma)

    ######################################################
    ## Case 2: Goals that have components ----
    ######################################################

    } else {
      if (debug) cat(' ',length(p.components),'components:', paste(p.components,collapse=', '), '\n')

      ## alpha [component x pressure]: pressure rank matrix applied to all categories
      alpha <- matrix(as.matrix(pm[pm$goal==g, p.layers]),
                      nrow=length(p.components), ncol=length(p.layers),
                      dimnames = list(category=p.components, pressure=p.layers))

      ## get data layer for determining the weights by region, which could be from layers_data or layers_data_bycountry

      ## error unless g is in components list in config.R
      if (!g %in% names(pc)){
        stop(sprintf('This goal must have registered pressures_components in config.R:\n%s', g))
      }

      ## error unless layer component is identified in config.R
      if (!pc[[g]] %in% names(layers)){
        stop(sprintf('This layer identified in config.R must be registered in layers.csv:\n%s',
                     paste(pc[[g]], collapse = ', ')))
      }

      ## get the "weighting file"
      d_w = SelectLayersData(layers, layers=pc[[g]], narrow=T) %>%
        dplyr::select(region_id = id_num,
                      category,
                      value = val_num) %>%
        dplyr::mutate(category = as.character(category))

      ## warning if there are categories in the pressure matrix that are not in the weighting .csv file
      if (!all(p.components %in% unique(d_w$category))){
        message(sprintf('These %s components are not registered in weighting csv file identified in config.R:\n%s.\n(Components are identified in %s: %s)',
                        g,
                        paste(p.components[!p.components %in% d_w$category], collapse=', '),
                        pc[[g]],
                        paste(unique(d_w$category), collapse=', ')))
      }


        ## loop to calculate pressure for each component of the goal (and save as krp):

        ## join region, category, pressure to weighting matrix
        krpw = krp %>%
          dplyr::inner_join(d_w, by=c('region_id', 'category')) %>%
          dplyr::arrange(region_id, category) %>%
          dplyr::select(region_id, category, p, w=value)

        d_region_ids = D[,'region_id',drop=F]

        krpwp = d_region_ids %>%
          dplyr::left_join(krpw, by='region_id') %>%
          dplyr::group_by(region_id) %>%
          dplyr::summarize(p = sum(w*p, na.rm=TRUE)/sum(w, na.rm=TRUE))

        P = round(krpwp$p, 2)
        names(P) = krpwp$region_id
    } # end components loop

    ## bind to results
    P <- setNames(data.frame(names(P), P), c('region_id', g))
    D <- merge(D, P, all.x=T)

  } # end iterate goals # for (g in subgoals)


  ## return scores
  scores = D %>%
    tidyr::gather(goal, score, -region_id) %>%
    dplyr::mutate(dimension = "pressures") %>%
    dplyr::select(goal, dimension, region_id, score)

  return(scores)
}
