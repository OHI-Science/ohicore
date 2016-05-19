#' Calculate all the pressures score for each (sub)goal.
#' 
#' @param layers object \code{\link{Layers}}
#' @param conf object \code{\link{Conf}}
#' @param gamma (optional) if not specified defaults to 0.5
#' @return data.frame containing columns 'region_id' and per subgoal pressures score 
#' @import dplyr
#' @import tidyr
#' @export
CalculatePressuresAll = function(layers, conf, gamma=0.5, debug=F){
  
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
                       filter(layer %in% p.layers & val_0to1==F) %>%
                       select(layer)), 
                   collapse = ', ')))
  }
  
  ## spread pressures layers data
  d.p = SelectLayersData(layers, layers=p.layers) %>% 
           select(region_id = id_num,
                  val_num,
                  layer) %>%
             dplyr::spread(layer, val_num) %>%
    filter(region_id %in% regions)

  ## identify number of regions and pressures layers
  nr = length(regions)  
  np = length(p.layers) 
  
  ## iterate goals to calculate pressures scores by region by goal. Has Case 1a and Case 1b
  subgoals = subset(conf$goals, !goal %in% unique(conf$goals$parent), goal, drop=T)
  for (g in subgoals) { # g=subgoals[1]  
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
    
    ## Case 1a: simple single component goal has 1 row (Case 1b else statement follows) ----
    if (length(p.components)==1){
      if (debug) cat('  no components\n')
      
      ## pressure weighting matrix applied to all regions [region_id x pressure: weights]
      w <- matrix(rep(unlist(pm[pm$goal==g, p.layers]), nr*np), 
                  byrow=T, nrow=nr, ncol=np, 
                  dimnames = list(region_id=regions, pressure=p.layers))        
      
      ## calculate pressures per region
      P = CalculatePressuresScore(p, w, pressures_categories=pk, GAMMA=gamma)
      
    ## Case 1b: Goals that have components ----
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
      if (!pc[[g]][['layer']] %in% names(layers)){
        stop(sprintf('This layer identified in config.R must be registered in layers.csv:\n%s', 
                     paste(pc[[g]][['layer']], collapse = ', ')))
      }
      
      ## setup 
      d_w = SelectLayersData(layers, layers=pc[[g]][['layer']], narrow=T) %>%
        dplyr::select(region_id = id_num,
                      category,
                      value = val_num)

      ## error unless all components are in the aggregation layer category
      if (!all(p.components %in% unique(d_w$category))){
        message(sprintf('These %s components are not registered in config.r:\n%s.\n(Components are identified in %s: %s)', 
                        g, 
                        paste(p.components[!p.components %in% d_w$category], collapse=', '),
                        pc[[g]][['layer']], 
                        paste(unique(d_w$category), collapse=', ')))
      }
      
      ## if config.r level == region_id-category, eg NP: aggregate using pressures_component_aggregation:layer_id.
      if (pc[[g]][['level']]=='region_id-category'){
        if (debug) cat(sprintf("  scoring pressures seperately by region and category, like a subgoal (pressures_calc_level=='region_id-category')\n"))
        
        ## get pressure per component
        if (exists('krp')) rm(krp)
        for (k in p.components){ # k = p.components[1]
          
          ## w specific to component, pressure weighting matrix applied to all regions [region_id x pressure: weights]
          w <- matrix(rep(unlist(pm[pm$goal==g & pm$component==k, p.layers]), nr*np), 
                      byrow=T, nrow=nr, ncol=np,
                      dimnames = list(region_id=regions, pressure=p.layers))
          
          ## calculate pressures per region, component
          rp.k = data.frame(category=k, region_id=as.integer(dimnames(p)$region_id), 
                            p = CalculatePressuresScore(p, w, pressures_categories=pk, GAMMA=gamma))
          if (exists('krp')){
            krp = rbind(krp, rp.k)
          } else {
            krp = rp.k
          }
        }
        
        ## join region, category, pressure to weighting matrix
        krpw = krp %>%
          inner_join(d_w, by=c('region_id', 'category')) %>%
          arrange(region_id, category) %>%
          select(region_id, category, p, w=value)  
        
        d_region_ids = D[,'region_id',drop=F]
        
        krpwp = d_region_ids %>%
          left_join(krpw, by='region_id') %>%
          group_by(region_id) %>%
          summarize(p = sum(w*p)/sum(w)) 
        
        P = round(krpwp$p, 2)
        names(P) = krpwp$region_id      
        
      ## if config.r level == region_id. Most goals like this: collapse weights across categories first, then calculate pressures per region
      } else if (pc[[g]][['level']]=='region_id'){
        if (debug) cat(sprintf("  aggregating across categories to region (pressures_calc_level=='region_id')\n"))
        
        ## cast and get sum of categories per region
        if (!is.na(subset(layers$meta, layer==pc[[g]][['layer']], fld_id_chr, drop=T))){
          # this condition seems to no onger apply, since all but NP (handled above if level is 'region_id-category')
          stop('surprise, layers_data_bycountry used')
          if (debug) cat(sprintf("  using layers_data='layers_data_bycountry'\n"))
          d_w_r = d_w %>%
            inner_join(regions_countries_areas, by='country_id') %>%
            filter(region_id %in% regions) %>%
            select(region_id, category, country_id, country_area_km2)
          
          ## error if category columns have NAs. To prevent 'Error: All columns must be named' in m_w below
          if (sum(is.na(d_w$category)) > 0){
            stop(sprintf('NAs detected in %s; please correct this before proceeding', pc[[g]][['layer']]))
          }
          
          m_w = subset(d_w, region_id %in% regions) %>%
            dplyr::spread(category, value) %>% 
            mutate(sum = rowSums(.[,-1], na.rm = TRUE))
          
        } else { # presume layers_data == 'layers_data'    
          if (debug) cat(sprintf("  using layers_data='layers_data'\n"))
          ## for CS: matrix of weights by category based on proportion of regional total for all categories
          
           ## error if category columns have NAs. To prevent 'Error: All columns must be named' in m_w below
          if (sum(is.na(d_w$category)) > 0){
            stop(sprintf('NAs detected in %s; please correct this before proceeding', pc[[g]][['layer']]))
          }
          
          m_w = subset(d_w, region_id %in% regions) %>%
            dplyr::spread(category, value) %>% 
            mutate(sum = rowSums(.[,-1], na.rm = TRUE))
            
          m_w = cbind(m_w[,'region_id',drop=F], m_w[,2:(ncol(m_w)-1)] / m_w[,'sum']) 
        }      
        
        ## beta [region_id x category]: aggregation matrix 
        beta = matrix(as.matrix(m_w[,-1]), 
                      nrow=nrow(m_w), ncol=ncol(m_w)-1, 
                      dimnames = list(region_id=m_w$region_id, category=names(m_w)[-1]))
        
        ## for LIV/ECO, limit beta columns to alpha rows
        beta = beta[, intersect(rownames(alpha), colnames(beta)), drop=F]
        
        ## calculate weighting matrix
        if (debug) cat(sprintf("  CalculatePressuresMatrix(alpha, beta, calc='avg')\n"))
        w = CalculatePressuresMatrix(alpha, beta, calc='avg')
        # TODO: test calc type of calculation, whether avg (default), mean (diff't from avg?) or presence (results in 1 or 0)
        
        ## append missing regions with NA
        region_ids.missing = setdiff(regions, dimnames(w)$region_id)
        pressures.missing = setdiff(p.layers, dimnames(w)$pressure)
        w = matrix(rbind(cbind(w, 
                               matrix(0, nrow=nrow(w), ncol=length(pressures.missing))), 
                         matrix(0, nrow=length(region_ids.missing), ncol=ncol(w)+length(pressures.missing))),
                   nrow=nrow(w)+length(region_ids.missing), ncol=ncol(w)+length(pressures.missing),
                   dimnames = list('region_id'=c(dimnames(w)$region_id, region_ids.missing), 
                                   'pressure'=c(dimnames(w)$pressure, pressures.missing)))[as.character(regions), p.layers, drop=F]
        w = w[dimnames(p)$region_id,,drop=F] # align w with p
        
        ## check matrices
        stopifnot(all(dimnames(w)$pressure == dimnames(w)$pressure))
        stopifnot(!is.null(dimnames(w)$region_id))
        stopifnot(all(dimnames(p)$region_id == dimnames(w)$region_id))
        
        ## calculate pressures per region
        P = CalculatePressuresScore(p, w, pressures_categories=pk, GAMMA=gamma)
        
      } else {
        stop(sprintf("pressures_component_aggregation.csv : pressures_calc_level of '%s' not handled. Must be either 'region_id' or 'region_id-category'.", 
                     agg$aggregation_sequence))
      }    
      
    } # end if (length(p.components)==1)
    
    ## bind to results
    D = merge(D, setNames(data.frame(names(P), P), c('region_id', g)), all.x=T)
    
  } # end iterate goals # for (g in subgoals)
  
  
  ## return scores
  scores = D %>%
    gather(goal, score, -region_id) %>%
    mutate(dimension = "pressures") %>%
    select(goal, dimension, region_id, score)
    
  return(scores)
}
