#' Calculate all the resilience score for each (sub)goal.
#'
#' @param layers object \code{\link{Layers}}
#' @param conf object \code{\link{Conf}}
#' @return data.frame containing columns 'region_id' and per subgoal resilience score
#' @import tidyr
#' @import dplyr
#' @export
CalculateResilienceAll = function(layers, conf, debug=FALSE){
  
  
  ## get resilience matrix, components, weights, categories, layers
  rm = conf$resilience_matrix
  rm = within(rm, {component[is.na(component)] = ''})
  rw = conf$resilience_weights
  rc = conf$config$resilience_components
  rk = conf$config$resilience_categories
  r.layers = setdiff(names(rm), c('goal','component','component_name'))
  
  ## error unless layer value range is correct
  if (!all(subset(layers$meta, layer %in% r.layers, val_0to1, drop=T))){
    stop(sprintf('These resilience layers do not range in value from 0 to 1:\n%s',
                 paste(
                   unlist(
                     layers$meta %>%
                       filter(layer %in% r.layers & val_0to1==F) %>%
                       select(layer)),
                   collapse = ', ')))
  }
  
  ## error unless all layers identified in resilience_matrix.csv are entered in resilience_weights.csv
  if (!all(r.layers %in% rw$layer)){
    stop(sprintf('These resilience layers must have weights in resilience_weights.csv:\n%s', 
                 paste(setdiff(r.layers,rw$layer), collapse = ', ')))
  }
  
  ## setup initial data.frame for column binding results by region
  D = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T) %>%
    dplyr::select(region_id = id_num)
  regions = D[['region_id']]
  
  ## w.layers: weighting vector [layer]
  weights = setNames(rw$weight, rw$layer)
  
  ## t: typing vector [layer]
  types = setNames(rw$type, rw$layer)
  
  ## iterate goals to calculate resilience scores by region by goal. Will encounter 3 Cases. 
  subgoals = subset(conf$goals, !goal %in% unique(conf$goals$parent), goal, drop=T)
  
  for (g in subgoals){ # g="NP"
    if (debug) cat(sprintf('goal: %s\n', g))
    
    ## setup: subset g row from resilience_matrix
    r.g = subset(rm, goal==g)
    
    ####################################################################################
    ## Case 1: simple single component goal has 1 row (see Case 2: when a goal has components)
    ####################################################################################
    if (nrow(r.g)==1){
      
      ## extract relavant resilience layers to goal
      lyrs = na.omit(as.character(r.g[!names(r.g) %in% c('goal','component','component_name')]))
      
      ## r: resilience value matrix [region_id x layer: value]
      r = tidyr::spread(SelectLayersData(layers, layers=lyrs) %>%
                          dplyr::filter(id_num %in% regions) %>%
                          dplyr::select(region_id = id_num, 
                                        val_num, 
                                        layer),
                        layer, val_num) 
      
      row.names(r)  = r$region_id 
      
      r = r %>%
        dplyr::select(-region_id) %>%
        as.matrix()
      names(dimnames(r)) <- c('region_id', 'layer')
      
      
      ## b: boolean value matrix [region_id x layer]
      b <- ifelse(!is.na(r), TRUE, FALSE); head(b)
      
      ## w: weighting matrix [region_id x layer]
      w <- CalculateResilienceMatrix(b, w.layers=weights[dimnames(b)[[2]]]); head(w)
      
      ## R: resilience score [region_id]
      R = CalculateResilienceScore(r, t=types[dimnames(b)[[2]]], w, resilience_categories=conf$config$resilience_categories)
      
      # assign R score for all regions to resilience data frame
      D = merge(D, setNames(data.frame(as.integer(names(R)), R), c('region_id', g)))
      
    } else {

      ####################################################################################
      ## Case 2: Goal has components (e.g., coral, seagrass, etc)
      ####################################################################################
      
            
      ## error unless g is in components list in config.R
      if (!g %in% names(rc)){
        stop(sprintf('This goal must have registered resilience_components in config.R:\n%s', g))
      }
      
      ## error unless layer component is identified in config.R
      if (!rc[[g]][['layer']] %in% layers$meta$layer){
        stop(sprintf('This layer identified in config.R must be registered in layers.csv:\n%s', 
                     paste(rc[[g]][['layer']], collapse = ', ')))
      }
      
      # aggregate all categories and values from layers for all regions
      lyr_agg = SelectLayersData(layers, layers=rc[[g]][['layer']], narrow=T) %>%  
        filter(id_num %in% regions) %>%
        dplyr::select(region_id = id_num,
                      category,
                      value = val_num)
      
      # check that all components are in lyr_agg
      cond.1 = sub('(.*)( only|, with |, without )(.*)', '\\1', r.g$component)
      cond.2 = sub('(.*)( only|, with |, without )(.*)', '\\3', r.g$component)
      component_categories = unique(na.omit(c(ifelse(nchar(cond.1)>0, cond.1, NA),
                                              ifelse(nchar(cond.2)>0, cond.2, NA))))
      
      # message regarding g's components
      if (!all(component_categories %in% unique(lyr_agg$category))){
        cat(sprintf('...based on the following components from resilience_matrix.csv for %s:\n %s', g, paste(r.g$component, collapse=',  ')))
      }
      
         ## iterate regions; not all regions have all categories within the components (ie products within product groups)
      for (id in D$region_id){ # id=11
        
        ## get components in given region
        components = subset(lyr_agg, region_id==id, category, drop=T)
        if (length(components)==0) next
        
        ## iterate components
        R.id.k <-  numeric()
        for (k in components){ # k=components[1]
          
          ## extract relavant resilience layers to goal
          lyrs <- na.omit(as.character(subset(rm, goal==g & component==k)[,c(-1,-2)]))
          
          ## r: resilience value matrix [region_id x layer: value] per region
          r <- tidyr::spread(SelectLayersData(layers, layers=lyrs) %>%
                               dplyr::filter(id_num == id) %>%
                               dplyr::select(region_id = id_num, 
                                             val_num, 
                                             layer),
                             layer, val_num) 
          
          r <- SelectLayersData(layers, layers=lyrs) %>%
            dplyr::filter(id_num == id) %>%
            dplyr::select(region_id = id_num, val_num, layer)
          
          r <- tidyr::spread(r, layer, val_num)
          
          row.names(r)  = r$region_id 
          
          r = r %>%
            dplyr::select(-region_id) %>%
            as.matrix()
          
          names(dimnames(r)) <- c('region_id', 'layer')
          
          if (nrow(r)==0) next # eg for g=CP, id=162 (Antarctica), condition='sea_ice_shoreline only'
          
          ## b: boolean value matrix [region_id x layer]
          b <- ifelse(!is.na(r), TRUE, FALSE)
          
          ## w: weighting matrix [region_id x layer]
          w <- CalculateResilienceMatrix(b, weights[dimnames(b)[[2]]])
          
          ## R: resilience score [region_id]
          R <-  CalculateResilienceScore(r, t=types[dimnames(b)[[2]]], w)
          R.id.k  <-  c(R.id.k, setNames(R, k))
        } # end of k components
        
        ## assign to resilience matrix
        if (!g %in% names(D)) D[[g]] = NA
        D[D$region_id==id,g] = round(weighted.mean(R.id.k, subset(lyr_agg, region_id==id, value, drop=T)), 2)
      } ## end of each region
    } ## end of goals with components 
  } # end for goal
  
  
  ## return scores
  scores = D %>%
    gather(goal, score, -region_id) %>% 
    mutate(dimension = 'resilience') 
  return(scores)
}