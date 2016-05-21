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
  for (g in subgoals){ # g="CS"
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

      ####################################################################################
      ## Case 2: Goals with components (e.g., mangrove, seagrass, etc.)
      ####################################################################################
      
       } else {
      
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
      
    #}     # end Case 1 if (nrow(r.g)==1)
    
    # ## Case 2: multiple components within goal, config.r 'level'=='region_id' ----
    # if (nrow(r.g) > 1 && rc[[g]][['level']]=='region_id'){
    #   
    #   ## check for valid component conditions
    #   cond = with(r.g,
    #               data.frame(
    #                 component    = component,
    #                 cond.default = ifelse(component=='', TRUE, NA),
    #                 cond.only    = grepl('(.*) only',          component),
    #                 cond.with    = grepl('(.*), with (.*)',    component),
    #                 cond.without = grepl('(.*), without (.*)', component), stringsAsFactors=F))
    #   class(cond$component)
    #   
    #   ## error unless only one TRUE per condition column
    #   if (!all.equal(apply(cond[,-1], 1, sum, na.rm=T), rep(1,length(r.g$component)))){
    #     stop(sprintf('The %s components identified in resilience_matrix.csv are not unique\n', g))
    #   }
    #   
    #   ## break down condition into individual components needed for later evaluation
    #   cond = cbind(
    #     cond,
    #     cond.only.1     = ifelse(cond$cond.only==TRUE, gsub("(.*) only",             "\\1", r.g$component), NA),
    #     cond.with.1     = ifelse(cond$cond.with==TRUE, gsub("(.*), with (.*)",       "\\1", r.g$component), NA),
    #     cond.with.2     = ifelse(cond$cond.with==TRUE, gsub("(.*), with (.*)",       "\\2", r.g$component), NA),
    #     cond.without.1  = ifelse(cond$cond.without==TRUE, gsub("(.*), without (.*)", "\\1", r.g$component), NA),
    #     cond.without.2  = ifelse(cond$cond.without==TRUE, gsub("(.*), without (.*)", "\\2", r.g$component), NA))
    #   
    #   ## iterate regions; not all regions have all categories within the components (ie habitats within the habitat groups)
    #   for (id in D$region_id){ # id=3
    #     
    #     ## get components in given region
    #     components = subset(lyr_agg, region_id==id, category, drop=T)
    #     
    #     if (length(components)==0) next
    #     # ?: CS default '' needs components or ok if 0 when having default?
    #     
    #     ## get condition for region "to see what condition my condition was in" :)
    #     cond.components = cond[,c('cond.default','cond.only.1','cond.with.1','cond.with.2','cond.without.1','cond.without.2')]
    #     components.in.cond = as.data.frame(apply(cond.components, c(1,2), function(x) x %in% components), 
    #                                        row.names=cond[['component']])
    #     
    #     # TODO: for HAB, seems wrong that for regions to qualify for "* only" component conditions, they can only have that component, even if other like sea_ice_edge included
    #     components.in.cond[['cond.only.1']] = ifelse(components.in.cond[['cond.only.1']]==T & length(components)==1, T, F)
    #     components.in.cond[['cond.without.2']] = !components.in.cond[['cond.without.2']] # invert without predicate
    #     components.in.cond[is.na(cond.components)] = NA
    #     
    #     ## assign condition to default if default ('') row exists and no other condition found to be TRUE
    #     if ('' %in% rownames(components.in.cond)){
    #       if(!any(apply(components.in.cond[''!=rownames(components.in.cond),], 1, function(x) all(x==T,na.rm=T)))){
    #         components.in.cond['','cond.default'] = TRUE
    #       }
    #     }
    #     
    #     ## get condition based on which is true
    #     condition = rownames(components.in.cond)[apply(components.in.cond, 1, function(x) all(x==T,na.rm=T))]
    #     #if (identical(condition, character(0))) condition = NA
    #     if (identical(condition, character(0))){
    #       if (debug) cat(sprintf('  skipping region %s for %s since no matching conditions, but having components: %s\n', id, g, paste(components, collapse=', ')))
    #       next # Wierd: with layers.Global2013.www2013, g=HAB, id=35, get condition=NA. and for HAB then lyrs bonks
    #     }
    #     
    #     lyrs <- na.omit(as.character(subset(rm, goal==g & component==condition)[,c(-1,-2)]))
    #     
    #     ## r: resilience value matrix [region_id x layer: value] per region
    #     r = spread(SelectLayersData(layers, layers=lyrs) %>%
    #                  filter(id_num == id) %>%
    #                  select(region_id = id_num, 
    #                         val_num, 
    #                         layer),
    #                layer, val_num) 
    #     row.names(r)  = r$region_id 
    #     r = r %>%
    #       select(-region_id) %>%
    #       as.matrix()
    #     names(dimnames(r)) <- c('region_id', 'layer')
    #     
    #     
    #     if (nrow(r)==0) next # eg for g=CP, id=162 (Antarctica), condition='sea_ice_shoreline only'
    #     
    #     ## b: boolean value matrix [region_id x layer]
    #     b <- ifelse(!is.na(r),T,F)
    #     
    #     ## w: weighting matrix [region_id x layer]
    #     w <- CalculateResilienceMatrix(b, weights[dimnames(b)[[2]]])
    #     
    #     ## R: resilience score [region_id]
    #     R = CalculateResilienceScore(r, types[dimnames(b)[[2]]], w)
    #     
    #     ## assign R score for id to resilience data frame
    #     if (!g %in% names(D)) D[[g]] = NA
    #     D[D$region_id==id,g] = R
    #     
    #   } # end iterate regions: for (id in D$region_id)
    #   
    # } # end Case 2: if (nrow(r.g) > 1 && rc[[g]][['level']]=='region_id')
    # 
    # 
    # ## Case 3: multiple components within goal (for NP only) ----
    # if (nrow(r.g) > 1 && rc[[g]][['level']]=='region_id-category'){
    #   
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
        }
        
        ## assign to resilience matrix
        if (!g %in% names(D)) D[[g]] = NA
        D[D$region_id==id,g] = round(weighted.mean(R.id.k, subset(lyr_agg, region_id==id, value, drop=T)), 2)
      }
     } 
  # } # end for g in...
  
  
  ## return scores
  scores = D %>%
    gather(goal, score, -region_id) %>% 
    mutate(dimension = 'resilience') 
  return(scores)
}
