#' Calculate all the resilience score for each (sub)goal.
#' 
#' @param layers object \code{\link{Layers}}
#' @param conf object \code{\link{Conf}}
#' @return data.frame containing columns 'region_id' and per subgoal resilience score 
#' @export
CalculateResilienceAll = function(layers, conf, debug=FALSE){
#DEBUG: load_all(); 
#conf=conf.Global2013.www2013; layers=layers.Global2013.www2013; debug=T; scores = scores.Global2013.www2013
#R.scores = CalculateResilience(layers, conf, debug=F); dim(R.scores); head(R.scores)
# subgoals = subset(conf$goals, !goal %in% unique(conf$goals$parent), goal, drop=T)
# head(dcast(subset(scores$data, dimension=='resilience' & goal %in% subset(conf$goals, !goal %in% unique(conf$goals$parent), goal, drop=T)), region_id ~ goal)[,c('region_id',subgoals)])
  
  # get resilience layers
  rm = conf$resilience_matrix
  rm = within(rm, {component[is.na(component)] = ''})
  rw = conf$resilience_weights
  rc = conf$config$resilience_components
  rk = conf$config$resilience_categories
  r.layers = setdiff(names(rm), c('goal','component','component_name'))
  if (!all(subset(layers$meta, layer %in% r.layers, val_0to1, drop=T))){
    message('Error: Not all resilence layers range in value from 0 to 1!')
    print(subset(layers$meta, layer %in% r.layers & val_0to1==F, c('val_min','val_max'), drop=F))
    stop('')    
  }
  stopifnot(all(r.layers %in% rw$layer))
  
  # setup initial data.frame for column binding results by region
  D = rename(SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T), c('id_num'='region_id'))[,'region_id',drop=F]
  regions = D[['region_id']]
  
  # w.layers: weighting vector [layer]
  weights = setNames(rw$weight, rw$layer)
  
  # t: typing vector [layer] 
  types = setNames(rw$type, rw$layer)  
  
  # iterate goals
  subgoals = subset(conf$goals, !goal %in% unique(conf$goals$parent), goal, drop=T)

  for (g in subgoals){ # g=subgoals[1]
    
    if (debug) cat(sprintf('goal: %s\n', g))    
    r.g = subset(rm, goal==g)
    
    if (nrow(r.g)==1){
      # simple single component goal
      
      # extract relavant resilience layers to goal
      lyrs = na.omit(as.character(r.g[!names(r.g) %in% c('goal','component','component_name')]))
      
      # r: resilience value matrix [region_id x layer: value]
      r = acast(subset(SelectLayersData(layers, layers=lyrs), id_num %in% regions), id_num ~ layer, value.var='val_num')
      names(dimnames(r)) <- c('region_id', 'layer')
            
      # b: boolean value matrix [region_id x layer]
      b <- ifelse(!is.na(r),T,F); head(b)
      
      # w: weighting matrix [region_id x layer]
      w <- CalculateResilienceMatrix(b, weights[dimnames(b)[[2]]]); head(w)
      
      # R: resilience score [region_id]
      R = CalculateResilienceScore(r, types[dimnames(b)[[2]]], w)
      
      # assign to resilience matrix    
      D = merge(D, setNames(data.frame(as.integer(names(R)), R), c('region_id',g)))
    } else {
      stopifnot(g %in% names(rc))
      stopifnot(rc[[g]][['layer']] %in% layers$meta$layer)
      
      lyr_agg = rename(subset(SelectLayersData(layers, layers=rc[[g]][['layer']], narrow=T),  id_num %in% regions),
                       c('id_num'='region_id','val_num'='value'))
      #stopifnot(subset(layers$meta, layer %in% rc[[g]][['layer']], val_0to1, drop=T))
      
      # check that all components are in lyr_agg
      cond.1 = sub('(.*)( only|, with |, without )(.*)', '\\1', r.g$component)
      cond.2 = sub('(.*)( only|, with |, without )(.*)', '\\3', r.g$component)
      component_categories = unique(na.omit(c(ifelse(nchar(cond.1)>0, cond.1, NA),
                                              ifelse(nchar(cond.2)>0, cond.2, NA))))      
      if (!all(component_categories %in% unique(lyr_agg$category))){
        cat(sprintf('Based on the following components for %s:\n  %s', g, paste(r.g$component, collapse='\n  ')))
#         stop(sprintf('The following component categories for %s are not in the aggregation layer %s categories (%s): %s', g, rc[[g]][['layer']], 
#                      paste(unique(lyr_agg$category), collapse=', '),
#                      paste(component_categories[!component_categories %in% lyr_agg$category], collapse=', ')))
      }
    }      
    
    if (nrow(r.g) > 1 && rc[[g]][['level']]=='region_id'){
      # multiple components within goal, selecting component row for layers to use, and calculating Resilience per region_id            
      
      # check for valid component conditions
      cond = with(r.g, 
                  data.frame(
                    component    = component,
                    cond.default = ifelse(component=='', TRUE, NA),
                    cond.only    = grepl('(.*) only',          component),
                    cond.with    = grepl('(.*), with (.*)',    component),
                    cond.without = grepl('(.*), without (.*)', component), stringsAsFactors=F))
      class(cond$component)
      
      # ensure only one TRUE per condition
      stopifnot(all.equal(apply(cond[,-1], 1, sum, na.rm=T), rep(1,length(r.g$component))))
      # break down condition into individual components needed for later evaluation
      cond = cbind(cond,
                   cond.only.1     = ifelse(cond$cond.only==TRUE, gsub("(.*) only",             "\\1", r.g$component), NA),
                   cond.with.1     = ifelse(cond$cond.with==TRUE, gsub("(.*), with (.*)",       "\\1", r.g$component), NA),
                   cond.with.2     = ifelse(cond$cond.with==TRUE, gsub("(.*), with (.*)",       "\\2", r.g$component), NA),
                   cond.without.1  = ifelse(cond$cond.without==TRUE, gsub("(.*), without (.*)", "\\1", r.g$component), NA),
                   cond.without.2  = ifelse(cond$cond.without==TRUE, gsub("(.*), without (.*)", "\\2", r.g$component), NA))      
      
      # iterate regions
      for (id in D$region_id){ # id=12
        # get components in given region
        components = subset(lyr_agg, region_id==id, category, drop=T)
        
        if (length(components)==0) next
        # ?: CS default '' needs components or ok if 0 when having default?
        
        # get condition for region "to see what condition my condition was in"
        cond.components = cond[,c('cond.default','cond.only.1','cond.with.1','cond.with.2','cond.without.1','cond.without.2')]
        components.in.cond = as.data.frame(apply(cond.components, c(1,2), function(x) x %in% components), row.names=cond[['component']])
        
        # TODO: for HAB, seems wrong that for regions to qualify for "* only" component conditions, they can only have that component, even if other like sea_ice_edge included
        components.in.cond[['cond.only.1']] = ifelse(components.in.cond[['cond.only.1']]==T & length(components)==1, T, F)      
        components.in.cond[['cond.without.2']] = !components.in.cond[['cond.without.2']] # invert without predicate
        components.in.cond[is.na(cond.components)] = NA
        # assign condition to default if default ('') row exists and no other condition found to be True
        if ('' %in% rownames(components.in.cond)){
          if(!any(apply(components.in.cond[''!=rownames(components.in.cond),], 1, function(x) all(x==T,na.rm=T)))){
            components.in.cond['','cond.default'] = TRUE
          }        
        }
        
        # get condition based on which is true
        condition = rownames(components.in.cond)[apply(components.in.cond, 1, function(x) all(x==T,na.rm=T))]
        #if (identical(condition, character(0))) condition = NA
        if (identical(condition, character(0))){
          if (debug) cat(sprintf('  skipping region %s for %s since no matching conditions, but having components: %s\n', id, g, paste(components, collapse=', ')))
          next # Wierd: with layers.Global2013.www2013, g=HAB, id=35, get condition=NA. and for HAB then lyrs bonks       
        } 
        
        lyrs <- na.omit(as.character(subset(rm, goal==g & component==condition)[,c(-1,-2)]))
        
        # r: resilience value matrix [region_id x layer: value]
        r = acast(subset(SelectLayersData(layers, layers=lyrs), id_num == id), id_num ~ layer, value.var='val_num')
        names(dimnames(r)) <- c('region_id', 'layer')
        
        if (nrow(r)==0) next # eg for g=CP, id=162 (Antarctica), condition='sea_ice_shoreline only'
        
        # b: boolean value matrix [region_id x layer]
        b <- ifelse(!is.na(r),T,F)
        
        # w: weighting matrix [region_id x layer]
        w <- CalculateResilienceMatrix(b, weights[dimnames(b)[[2]]])
        
        # R: resilience score [region_id]
        R = CalculateResilienceScore(r, types[dimnames(b)[[2]]], w)
        
        # assign to resilience matrix
        if (!g %in% names(D)) D[[g]] = NA
        D[D$region_id==id,g] = R         
      }    
    }
    
    if (nrow(r.g) > 1 && rc[[g]][['level']]=='region_id-category'){
      # multiple components within goal, calculating Resilience per category, and averaging up to region_id (for NP only)
      
      # iterate regions
      for (id in D$region_id){ # id=1
        
        # get components in given region
        components = subset(lyr_agg, region_id==id, category, drop=T)      
        if (length(components)==0) next
        
        # iterate components
        R.id.k = numeric()
        for (k in components){ # k=components[1]
          
          # extract relavant resilience layers to goal
          lyrs <- na.omit(as.character(subset(rm, goal==g & component==k)[,c(-1,-2)]))
          
          # r: single region resilience value matrix [region_id x layer: value]
          r = acast(subset(SelectLayersData(layers, layers=lyrs), id_num==id), id_num ~ layer, value.var='val_num')
          names(dimnames(r)) <- c('region_id', 'layer')
          
          if (nrow(r)==0) next # eg for g=CP, id=162 (Antarctica), condition='sea_ice_shoreline only'
          
          # b: boolean value matrix [region_id x layer]
          b <- ifelse(!is.na(r),T,F)
          
          # w: weighting matrix [region_id x layer]
          w <- CalculateResilienceMatrix(b, weights[dimnames(b)[[2]]])
          
          # R: resilience score [region_id]
          R = CalculateResilienceScore(r, types[dimnames(b)[[2]]], w)
          R.id.k = c(R.id.k, setNames(R, k))                  
        }  
        
        # assign to resilience matrix
        if (!g %in% names(D)) D[[g]] = NA
        D[D$region_id==id,g] = round(weighted.mean(R.id.k, subset(lyr_agg, region_id==id, value, drop=T)), 2)    
      }      
    }
  } # end for g in...
  
  # return scores
  scores = cbind(melt(D, id.vars='region_id', variable.name='goal', value.name='score'), dimension='resilience'); head(scores)
  return(scores)
}