calc.Pressures = function(){
  # TODO: use regions from layers and load.regions.countries()
  
  # copied from '/Volumes/local_edit/src/toolbox/code/calc_pressures.R' on 2013-09-02
  #
  # intro -------------------------------------------------------------------
  # Calculate pressures using ohi R functions:
  #
  #   p = ohi.model.pressures.matrix(alpha, beta), where
  #       alpha = weighting matrix of the form [category x pressure]
  #       beta  = aggregation matrix of the form [region_id x category] to collapse across each category
  #
  #   P = ohi.model.pressures(p, w, GAMMA=0.5), where
  #       p = the pressures value matrix [region_id x pressure]
  #       w = rank weighting matrix of the form [region_id x pressure]
  #
  # TODO: CS habitat extent column names
  # TODO: add w_max_p=3 as argument to ohi.model.pressures to allow for modification
  # TODO: Are these regions the only ones that fail? 2, 24, 67, 79, 114, and 172
  #       If so, I suspect that it's a problem with a regions -> country mapping or some other aggregation method related to the regions definitions. 
  #       These regions are either a collection of islands (2, 67, 114), or other small islands (24, 79, 172).
  # TODO: automate ingestion of other layers and merge with prep_pressures_layers.R
  # TODO: compare alpha.all when setting NAs to 0
  #       alpha.all[is.na(alpha.all)] = 0
  # TODO: For now, warn of missing layers from pressures_matrix.csv and remove. Later, stop with error.  
  #
  # see: /var/data/ohi/model/GL-NCEAS-Pressures_Matrix/README_EXAMPLES.html
  #      /var/data/ohi/model/GL-NCEAS-Pressures_Matrix/report{5-7}.R - basic HAB component calc and compare example
  
  # libraries and paths -----------------------------------------------
  require(reshape2)
  require(plyr)
  options(gsubfn.engine = "R") # for sqldf
  library(sqldf)
  options(sqldf.driver = 'SQLite')
  options(sqldf.verbose = F)
  
  debug = TRUE # for saving and later comparing pressure components
  
  # read data -----------------------------------------------------
  
  # ensure variables set from config.R
  stopifnot(exists('layers_navigation.csv'))
  
  # load data
  layers_navigation = read.csv(layers_navigation.csv, na.strings=c('','NA'))
  layers_data = read.csv(layers_data.csv, na.strings=c('','NA'))
  pressures_matrix = read.csv(pressures_matrix.csv, na.strings='')
  
  # get unique region_ids for analysis, and setup initial pressures data.frame for column binding results
  load.regions.countries()
  region_ids = rgn_global$rgn_id
  d_region_ids = data.frame(region_id=region_ids)
  D = data.frame(rgn_id=region_ids)
  
  # ingest layers data ----------------------------------------------------------
  
  # read in pressures matrix
  alpha.all = pressures_matrix
  
  # cast pressures layer data
  p.layers = sort(names(alpha.all)[!names(alpha.all) %in% c('goal','component','component_name')])
  d.p = rename(dcast(layers_data, id_num ~ layer, value.var='value_num', subset=.(layer %in% p.layers)),
               c('id_num'='region_id')); head(d.p)
  
  # check for any duplicates in layers, which forces the dcast to use length as the input value (BIG PROBLEM)
  # TODO: add this to check.layers_navigation()
  stopifnot(ddply(subset(layers_data, layer %in% p.layers), .(layer), summarize, duplicated = !anyDuplicated(id_num)>0 )[,'duplicated'])  
  
  # # run once: update layers_navigation.csv to show layers used
  # ln = read.csv(layers_navigation.csv, na.strings='')
  # stopifnot(p.layers %in% ln$layer)
  # ln[ln$layer %in% p.layers, 'used_nature2012'] = 'pressures_matrix'; ln[ln$layer %in% p.layers, 1:5]
  # write.csv(ln, layers_navigation.csv, row.names=F, na='')
  
  # handle still missing layers
  p.layers.missing = p.layers[!p.layers %in% layers_data$layer]
  if (length(p.layers.missing)>0){
    warning('MISSING pressures_matrix.csv stressors from layers_data*.csv: ', paste(p.layers.missing, collapse=', '))  
    # MISSING pressures_matrix.csv stressors from layers_data*.csv: cc_slr, hd_shrimp_pct
    p.layers = setdiff(p.layers, p.layers.missing) 
    alpha.all = alpha.all[, !names(alpha.all) %in% p.layers.missing]
  }
  
  # confirm that all the resilience layers in the layers_data range from 0 to 1
  p.vals =  ddply(subset(layers_data, layer %in% p.layers), .(layer), summarize,
                  val_min = min(value_num, na.rm=T),
                  val_max = max(value_num, na.rm=T),
                  val_ok  = val_min >= 0 & val_max <= 1)
  if (any(!p.vals$val_ok)){
    lyrs.notok = subset(p.vals, val_ok==F, layer, drop=T)
    print('ORIGINAL problematic layers...')
    print(subset(p.vals, val_ok==F), row.names=F)    
    #stop('Some pressures matrix layers are outside the require range [0-1].')
    warning('Some pressures matrix layers are outside the require range [0-1]. So quick HACK rescaling 0 to 1.') # DEBUG
    #DEBUG HACKY KLUDGE to get reasonable pressures outputs
    for (lyr in lyrs.notok){ # lyr = lyrs.notok[1]
      d = subset(layers_data, layer==lyr); summary(d)
      d = within(d, {
        value_num = ( value_num - min(value_num, na.rm=T) ) / ( max(value_num, na.rm=T) - min(value_num, na.rm=T) ) }); summary(d)
      layers_data = rbind(subset(layers_data, layer != lyr),
                          d)
    }
    p.vals =  ddply(subset(layers_data, layer %in% lyrs.notok), .(layer), summarize,
                    val_min = min(value_num, na.rm=T),
                    val_max = max(value_num, na.rm=T),
                    val_ok  = val_min >= 0 & val_max <= 1)
    print('HACK RESCALED layers...')
    print(p.vals, row.names=F)    
  }
  
  
  # iterate goals -----------------------------------------------------------
  # TODO: generate ohi.goal.subgoal.unique from goals.csv
  for (g in ohi.goal.subgoal.unique){ # g=ohi.goal.subgoal.unique[1]   # g='NP' # g='LIV' # g='FIS'  # g='CP'
    #for (g in c('LIV')){  # DEBUG
    
    if (debug) cat(sprintf('goal: %s\n', g))
    
    # reset components for so when debug==TRUE and saving, is per goal
    P = w = p = alpha = beta = NA
    
    # p: pressures value matrix [region_id x pressure]
    p <- matrix(as.matrix(d.p[match(region_ids, d.p$region_id), p.layers]), 
                nrow=length(region_ids), ncol=length(p.layers), 
                dimnames = list(region_id=region_ids, pressure=p.layers)); dim(p)
    
    # components
    p.components = alpha.all$component[alpha.all$goal==g]
    
    if (length(p.components)==1){
      cat('  no components\n')
      
      # pressure weighting matrix applied to all regions
      w <- matrix(rep(unlist(alpha.all[alpha.all$goal==g, p.layers]), length(region_ids)*length(p.layers)), byrow=T, 
                  nrow=length(region_ids), ncol=length(p.layers), 
                  dimnames = list(region_id=region_ids, pressure=p.layers))        
      
      # debug spot check Ascension Island in middle of N Atlantic which should have low CW pressures, but is at max of 1
      #       if (g=='CW') browser()
      #       p['85', names(na.omit(w[1,]))]
      #       w['85', names(na.omit(w[1,]))]
      #   po_chemicals_3nm   po_nutrients_3nm       po_pathogens           po_trash             ss_wgi 
      # 0.0671219881369822 0.0000000000000000 0.2426123407727320 0.8285002918469240 0.2233172297507260
      # 
      # po_chemicals_3nm po_nutrients_3nm     po_pathogens         po_trash           ss_wgi 
      #                3                3                3                3                1      
      # If the w(g, i, j) is NoData or 0, the weighted pressure pw(g, i, j) is NoData.
      #   p.k = ( 3 * 0.0671219881369822 + 3 * 0 + 3 * 0.2426123407727320 + 3 * 0.8285002918469240 ) / 3; p.k # 0.379411540252213
      #   
      #   p.i = c( 3 * 0.0671219881369822 / 3, 3 * 0.2426123407727320 / 3, 3 * 0.8285002918469240 / 3 )
      #   w.max = max(c(3, 3, 3))
      #   p.e = sum(w.max * p.i) / (w.max * 3); p.e # 0.828500291846924 
      #   
      #       
      #       sum( p.w * p.k ) / p.k * 3
      #   p.worst = max(c(3 * 0.0671219881369822 / 3, 3 * 0.2426123407727320 / 3, 3 * 0.8285002918469240 / 3 )); p.worst # 0.828500291846924
      #   p.env = ( 3 * 0.0671219881369822 + 3 * 0 + 3 * 0.2426123407727320 + 3 * 0.8285002918469240 ) / ( 3 *3); p.env # 0.379411540252213
      #       
      #   p.soc = 0.2233172297507260
      #   gamma = 0.5
      #   pressure = gamma * p.env + 0.5 * p.soc; pressure
      #   pressure = 0.6116586149
      #   p.soc = 0.2233172297507260
      #   p.env = pressure - 0.5 * p.soc = 0.500000000024637
      
      # calculate pressures per region
      P = ohi.model.pressures(p, w, GAMMA=0.5)
      # if (g=='CW'){
      #   P = ohi.model.pressures(p, w, GAMMA=0.5, browse=T)
      # } else {
      #   P = ohi.model.pressures(p, w, GAMMA=0.5)
      # }
      
    } else { 
      if (debug) cat(' ',length(p.components),'components:', paste(p.components,collapse=', '), '\n')
      
      # alpha [component x pressure]: pressure rank matrix applied to all categories
      alpha <- matrix(as.matrix(alpha.all[alpha.all$goal==g, p.layers]), 
                      nrow=length(p.components), ncol=length(p.layers), 
                      dimnames = list(category=p.components, pressure=p.layers))
      
      # get data layer for determining the weights by region, which could be from layers_data or layers_data_bycountry
      stopifnot(g %in% names(pressures_components))
      #     if (!agg$layer_id %in% get(agg$layers_data)$layer_id & debug==TRUE){
      #       if (debug) cat('  skipping: pressures_component_aggregation.layer_id not yet configured\n')
      #       next()
      #     } else if (!agg$layer_id %in% get(agg$layers_data)$layer_id & debug==FALSE){
      #       stopifnot(agg$layer_id %in% get(agg$layers_data)$layer_id)
      #     }        
      #d_w = subset(get(agg$layers_data), layer_id==agg$layer_id)
      d_w = rename(subset(layers_data, layer==pressures_components[[g]][['layer']], c(id_num,category,value_num)),
                   c('id_num'='region_id','value_num'='value'))
      
      # ensure that all components are in the aggregation layer category
      if (!all(p.components %in% unique(d_w$category))){
        stop(sprintf('The following components for %s are not in the aggregation layer %s categories (%s): %s', g, pressures_components[[g]][['layer']], 
                     paste(unique(d_w$category), collapse=', '),
                     paste(p.components[!p.components %in% d_w$category], collapse=', ')))
      }
      #       cond.1 = sub('(.*)( only|, with |, without )(.*)', '\\1', r.g$component)
      #       cond.2 = sub('(.*)( only|, with |, without )(.*)', '\\3', r.g$component)
      #       component_categories = unique(na.omit(c(ifelse(nchar(cond.1)>0, cond.1, NA),
      #                                               ifelse(nchar(cond.2)>0, cond.2, NA))))      
      #       if (!all(component_categories %in% unique(lyr_agg$category))){
      #         cat(sprintf('Based on the following components for %s:\n  %s', g, paste(r.g$component, collapse='\n  ')))
      #         stop(sprintf('The following component categories for %s are not in the aggregation layer %s categories (%s): %s', g, resilience_components[[g]][['layer']], 
      #                      paste(unique(lyr_agg$category), collapse=', '),
      #                      paste(component_categories[!component_categories %in% lyr_agg$category], collapse=', ')))
      #       }
      
      # based on sequence of aggregation
      if (pressures_components[[g]][['level']]=='region_id-category'){
        # eg NP: calculate a pressure by region_id (like a subgoal pressure per category), Then aggregate using pressures_component_aggregation:layer_id.
        if (debug) cat(sprintf("  scoring pressures seperately by region and category, like a subgoal (pressures_calc_level=='region_id-category')\n"))
        
        # get pressure per category
        if (exists('krp')) rm(krp)
        for (k in p.components){ # k = p.components[1]
          
          # w [region_id x pressure]: pressure weighting matrix applied to all regions
          w <- matrix(rep(unlist(alpha.all[alpha.all$goal==g & alpha.all$component==k, p.layers]), length(region_ids)*length(p.layers)), byrow=T, 
                      nrow=length(region_ids), ncol=length(p.layers), 
                      dimnames = list(region_id=region_ids, pressure=p.layers))
          
          # calculate pressures per region, category
          rp.k = data.frame(category=k, region_id=dimnames(p)$region_id, 
                            p=ohi.model.pressures(p, w, GAMMA=0.5))
          if (exists('krp')){
            krp = rbind(krp, rp.k)
          } else {
            krp = rp.k
          }
        }
        
        # join region, category, pressure to weighting matrix
        #browser()
        krpw = sqldf("
                     SELECT CAST(region_id AS INTEGER) AS region_id, category, krp.p, d_w.value AS w 
                     FROM krp 
                     INNER JOIN d_w USING (region_id, category) 
                     ORDER BY CAST(region_id AS INTEGER), category")
        #krpwp = sqldf("SELECT region_id, SUM(w*p)/COUNT(category) AS p FROM d_region_ids LEFT JOIN krpw USING (region_id) GROUP BY region_id")
        krpwp = sqldf("SELECT region_id, SUM(w*p)/SUM(w) AS p FROM d_region_ids LEFT JOIN krpw USING (region_id) GROUP BY region_id")
        P = krpwp$p
        names(P) = krpwp$region_id      
        
      } else if (pressures_components[[g]][['level']]=='region_id'){
        # most goals like this: collapse weights across categories first, then calculate pressures per region
        if (debug) cat(sprintf("  aggregating across categories to region (pressures_calc_level=='region_id')\n"))
        
        # cast and get sum of categories per region
        #if (g=='CS') browser()
        if (!is.na(subset(layers_navigation, layer==pressures_components[[g]][['layer']], id_chr, drop=T))){
          #if (agg$layers_data == 'layers_data_bycountry'){ # OLD, before agg moved to config.R
          # this condition seems to no onger apply, since all but NP (handled above if level is 'region_id-category')
          stop('surprise, layers_data_bycountry used')
          if (debug) cat(sprintf("  using layers_data='layers_data_bycountry'\n"))
          d_w_r = sqldf(paste("SELECT DISTINCT region_id, category, country_id, country_area_km2 value FROM d_w JOIN regions_countries_areas USING(country_id) WHERE region_id IN (",paste(region_ids, collapse=','),")"))        
          m_w = dcast(d_w_r, region_id ~ category, sum)  # function(x) sum(x, na.rm=T)>0)
        } else { # presume layers_data == 'layers_data'    
          if (debug) cat(sprintf("  using layers_data='layers_data'\n"))
          # for CS: matrix of weights by category based on proportion of regional total for all categories
          #browser()
          m_w = dcast(subset(d_w, region_id %in% region_ids), region_id ~ category, sum, margins=c('category'))
          m_w = cbind(m_w[,'region_id',drop=F], m_w[,2:(ncol(m_w)-1)] / m_w[,'(all)'])  #print(summary(m_w))        
        }      
        
        # beta [region_id x category]: aggregation matrix 
        beta = matrix(as.matrix(m_w[,-1]), 
                      nrow=nrow(m_w), ncol=ncol(m_w)-1, 
                      dimnames = list(region_id=m_w$region_id, category=names(m_w)[-1]))
        
        # for LIV/ECO, limit beta columns to alpha rows
        beta = beta[, rownames(alpha)]
        
        # calculate weighting matrix
        if (debug) cat(sprintf("  ohi.model.pressures.matrix(alpha, beta, calc='avg')\n"))
        w = ohi.model.pressures.matrix(alpha, beta, calc='avg')
        # TODO: test calc type of calculation, whether avg (default), mean (diff't from avg?) or presence (results in 1 or 0)
        
        # append missing regions with NA
        region_ids.missing = setdiff(region_ids, dimnames(w)$region_id)
        pressures.missing = setdiff(p.layers, dimnames(w)$pressure)
        w = matrix(rbind(cbind(w, 
                               matrix(0, nrow=nrow(w), ncol=length(pressures.missing))), 
                         matrix(0, nrow=length(region_ids.missing), ncol=ncol(w)+length(pressures.missing))),
                   nrow=nrow(w)+length(region_ids.missing), ncol=ncol(w)+length(pressures.missing),
                   dimnames = list('region_id'=c(dimnames(w)$region_id, region_ids.missing), 
                                   'pressure'=c(dimnames(w)$pressure, pressures.missing)))[as.character(region_ids), p.layers]    
        
        # check matrices
        stopifnot(all(dimnames(w)$pressure == dimnames(w)$pressure))
        stopifnot(!is.null(dimnames(w)$region_id))
        stopifnot(all(dimnames(p)$region_id == dimnames(w)$region_id))
        
        # calculate pressures per region
        P = ohi.model.pressures(p, w, GAMMA=0.5)
        
      } else {
        stop(sprintf("pressures_component_aggregation.csv : pressures_calc_level of '%s' not handled. Must be either 'region_id' or 'region_id-category'.", agg$aggregation_sequence))
      }    
    } # end if (length(p.components)==1)
    
    #     # contrast
    #     P.tbx = data.frame(goal.subgoal=g, id=names(P), pressures=P*100)
    #     P.ans = subset(results_global_data, goal.subgoal==g, c(goal.subgoal, id, pressures))
    #     cat('Compare x=P.tbx with y=P.ans...\n')
    #     ck.P = contrast(x=P.tbx, y=P.ans, by=c('goal.subgoal','id'), on='pressures', drop.mutual.na=T, precision=2, verbosity=1)
    # notice that some Nature 2012 answer region_ids are consistently NA: 110, 114, 79
    
    #     # save individual goal pressure components for later comparison if debug==TRUE
    #     if (debug==TRUE){
    #       fn = sprintf('data/debug/pressures_%s.RData', g)
    #       if (!file.exists(dirname(fn))){ dir.create((dirname(fn))) }
    #       save(P, w, p, alpha, beta, file=fn)      
    #     }  
    
    # bind to results
    D = merge(D, setNames(data.frame(names(P),
                                     P), c('rgn_id',g)), all.x=T)
    
  }
  return(D)
}
