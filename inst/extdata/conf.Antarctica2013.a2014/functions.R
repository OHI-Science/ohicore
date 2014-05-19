
FIS = function(layers){
  # scores
  return(cbind(rename(SelectLayersData(layers, layers=c('fis_status'='status','fis_trend'='trend'), narrow=T),
                      c(id_num='sp_id', layer='dimension', val_num='score')), 
               data.frame('goal'='FIS')))
}


FP = function(layers, scores){
  
  # scores
s = scores %.%
    filter(goal %in% c('FIS') & dimension %in% c('status','trend','future','score')) %.%
    # NOTE: resilience and pressure skipped for supra-goals
    mutate(goal = 'FP')
  
  # return all scores
  return(rbind(scores, s))
}


NP = function(layers){
  # scores
  return(cbind(rename(SelectLayersData(layers, layers=c('np_status'='status','np_trend'='trend'), narrow=T),
                      c(id_num='sp_id', layer='dimension', val_num='score')), 
               data.frame('goal'='NP')))
}



CP = function(layers){
  
  # layers
  lyrs = list('rk' = c('rnk_hab_health' = 'health',
                       'rnk_hab_extent' = 'extent',
                       'rnk_hab_trend'  = 'trend'))
  lyr_names = sub('^\\w*\\.','', names(unlist(lyrs)))
  
  # get layer data
  D = SelectLayersData(layers, layers=lyr_names)
    # cast
  rk = rename(dcast(D, id_num + category ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['rk']]))),
              c('id_num'='sp_id', 'category'='habitat', lyrs[['rk']]))
  
  # limit to CP habitats and add rank
  habitat.rank = c('coral'            = 4,
                   'mangrove'         = 4,
                   'saltmarsh'        = 3,
                   'seagrass'         = 1,
                   'seaice_shoreline' = 4)
  
  rk = subset(rk, habitat %in% names(habitat.rank))
  rk$rank = habitat.rank[as.character(rk$habitat)]
  
  # assign extent of 0 as NA
  rk$extent[rk$extent==0] = NA
  
  # status  
  r.status = ddply(na.omit(rk[,c('sp_id','habitat','rank','extent','health')]), .(sp_id), summarize,
                   goal = 'CP',
                   dimension = 'status',
                   score = min(1, sum(rank * health * extent) / (sum(extent) * max(rank)) ) * 100 )    
  
  # trend
  r.trend = ddply(na.omit(rk[,c('sp_id','habitat','rank','extent','trend')]), .(sp_id), summarize,
                  goal = 'CP',
                  dimension = 'trend',
                  score = sum(rank * trend * extent) / (sum(extent)* max(rank)) )
  
  # return scores
  return(rbind(r.status, r.trend))  
}


TR = function(layers){
  
  # scores
  return(cbind(rename(SelectLayersData(layers, layers=c('rn_tr_status'='status','rn_tr_trend'='trend'), narrow=T),
                      c(id_num='sp_id', layer='dimension', val_num='score')), 
               data.frame('goal'='TR')))
}



LIV = function(layers){
  
  # scores
  return(cbind(rename(SelectLayersData(layers, layers=c('rn_liveco_status'='status','rn_liveco_trend'='trend'), narrow=T),
                      c(id_num='sp_id', layer='dimension', val_num='score')), 
               data.frame('goal'='LIV')))
}


LE = function(layers, scores){
  
  # scores
  s = scores %.%
    filter(goal %in% c('LIV') & dimension %in% c('status','trend','future','score')) %.%
    # NOTE: resilience and pressure skipped for supra-goals
    mutate(goal = 'LE')
  
  # return all scores
  return(rbind(scores, s))
}


ICO = function(layers){
  
  # scores
  scores = cbind(rename(SelectLayersData(layers, layers=c('rn_ico_status'='status','rn_ico_trend'='trend'), narrow=T),
                         c(id_num='sp_id', layer='dimension', val_num='score')), 
                 data.frame('goal'='ICO'))
  return(scores) 
}


LSP = function(layers){
  
  # scores
  scores = cbind(rename(SelectLayersData(layers, layers=c('rny_lsp_prot_area_status'='status','rny_lsp_prot_area_trend'='trend'), narrow=T),
                        c(id_num='sp_id', layer='dimension', val_num='score')), 
                 data.frame('goal'='LSP'))
  return(scores) 
}

SP = function(scores){
  
  d = within(
    dcast(
      scores, 
      sp_id + dimension ~ goal, value.var='score', 
      subset=.(goal %in% c('ICO','LSP') & !dimension %in% c('pressures','resilience')))
    , {
      goal = 'SP'
      score = rowMeans(cbind(ICO, LSP), na.rm=T)})
  
  
  # return all scores
  return(rbind(scores, d[,c('sp_id','goal','dimension','score')]))
}


CW = function(layers){
  browser()
  # layers
  lyrs = c('po_chemicals' = 'l',
           'po_trash'     = 'd',
           'rn_cw_chemical_trend'   = 'chem_trend',
           'rn_cw_trash_trend'  = 'trash_trend')
    
  # cast data
  d = SelectLayersData(layers, layers=names(lyrs))  
  r = rename(dcast(d, id_num ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs))),
              c('id_num'='sp_id', lyrs)); head(r); summary(r)
  
  # invert pressures
  r$l = 1 - r$l
  r$d = 1 - r$d
  
  # status
  r$status = psych::geometric.mean(t(r[,c('l','d')]), na.rm=T) * 100
  
  # trend
  r$trend = rowMeans(r[,c('chem_trend','trash_trend')], na.rm=T)
  
  # return scores
  scores = rbind(
    within(r, {
      goal      = 'CW'
      dimension = 'status'
      score     = status}),
    within(r, {
      goal      = 'CW'
      dimension = 'trend'
      score     = trend}))[,c('sp_id','goal','dimension','score')]
  return(scores)  
}


HAB = function(layers){
  
  # layers
  lyrs = c('rnk_hab_health' = 'health',
           'rnk_hab_extent' = 'extent',
           'rnk_hab_trend'  = 'trend')
  
#  browser()
  
  # cast data
  d = SelectLayersData(layers, layers=names(lyrs))  
  rk = rename(dcast(d, id_num + category ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs))),
              c('id_num'='sp_id', 'category'='habitat', lyrs))
  
  # limit to HAB habitats
  rk = subset(rk, habitat %in% c('coral','mangrove','saltmarsh','seaice_extent','seagrass','soft_bottom'))  
  
  # presence as weight
  rk$w = ifelse(!is.na(rk$extent) & rk$extent>0, 1, NA)
  
  # status
  r.status = ddply(na.omit(rk[,c('sp_id','habitat','w','health')]), .(sp_id), summarize,
                   goal      = 'HAB',
                   dimension = 'status',
                   score     = min(1, sum(w * health) / sum(w)) * 100); summary(r.status)
  
  # trend
  r.trend = ddply(na.omit(rk[,c('sp_id','habitat','w','trend')]), .(sp_id), summarize,
                  goal      = 'HAB',
                  dimension = 'trend',
                  score     = sum(w * trend) / sum(w))
  ### should these be multiplied by 5?
  # return scores
  scores = cbind(rbind(r.status, r.trend))
  return(scores)  
}


SPP = function(layers){

  # scores
  scores = cbind(rename(SelectLayersData(layers, layers=c('rn_spp_status'='status','rn_spp_trend'='trend'), narrow=T),
                      c(id_num='sp_id', layer='dimension', val_num='score')), 
               data.frame('goal'='SPP'))
  return(scores) 
}

BD = function(scores){
  
  d = within(
    dcast(
      scores, 
      sp_id + dimension ~ goal, value.var='score', 
      subset=.(goal %in% c('HAB','SPP') & !dimension %in% c('pressures','resilience'))), 
    {
      goal = 'BD'
      score = rowMeans(cbind(HAB, SPP), na.rm=T)})
  
  # return all scores
  return(rbind(scores, d[,c('sp_id','goal','dimension','score')]))
}

PreGlobalScores = function(layers, conf, scores){
    
  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T)
  
  # limit to just desired regions and global (region_id==0)
  scores = subset(scores, sp_id %in% c(rgns[,'id_num'], 0))
  
  # apply NA to Antarctica
  id_ant = subset(rgns, val_chr=='Antarctica', id_num, drop=T)
  scores[scores$sp_id==id_ant, 'score'] = NA
    
  return(scores)
}

FinalizeScores = function(layers, conf, scores){
  
  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T)
    
  # add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       sp_id = c(rgns[,'id_num'], 0),
                       dimension = c('pressures','resilience','status','trend','future','score'), 
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors=F); head(d)
  d = subset(d, 
             !(dimension %in% c('pressures','resilience','trend') & sp_id==0) & 
             !(dimension %in% c('pressures','resilience','status','trend') & goal=='Index'))
  scores = merge(scores, d, all=T)[,c('goal','dimension','sp_id','score')]
      
  # order
  scores = arrange(scores, goal, dimension, sp_id)
  
  # round scores
  scores$score = round(scores$score, 2)
    
  return(scores)
}