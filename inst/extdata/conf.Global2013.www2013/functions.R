FIS = function(layers){
  
  # status
  r.status = rename(SelectLayersData(layers, layers='rn_fis_status'), c('id_num'='region_id','val_num'='score'))[,c('region_id','score')]
  r.status$score = r.status$score * 100
  
  # trend
  r.trend = rename(SelectLayersData(layers, layers='rn_fis_trend'), c('id_num'='region_id','val_num'='score'))[,c('region_id','score')]
  
  # return scores
  s.status = cbind(r.status, data.frame('dimension'='status'))
  s.trend  = cbind(r.trend , data.frame('dimension'='trend' ))
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='FIS'))
  return(scores)  
}

MAR = function(layers){
  
  # status
  r.status = rename(SelectLayersData(layers, layers='rn_mar_status', narrow=T), c('id_num'='region_id','val_num'='score'))
  r.status$score = r.status$score * 100
  
  # trend
  r.trend = rename(SelectLayersData(layers, layers='rn_mar_trend', narrow=T), c('id_num'='region_id','val_num'='score'))
  
  # return scores
  s.status = cbind(r.status, data.frame('dimension'='status'))
  s.trend  = cbind(r.trend , data.frame('dimension'='trend' ))
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='MAR'))
  return(scores)  
}

FP = function(layers, scores){
  # TODO: check with differences calculated here (P,R,S,T now -> F,G later) vs layers_2013.R (S,T,F,G now)

  # weights
  w = rename(SelectLayersData(layers, layers='rn_fp_wildcaught_weight', narrow=T),
             c('id_num'='region_id', 'val_num'='w_FIS')); head(w)
  
  # scores
  s = dcast(scores, region_id + dimension ~ goal, value.var='score', subset=.(goal %in% c('FIS','MAR'))); head(s)
  
  # combine
  scores = mutate(merge(s, w),
                  goal='FP',
                  score=weighted.mean(c(FIS, MAR), c(w_FIS, 1-w_FIS), na.rm=T))[,c('region_id','goal','dimension','score')]; head(scores)
  return(scores)
}

AO = function(layers, 
              year_max=max(layers_data$year, na.rm=T), 
              year_min=max(min(layers_data$year, na.rm=T), max(layers_data$year, na.rm=T)-10), 
              Sustainability=1.0){
  
  # cast data
  layers_data = SelectLayersData(layers, targets='AO')
  
  ry = rename(dcast(layers_data, id_num + year ~ layer, value.var='val_num', 
                    subset = .(layer %in% c('rny_ao_need'))),
              c('id_num'='region_id', 'rny_ao_need'='need')); head(ry); summary(ry)
  
  r = na.omit(rename(dcast(layers_data, id_num ~ layer, value.var='val_num', 
                           subset = .(layer %in% c('rn_ao_access'))),
                     c('id_num'='region_id', 'rn_ao_access'='access'))); head(r); summary(r)
  
  ry = merge(ry, r); head(r); summary(r); dim(r)
  
  # model
  ry = within(ry,{
    Du = (1.0 - need) * (1.0 - access)
    status = ((1.0 - Du) * Sustainability) * 100    
  })
  
  # status
  r.status = subset(ry, year==year_max, c(region_id, status)); summary(r.status); dim(r.status)
  
  # trend
  r.trend = ddply(
    subset(ry, year >= year_min), .(region_id), summarize,      
    trend = 
      if(length(na.omit(status))>1) {
        # use only last valid 5 years worth of status data since year_min
        d = data.frame(status=status, year=year)[tail(which(!is.na(status)), 5),]
        lm(status ~ year, d)$coefficients[['year']] / 100
      } else {
        NA
      }); summary(r.trend); dim(r.trend)
  
  # return scores
  s.status = cbind(rename(r.status, c('status'='score')), data.frame('dimension'='status')); head(s.status)
  s.trend  = cbind(rename(r.trend , c('trend' ='score')), data.frame('dimension'='trend')); head(s.trend)
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='AO')); dlply(scores, .(dimension), summary)
  return(scores)  
}

NP = function(layers, 
              status_year=2008, 
              trend_years = list('corals'=2003:2007,'ornamentals'=2003:2007,'shells'=2003:2007,
                                 'fish_oil'=2004:2008,'seaweeds'=2004:2008,'sponges'=2004:2008)){
  # 2013: NP(status_year=2009, trend_years = list('corals'=2004:2008,'ornamentals'=2004:2008,'shells'=2004:2008, 'fish_oil'=2005:2009,'seaweeds'=2005:2009,'sponges'=2005:2009))
  # 2012: NP(status_year=2008, trend_years = list('corals'=2003:2007,'ornamentals'=2003:2007,'shells'=2003:2007, 'fish_oil'=2004:2008,'seaweeds'=2004:2008,'sponges'=2004:2008))
  
    
  # layers
  lyrs = list('rky' = c('rnky_np_harvest_relative'    = 'H'),
              'rk'  = c('rnk_np_sustainability_score' = 'S',
                        'rnk_np_weights_combo'        = 'w'),
              'r'   = c('rn_fis_status'               = 'fis_status'))
  lyr_names = sub('^\\w*\\.', '', names(unlist(lyrs))) 
  
  # cast data  
  D = SelectLayersData(layers, layers=lyr_names)
  rky = rename(dcast(D, id_num + category + year ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['rky']]))),
               c('id_num'='region_id', 'category'='product', lyrs[['rky']]))
  rk  = rename(dcast(D, id_num + category ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['rk']]))),
               c('id_num'='region_id', 'category'='product', lyrs[['rk']]))
  r   = rename(dcast(D, id_num ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['r']]))),
               c('id_num'='region_id', lyrs[['r']]))
  
  # turn rn_fis_status to S for fish_oil
  r$product = 'fish_oil'
  rk = merge(rk, r, all.x=T)
  rk$S[rk$product=='fish_oil'] = rk$fis_status[rk$product=='fish_oil']
  rk = rk[,names(rk)!='fis_status']
  
  # merge H with S & w
  rky = merge(rky, rk, all.x=T)
  summary(rky)
  
  # get status across products, per region and year
  rky$w = ifelse(is.na(rky$w), 0, rky$w)
  rky = na.omit(rky)
  ry = ddply(rky, .(region_id, year), summarize,
             status = sum(w * H * S) / sum(w) * 100); head(ry)
  r.status = subset(ry, year==status_year, c(region_id,status))
  
  # get trend per product based on product-specific trend_years
  rk.trend = rename(ddply(rky, .(region_id, product), function(x){
    lm(H * S ~ year, x[x$year %in% trend_years[[x$product[1]]],])$coefficients[['year']] * 5
  }), c('V1'='trend.k')); head(rk.trend); head(rk)
  
  # summarize trend per region
  rk.trend.w = na.omit(merge(rk.trend, rk)); summary(rk.trend.w)
  r.trend = ddply(rk.trend.w, .(region_id), summarize,
                  trend = min(1, max(-1, sum(w * trend.k) / sum(w))))
  
  # return scores
  s.status = cbind(rename(r.status, c('status'='score')), data.frame('dimension'='status'))
  s.trend  = cbind(rename(r.trend , c('trend' ='score')), data.frame('dimension'='trend'))
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='NP'))
  return(scores)  
}

CS = function(layers){
  
  # layers
  lyrs = list('rk' = c('rnk_hab_health' = 'health',
                       'rnk_hab_extent' = 'extent',
                       'rnk_hab_trend'  = 'trend'))
  lyr_names = sub('(r|ry|rk)\\.','', names(unlist(lyrs)))
  
  # cast data
  D = SelectLayersData(layers, layers=lyr_names)
  rk = rename(dcast(D, id_num + category ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['rk']]))),
              c('id_num'='region_id', 'category'='habitat', lyrs[['rk']]))
  
  # limit to CS habitats
  rk = subset(rk, habitat %in% c('mangrove','saltmarsh','seagrass'))
  
  # BUG fix
  rk$extent[rk$extent==0] = NA
  
  # status
  r.status = ddply(na.omit(rk[,c('region_id','habitat','extent','health')]), .(region_id), summarize,
                   status = min(1, sum(extent * health) / sum(extent)) * 100)    
  
  # trend
  r.trend = ddply(na.omit(rk[,c('region_id','habitat','extent','trend')]), .(region_id), summarize,
                  trend = sum(extent * trend) / sum(extent) )

  # return scores
  s.status = cbind(rename(r.status, c('status'='score')), data.frame('dimension'='status'))
  s.trend  = cbind(rename(r.trend , c('trend' ='score')), data.frame('dimension'='trend'))
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='CS'))
  return(scores)  
}

CP = function(layers){
  
  # layers
  lyrs = list('rk' = c('rnk_hab_health' = 'health',
                       'rnk_hab_extent' = 'extent',
                       'rnk_hab_trend'  = 'trend'))
  lyr_names = sub('(r|ry|rk)\\.','', names(unlist(lyrs)))
  
  # get layer data
  D = SelectLayersData(layers, layers=lyr_names)
  
  # for habitat extent do not use all mangrove, but sum of mangrove_offshore1km + mangrove_inland1km = mangrove to match with extent and trend
  m = dcast(D, layer + id_num ~ category, value.var='val_num', subset = .(layer=='rnk_hab_extent' & category %in% c('mangrove_inland1km','mangrove_offshore1km')))
  m$val_num = rowSums(m[,c('mangrove_inland1km','mangrove_offshore1km')], na.rm=T)
  m$category = as.factor('mangrove')
  d = subset(D, !(layer=='rnk_hab_extent' & category %in% c('mangrove','mangrove_inland1km','mangrove_offshore1km')))
  D = rbind.fill(m, d)
  
  # cast
  rk = rename(dcast(D, id_num + category ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['rk']]))),
              c('id_num'='region_id', 'category'='habitat', lyrs[['rk']]))
  
  # limit to CP habitats and add rank
  habitat.rank = c('coral'            = 4,
                   'mangrove'         = 4,
                   'saltmarsh'        = 3,
                   'seagrass'         = 1,
                   'seaice_shoreline' = 4)
  
  rk = subset(rk, habitat %in% names(habitat.rank))
  rk$rank = habitat.rank[as.character(rk$habitat)]
  
  # BUG fix
  rk$extent[rk$extent==0] = NA
  
  # status  
  r.status = ddply(na.omit(rk[,c('region_id','habitat','rank','extent','health')]), .(region_id), summarize,
                   status = min(1, sum(rank * health * extent) / (sum(extent) * max(rank)) ) * 100 )    
  
  # trend
  r.trend = ddply(na.omit(rk[,c('region_id','habitat','rank','extent','trend')]), .(region_id), summarize,
                  trend = sum(rank * trend * extent) / (sum(extent)* max(rank)) )
  
  # return scores
  s.status = cbind(rename(r.status, c('status'='score')), data.frame('dimension'='status'))
  s.trend  = cbind(rename(r.trend , c('trend' ='score')), data.frame('dimension'='trend'))
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='CP'))
  return(scores)  
}

TR = function(layers){
  
  # scores
  return(cbind(rename(SelectLayersData(layers, layers=c('rn_tr_status'='status','rn_tr_trend'='trend'), narrow=T),
                      c(id_num='region_id', layer='dimension', val_num='score')), 
               data.frame('goal'='TR')))
}

LIV = function(layers){
  
  # scores
  scores = rename(subset(SelectLayersData(layers, layers=c('rn_liveco_status'='status','rn_liveco_trend'='trend'), narrow=T),
                    category=='livelihood'),
             c(id_num='region_id', category='goal', layer='dimension', val_num='score'))
  scores$goal = 'LIV'
  return(scores)  
}

ECO = function(layers){

  # scores
  scores = rename(subset(SelectLayersData(layers, layers=c('rn_liveco_status'='status','rn_liveco_trend'='trend'), narrow=T),
                         category=='economy'),
                  c(id_num='region_id', category='goal', layer='dimension', val_num='score'))
  scores$goal = 'ECO'
  return(scores)
}

calc.LSP = function(ld.csv=layers_data.csv, 
                    status.csv = file.path(dir.results, sprintf('LSP_status_%s.csv', sfx.scenario)),
                    trend.csv  = file.path(dir.results, sprintf('LSP_trend_%s.csv' , sfx.scenario)), 
                    ref.pct.cmpa=30, ref.pct.cp=30, status_year=2012, trend_years=2005:2009, trend.method='default'){
  # Use Status of 2013 to again be as inclusive as possible of established protected areas, but presume a lag in exhaustive reporting of 3 years so use 2006-2010 for Trend.
  # 2012a: calc.LSP(status_year=2013, trend_years=2006:2010)
  # 2013a: calc.LSP(status_year=2009, trend_years=2002:2006)
    
  library(plyr)
  library(reshape2)
  
  lyrs = list('r'  = c('rn_rgn_area_inland1km'   = 'area_inland1km',
                       'rn_rgn_area_offshore3nm' = 'area_offshore3nm'),
              'ry' = c('rny_lsp_prot_area_offshore3nm' = 'cmpa',
                        'rny_lsp_prot_area_inland1km'   = 'cp'))              
  lyrs.names = sub('(r|ry)\\.','', names(unlist(lyrs)))
  
#   # DEBUG: load data
#     load_all('/usr/local/ohi/src/R/ohi'); config.check('~/ohi_tbx/scenarios/global_2012_nature/conf/config.R')
#     dimensions=c('LSP_status'='status','LSP_trend'='trend'); ref.pct.cmpa=30; ref.pct.cp=30; status_year=2009; trend_years=2002:2006
#     d = subset(read.csv(layers_data.csv, na.strings=''), layer %in% lyrs.names); head(d); table(d$layer)
#   
#   # DEBUG: compare with Nature 2012 database
#     require('RPostgreSQL')
#     dbi.pg = dbDriver("PostgreSQL") 
#     pg = dbConnect(dbi.pg, host='neptune.nceas.ucsb.edu', dbname='ohi_nature2012', user='bbest', password=pg.pass) # ASSUMES: pg.password manually set # dbDisconnect(pg)  
#   
#     status_region_year = dbGetQuery(pg, "SELECT * FROM global_sp.status_region_year"); head(status_region_year)
#     status_region      = dbGetQuery(pg, "SELECT * FROM global_sp.status_region"); head(status_region)
#     trend_region_year  = dbGetQuery(pg, "SELECT * FROM global_sp.trend_region_year ORDER BY component, id, metric, year"); head(trend_region_year)
#     trend_region       = dbGetQuery(pg, "SELECT * FROM global_sp.trend_region      ORDER BY component, id, metric")      ; head(trend_region)
#   # DEBUG: compare with Nature 2012 results
#    results_global_data = read.csv('/usr/local/ohi/src/model/global2012/doc/results_global_data.csv', na.strings='')
  
  # cast data ----
  d = subset(read.csv(ld.csv, na.strings=''), layer %in% lyrs.names)  
  r  = rename(dcast(d, id_num ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['r']]))),
              c('id_num'='region_id', lyrs[['r']]))
  ry = rename(dcast(d, id_num + year ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['ry']]))),
              c('id_num'='region_id', lyrs[['ry']]))
    
  # fill in time series from first year specific region_id up to max year for all regions and generate cumulative sum
  yr.max = max(ry$year)
  r.yrs = ddply(ry, .(region_id), function(x){
    data.frame(region_id=x$region_id[1],
               year=min(x$year):yr.max)
    })
  r.yrs = merge(r.yrs, ry, all.x=T)
  r.yrs$cp[is.na(r.yrs$cp)]     = 0
  r.yrs$cmpa[is.na(r.yrs$cmpa)] = 0
  r.yrs = within(r.yrs, {
    cp_cumsum    = ave(cp  , region_id, FUN=cumsum)
    cmpa_cumsum  = ave(cmpa, region_id, FUN=cumsum)
    pa_cumsum    = cp_cumsum + cmpa_cumsum
  })
  
  # get percent of total area that is protected for inland1km (cp) and offshore3nm (cmpa) per year
  # and calculate status score
  r.yrs = merge(r.yrs, r, all.x=T); head(r.yrs)
  r.yrs = within(r.yrs,{
    pct_cp    = pmin(cp_cumsum   / area_inland1km   * 100, 100)
    pct_cmpa  = pmin(cmpa_cumsum / area_offshore3nm * 100, 100)
    pct_pa    = pmin( (cp_cumsum + cmpa_cumsum) / (area_inland1km + area_offshore3nm) * 100, 100)
    status    = ( pmin(pct_cp / ref.pct.cp, 1) + pmin(pct_cmpa / ref.pct.cmpa, 1) ) / 2 * 100
  })
  
  # extract status based on specified year
  status = r.yrs[r.yrs$year==status_year, c('region_id','status')]
  write.csv(rename(status,
                   c('region_id'='region_id')), status.csv, row.names=F, na='')
  
  # calculate trend ----

  if (trend.method=='nature2012'){
    # calculate trend Nature 2012 WRONG way ----
    
    # get cumulative sum of total protected areas added within trend years
    t = subset(ry, year %in% trend_years)
    t$cp[is.na(t$cp)]     = 0
    t$cmpa[is.na(t$cmpa)] = 0
    t = within(t, {
      pa_delta_cum = ave(cp+cmpa, region_id, FUN=cumsum)
    }); head(t)
  
    # add all years
    r.yrs = ddply(ry, .(region_id), function(x){
      data.frame(region_id=x$region_id[1],
                 year=min(trend_years):max(trend_years))
    }); head(r.yrs)
    t = merge(r.yrs, t, all.x=T); head(t)
  
    # last obs carried forward per region
    library(zoo)
    t = within(t, {
      pa_delta_cum = ave(pa_delta_cum, region_id, function(x){ na.locf(x, fromLast=F, na.rm=F) })
    });
    t$pa_delta_cum[is.na(t$pa_delta_cum)] = 0
    head(t)
    
    # merge r to get coastal area and percent total delta
    t = merge(t, r, all.x=T)  
    t = within(t, {
      area_coastal     = area_inland1km + area_offshore3nm
      pa_delta_cum_pct = pa_delta_cum / area_coastal    
    })
    
    # calculate trend: OLD WRONG
    trend = ddply(t, .(region_id), summarize,
                  annual = lm(pa_delta_cum_pct ~ year)[['coefficients']][['year']],
                  trend = min(1, max(-1, 5 * annual)))
  } else if (trend.method=='default'){
    # calculate trend new RIGHT way ----  
    trend = ddply(subset(r.yrs, year %in% trend_years), .(region_id), summarize,
                  annual = lm(pct_pa ~ year)[['coefficients']][['year']],
                  trend = min(1, max(0, 5 * annual))) # HACK: capping trend to 0 since getting a few infinitessimally small negatives, eg 91  Crozet Islands	-6.15E-14 for LSP.t.12
  } else {
    stop(sprintf('trend.method=%s not supported in calc.LSP().', trend.method))
  }

  # return dimensions ----
  write.csv(rename(trend,
                   c('region_id'='region_id'))[,c('region_id','trend')], trend.csv, row.names=F, na='')
  
#   # DEBUG: compare 
#   ans = subset(results_global_data, goal.subgoal=='LSP', c(id, status, trend)); 
#   print(subset(ans, id %in% c(1,2,11,12)), row.names=F)  
#   #  id    status     trend
#   #   1 100.00000 0.3677242
#   #   2   1.24302 0.0000000
#   #  11  25.90764 0.0000663
#   #  12  69.26206 0.0387219  
}


ICO = function(layers){
  
  # layers
  lyrs = c('rnk_ico_spp_extinction_status' = 'risk_category',
           'rnk_ico_spp_popn_trend'        = 'popn_trend')
  
  # cast data ----
  layers_data = SelectLayersData(layers, layers=names(lyrs))  
  rk = rename(dcast(layers_data, id_num + category ~ layer, value.var='val_chr'),
              c('id_num'='region_id', 'category'='sciname', lyrs))
  
  # lookup for weights status
  w.risk_category = c('LC' = 0,
                      'NT' = 0.2,
                      'VU' = 0.4,
                      'EN' = 0.6,
                      'CR' = 0.8,
                      'EX' = 1)
  
  # lookup for population trend
  w.popn_trend = c('Decreasing' = -0.5,
                   'Stable'     =  0,                                           
                   'Increasing' =  0.5)
  
  # status
  r.status = rename(ddply(rk, .(region_id), function(x){ 
                          round(mean(1 - w.risk_category[x$risk_category], na.rm=T) * 100, 2) }), 
                    c('V1'='score'))
  
  # trend
  r.trend = rename(ddply(rk, .(region_id), function(x){ 
                        round(mean(w.popn_trend[x$popn_trend], na.rm=T), 2) }), 
                    c('V1'='score'))

  # return scores
  s.status = cbind(r.status, data.frame('dimension'='status'))
  s.trend  = cbind(r.trend , data.frame('dimension'='trend' ))
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='ICO'))
  return(scores)  
  
}

calc.CW = function(ld.csv=layers_data.csv, 
                   status.csv = file.path(dir.results, sprintf('CW_status_%s.csv', sfx.scenario)),
                   trend.csv  = file.path(dir.results, sprintf('CW_trend_%s.csv' , sfx.scenario)),
                   verbose=F){
  # layers
  lyrs = list('r' = c('po_pathogens' = 'a',
                      'po_nutrients' = 'u',
                      'po_chemicals' = 'l',
                      'po_trash'     = 'd',
                      'rn_cw_pesticide_trend'   = 'pest_trend',
                      'rn_cw_fertilizer_trend'  = 'fert_trend',
                      'rn_cw_coastalpopn_trend' = 'popn_trend',
                      'rn_cw_pathogen_trend'    = 'path_trend'))
  layers = sub('(r|ry|rk)\\.','', names(unlist(lyrs)))
  
  # cast data
  D = subset(read.csv(ld.csv, na.strings=''), layer %in% layers)  
  r = rename(dcast(D, id_num ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['r']]))),
              c('id_num'='region_id', lyrs[['r']])); head(r); summary(r)
  
  # invert pressures
  r$a = 1 - r$a
  r$u = 1 - r$u
  r$l = 1 - r$l
  r$d = 1 - r$d
  
  # invert trends for CW
  r$popn_trend = -1 * r$popn_trend
  r$path_trend = -1 * r$path_trend
  
  # status
  r$status = psych::geometric.mean(t(r[,c('a','u','l','d')]), na.rm=T) * 100
  write.csv(r[,c('region_id','status')], status.csv, row.names=F, na='')
  
  # trend
  r$trend = rowMeans(r[,c('pest_trend','fert_trend','popn_trend','path_trend')], na.rm=T)
  write.csv(r[,c('region_id','trend')], trend.csv, row.names=F, na='')  
  
  if (verbose==T) {
    debug.csv = file.path(dir.results, sprintf('debug_CW_%s.csv',sfx.scenario))
    cat(sprintf('\noutputting debug file to: %s/%s\n', getwd(), debug.csv))
    write.csv(r, debug.csv, row.names=F, na='')
    cat(sprintf('\nSummary for: %s,%s\n',basename(status.csv),basename(trend.csv)))
    print(summary(r))
  }
}

calc.SPP = function(ld.csv=layers_data.csv,
                    status.csv = file.path(dir.results, sprintf('SPP_status_%s.csv', sfx.scenario)),
                    trend.csv  = file.path(dir.results, sprintf('SPP_trend_%s.csv' , sfx.scenario))){
  
  d = rename(subset(read.csv(ld.csv, na.strings=''), layer=='rn_spp_status'),
             c('id_num'='region_id','val_num'='status'))[,c('region_id','status')]
  d$status = d$status * 100
  write.csv(d, status.csv, row.names=F, na='')  
  
  write.csv(rename(subset(read.csv(ld.csv, na.strings=''), layer=='rn_spp_trend'),
                   c('id_num'='region_id','val_num'='trend'))[,c('region_id','trend')], trend.csv, row.names=F, na='')  
  
}



calc.HAB = function(ld.csv=layers_data.csv, 
                    status.csv = file.path(dir.results, sprintf('HAB_status_%s.csv', sfx.scenario)),
                    trend.csv  = file.path(dir.results, sprintf('HAB_trend_%s.csv' , sfx.scenario))){
  
  # layers
  lyrs = list('rk' = c('rnk_hab_health' = 'health',
                       'rnk_hab_extent' = 'extent',
                       'rnk_hab_trend'  = 'trend'))
  layers = sub('(r|ry|rk)\\.','', names(unlist(lyrs)))
  
  # cast data  
  D = subset(read.csv(ld.csv, na.strings=''), layer %in% layers)
  
  rk = rename(dcast(D, id_num + category ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['rk']]))),
              c('id_num'='region_id', 'category'='habitat', lyrs[['rk']])); head(rk); summary(rk)
  
  # limit to HAB habitats
  rk = subset(rk, habitat %in% c('coral','mangrove','saltmarsh','seaice_edge','seagrass','soft_bottom'))  
  
  # presence as weight
  rk$w = ifelse(!is.na(rk$extent) & rk$extent>0, 1, NA)
  
  # status
  r.status = ddply(na.omit(rk[,c('region_id','habitat','w','health')]), .(region_id), summarize,
                   status = min(1, sum(w * health) / sum(w)) * 100); summary(r.status)
  write.csv(r.status, status.csv, row.names=F, na='')
  
  # trend
  r.trend = ddply(na.omit(rk[,c('region_id','habitat','w','trend')]), .(region_id), summarize,
                  trend = sum(w * trend) / sum(w))
  write.csv(r.trend, trend.csv, row.names=F, na='')  
}