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

MAR = function(layers, status_years=2005:2011){  
  # status_years = 2005:2011
  
  # layers
  wd = '/Volumes/data_edit/model/GL-NCEAS_MAR_v2013a/Revision_Dec202013'
  setwd(wd)
  harvest_tonnes       = read.csv('mar_harvest_tonnes_lyr.csv')
  harvest_species      = read.csv('mar_harvest_species_lyr.csv')
  sustainability_score = read.csv('mar_sustainability_score_lyr.csv')
  popn_inland25mi      = read.csv('rgn_popsum2005to2015_inland25mi.csv')
  trend_years          = read.csv('mar_trend_years_lyr.csv')
  
  # merge and cast harvest with sustainability
  #harvest_species$species = as.character(harvest_species$species)
  rky = dcast(merge(merge(harvest_tonnes, 
                          harvest_species, all.x=TRUE, by=c('species_code')),
                    sustainability_score, all.x=TRUE, by=c('rgn_id', 'species')),
              rgn_id + species + species_code + sust_coeff ~ year, value.var='tonnes', mean, na.rm=T); head(rky)
  
  # smooth each species-country time-series using a running mean with 4-year window, excluding NAs from the 4-year mean calculation
  yrs_smooth <- names(rky)[!names(rky) %in% c('rgn_id','species','species_code','sust_coeff')]
  rky_smooth = zoo::rollapply(t(rky[,yrs_smooth]), 4, mean, na.rm = TRUE, partial=T) 
  rownames(rky_smooth) = as.character(yrs_smooth)
  rky_smooth = t(rky_smooth)
  rky = as.data.frame(cbind(rky[, c('rgn_id','species','species_code','sust_coeff')], rky_smooth)); head(rky)
  
  # melt
  m = melt(rky,
           id=c('rgn_id', 'species', 'species_code', 'sust_coeff'),
           variable.name='year', value.name='sm_tonnes'); head(m)
  
  # for each species-country-year, smooth mariculture harvest times the sustainability coefficient
  m = within(m, {
    sust_tonnes = sust_coeff * sm_tonnes
    year        = as.numeric(as.character(m$year))
  })
  
  # merge the MAR and coastal human population data
  m = merge(m, popn_inland25mi, by=c('rgn_id','year'), all.x=T)
  
  # must first aggregate all weighted timeseries per region, before dividing by total population
  ry = ddply(m, .(rgn_id, year, popsum), summarize, 
             sust_tonnes_sum = sum(sust_tonnes),
             mar_pop         = sum(sust_tonnes) / popsum[1])
  
  # get reference quantile based on argument years
  ref_95pct = quantile(subset(ry, year %in% status_years, mar_pop, drop=T), 0.95, na.rm=T)
  
  ry = within(ry, {
    status = ifelse(mar_pop / ref_95pct > 1, 
                    1,
                    mar_pop / ref_95pct)})
  status <- subset(ry, year == max(status_years), c('rgn_id', 'status'))
  status$status <- round(status$status*100, 2)
  
  # get list where trend is only to be calculated up to second-to-last-year
  # species where the last year of the time-series was 2010, and the same value was copied over to 2011
  # i.e. it was gapfilled using the previous year
  
  # get MAR trend
  ry = merge(ry, trend_years, all.x=T)
  yr_max = max(status_years)
  trend = ddply(ry, .(rgn_id), function(x){  # x = subset(ry, rgn_id==5)
    yrs = ifelse(x$trend_yrs=='4_yr',
                 (yr_max-5):(yr_max-1), # 4_yr
                 (yr_max-5):(yr_max))   # 5_yr
    y = subset(x, year %in% yrs)
    return(data.frame(
      trend = round(min(lm(status ~ year, data=y)$coefficients[['year']] * 5,1), 2)))  
    })

  # return scores
  scores = rbind(
    within(status, { 
      region_id = rgn_id
      score     = status
      dimension = 'status'})[,c('region_id','dimension','score')],
    within(trend, { 
      region_id = rgn_id
      score     = trend
      dimension = 'trend'})[,c('region_id','dimension','score')])
  return(scores)
}

FP = function(layers, scores){
  # weights
  w = rename(SelectLayersData(layers, layers='rn_fp_wildcaught_weight', narrow=T),
             c('id_num'='region_id', 'val_num'='w_FIS')); head(w)
  
  # scores
  s = dcast(scores, region_id + dimension ~ goal, value.var='score', subset=.(goal %in% c('FIS','MAR') & !dimension %in% c('pressures','resilience'))); head(s)
  
  # combine
  d = merge(s, w)
  d$w_MAR = 1 - d$w_FIS
  d$score = apply(d[,c('FIS','MAR','w_FIS', 'w_MAR')], 1, function(x){ weighted.mean(x[1:2], x[3:4]) })
  d$goal = 'FP'
  
  # return all scores
  return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
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
      }); # summary(r.trend); summary(subset(scores_www, goal=='AO' & dimension=='trend'))
  
  # return scores
  #browser()
  s.status = cbind(rename(r.status, c('status'='score')), data.frame('dimension'='status')); head(s.status)
  s.trend  = cbind(rename(r.trend , c('trend' ='score')), data.frame('dimension'='trend')); head(s.trend)
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='AO')); dlply(scores, .(dimension), summary)
  return(scores)  
}


NP = function(layers, 
              status_year=2008, 
              trend_years = list('corals'=2003:2007,'ornamentals'=2003:2007,'shells'=2003:2007,
                                 'fish_oil'=2004:2008,'seaweeds'=2004:2008,'sponges'=2004:2008)){
  # 2013: NP(layers, status_year=2009, trend_years = list('corals'=2004:2008,'ornamentals'=2004:2008,'shells'=2004:2008, 'fish_oil'=2005:2009,'seaweeds'=2005:2009,'sponges'=2005:2009))
  # 2012: NP(layers, status_year=2008, trend_years = list('corals'=2003:2007,'ornamentals'=2003:2007,'shells'=2003:2007, 'fish_oil'=2004:2008,'seaweeds'=2004:2008,'sponges'=2004:2008))
    
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
  
  # get status across products, per region and year
  rky$w = ifelse(is.na(rky$w), 0, rky$w)
  rky = na.omit(rky)
  ry = ddply(rky, .(region_id, year), summarize,
             status = sum(w * H * S) / sum(w) * 100); head(ry)
  r.status = subset(ry, year==status_year, c(region_id,status))
  
  # get trend per product based on product-specific trend_years
  rk.trend = rename(ddply(rky, .(region_id, product), function(x){
    lm(H * S ~ year, x[x$year %in% trend_years[[as.character(x$product[1])]],])$coefficients[['year']] * 5
  }), c('V1'='trend.k')); head(rk.trend)  
  
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
  lyr_names = sub('^\\w*\\.','', names(unlist(lyrs)))  
  
  # cast data
  D = SelectLayersData(layers, layers=lyr_names)
  rk = rename(dcast(D, id_num + category ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['rk']]))),
              c('id_num'='region_id', 'category'='habitat', lyrs[['rk']]))
  
  # limit to CS habitats
  rk = subset(rk, habitat %in% c('mangrove','saltmarsh','seagrass'))
  
  # assign extent of 0 as NA
  rk$extent[rk$extent==0] = NA
  
  # status
  r.status = ddply(na.omit(rk[,c('region_id','habitat','extent','health')]), .(region_id), summarize,
                   goal = 'CS',
                   dimension = 'status',
                   score = min(1, sum(extent * health) / sum(extent)) * 100)    
  
  # trend
  r.trend = ddply(na.omit(rk[,c('region_id','habitat','extent','trend')]), .(region_id), summarize,
                  goal = 'CS',
                  dimension = 'trend',
                  score = sum(extent * trend) / sum(extent) )

  # return scores
  scores = cbind(rbind(r.status, r.trend))
  return(scores)  
}


CP = function(layers){
  
  # layers
  lyrs = list('rk' = c('rnk_hab_health' = 'health',
                       'rnk_hab_extent' = 'extent',
                       'rnk_hab_trend'  = 'trend'))
  lyr_names = sub('^\\w*\\.','', names(unlist(lyrs)))
  
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
  
  # assign extent of 0 as NA
  rk$extent[rk$extent==0] = NA
  
  # status  
  r.status = ddply(na.omit(rk[,c('region_id','habitat','rank','extent','health')]), .(region_id), summarize,
                   goal = 'CP',
                   dimension = 'status',
                   score = min(1, sum(rank * health * extent) / (sum(extent) * max(rank)) ) * 100 )    
  
  # trend
  r.trend = ddply(na.omit(rk[,c('region_id','habitat','rank','extent','trend')]), .(region_id), summarize,
                  goal = 'CP',
                  dimension = 'trend',
                  score = sum(rank * trend * extent) / (sum(extent)* max(rank)) )
  
  # return scores
  return(rbind(r.status, r.trend))  
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
  scores = mutate(scores, 
                  score = ifelse(dimension=='status', score * 100, score),
                  goal  = 'LIV')
  return(scores)  
}


ECO = function(layers){

  # scores
  scores = rename(subset(SelectLayersData(layers, layers=c('rn_liveco_status'='status','rn_liveco_trend'='trend'), narrow=T),
                         category=='economy'),
                  c(id_num='region_id', category='goal', layer='dimension', val_num='score'))
  scores = mutate(scores, 
                  score = ifelse(dimension=='status', score * 100, score),
                  goal  = 'ECO')
  return(scores)
}

LE = function(scores, layers){
  
  # calculate LE scores
  scores.LE = within(dcast(scores, 
                        region_id + dimension ~ goal, value.var='score', 
                        subset=.(goal %in% c('LIV','ECO') & !dimension %in% c('pressures','resilience'))), {
    goal = 'LE'
    score = rowMeans(cbind(ECO, LIV), na.rm=T)
  })
  scores = rbind(scores, scores.LE[c('region_id','goal','dimension','score')])
  
  # LIV, ECO and LE: nullify unpopulated regions and those of the Southern Ocean Islands
  r_s_islands   = subset(SelectLayersData(layers, layers='rnk_rgn_georegions', narrow=T), 
                         category=='r2' & val_num==999, id_num, drop=T)
  r_unpopulated = subset(ddply(SelectLayersData(layers, layers='rny_le_popn', narrow=T), .(id_num), summarize, 
                               count = val_num[which.max(year)]),
                         is.na(count) | count==0, id_num, drop=T)
  scores[with(scores, 
              goal %in% c('LIV','ECO','LE') & 
                !dimension %in% c('pressures','resilience') & 
                region_id %in% union(r_s_islands, r_unpopulated)),
         'score'] = NA
  
  # return scores
  return(scores)  
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
    mean(1 - w.risk_category[x$risk_category], na.rm=T) * 100 }), 
                    c('V1'='score'))
  
  # trend
  r.trend = rename(ddply(rk, .(region_id), function(x){ 
    mean(w.popn_trend[x$popn_trend], na.rm=T) }), 
                   c('V1'='score'))
  
  # return scores
  s.status = cbind(r.status, data.frame('dimension'='status'))
  s.trend  = cbind(r.trend , data.frame('dimension'='trend' ))
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='ICO'))
  return(scores)  
  
}

LSP = function(layers, ref_pct_cmpa=30, ref_pct_cp=30, status_year=2012, trend_years=2005:2009){
  # 2013: LSP(layers, status_year=2013, trend_years=2006:2010)
  # 2012: LSP(layers, status_year=2009, trend_years=2002:2006)
    
  lyrs = list('r'  = c('rn_rgn_area_inland1km'   = 'area_inland1km',
                       'rn_rgn_area_offshore3nm' = 'area_offshore3nm'),
              'ry' = c('rny_lsp_prot_area_offshore3nm' = 'cmpa',
                        'rny_lsp_prot_area_inland1km'   = 'cp'))              
  lyr_names = sub('^\\w*\\.','', names(unlist(lyrs)))
  
  # cast data ----
  d = SelectLayersData(layers, layers=lyr_names)  
  r  = rename(dcast(d, id_num ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['r']]))),
              c('id_num'='region_id', lyrs[['r']]))
  ry = rename(dcast(d, id_num + year ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['ry']]))),
              c('id_num'='region_id', lyrs[['ry']]))
    
  # fill in time series from first year specific region_id up to max year for all regions and generate cumulative sum
  yr.max = max(max(ry$year), status_year)
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
    status    = ( pmin(pct_cp / ref_pct_cp, 1) + pmin(pct_cmpa / ref_pct_cmpa, 1) ) / 2 * 100
  })
  
  # extract status based on specified year
  r.status = r.yrs[r.yrs$year==status_year, c('region_id','status')]; head(r.status)
  
  # calculate trend
  r.trend = ddply(subset(r.yrs, year %in% trend_years), .(region_id), summarize,
                  annual = lm(pct_pa ~ year)[['coefficients']][['year']],
                  trend = min(1, max(0, 5 * annual)))
  
  # return scores
  scores = rbind.fill(
    within(r.status, {
      goal      = 'LSP'
      dimension = 'status'
      score     = status}),
    within(r.trend, {
      goal      = 'LSP'
      dimension = 'trend'
      score     = trend}))
  return(scores[,c('region_id','goal','dimension','score')])    
}

SP = function(scores){
  
  d = within(
    dcast(
      scores, 
      region_id + dimension ~ goal, value.var='score', 
      subset=.(goal %in% c('ICO','LSP') & !dimension %in% c('pressures','resilience')))
    , {
      goal = 'SP'
      score = rowMeans(cbind(ICO, LSP), na.rm=T)})
  
  
  # return all scores
  return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}


CW = function(layers){
  
  # layers
  lyrs = c('po_pathogens' = 'a',
           'po_nutrients' = 'u',
           'po_chemicals' = 'l',
           'po_trash'     = 'd',
           'rn_cw_pesticide_trend'   = 'pest_trend',
           'rn_cw_fertilizer_trend'  = 'fert_trend',
           'rn_cw_coastalpopn_trend' = 'popn_trend',
           'rn_cw_pathogen_trend'    = 'path_trend')
  
  # cast data
  d = SelectLayersData(layers, layers=names(lyrs))  
  r = rename(dcast(d, id_num ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs))),
              c('id_num'='region_id', lyrs)); head(r); summary(r)
  
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
  
  # trend
  r$trend = rowMeans(r[,c('pest_trend','fert_trend','popn_trend','path_trend')], na.rm=T)
  
  # return scores
  scores = rbind(
    within(r, {
      goal      = 'CW'
      dimension = 'status'
      score     = status}),
    within(r, {
      goal      = 'CW'
      dimension = 'trend'
      score     = trend}))[,c('region_id','goal','dimension','score')]
  return(scores)  
}


HAB = function(layers){
  
  # layers
  lyrs = c('rnk_hab_health' = 'health',
           'rnk_hab_extent' = 'extent',
           'rnk_hab_trend'  = 'trend')
  
  # cast data
  d = SelectLayersData(layers, layers=names(lyrs))  
  rk = rename(dcast(d, id_num + category ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs))),
              c('id_num'='region_id', 'category'='habitat', lyrs))
  
  # limit to HAB habitats
  rk = subset(rk, habitat %in% c('coral','mangrove','saltmarsh','seaice_edge','seagrass','soft_bottom'))  
  
  # presence as weight
  rk$w = ifelse(!is.na(rk$extent) & rk$extent>0, 1, NA)
  
  # status
  r.status = ddply(na.omit(rk[,c('region_id','habitat','w','health')]), .(region_id), summarize,
                   goal      = 'HAB',
                   dimension = 'status',
                   score     = min(1, sum(w * health) / sum(w)) * 100); summary(r.status)
  
  # trend
  r.trend = ddply(na.omit(rk[,c('region_id','habitat','w','trend')]), .(region_id), summarize,
                  goal      = 'HAB',
                  dimension = 'trend',
                  score     = sum(w * trend) / sum(w))
  
  # return scores
  scores = cbind(rbind(r.status, r.trend))
  return(scores)  
}


SPP = function(layers){

  # scores
  scores = cbind(rename(SelectLayersData(layers, layers=c('rn_spp_status'='status','rn_spp_trend'='trend'), narrow=T),
                      c(id_num='region_id', layer='dimension', val_num='score')), 
               data.frame('goal'='SPP'))
  scores = mutate(scores, score=ifelse(dimension=='status', score*100, score))
  return(scores) 
}

BD = function(scores){
  
  d = within(
    dcast(
      scores, 
      region_id + dimension ~ goal, value.var='score', 
      subset=.(goal %in% c('HAB','SPP') & !dimension %in% c('pressures','resilience'))), 
    {
      goal = 'BD'
      score = rowMeans(cbind(HAB, SPP), na.rm=T)})
  
  # return all scores
  return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}

PreGlobalScores = function(layers, conf, scores){
    
  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T)
  
  # limit to just desired regions and global (region_id==0)
  scores = subset(scores, region_id %in% c(rgns[,'id_num'], 0))
  
  # apply NA to Antarctica
  id_ant = subset(rgns, val_chr=='Antarctica', id_num, drop=T)
  scores[scores$region_id==id_ant, 'score'] = NA
    
  return(scores)
}

FinalizeScores = function(layers, conf, scores){
  
  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T)
    
  # add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       region_id = c(rgns[,'id_num'], 0),
                       dimension = c('pressures','resilience','status','trend','future','score'), 
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors=F); head(d)
  d = subset(d, 
             !(dimension %in% c('pressures','resilience','trend') & region_id==0) & 
             !(dimension %in% c('pressures','resilience','status','trend') & goal=='Index'))
  scores = merge(scores, d, all=T)[,c('goal','dimension','region_id','score')]
      
  # order
  scores = arrange(scores, goal, dimension, region_id)
  
  # round scores
  scores$score = round(scores$score, 2)
    
  return(scores)
}