# TODO: check for missing layers


calc.FIS.Nature2012 = function(d, dimensions=c('FIS.status'='status','FIS.trend'='trend'), status.year=2006, trend.years=2001:2006){
  
  # cast layers ----
  # since SQLite, the default db of sqldf, doesn't support crosstab queries, we use the reshape package to transpose data into columns
  # TODO: translate into prefixe values with setNames like MAR for readability
  d = merge(merge(merge(
    dcast(d, id_num + year ~ layer, value.var='value_num', na.rm=T,
          subset = .(layer %in% c('sny_fis_biomass'))),
    dcast(d, id_num ~ layer, value.var='value_num', na.rm=T,
          subset = .(layer %in% c('sn_fis_mmsy','sn_area_km2','sn_fis_taxacorrection','sn_region_id'))), # 'sn_fis_flags',
    all=T),
                  dcast(d, id_num ~ layer, value.var='value_chr', na.rm=T,
                        subset = .(layer %in% c('st_label','st_country_id'))),
                  all=T),
            dcast(d, id_num ~ category, value.var='value_chr', na.rm=T,
                  subset = .(layer %in% c('stk_fis_flag'))),
            all=T)
  
  # trim and rename columns for ease
  names(d) = sub('sny_fis_|sn_fis_|sn_|st_fis|st_', '', names(d))
  names(d) = sub('^id_num$' ,'saup_id'   , names(d))
  
  # calculate status ----  
  #browser(); names(d)
  
  # use short names for variables
  d = rename(d, c('taxacorrection'='tc',
                  'biomass'='bt'))
  d = within(d, {
    
    # override tc by sovereign country where flag set
    idx = na.to.F(!is.na(use_sovereign))  
    tc[idx] = tc[match(use_sovereign[idx], country_id)]
    
    # set reference mMSY
    mmsy_ref = 0.75 * mmsy
    
    # main FIS status equation
    x = (1 - bt / mmsy_ref ) * tc
    
    # set the reference point
    deltabt <- abs(mmsy_ref - bt)
    
    # reset distance to 0 for bt that are within a +/- 5% buffer
    deltabt[deltabt < 0.05 * mmsy] <- 0
    # TODO: FIX code to match Eq. S16:
    #deltabt[deltabt < 0.05 * mmsy_ref] <- 0
    
    # cap the distance to mMSYRef
    idx = na.to.F(deltabt > mmsy_ref)
    deltabt[idx] <- mmsy_ref[idx]
    
    # compute status model, rescale the distance to score
    status <- (1 - deltabt / mmsy_ref) * tc  
    
    # override unreliable where flag set
    idx = !is.na(unreliable)
    status[idx] = tc[idx] * 0.25  
    
    # exclude where flag set
    status[!is.na(exclude_all)] = NA 
  })
  
  # populate years for those missing, as when defined by flag
  d = rbind(
    merge(
      subset(d, is.na(year), setdiff(names(d),'year')), 
      data.frame(year=na.omit(unique(d$year)))),
    subset(d, !is.na(year)))
  
  # summarize to region by area, limiting to reference year and excluding any saup_id's without a matching region_id
  x.allyears = ddply(
    subset(d, !is.na(region_id)), .(region_id, year), summarize,
    status = sum(status * area_km2, na.rm=T) / sum(area_km2) * 100
    # TODO: update aggregation to region to exclude denominator area where status is NA using weighted.mean()
    #FIS.status = weighted.mean(status, area_km2, na.rm=T) * 100
  )
  x = subset(x.allyears, year %in% c(NA,status.year), c(region_id, status)) 
  
  # calculate trend ----
  y = ddply(
    subset(x.allyears, year %in% trend.years), .(region_id), summarize,
    trend = 
      if(length(na.omit(status))>1) { 
        lm(status ~ year)$coefficients[['year']] * 5 / 100
      } else {
        0
      })
  
  # return dimensions ----
  r = setNames(merge(x, y, all=T), c('region_id','status','trend'))
  
  # attach latex equations ----
  #   \frac{B_T}{B_T+\sum\limits_{k} Y_k}
  
  #   (1 + \delta)^{-1} [ 1 + \beta T_i + (1 - \beta) (r_i - p_i) ] x_i
  #   (x_i + \hat{x}_{i,F})/2
  #   \sum_{i=1}^{N}\alpha_i I_i
  
  return(r[,c('region_id',dimensions)])
}

calc.MAR.Nature2012 = function(d, dimensions=c('MAR.status'='status','MAR.trend'='trend'), 
                               status.year=2009, trend.years=2001:2006){
  
  # cast layers ----
  cnky = setNames(dcast(d, id_chr + category + year ~ layer, value.var='value_num', na.rm=T,
                        subset = .(layer %in% c('cnky_mar_species_yield_smoothed'))),
                  c('country_id','species','year','Y_k'))
  cnk = setNames(dcast(d, id_chr + category ~ layer, value.var='value_num', na.rm=T,
                       subset = .(layer %in% c('cnk_mar_species_sustainability'))),
                 c('country_id','species','S_k'))
  tk = setNames(dcast(d, category ~ layer, value.var='value_chr', na.rm=T,
                      subset = .(layer %in% c('tk_mar_species_flags'))),
                c('species','flag'))
  cn = setNames(dcast(d, id_chr ~ layer, value.var='value_num', na.rm=T,
                      subset = .(layer %in% c('cn_cntry_rgn'))),
                c('country_id','region_id'))
  rn = setNames(dcast(d, id_num ~ layer, value.var='value_num', na.rm=T,
                      subset = .(layer %in% c('rn_rgn_area_offshore3nm'))),
                c('region_id','A_r'))
  
  # calculate status ----  
  
  # summarize by country
  cny = sqldf(
    "SELECT country_id, year, 
    SUM(Y_k * S_k) AS YS_c, 
    COUNT(*) AS n
    FROM    cnky
    JOIN    cnk USING (country_id,species)
    LEFT JOIN (
    SELECT  species 
    FROM    tk 
    WHERE   flag = 'remove'
    ) skip USING (species)
    WHERE skip.species IS NULL
    GROUP BY country_id, year
    ORDER BY country_id, year")
  
  # summarize by region
  rny = sqldf("SELECT region_id, year, A_r,
    SUM(YS_c) AS yield,
    SUM(YS_c)/A_r AS Y_c,
    LOG((SUM(YS_c) / A_r) + 1) AS status_raw
    FROM  cny
    JOIN  cn USING (country_id)
    JOIN  rn USING (region_id)
    GROUP BY region_id, year, A_r
    ORDER BY region_id, year")
  
  # rescale status
  status.max = max(rny[,'status_raw'])  
  rny[['status']] = rny[['status_raw']]/status.max * 100
  
  d.status = subset(rny, year==status.year, c(region_id, status)) 
  
  # calculate trend ----
  d.trend = ddply(
    subset(rny, year %in% trend.years), .(region_id), summarize,
    trend = 
      if(length(na.omit(status))>1) { 
        lm(status ~ year)$coefficients[['year']] * 5 / 100
      } else {
        0
      })
  
  # return dimensions ----
  r = setNames(merge(d.status, d.trend, all=T), c('region_id','status','trend'))
  return(setNames(r[,c('region_id',dimensions)], c('region_id', names(dimensions))))
}

calc.SPP = function(ld.csv=layers_data.csv, 
                    status.csv = file.path(dir.results, sprintf('SPP_status_%s.csv', sfx.scenario)),
                    trend.csv  = file.path(dir.results, sprintf('SPP_trend_%s.csv' , sfx.scenario))){
  
  d = rename(subset(read.csv(ld.csv, na.strings=''), layer=='rn_spp_status'),
             c('id_num'='rgn_id','value_num'='status'))[,c('rgn_id','status')]
  d$status = d$status * 100
  write.csv(d, status.csv, row.names=F, na='')  
  
  write.csv(rename(subset(read.csv(ld.csv, na.strings=''), layer=='rn_spp_trend'),
                   c('id_num'='rgn_id','value_num'='trend'))[,c('rgn_id','trend')], trend.csv, row.names=F, na='')  
  
}

calc.FIS = function(ld.csv=layers_data.csv, 
                    status.csv = file.path(dir.results, sprintf('FIS_status_%s.csv', sfx.scenario)),
                    trend.csv  = file.path(dir.results, sprintf('FIS_trend_%s.csv' , sfx.scenario))){
  
  d = rename(subset(read.csv(ld.csv, na.strings=''), layer=='rn_fis_status'),
         c('id_num'='rgn_id','value_num'='status'))[,c('rgn_id','status')]
  d$status = d$status * 100
  write.csv(d, status.csv, row.names=F, na='')  

  write.csv(rename(subset(read.csv(ld.csv, na.strings=''), layer=='rn_fis_trend'),
                   c('id_num'='rgn_id','value_num'='trend'))[,c('rgn_id','trend')], trend.csv, row.names=F, na='')  
  
}

calc.TR = function(ld.csv=layers_data.csv, 
                   status.csv = file.path(dir.results, sprintf('TR_status_%s.csv', sfx.scenario)),
                   trend.csv  = file.path(dir.results, sprintf('TR_trend_%s.csv' , sfx.scenario))){
  
  d = rename(subset(read.csv(ld.csv, na.strings=''), layer=='rn_tr_status'),
             c('id_num'='rgn_id','value_num'='status'))[,c('rgn_id','status')]
  write.csv(d, status.csv, row.names=F, na='')  
  
  write.csv(rename(subset(read.csv(ld.csv, na.strings=''), layer=='rn_tr_trend'),
                   c('id_num'='rgn_id','value_num'='trend'))[,c('rgn_id','trend')], trend.csv, row.names=F, na='')  
  
}

calc.MAR = function(ld.csv=layers_data.csv, 
                    status.csv = file.path(dir.results, sprintf('MAR_status_%s.csv', sfx.scenario)),
                    trend.csv  = file.path(dir.results, sprintf('MAR_trend_%s.csv' , sfx.scenario))){
  
  d = rename(subset(read.csv(ld.csv, na.strings=''), layer=='rn_mar_status'),
             c('id_num'='rgn_id','value_num'='status'))[,c('rgn_id','status')]
  d$status = d$status * 100
  write.csv(d, status.csv, row.names=F, na='')  
  
  write.csv(rename(subset(read.csv(ld.csv, na.strings=''), layer=='rn_mar_trend'),
                   c('id_num'='rgn_id','value_num'='trend'))[,c('rgn_id','trend')], trend.csv, row.names=F, na='')  
  
}

calc.FP = function(ld.csv=layers_data.csv, 
                   status.csv = file.path(dir.results, sprintf('FP_status_%s.csv', sfx.scenario)),
                   trend.csv  = file.path(dir.results, sprintf('FP_trend_%s.csv' , sfx.scenario))){
  # w is the weighting matrix per Eq.S21a
  # f is FIS: region_id, FIS_status, FIS_trend
  # m is MAR: region_id, MAR_status, MAR_trend
  #
  # attach latex equations
  #   w * x_FIS + (1-w)*x_MAR

  # layers
  lyrs = list('r'  = c('rn_fis_status'           = 'FIS_status',
                       'rn_fis_trend'            = 'FIS_trend',
                       'rn_mar_status'           = 'MAR_status',
                       'rn_mar_trend'            = 'MAR_trend',
                       'rn_fp_wildcaught_weight' = 'w'))  
  layers = sub('(r|ry|rk)\\.','', names(unlist(lyrs)))
    
  # cast data
  D = subset(read.csv(ld.csv, na.strings=''), layer %in% layers); table(D$layer)
  
  r = rename(dcast(D, id_num + category ~ layer, value.var='value_num', subset = .(layer %in% names(lyrs[['r']]))),
             c('id_num'='rgn_id', lyrs[['r']])); head(r); summary(r)
    
  # calculate status and trend
  r = within(r,{
    status = ifelse(is.na(FIS_status), 0, w * FIS_status) + ifelse(is.na(MAR_status), 0, (1 - w) * MAR_status)
    trend  = ifelse(is.na(FIS_trend) , 0, w * FIS_trend)  + ifelse(is.na(MAR_trend) , 0, (1 - w) * MAR_trend)
  })
  r$status = r$status * 100
  write.csv(r[,c('rgn_id','status')], status.csv, row.names=F, na='')
  write.csv(r[,c('rgn_id','trend' )], trend.csv , row.names=F, na='')
}

calc.AO = function(ld.csv=layers_data.csv, 
                   status.csv = file.path(dir.results, sprintf('AO_status_%s.csv', sfx.scenario)),
                   trend.csv  = file.path(dir.results, sprintf('AO_trend_%s.csv' , sfx.scenario)),
                   year.max=2012, year.min=2002, Sustainability=1.0){
    
  # TODO: gap fill
  
  # layers
  lyrs = list('r'  = c('rn_ao_access'        = 'access'),
              'ry' = c('rny_ao_need'         = 'need'))
  layers = sub('(r|ry|rk)\\.','', names(unlist(lyrs)))
  
  # cast data
  D = subset(read.csv(ld.csv, na.strings=''), layer %in% layers)
  
  ry = rename(dcast(D, id_num + year ~ layer, value.var='value_num', subset = .(layer %in% names(lyrs[['ry']]))),
              c('id_num'='rgn_id', lyrs[['ry']])); head(ry); summary(ry)

  r = na.omit(rename(dcast(D, id_num ~ layer, value.var='value_num', subset = .(layer %in% names(lyrs[['r']]))),
             c('id_num'='rgn_id', lyrs[['r']]))); head(r); summary(r)
  
  ry = merge(ry, r); head(r); summary(r); dim(r)

  # model
  ry = within(ry,{
    Du = (1.0 - need) * (1.0 - access)
    status = ((1.0 - Du) * Sustainability) * 100    
  })

  # status
  r.status = subset(ry, year==year.max, c(rgn_id, status)); summary(r.status); dim(r.status)
    
  #   # aggregate status to region, gap-filling by georegions
  #   d.status = aggregate_by_country(cn, col.value='status', col.country='country_id')
  write.csv(r.status, status.csv, row.names=F, na='')
  
  # trend
  r.trend = ddply(
    subset(ry, year >= year.min), .(rgn_id), summarize,      
    trend = 
      if(length(na.omit(status))>1) {
        # use only last valid 5 years worth of status data since year.min
        d = data.frame(status=status, year=year)[tail(which(!is.na(status)), 5),]
        lm(status ~ year, d)$coefficients[['year']] / 100
      } else {
        NA
      }); summary(r.trend); dim(r.trend)
  
  #   # aggregate trend to region, gap-filling by georegions
  #   d.trend = aggregate_by_country(cn.trend, col.value='trend', lyrs.dat.csv=layers_data.csv)
  write.csv(r.trend, trend.csv, row.names=F, na='')  
}

calc.NP.Nature2012 = function(d, dimensions=c('NP_status'='status','NP_trend'='trend'), status.year=2008, 
                   trend.years = list('corals'=2003:2007,'ornamentals'=2003:2007,'shells'=2003:2007,
                                      'fish_oil'=2004:2008,'seaweeds'=2004:2008,'sponges'=2004:2008)){
  # debug: d=layers_data_NP; dimensions=c('NP_status'='status','NP_trend'='trend'); status.year=2008; trend.years = list('corals'=2003:2007,'ornamentals'=2003:2007,'shells'=2003:2007,'fish_oil'=2004:2008,'seaweeds'=2004:2008,'sponges'=2004:2008)
  # TODO: rnk_np_weights/np_product_weights_byregion -> rnk_np_product_weight
  
  # cast data ----
  cky = setNames(dcast(d, id_chr + category + year ~ layer, value.var='value_num', na.rm=T,
                       subset = .(layer %in% c('cnky_np_harvest_relative','cnky_np_sustainability_score','cnky_np_weights_combo'))),
                 c('country_id','product','year','Hp','Sp','Wp')) 
  
  rk = setNames(dcast(d, id_num + category ~ layer, value.var='value_num', na.rm=T,
                      subset = .(layer %in% c('rnk_np_product_weight'))),
                c('region_id','product','w')) 
  
  # calculate status ----
  
  # calculate status per country, product, year
  cky = within(cky,{
    status = Hp * Sp
  })
  
  # populate status of 0 for missing years by country, product
  d = cky.beg.end = ddply(cky, .(country_id, product), summarize, n=length(year), year.min=min(year), year.max=max(year))
  cky.0 = cky[0,c('country_id','product','year')]
  for (i in 1:nrow(d)){
    yrs = d$year.min[i]:d$year.max[i]
    n = length(yrs)
    cky.0 = rbind(cky.0,
                  data.frame(country_id = rep(d$country_id[i], n),
                             product    = rep(d$product[i], n),
                             year       = yrs)) # , Hp=rep(0,n), Sp=rep(0,n), Wp=rep(0,n), status     = rep(0, n)
  }
  cky = merge(cky.0, cky, all=T)
  cky$status = ifelse(is.na(cky$status), 0, cky$status)
  cky$Wp = ifelse(is.na(cky$Wp), 0, cky$Wp)
    
  # add combination products based on Eq S27 (combo-without-corals not used)
  cky = sqldf(
    "SELECT country_id, product, year, status
    FROM cky
    UNION
    SELECT country_id, 'combo' AS product, year, 
    SUM(Wp * status) / SUM(Wp) * 100 AS status
    FROM cky
    GROUP BY country_id, year
    UNION
    SELECT country_id, 'combo-without-corals' AS product, year, 
    SUM(Wp * status) / SUM(Wp)  * 100 AS status
    FROM cky
    WHERE product NOT IN ('corals')
    GROUP BY country_id, year")  
  
  # aggregate status to region, gap-filling by georegions
  rky = ddply(cky, .(product), function(cy_k){
    x = aggregate_by_country_year(cy_k, col.value='status', col.country='country_id')
    return(x[attr(x,'source')=='actual',])
  })
  
  # get status
  d.status = subset(rky, year==status.year & product=='combo', c(region_id, status))
  
  trend.rk = function(region_id, product, year, status, trend.years){
    k = unique(product)     # product    
    yrs = trend.years[[k]]  # years specific to product
    idx = year %in% yrs     # index of years with status    
    d = data.frame(year=yrs, status=0, row.names=as.character(yrs)) # default all years to zero
    d[as.character(year[idx]),'status'] = status[idx]
    # TODO: shouldn't we rely on year.min & year.max above and not automatically fill in all years as zero?
    #   FIX:
    #     d = data.frame(year=year[idx], status=status[idx])   # subset data to trend years of given product
    m = lm(status ~ year, d)$coefficients[['year']] * 5  # get linear model coefficient for 5 year period    
    return(min(1, max(-1, m)))                           # clamp between -1 and 1    
  }
  
  # get trend for each product and region. NOTE: here() trick to get ddply to recognize function trend.rk()
  trends = ddply(subset(rky, product %in% names(trend.years)), .(region_id, product), here(summarize),
                 trend = trend.rk(region_id, product, year, status, trend.years))
  
  # add combination products based on Eq S27 (combo-without-corals not used)
  combo = sqldf(
    "SELECT region_id, product, w, trend 
    FROM trends
    JOIN rk USING (region_id, product)
    UNION
    SELECT region_id, 'combo' AS product, NULL AS w,
      SUM(w * trend) AS trend
    FROM trends
    JOIN rk USING (region_id, product)
    GROUP BY region_id, product
    UNION
    SELECT region_id, 'combo-without-corals' AS product, NULL AS w,  
      SUM(w * trend) AS trend
    FROM trends
    JOIN rk USING (region_id, product)
    WHERE product NOT IN ('corals')
    GROUP BY region_id, product
    ORDER BY region_id, product")
    
  # get trend
  d.trend = subset(combo, product=='combo', c(region_id, trend))  
  
  # return dimensions ----
  r = setNames(merge(d.status, d.trend, all=T), c('region_id','status','trend'))
  return(setNames(r[,c('region_id',dimensions)], c('region_id', names(dimensions))))
}

calc.NP = function(ld.csv=layers_data.csv, 
                   status.csv = file.path(dir.results, sprintf('NP_status_%s.csv', sfx.scenario)),
                   trend.csv  = file.path(dir.results, sprintf('NP_trend_%s.csv' , sfx.scenario)),
                   verbose=F,
                   status.year=2008, 
                   trend.years = list('corals'=2003:2007,'ornamentals'=2003:2007,'shells'=2003:2007,
                                      'fish_oil'=2004:2008,'seaweeds'=2004:2008,'sponges'=2004:2008)){
  # KL: if harvest is NA then it should be gapfilled with 0.
  # BH: The 2012a harvest file is already rescaled from 0 to 1, meaning that the "value" field is actually a score, not raw tonnage (the rescaling step is already done). The 2013a NP file prepared by Julie should be raw tonnage (i.e. not rescaled to the peak), unless BH processed it? I can't check myself because I don't know where the 2013a NP files are.
  # BH: The 'allyears' file is absolute values of harvest through 2009 (the year of assessment for 2013a) so that you can calculate peak harvest and then rescale to 1.0. The weights_combo file is already converted to proportional value and should be set to go.
  # BB: It looks like I need value in dollars per region and product in order to calculate the product weights (based on value_peak / total_value)
  #   calc.NP(status.year=2008, 
  #     trend.years = list('corals'=2003:2007,
  #                        'ornamentals'=2003:2007,
  #                        'shells'=2003:2007,
  #                        'fish_oil'=2004:2008,
  #                        'seaweeds'=2004:2008,
  #                        'sponges'=2004:2008))
  #  I presume that once we get the harvest data sorted, then we'll use the most recent year per product for 2013a, and minus one year for 2012a.
  #  np_harvest_v2012a.csv only goes up to 2008, yet has more rows (n=7190) of data than harvest GL-FAO-Commodities_v2009-cleaned.csv (n=7079) which goes up to 2009 or np_harvest_absolutevalue_all_yrs.xls which has the least (n=5203)
  # 2008 in np_weights_combo_2012a.csv, 2009 in np_weights_combo_2013a.csv)
  # calculate relative harvest from np_harvest_absolutevalue_all_yrs.xls to produce something akin to np_harvest_v2012a.csv for 2013
  # so for now just use the same 2012a weights for all years preceding
  
  #   For calculating this 2013 relative harvest, I see the peak and 35% discussed in the supplement:
  #   
  #   For the Status of each product, we assessed the most recent harvest rate (in metric tons) per country relative to the maximum value (in 2008 USD) ever achieved in that country, under the assumption that the maximum achieved at any point in time was likely the maximum possible. This creates a reference point internal to each country. We then established a buffer around this peak catch because we do not know whether it is sustainable (similar to what was done for the fisheries sub-goal of food provision, Section 6A1). Any value within 35% of the peak was set to 1.0, with values below that rescaled to this 35% buffer value. We chose a 35% buffer following the logic of the wild-caught fisheries sub-goal, where a 25% buffer is used around mMSY. Because mMSY for wild-caught fisheries is already 10% below peak landings, we added 10% to the Natural Products buffer as a coarse approximation of the buffer mMSY builds around peak landings.
  #   
  #   Just to confirm the methods I use for 2013 are consistent with 2012, here's some of the SQL:
  #   
  #   LEAST(1, (yield / (0.65 * p.yield_peak))) AS P_i
  #   
  #   The exposure and risk to get at sustainability is no walk in the park, but doable. The supplement suggests a log transform and many product-specific codes, part of which I see as:
  #   
  #   LN(exposure+1)/LN(exposure_max+1) AS exposure
  #   
  #   ...
  #   
  #   CASE  WHEN e.exposure IS NOT NULL AND r.risk IS NOT NULL
  #              THEN 1.0 - (0.5 * e.exposure + 0.5 * r.risk)
  #              WHEN e.exposure IS NULL AND r.risk IS NOT NULL
  #              THEN 1.0 - r.risk
  #              WHEN e.exposure IS NOT NULL AND r.risk IS NULL
  #              THEN 1.0 - e.exposure
  #          ELSE 1
  #        END AS s1
  # BH: for 2013a status we used the 35% buffer, so you find the peak, multiply by 65%, then penalize for above or below this value
  
  # layers
  lyrs = list('rky' = c('rnky_np_harvest_relative'    = 'H'),
              'rk'  = c('rnk_np_sustainability_score' = 'S',
                        'rnk_np_weights_combo'        = 'w'),
              'r'   = c('rn_fis_status'               = 'fis_status'))
  layers = sub('(r|ry|rk|rky)\\.','', names(unlist(lyrs)))
  
  # cast data  
  D = subset(read.csv(ld.csv, na.strings=''), layer %in% layers)
  rky = rename(dcast(D, id_num + category + year ~ layer, value.var='value_num', subset = .(layer %in% names(lyrs[['rky']]))),
               c('id_num'='rgn_id', 'category'='product', lyrs[['rky']])); head(rky); summary(rky)
  rk  = rename(dcast(D, id_num + category ~ layer, value.var='value_num', subset = .(layer %in% names(lyrs[['rk']]))),
               c('id_num'='rgn_id', 'category'='product', lyrs[['rk']])); head(rk); summary(rk); table(rk$product)
  r   = rename(dcast(D, id_num ~ layer, value.var='value_num', subset = .(layer %in% names(lyrs[['r']]))),
               c('id_num'='rgn_id', lyrs[['r']])); head(r); summary(r)
  
  # turn rn_fis_status to S for fish_oil
  r$product = 'fish_oil'
  rk = merge(rk, r, all.x=T)
  rk$S[rk$product=='fish_oil'] = rk$fis_status[rk$product=='fish_oil']
  rk = rk[,names(rk)!='fis_status']

  # merge H with S & w
  rky = merge(rky, rk, all.x=T)
  summary(rky)
  
  # missing S & w
  cat('NP summary(rk):\n')
  print(summary(rk))
  write.csv(rk, file.path(dir.results, sprintf('NP_debug_rk_%s.csv', sfx.scenario)), na='')

  # missing S wrt H
  cat('NP summary(rky):\n')
  print(summary(rky))
  write.csv(rk, file.path(dir.results, sprintf('NP_debug_rky_%s.csv', sfx.scenario)), na='')
  
  # get status across products, per region and year
  rky$w = ifelse(is.na(rky$w), 0, rky$w)
  rky = na.omit(rky)
  ry = ddply(rky, .(rgn_id, year), summarize,
             status = sum(w * H * S) / sum(w) * 100); head(ry)
  write.csv(na.omit(subset(ry, year==status.year, c(rgn_id,status))), status.csv, row.names=F, na='')
  
  # get trend per product based on product-specific trend.years
  rk.trend = rename(ddply(rky, .(rgn_id, product), function(x){
    lm(H * S ~ year, x[x$year %in% trend.years[[x$product[1]]],])$coefficients[['year']] * 5
    }), c('V1'='trend.k')); head(rk.trend); head(rk)
  
  # summarize trend per region
  rk.trend.w = na.omit(merge(rk.trend, rk)); summary(rk.trend.w)
  r.trend = ddply(rk.trend.w, .(rgn_id), summarize,
                          trend = min(1, max(-1, sum(w * trend.k) / sum(w))))
  write.csv(na.omit(r.trend), trend.csv, row.names=F, na='')
}

calc.HAB = function(ld.csv=layers_data.csv, 
                    status.csv = file.path(dir.results, sprintf('HAB_status_%s.csv', sfx.scenario)),
                    trend.csv  = file.path(dir.results, sprintf('HAB_trend_%s.csv' , sfx.scenario))){
  # TODO: check that mangroves were sampled inland too! (per 6D)
  
  # layers
  lyrs = list('rk' = c('rnk_hab_health' = 'health',
                       'rnk_hab_extent' = 'extent',
                       'rnk_hab_trend'  = 'trend'))
  layers = sub('(r|ry|rk)\\.','', names(unlist(lyrs)))
  
  # cast data  
  D = subset(read.csv(ld.csv, na.strings=''), layer %in% layers)
    
  rk = rename(dcast(D, id_num + category ~ layer, value.var='value_num', subset = .(layer %in% names(lyrs[['rk']]))),
              c('id_num'='rgn_id', 'category'='habitat', lyrs[['rk']])); head(rk); summary(rk)
  
#   # DEBUG
#   browser()
#   r = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/data/rgn_details.csv', na.strings='')[, c('rgn_id','rgn_nam')]; head(r)
#   e = na.omit(rk[,c('rgn_id','habitat','extent')]) ; head(e)
#   e$habitat = sprintf('%s.extent', e$habitat)
#   ew = dcast(e, rgn_id ~ habitat, value.var='extent'); head(ew)
#   h = na.omit(rk[,c('rgn_id','habitat','health')]) ; head(h)
#   h$habitat = sprintf('%s.health', h$habitat); head(h)
#   hw = dcast(h, rgn_id ~ habitat, value.var='health'); head(hw)
#   t = na.omit(rk[,c('rgn_id','habitat','trend')]); head(t)
#   t$habitat = sprintf('%s.trend', t$habitat)
#   tw = dcast(t, rgn_id ~ habitat, value.var='trend'); head(tw)  
#   
#   rk.HAB = subset(rk, habitat %in% c('coral','mangrove','seagrass','seaice_edge','soft_bottom'))
#   
#   rk.HAB$w.old = ifelse(!is.na(rk.HAB$extent), 1, 0) # BUG fix
#   rk.HAB$habitat.w.old = sprintf('%s.w.old', rk.HAB$habitat)
#   w.old = dcast(rk.HAB, rgn_id ~ habitat.w.old, value.var='w.old'); head(w.old)
#   
#   rk.HAB$w     = ifelse(!is.na(rk.HAB$extent) & rk.HAB$extent>0, 1, NA); head(rk.HAB)
#   rk.HAB$habitat.w = sprintf('%s.w', rk.HAB$habitat)
#   w = dcast(rk.HAB, rgn_id ~ habitat.w, value.var='w')
#   
# 
#   HAB.status.old = ddply(na.omit(rk.HAB[,c('rgn_id','habitat','w.old','health')]), .(rgn_id), summarize,
#                          HAB.status.old = min(1, sum(w.old * health) / sum(w.old)) * 100); summary(HAB.status.old); head(HAB.status.old)
#   
#   HAB.status = ddply(na.omit(rk.HAB[,c('rgn_id','habitat','w','health')]), .(rgn_id), summarize,
#                      HAB.status = min(1, sum(w * health) / sum(w)) * 100); summary(HAB.status); head(HAB.status)
#   
#   
#   HAB.trend.old = ddply(na.omit(rk.HAB[,c('rgn_id','habitat','w.old','trend')]), .(rgn_id), summarize,
#                   HAB.trend.old = sum(w * trend) / sum(w)); summary(HAB.trend.old); head(HAB.trend.old)  
#   
#   HAB.trend = ddply(na.omit(rk.HAB[,c('rgn_id','habitat','w','trend')]), .(rgn_id), summarize,
#                     HAB.trend = sum(w * trend) / sum(w)); summary(HAB.trend); head(HAB.trend)
#   
#   HAB.woldNA.not.trend = ddply(rk.HAB[,c('rgn_id','habitat','w.old','trend')], .(rgn_id), summarize,
#                                HAB.woldNA.not.trend = ifelse(sum(!is.na(trend)>0) &  sum(!is.na(w.old)==0), TRUE, NA))
#   nrow(subset(HAB.woldNA.not.trend, HAB.woldNA.not.trend==TRUE)) # 0
#   
#   # CS
#   rk.CS = subset(rk, habitat %in% c('mangrove','saltmarsh','seagrass'))
#   CS.status = ddply(na.omit(rk.CS[,c('rgn_id','habitat','extent','health')]), .(rgn_id), summarize,
#                     CS.status = min(1, sum(extent * health) / sum(extent)) * 100)    
#   CS.trend = ddply(na.omit(rk.CS[,c('rgn_id','habitat','extent','trend')]), .(rgn_id), summarize,
#                    CS.trend = sum(extent * trend) / sum(extent) )
#   CS.extentNA.not.trend = ddply(rk.CS[,c('rgn_id','habitat','extent','trend')], .(rgn_id), summarize,
#                                 CS.extentNA.not.trend = ifelse(sum(!is.na(trend)>0) &  sum(!is.na(extent)==0), TRUE, NA))
#   nrow(subset(CS.extentNA.not.trend, CS.extentNA.not.trend==TRUE))) # 13
#   
#   # CP
#   habitat.rank = c('coral'            = 4,
#                    'mangrove'         = 4,
#                    'saltmarsh'        = 3,
#                    'seagrass'         = 1,
#                    'seaice_shoreline' = 4)  
#   rk.CP = subset(rk, habitat %in% names(habitat.rank)); table(rk.CP$habitat)
#   rk.CP$rank = habitat.rank[rk.CP$habitat]; summary(rk.CP); head(rk.CP)
#   CP.status = ddply(na.omit(rk.CP[,c('rgn_id','habitat','rank','extent','health')]), .(rgn_id), summarize,
#                     CP.status = min(1, sum(rank * health * extent) / (sum(extent) * max(rank)) ) * 100 )    
#   CP.trend = ddply(na.omit(rk.CP[,c('rgn_id','habitat','rank','extent','trend')]), .(rgn_id), summarize,
#                    CP.trend = sum(rank * trend * extent) / (sum(extent)* max(rank)) )
#   CP.extentNA.not.trend = ddply(rk.CP[,c('rgn_id','habitat','rank','extent','trend')], .(rgn_id), summarize,
#                                 CP.extentNA.not.trend = ifelse(sum(!is.na(trend)>0) &  sum(!is.na(extent)==0), TRUE, NA))
#   nrow(subset(CP.extentNA.not.trend, CP.extentNA.not.trend==TRUE)) # 20
# 
#   z = merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(r, ew, all=T), w.old, all=T), w, all=T), hw, all=T), tw, all=T), 
#             HAB.status.old, all=T), HAB.status, all=T), HAB.trend.old, all=T), HAB.trend, all=T),
#             CS.status, all=T), CS.trend, all=T), CP.status, all=T), CP.trend, all=T),
#             HAB.woldNA.not.trend, all=T), CS.extentNA.not.trend, all=T), CP.extentNA.not.trend, all=T); head(z)  
#   write.csv(z, '/Volumes/local_edit/src/toolbox/scenarios/global_2013a/results/HAB_CS_CP_debug.csv', row.names=F, na='')
  
  # limit to HAB habitats
  rk = subset(rk, habitat %in% c('coral','mangrove','saltmarsh','seaice_edge','seagrass','soft_bottom'))  
  #browser()
  
  # DEBUG
#   r = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/data/rgn_details.csv', na.strings='')[, c('rgn_id','rgn_nam')]; head(r)
#   q = merge(subset(rk, habitat=='saltmarsh'), r); head(q)
#   print(q[,c('rgn_id','rgn_nam','habitat','extent','health','trend')], row.names=F)
  
  # presence as weight
  #rk$w = ifelse(!is.na(rk$extent), 1, 0) # BUG fix
  rk$w = ifelse(!is.na(rk$extent) & rk$extent>0, 1, NA)
    
  # status
  r.status = ddply(na.omit(rk[,c('rgn_id','habitat','w','health')]), .(rgn_id), summarize,
                   status = min(1, sum(w * health) / sum(w)) * 100); summary(r.status)
  write.csv(r.status, status.csv, row.names=F, na='')
  
  # trend
  r.trend = ddply(na.omit(rk[,c('rgn_id','habitat','w','trend')]), .(rgn_id), summarize,
                  trend = sum(w * trend) / sum(w))
  write.csv(r.trend, trend.csv, row.names=F, na='')  
}

calc.CS = function(ld.csv=layers_data.csv, 
                   status.csv = file.path(dir.results, sprintf('CS_status_%s.csv', sfx.scenario)),
                   trend.csv  = file.path(dir.results, sprintf('CS_trend_%s.csv' , sfx.scenario))){
  # TODO: check that mangroves were sampled inland too! (per 6D)
  
  # DEBUG browser()
  
  # layers
  lyrs = list('rk' = c('rnk_hab_health' = 'health',
                       'rnk_hab_extent' = 'extent',
                       'rnk_hab_trend'  = 'trend'))
  layers = sub('(r|ry|rk)\\.','', names(unlist(lyrs)))
  
  # cast data
  D = subset(read.csv(ld.csv, na.strings=''), layer %in% layers)
    
  rk = rename(dcast(D, id_num + category ~ layer, value.var='value_num', subset = .(layer %in% names(lyrs[['rk']]))),
              c('id_num'='rgn_id', 'category'='habitat', lyrs[['rk']])); head(rk); summary(rk)
    
  # limit to CS habitats
  rk = subset(rk, habitat %in% c('mangrove','saltmarsh','seagrass'))
  
  # BUG fix
  rk$extent[rk$extent==0] = NA
  
  # debug file
  #write.csv(rk, file.path(dir.results, sprintf('CS_debug_%s.csv', sfx.scenario)), na='', row.names=F)
  
  # status
  r.status = ddply(na.omit(rk[,c('rgn_id','habitat','extent','health')]), .(rgn_id), summarize,
                   status = min(1, sum(extent * health) / sum(extent)) * 100)    
  write.csv(r.status, status.csv, row.names=F, na='')
  
  # trend
  r.trend = ddply(na.omit(rk[,c('rgn_id','habitat','extent','trend')]), .(rgn_id), summarize,
                  trend = sum(extent * trend) / sum(extent) )
  write.csv(r.trend, trend.csv, row.names=F, na='')   
}

calc.CP = function(ld.csv=layers_data.csv, 
                   status.csv = file.path(dir.results, sprintf('CP_status_%s.csv', sfx.scenario)),
                   trend.csv  = file.path(dir.results, sprintf('CP_trend_%s.csv' , sfx.scenario))){
#                    status.csv = file.path(dir.results, sprintf('CP_status_%s_BUG-seaice_shore.csv', sfx.scenario)),
#                    trend.csv  = file.path(dir.results, sprintf('CP_trend_%s_BUG-seaice_shore.csv' , sfx.scenario))){
  
  # layers
  lyrs = list('rk' = c('rnk_hab_health' = 'health',
                       'rnk_hab_extent' = 'extent',
                       'rnk_hab_trend'  = 'trend'))
  layers = sub('(r|ry|rk)\\.','', names(unlist(lyrs)))
  
  # get layer data
  D = subset(read.csv(ld.csv, na.strings=''), layer %in% layers)
  
  # for habitat extent do not use all mangrove, but sum of mangrove_offshore1km + mangrove_inland1km = mangrove to match with extent and trend
  m = dcast(D, layer + id_num ~ category, value.var='value_num', subset = .(layer=='rnk_hab_extent' & category %in% c('mangrove_inland1km','mangrove_offshore1km'))); head(m); table(m$category)
  m$value_num = rowSums(m[,c('mangrove_inland1km','mangrove_offshore1km')], na.rm=T)
  m$category = 'mangrove'; head(m)
  d = subset(D, !(layer=='rnk_hab_extent' & category %in% c('mangrove','mangrove_inland1km','mangrove_offshore1km'))); table(subset(d, layer=='rnk_hab_extent', category)); table(subset(d, category=='mangrove', layer)); head(d)  
  D = rbind.fill(m, d)
  
  # cast
  rk = rename(dcast(D, id_num + category ~ layer, value.var='value_num', subset = .(layer %in% names(lyrs[['rk']]))),
              c('id_num'='rgn_id', 'category'='habitat', lyrs[['rk']])) #; table(rk$habitat); print(head(subset(rk, habitat=='mangrove')), row.names=F); head(rk)
  
  # limit to CP habitats and add rank
  habitat.rank = c('coral'            = 4,
                   'mangrove'         = 4,
                   'saltmarsh'        = 3,
                   'seagrass'         = 1,
                   'seaice_shoreline' = 4)
  
  rk = subset(rk, habitat %in% names(habitat.rank)); table(rk$habitat)
  rk$rank = habitat.rank[rk$habitat]; summary(rk); head(rk)

  # BUG fix
  rk$extent[rk$extent==0] = NA
  
  # status  
  r.status = ddply(na.omit(rk[,c('rgn_id','habitat','rank','extent','health')]), .(rgn_id), summarize,
                   status = min(1, sum(rank * health * extent) / (sum(extent) * max(rank)) ) * 100 )    
  write.csv(r.status, status.csv, row.names=F, na='')
  
  # trend
  r.trend = ddply(na.omit(rk[,c('rgn_id','habitat','rank','extent','trend')]), .(rgn_id), summarize,
                  trend = sum(rank * trend * extent) / (sum(extent)* max(rank)) )
  write.csv(r.trend, trend.csv, row.names=F, na='')
  
#   # compare with OLD when habitat.rank of 'seaice_shore' vs 'seaice_shoreline' (n=25)
#   s.bug = read.csv('CP_status_2013a_BUG-seaice_shore.csv')
#   s = read.csv('CP_status_2013a.csv')
#   t.bug = read.csv('CP_trend_2013a_BUG-seaice_shore.csv')
#   t = read.csv('CP_trend_2013a.csv')
#   s = merge(s, rename(s.bug, c('status'='status.bug')), all=T)
#   t = merge(t, rename(t.bug, c('trend'='trend.bug')), all=T)
#   r = merge(s, t, all=T)
#   r$status.dif = with(r, status - status.bug)
#   r$trend.dif = with(r, trend - trend.bug)
#   head(r)
#   write.csv(subset(r, abs(status.dif) > 0 | abs(trend.dif) > 0 ), 'CP_bug_dif.csv', row.names=F, na='')
  
}



calc.LIV.ECO.Nature2012copy = function(ld.csv=layers_data.csv, 
                        LIV.status.csv = file.path(dir.results, sprintf('LIV_status_%s.csv', sfx.scenario)),
                        LIV.trend.csv  = file.path(dir.results, sprintf('LIV_trend_%s.csv' , sfx.scenario)),
                        ECO.status.csv = file.path(dir.results, sprintf('ECO_status_%s.csv', sfx.scenario)),
                        ECO.trend.csv  = file.path(dir.results, sprintf('ECO_trend_%s.csv' , sfx.scenario))){
  
  # QUICK HACK to consume Nature 2012 results as a placeholder
  d = read.csv(file.path(root.data ,'model/GL-NCEAS-LayersDisaggregated_v2013a/data/rgn_results_2012n_long.csv'), na.strings=''); head(d)

  for (g in c('ECO','LIV')){ # g = 'LIV'
    # status
    status = rename(subset(d, goal==g & dimension=='s', c(rgn_id, value)),
                    c('value'='status')); head(status)
    write.csv(status, get(sprintf('%s.status.csv', g)), row.names=F, na='')
    
    # trend
    trend = rename(subset(d, goal==g & dimension=='t', c(rgn_id, value)),
                   c('value'='trend')); head(trend)
    write.csv(trend, get(sprintf('%s.trend.csv', g)), row.names=F, na='')
  }
  
}

calc.LIV.ECO = function(ld.csv=layers_data.csv, 
                        LIV.status.csv = file.path(dir.results, sprintf('LIV_status_%s.csv', sfx.scenario)),
                        LIV.trend.csv  = file.path(dir.results, sprintf('LIV_trend_%s.csv' , sfx.scenario)),
                        ECO.status.csv = file.path(dir.results, sprintf('ECO_status_%s.csv', sfx.scenario)),
                        ECO.trend.csv  = file.path(dir.results, sprintf('ECO_trend_%s.csv' , sfx.scenario)),
                        scenario){
  
  # status
  d = rename(read.csv(sprintf('/Volumes/data_edit/model/GL-NCEAS-LayersDisaggregated_v2013a/data/rgn_liv.eco_status_%s.csv', scenario), na.strings=''),
             c('value'='status')); head(d)  
  d$status = d$status * 100
  write.csv(subset(d, component=='livelihood', c(rgn_id, status)), LIV.status.csv, row.names=F, na='')
  write.csv(subset(d, component=='economy', c(rgn_id, status)), ECO.status.csv, row.names=F, na='')
  
  # trend
  d = rename(read.csv(sprintf('/Volumes/data_edit/model/GL-NCEAS-LayersDisaggregated_v2013a/data/rgn_liv.eco_trend_%s.csv', scenario), na.strings=''), 
             c('value'='trend')); head(d)  
  write.csv(subset(d, component=='livelihood', c(rgn_id, trend)), LIV.trend.csv, row.names=F, na='')
  write.csv(subset(d, component=='economy', c(rgn_id, trend)), ECO.trend.csv, row.names=F, na='')    
}


calc.LIV.ECO.Later = function(ld.csv=layers_data.csv, 
                   LIV.status.csv = file.path(dir.results, sprintf('LIV_status_%s.csv', sfx.scenario)),
                   LIV.trend.csv  = file.path(dir.results, sprintf('LIV_trend_%s.csv' , sfx.scenario)),
                   ECO.status.csv = file.path(dir.results, sprintf('ECO_status_%s.csv', sfx.scenario)),
                   ECO.trend.csv  = file.path(dir.results, sprintf('ECO_trend_%s.csv' , sfx.scenario)),
                   liv_adj_year=2009, eco_adj_min_year=2000){
  # TODO: remove Brazil entries
  # TODO: i_eco_rev_adj_gdp -> cnky_eco_rev_whence in functions.R
  # i_eco_rev_adj_gdp     country_id,year,value_num            Economies model variable	revenue adjustment (GDP)	                   6F	  USD	    ECO	6F_eco_rev_adj_gdp.csv
  
  # since i_eco_rev_adj_gdp missing sector og for generating cny_eco_rev_adj_gdp, creating from filesystem
  ohi.load('global_li_adjustments', dir='/var/data/ohi/model/GL-NCEAS-Livelihoods/data')
  cnky_eco_rev_whence = sqldf(
    "SELECT iso3166 AS country_id, whence, year, value AS USD 
  FROM global_li_adjustments
  WHERE metric='rev_adj'")
  # TODO: i_eco_rev_adj_gdp -> cnky_eco_rev_whence in functions.R
  # TODO: consider splitting into two layers, since currently funkily incorporating [whence] (actual or model) as [category] but category isn't sector like other layers.
  
  
  # TODO: cny_liv_jobs_adj_unemp -> i_liv_jobs_adj_unemp in functions.R
  # TODO: ?cny_eco_rev_adj_gdp
  # TODO: cny_ao_ppp -> cny_le_ppp -> cny_ao_need (or ppppcgdp layer_id from Nature 2012?)
  # x = contrast(x=head(b), by.x=c('metric','sector','country_id','year'), on.x=c('value','base_value','base_whence','adj_value'),
  #              y=head(a), by.y=c('metric','sector','iso3166'   ,'year'), skip.y.na=F)
  
  # DEBUG isolated run
  #liv_adj_year=2009
  #eco_adj_min_year=2000
  #layers_data.csv = '/Volumes/local_edit/src/toolbox/data/global_2012_nature/layers_data.csv'  
  #source('/Volumes/local_edit/src/R/ohi/R/ohi.R')
  
  library(plyr)
  library(reshape2)
  library(sqldf)
  
  # cast data ----  

  # layers
  lyrs = list('cky' = c('cnky_eco_rev'             = 'rev',
                        'cnky_liv_jobs'            = 'jobs',
                        'cnky_liv_wages'           = 'wage'),
              'cy'  = c('cny_ao_need'              = 'ppp',
                        'cny_liv_pct_unemp'        = 'jobs',
                        'cny_le_workforcesize_adj' = 'workforce',
                        'cnky_eco_rev_whence'      = 'rev',
                        'cnky_liv_wages_adj_lilo'  = 'wage'))
  # NOTE: cnky_eco_rev_whence & cnky_liv_wages_adj_lilo are wierd ones unique by country & year with additional category column of whence
  layers = sub('(cky|cy)\\.','', names(unlist(lyrs)))
  
  # get layer data
  d = subset(read.csv(ld.csv, na.strings=''), layer %in% layers)
  
  # check for missing layers
  layers.missing = layers[!layers %in% d$layer]
  if (length(layers.missing)>0) stop(sprintf('Missing layer(s): %s', paste(layers.missing, collapse=', ')))
  # missing: cny_liv_pct_unemp
  
  # HACK: rbind unemp
  # adj_unemp  National unemployment statistics		7.48	percent	48	7_48_adj_unemp.csv																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																									

  # unemployment
  #   /Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_uem_2012a.csv
  #   /Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_uem_2013a.csv
  #     
  # total labor force
  #   /Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_lab_2012a.csv
  #   /Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_lab_2013a.csv
  
  x = read.csv('/Volumes/local_edit/src/toolbox/data/global_2012_nature_ftp/layers/7_48_adj_unemp.csv', na.strings=''); head(x)
  # TODO: disaggregate this layer too for 2013
  x = rename(x, c('country_id' = 'id_chr',
                  'percent'    = 'value_num'))
  x$layer = 'cny_liv_pct_unemp'
  names(d)[!names(d) %in% names(x)]
  d = rbind.fill(d, x)

  # check for duplicates
  for (lyr in lyrs[['cky']]){
    x = subset(d, layer==lyr)
    print(x[duplicated(x[,c('id_chr','category','year')]),])
  }
  for (lyr in lyrs[['cy']]){ # lyr='cny_le_workforcesize_adj'
    x = subset(d, layer==lyr)    
    print(x[duplicated(x[,c('id_chr','year')]),])
  }
    
  # cast per country, sector, year
  cky = rename(dcast(d, id_chr + category + year ~ layer, value.var='value_num', subset = .(layer %in% names(lyrs[['cky']]))),
               c('id_chr'='country_id', 'category'='sector', lyrs[['cky']])) #; table(rk$habitat); print(head(subset(rk, habitat=='mangrove')), row.names=F); head(rk)
  #print(cky[duplicated(cky[,c('country_id','sector','year')]),])
  
  # cast per country, year
  cy = rename(dcast(d, id_chr + year ~ layer, value.var='value_num', subset = .(layer %in% names(lyrs[['cy']]))),
              c('id_chr'='country_id', lyrs[['cy']])) #; table(rk$habitat); print(head(subset(rk, habitat=='mangrove')), row.names=F); head(rk)
  #print(head(cy[duplicated(cy[,c('country_id','year')]),]))
  
  # add whence 
  cy.t = rename(dcast(d, id_chr + year ~ layer, value.var='category', subset = .(layer %in% c('cnky_eco_rev_whence','cnky_liv_wages_adj_lilo'))),
                c('id_chr'='country_id', 'cnky_eco_rev_whence'='rev_whence', 'cnky_liv_wages_adj_lilo'='wage_whence')); head(cy); head(cy.t); table(cy.t$rev_whence); table(cy.t$wage_whence)
  cy = merge(cy, cy.t, all.x=T); head(cy); summary(cy)
  #print(cy[duplicated(cy[,c('country_id','year')]),])
        
  # calculate status ----
  
  # get most recent (cur) and reference (ref) year, optimally going back 5 years
  jobs = ddply(subset(cky, !is.na(jobs), c('country_id','sector','year','jobs')), .(country_id, sector), summarize,
                 metric = 'jobs',
                 year_cur = max(year),
                 year_ref = year[na.omit(match(c(5, 6, 4, 7, 3, 8, 2, 9, 1, 10), max(year) - year))[1]])
  rev = ddply(subset(cky, !is.na(rev), c('country_id','sector','year','rev')), .(country_id, sector), summarize,
                 metric = 'rev',
                 year_cur = max(year),
                 year_ref = year[na.omit(match(c(5, 6, 4, 7, 3, 8, 2, 9, 1, 10), max(year) - year))[1]])
  wage = ddply(subset(cky, !is.na(wage), c('country_id','sector','year','wage')), .(country_id, sector), summarize,
                metric = 'wage',
                year_cur = max(year),
                year_ref = year[na.omit(match(c(5, 6, 4, 7, 3, 8, 2, 9, 1, 10), max(year) - year))[1]])
  
  # append jobs/rev/wage to base_cur and base_ref based on country_id, sector
  jobs  = sqldf("SELECT j.*, d.jobs  AS base_cur FROM jobs  AS j JOIN cky AS d ON (j.year_cur=d.year AND j.country_id=d.country_id AND j.sector=d.sector)")
  jobs  = sqldf("SELECT j.*, d.jobs  AS base_ref FROM jobs  AS j JOIN cky AS d ON (j.year_ref=d.year AND j.country_id=d.country_id AND j.sector=d.sector)")
  rev   = sqldf("SELECT j.*, d.rev   AS base_cur FROM rev   AS j JOIN cky AS d ON (j.year_cur=d.year AND j.country_id=d.country_id AND j.sector=d.sector)")
  rev   = sqldf("SELECT j.*, d.rev   AS base_ref FROM rev   AS j JOIN cky AS d ON (j.year_ref=d.year AND j.country_id=d.country_id AND j.sector=d.sector)")
  wage  = sqldf("SELECT j.*, d.wage AS base_cur FROM wage AS j JOIN cky AS d ON (j.year_cur=d.year AND j.country_id=d.country_id AND j.sector=d.sector)")
  wage  = sqldf("SELECT j.*, d.wage AS base_ref FROM wage AS j JOIN cky AS d ON (j.year_ref=d.year AND j.country_id=d.country_id AND j.sector=d.sector)")
  
  # append jobsrev/wage to adj_cur and adj_ref based on just country_id
  jobs  = sqldf("SELECT j.*, d.jobs  AS adj_cur FROM jobs  AS j JOIN cy AS d ON (j.year_cur=d.year AND j.country_id=d.country_id)")
  jobs  = sqldf("SELECT j.*, d.jobs  AS adj_ref FROM jobs  AS j JOIN cy AS d ON (j.year_ref=d.year AND j.country_id=d.country_id)")
  rev   = sqldf("SELECT j.*, d.rev   AS adj_cur FROM rev   AS j JOIN cy AS d ON (j.year_cur=d.year AND j.country_id=d.country_id)")
  rev   = sqldf("SELECT j.*, d.rev   AS adj_ref FROM rev   AS j JOIN cy AS d ON (j.year_ref=d.year AND j.country_id=d.country_id)")
  wage  = sqldf("SELECT j.*, d.wage AS adj_cur FROM wage AS j JOIN cy AS d ON (j.year_cur=d.year AND j.country_id=d.country_id)")
  wage  = sqldf("SELECT j.*, d.wage AS adj_ref FROM wage AS j JOIN cy AS d ON (j.year_ref=d.year AND j.country_id=d.country_id)")
  
  # combine jobs, rev, wage into single metrics table
  jrw = rbind(jobs, rev, wage)

  # quality enforce minimum data standards for countries to include: sum of sector years must be non-zero, 
  #   and at least 2 sectors are required across revenue and jobs, but any ok for wage
  jrw$base_sum = with(jrw, base_ref + base_cur)
  q = ddply(ddply(jrw, 
                  .(country_id, metric), summarize,
                  n_nonzero_sectors = length(sector) - sum(base_sum==0)), 
            .(country_id), summarize,
            n_nonzero_metric_sectors = sum(n_nonzero_sectors))
  mq = merge(jrw, q, all=T)
  m = subset(mq, (base_sum != 0) & (n_nonzero_metric_sectors > 1 | metric=='wage'))
#m0 = subset(mq, n_nonzero_metric_sectors > 1 | (metric=='wage' & base_sum != 0))
#   sqldf("SELECT m.* FROM m LEFT JOIN n USING (country_id,sector,metric) WHERE n.country_id IS NULL")
#   x = merge(m[,1:3],m0[,1:3], all=T)
  # TODO: should this quality control really be for nonzero sectors per country,metric and not just country?
  #  FIX:
  #   q = ddply(jrw, .(country_id, metric), summarize,
  #             n_nonzero_sectors = length(sector) - sum(base_sum==0)))
  #   mq = merge(jrw, q, all=T)
  #   m = subset(mq, base_sum != 0 & (n_nonzero_sectors > 1 | metric=='wage')))

# # b = sqldf("SELECT * FROM m WHERE base_ref <> 0 AND adj_ref <> 0")
# # dim(b)
# status_model_curref = dbGetQuery(pg, "SELECT * FROM global_li.status_model_curref")
# subset(status_model_curref, iso3166=='BRA' & metric=='wage')
# subset(m, country_id=='BRA' & metric=='wage')  
# # status_model_curref_lim = dbGetQuery(pg, "SELECT * FROM global_li.status_model_curref WHERE ref_base_value <> 0 AND ref_adj_value <> 0")
# # dim(status_model_curref_lim)

  # calculate jobs (LIV) and revenue (ECO) scores
  s_jr = sqldf(
    "SELECT  metric, country_id, COUNT(*) AS n_sector,
    (SUM(base_cur) / SUM(base_ref)) / (AVG(adj_cur) / AVG(adj_ref)) AS score
    FROM     m
    WHERE    base_ref <> 0 AND adj_ref <> 0 AND metric IN ('jobs', 'rev')
    GROUP BY metric, country_id
    ORDER BY country_id, metric")
  
  # for wage (LIV), compute the corrected relative value per metric per country:
  #   0. w'_i = (w_c/w_r)/(W_c/W_r) for each sector i per country
  #   1. let w' = unweighted mean(w'_i) across all sector i per country
  #   2. multiply w' by the purchasing power parity (PPP) value for the country
  s_w = sqldf(
    "SELECT  metric, country_id, (w_prime * ppp) AS score, n_sector
    FROM     (
    SELECT  metric, country_id, AVG(w_prime_i) AS w_prime, COUNT(*) AS n_sector
    FROM    (
    SELECT  metric, country_id, sector, 
    (base_cur / base_ref) / (adj_cur / adj_ref) AS w_prime_i
    FROM    m
    WHERE   metric = 'wage' AND base_ref <> 0 AND adj_ref <> 0
    ) t0
    GROUP BY  metric, country_id
    ) t1
    JOIN    ( -- find current ppp value per country
    SELECT  country_id, year, ppp
    FROM    cy
    JOIN    ( -- find most recent ppp year
    SELECT    country_id, MAX(year) AS year 
    FROM      cy
    WHERE     ppp IS NOT NULL
    GROUP BY  country_id
    ) max USING (country_id, year)
    ) p USING (country_id)
    ORDER BY metric, country_id")

  # 3. set the best country (PPP-adjusted average wage) equal to 1.0 and then rescale all countries to that max  
  s_w$score = s_w$score / max(s_w$score)
  
  # combine the corrected relative values into a single status score for LIV (jobs & wage) and ECO (revenue)
  s = sqldf(
    "SELECT *
    FROM (
    SELECT d.country_id, 
    cast('livelihood' AS varchar) AS component,   
    CASE WHEN j.score IS NOT NULL AND w.score IS NOT NULL 
    THEN (0.5 * j.score + 0.5 * w.score) 
    ELSE COALESCE(j.score, w.score) 
    END AS value
    FROM      (SELECT DISTINCT country_id FROM m) d    
    LEFT JOIN (SELECT * FROM s_jr WHERE metric = 'jobs') j USING (country_id)
    LEFT JOIN (SELECT * FROM s_w WHERE metric = 'wage') w USING (country_id)
    UNION        
    SELECT    d.country_id, 'economy', e.score
    FROM      (SELECT DISTINCT country_id FROM m) d    
    LEFT JOIN (SELECT * FROM s_jr WHERE metric = 'rev') e USING (country_id)        
    ) t
    WHERE value IS NOT NULL
    ORDER BY country_id")
  
  # assign status as value clamped 0 to 1, and multiply by 100
  s$score = pmin(s$value, 1) * 100
  
  # aggregate ----
  
  # aggregate countries to regions by country workforce size for livelihood
  w_liv = subset(cy, year==liv_adj_year & !is.na(workforce), c(country_id,workforce))
  
  s_liv = aggregate_by_country_weighted(df=subset(s, component=='livelihood'), w=w_liv,
                                        col.value='score', col.country='country_id', col.weight='workforce') # ABW workforce==NA  # summary(s_liv)
  # TODO: look for difference in LIV between ftp Nature2012 and global2012
  # s_liv.a = dbGetQuery(pg, "SELECT id, value*100 AS status FROM global_li.status_region WHERE component = 'livelihood'"); head(s_liv.a)
  #ck.LIV = contrast(s_liv, s_liv.a, by.x='region_id', by.y='id', on.x='score', on.y='status', precision=4)  
  # all y in x success: 171 
  # all x in y success: 171 
  # score.equ success
  #ck.LIV = contrast(x=s_liv, by.x='region_id', by.y='id', on.x='score', on.y='status',
  #                     y=subset(results_global_data, goal.subgoal=='LIV'), precision=2)
  # dropping mutual NAs: 1 / 172 
  # all y in x success: 172 
  # all x in y FAIL!: 3 / 171 
  #     region_id            score
  # 79         79 93.6637698071856
  # 110       110 93.6637698071856
  # 114       114 93.6637698071856
  # score.equ  FAIL! on 3 / 171 
  #  region_id            score  score.y          score.dif
  #        121 68.3349668096027 66.75735  1.577616809602731
  #        122 58.4077369325363 55.57214  2.835596932536262
  #        171 82.5785097996376 83.16348 -0.584970200362378
  
  # aggregate countries to regions by country revenue for economies
  w_eco = w = ddply(subset(cy, !is.na(rev) & year >= eco_adj_min_year & rev_whence=='actual'), .(country_id), summarize,
                    rev_adj = rev[which.max(year)])  
  s_eco = aggregate_by_country_weighted(df=subset(s, component=='economy'), w=w_eco,
                                        col.value='score', col.country='country_id', col.weight='rev_adj') # ABW workforce==NA  
  # TODO: look for difference in ECO between ftp Nature2012 and global2012
  # s_eco.a = dbGetQuery(pg, "SELECT id, value*100 AS status FROM global_li.status_region WHERE component = 'economy'"); head(s_eco.a)
  #ck.ECO = contrast(s_eco, s_eco.a, by.x='region_id', by.y='id', on.x='score', on.y='status', precision=4)
  # all y in x success: 171 
  # all x in y success: 171 
  # score.equ success  
  # ck.ECO = contrast(x=s_eco, by.x='region_id', by.y='id', on.x='score', on.y='status',
  #                    y=subset(results_global_data, goal.subgoal=='ECO'), precision=2)
  # dropping mutual NAs: 1 / 172 
  # all y in x success: 172 
  # all x in y FAIL!: 3 / 171 
  #     region_id            score
  # 79         79 75.0066169564487
  # 110       110 75.0066169564487
  # 114       114 75.0066169564487
  # score.equ  FAIL! on 73 / 171 
  #  region_id            score  score.y          score.dif
  #          1 74.2692832666138 60.35782 13.911463266613758
  #          2 74.2692832666138 60.35782 13.911463266613758
  #         10 76.9955069354751 73.61761  3.377896935475107
  #         12 77.2720562615789 62.23359 15.038466261578854
  #         15 77.3206334298282 77.91991 -0.599276570171767
  #         16 99.8922517164328 94.90722  4.985031716432843
  # ...  

  # gather status
  d.status = merge(setNames(s_liv[,c('region_id','score')], c('region_id','LIV_status')),
                   setNames(s_eco[,c('region_id','score')], c('region_id','ECO_status')), all=T)

  print('LE.browser')
  browser()
  LIV.status.csv = file.path(dir.results, sprintf('LIV_status_%s.csv', sfx.scenario))
  LIV.trend.csv  = file.path(dir.results, sprintf('LIV_trend_%s.csv' , sfx.scenario))
  ECO.status.csv = file.path(dir.results, sprintf('ECO_status_%s.csv', sfx.scenario))
  
  
#  head(d.status)
  
  # calculate trend ----
  # TODO: get whence for jobs (have for rev & wage)  

#   adjustments = dbGetQuery(pg, "SELECT * FROM global_li.adjustments"); head(adjustments); dim(adjustments); head(metric_sector_refperiod)
#   b = sqldf("SELECT 'jobs_adj'  AS metric, country_id, year, NULL  AS whence, jobs  AS value FROM cy WHERE jobs  IS NOT NULL
#         UNION
#         SELECT 'rev_adj'   AS metric, country_id, year, rev_whence AS whence, rev   AS value FROM cy WHERE rev   IS NOT NULL
#         UNION
#         SELECT 'wage_adj' AS metric, country_id, year, NULL       AS whence, wage AS value FROM cy WHERE wage IS NOT NULL" ); head(b)
#   x = contrast(x=b,           by.x=c('metric','country_id','year'), on.x='value',
#                y=adjustments, by.y=c('metric','iso3166'   ,'year'), on.y='value', skip.y.na=F)
#  all good
  
#   head(jrw)
#   head(cky)
#   status_model_curref = dbGetQuery(pg, "SELECT * FROM global_li.status_model_curref"); head(a); dim(a)
#   adjustments = dbGetQuery(pg, "SELECT * FROM global_li.adjustments"); head(adjustments); dim(adjustments); head(metric_sector_refperiod)
#   dlply(adjustments, .(metric), function(x) table(x$whence))
  
#   a = dbGetQuery(pg, "    -- grab adjustment data per metric-sector-year in sector timeframe
#     SELECT  d.metric, d.sector, d.iso3166,
#             a.year, a.whence AS adj_whence, a.value AS adj_value
#     FROM    global_li.metric_sector_refperiod d
#     JOIN    global_li.adjustments a USING (iso3166)
#     WHERE   a.metric = d.metric || '_adj' AND 
#             a.year <= d.cur_year AND
#             a.year >= d.ref_year"); head(a)  
  b = sqldf(
    "-- SELECT d.metric, d.sector, d.country_id, 
    --   a.year, a.
    -- grab adjustment data per metric-sector-year in sector timeframe
      SELECT  d.metric, d.sector, d.country_id,
              a.year, a.whence AS adj_whence, a.value AS adj_value
      FROM    jrw AS d
      JOIN (
        SELECT 'jobs'  AS metric, country_id, year, NULL       AS whence, jobs  AS value FROM cy WHERE jobs  IS NOT NULL
        UNION
        SELECT 'rev'   AS metric, country_id, year, rev_whence AS whence, rev   AS value FROM cy WHERE rev   IS NOT NULL
        UNION
        SELECT 'wage' AS metric, country_id, year, NULL       AS whence, wage AS value FROM cy WHERE wage IS NOT NULL
      ) AS a USING (country_id, metric)
      WHERE   a.year <= d.year_cur AND
              a.year >= d.year_ref"); head(b)
  

#   a = dbGetQuery(pg, "SELECT  d.metric, d.sector, d.iso3166, d.year,
#           cast(NULL as double precision)AS value,
#           m.value                       AS base_value, 
#           cast('actual' as varchar(80)) AS base_whence,
#           d.adj_value,
#           d.adj_whence
#   FROM    ( 
#     -- grab adjustment data per metric-sector-year in sector timeframe
#     SELECT  d.metric, d.sector, d.iso3166,
#             a.year, a.whence AS adj_whence, a.value AS adj_value
#     FROM    global_li.metric_sector_refperiod d
#     JOIN    global_li.adjustments a USING (iso3166)
#     WHERE   a.metric = d.metric || '_adj' AND 
#             a.year <= d.cur_year AND
#             a.year >= d.ref_year
#     ) d
#   JOIN    global_li.metric_sector_year m USING (metric, sector, iso3166, year)
#   WHERE   m.value IS NOT NULL -- double-check we're getting an actual base
#   ORDER BY d.metric, d.sector, d.iso3166, d.year"); head(a)

  b = sqldf(
    "SELECT metric, sector, country_id, year,
      CAST(NULL as REAL) AS value,
      m.value                       AS base_value, 
      cast('actual' as varchar(80)) AS base_whence,
      adj_value,
      adj_whence
    FROM  (
      -- grab adjustment data per metric-sector-year in sector timeframe
      SELECT  d.metric, d.sector, d.country_id,
              a.year, a.whence AS adj_whence, a.value AS adj_value
      FROM    jrw AS d
      JOIN (
        SELECT 'jobs'  AS metric, country_id, year, wage_whence AS whence, jobs  AS value FROM cy WHERE jobs  IS NOT NULL
        UNION
        SELECT 'rev'   AS metric, country_id, year, rev_whence   AS whence, rev  AS value FROM cy WHERE rev   IS NOT NULL
        UNION
        SELECT 'wage' AS metric, country_id, year, NULL          AS whence, wage AS value FROM cy WHERE wage IS NOT NULL
      ) AS a USING (country_id, metric)
      WHERE   a.year <= d.year_cur AND
              a.year >= d.year_ref) AS d
    JOIN  (
      SELECT 'jobs' AS metric, sector, country_id, year, jobs AS value FROM cky WHERE jobs  IS NOT NULL
      UNION
      SELECT 'rev'  AS metric, sector, country_id, year, rev  AS value FROM cky WHERE rev   IS NOT NULL
      UNION
      SELECT 'wage' AS metric, sector, country_id, year, wage AS value FROM cky WHERE wage IS NOT NULL
    ) AS m USING (metric, sector, country_id, year)
    WHERE   m.value IS NOT NULL -- double-check we're getting an actual base
    ORDER BY d.metric, d.sector, d.country_id, d.year"); head(b)  
  #x = contrast(x=b, by.x=c('metric','sector','country_id','year'), on.x=c('value','base_value','base_whence','adj_value'),
  #             y=a, by.y=c('metric','sector','iso3166'   ,'year'), skip.y.na=F)
  # all y in x FAIL!: 28 / 5933 
  #      metric sector     country_id year value.y base_value.y base_whence.y adj_value.y
  # 1085   jobs    mmw            ATA 1998      NA            5        actual          NA
  # 1086   jobs    mmw            ATA 2008      NA           84        actual          NA
  # 1089   jobs    mmw         Azores 1998      NA           11        actual          NA
  # 1090   jobs    mmw         Azores 2008      NA           46        actual          NA
  # 1107   jobs    mmw Canary Islands 1998      NA          943        actual          NA
  # 1108   jobs    mmw Canary Islands 2008      NA          576        actual          NA"  
  #table(subset(x, is.na(base_value), c(metric, sector)))
  #        sector
  # metric mmw og tour
  #   jobs   6  0    0
  #   rev    6 12    0
  #   wage   0  2    2
  #table(subset(x, is.na(base_value), c(metric, country_id)))
  #          country_id
  #   metric ATA Azores Canary Islands KAZ Wake Island
  #   jobs   2      2              2   0           0
  #   rev    8      2              2   0           6
  #   wage   0      0              0   4           0
  
  
        
}

calc.LSP = function(ld.csv=layers_data.csv, 
                    status.csv = file.path(dir.results, sprintf('LSP_status_%s.csv', sfx.scenario)),
                    trend.csv  = file.path(dir.results, sprintf('LSP_trend_%s.csv' , sfx.scenario)), 
                    ref.pct.cmpa=30, ref.pct.cp=30, status.year=2012, trend.years=2005:2009, trend.method='default'){
  # Use Status of 2013 to again be as inclusive as possible of established protected areas, but presume a lag in exhaustive reporting of 3 years so use 2006-2010 for Trend.
  # 2012a: calc.LSP(status.year=2013, trend.years=2006:2010)
  # 2013a: calc.LSP(status.year=2009, trend.years=2002:2006)
    
  library(plyr)
  library(reshape2)
  
  lyrs = list('r'  = c('rn_rgn_area_inland1km'   = 'area_inland1km',
                       'rn_rgn_area_offshore3nm' = 'area_offshore3nm'),
              'ry' = c('rny_lsp_prot_area_offshore3nm' = 'cmpa',
                        'rny_lsp_prot_area_inland1km'   = 'cp'))              
  lyrs.names = sub('(r|ry)\\.','', names(unlist(lyrs)))
  
#   # DEBUG: load data
#     load_all('/usr/local/ohi/src/R/ohi'); config.check('~/ohi_tbx/scenarios/global_2012_nature/conf/config.R')
#     dimensions=c('LSP_status'='status','LSP_trend'='trend'); ref.pct.cmpa=30; ref.pct.cp=30; status.year=2009; trend.years=2002:2006
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
  r  = rename(dcast(d, id_num ~ layer, value.var='value_num', subset = .(layer %in% names(lyrs[['r']]))),
              c('id_num'='region_id', lyrs[['r']]))
  ry = rename(dcast(d, id_num + year ~ layer, value.var='value_num', subset = .(layer %in% names(lyrs[['ry']]))),
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
  status = r.yrs[r.yrs$year==status.year, c('region_id','status')]
  write.csv(rename(status,
                   c('region_id'='rgn_id')), status.csv, row.names=F, na='')
  
  # calculate trend ----

  if (trend.method=='nature2012'){
    # calculate trend Nature 2012 WRONG way ----
    
    # get cumulative sum of total protected areas added within trend years
    t = subset(ry, year %in% trend.years)
    t$cp[is.na(t$cp)]     = 0
    t$cmpa[is.na(t$cmpa)] = 0
    t = within(t, {
      pa_delta_cum = ave(cp+cmpa, region_id, FUN=cumsum)
    }); head(t)
  
    # add all years
    r.yrs = ddply(ry, .(region_id), function(x){
      data.frame(region_id=x$region_id[1],
                 year=min(trend.years):max(trend.years))
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
    trend = ddply(subset(r.yrs, year %in% trend.years), .(region_id), summarize,
                  annual = lm(pct_pa ~ year)[['coefficients']][['year']],
                  trend = min(1, max(0, 5 * annual))) # HACK: capping trend to 0 since getting a few infinitessimally small negatives, eg 91  Crozet Islands	-6.15E-14 for LSP.t.12
  } else {
    stop(sprintf('trend.method=%s not supported in calc.LSP().', trend.method))
  }

  # return dimensions ----
  write.csv(rename(trend,
                   c('region_id'='rgn_id'))[,c('rgn_id','trend')], trend.csv, row.names=F, na='')
  
#   # DEBUG: compare 
#   ans = subset(results_global_data, goal.subgoal=='LSP', c(id, status, trend)); 
#   print(subset(ans, id %in% c(1,2,11,12)), row.names=F)  
#   #  id    status     trend
#   #   1 100.00000 0.3677242
#   #   2   1.24302 0.0000000
#   #  11  25.90764 0.0000663
#   #  12  69.26206 0.0387219  
}


calc.ICO = function(ld.csv=layers_data.csv, 
                    status.csv = file.path(dir.results, sprintf('ICO_status_%s.csv', sfx.scenario)),
                    trend.csv  = file.path(dir.results, sprintf('ICO_trend_%s.csv' , sfx.scenario))){

  # layers
  lyrs = list('rk' = c('rnk_ico_spp_extinction_status' = 'risk_category',
                       'rnk_ico_spp_popn_trend'        = 'popn_trend'))
  layers = sub('(r|ry|rk)\\.','', names(unlist(lyrs)))
  
  # cast data ----
  D = subset(read.csv(ld.csv, na.strings=''), layer %in% layers)
  
  rk = rename(dcast(D, id_num + category ~ layer, value.var='value_chr', subset = .(layer %in% names(lyrs[['rk']]))),
              c('id_num'='rgn_id', 'category'='sciname', lyrs[['rk']])); head(rk); summary(rk)
  
  # lookups for weights
  w.risk_category = c('LC' = 0,
                      'NT' = 0.2,
                      'VU' = 0.4,
                      'EN' = 0.6,
                      'CR' = 0.8,
                      'EX' = 1)
  
  w.popn_trend = c('Decreasing' = -0.5,
                   'Stable'     =  0,                                           
                   'Increasing' =  0.5)
  
  # status
  r.status = rename(ddply(rk, .(rgn_id), function(x){ 
                          mean(1 - w.risk_category[x$risk_category], na.rm=T) * 100 }), 
                    c('V1'='status')); head(r.status); summary(r.status)
  write.csv(r.status, status.csv, row.names=F, na='')
  
  # trend
  r.trend = rename(ddply(rk, .(rgn_id), function(x){ 
                        mean(w.popn_trend[x$popn_trend], na.rm=T) }), 
                    c('V1'='trend')); head(r.trend); summary(r.trend)
  write.csv(r.trend, trend.csv, row.names=F, na='')  
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
  r = rename(dcast(D, id_num ~ layer, value.var='value_num', subset = .(layer %in% names(lyrs[['r']]))),
              c('id_num'='rgn_id', lyrs[['r']])); head(r); summary(r)
  
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
  write.csv(r[,c('rgn_id','status')], status.csv, row.names=F, na='')
  
  # trend
  r$trend = rowMeans(r[,c('pest_trend','fert_trend','popn_trend','path_trend')], na.rm=T)
  write.csv(r[,c('rgn_id','trend')], trend.csv, row.names=F, na='')  
  
  if (verbose==T) {
    debug.csv = file.path(dir.results, sprintf('debug_CW_%s.csv',sfx.scenario))
    cat(sprintf('\noutputting debug file to: %s/%s\n', getwd(), debug.csv))
    write.csv(r, debug.csv, row.names=F, na='')
    cat(sprintf('\nSummary for: %s,%s\n',basename(status.csv),basename(trend.csv)))
    print(summary(r))
  }
}

