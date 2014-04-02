FIS = function(layers, status_year=2011){
  # layers used: snk_fis_meancatch, fnk_fis_b_bmsy, snk_fis_proparea_saup2rgn
      
  # catch data
  c = SelectLayersData(layers, layer='snk_fis_meancatch', narrow=T) %.%
    select(
      fao_saup_id    = id_chr,
      taxon_name_key = category,
      year,
      catch          = val_num)  
  
  # separate out the region ids:
  c$fao_id    <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[1]))
  c$saup_id   <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[2]))
  c$TaxonName <- sapply(strsplit(as.character(c$taxon_name_key), "_"), function(x)x[1])
  c$TaxonKey  <- as.numeric(sapply(strsplit(as.character(c$taxon_name_key), "_"), function(x)x[2]))
  c$catch     <- as.numeric(c$catch)
  c$year      <- as.numeric(as.character(c$year))
  #Create Identifier for linking assessed stocks with country-level catches
  c$stock_id <- paste(as.character(c$TaxonName),
                      as.character(c$fao_id), sep="_")
  
  # b_bmsy data
  b = SelectLayersData(layers, layer='fnk_fis_b_bmsy', narrow=T) %.%
    select(
      fao_id         = id_num,
      TaxonName      = category,
      year,
      bmsy           = val_num)
  # Identifier taxa/fao region:
  b$stock_id <- paste(b$TaxonName, b$fao_id, sep="_")
  b$bmsy     <- as.numeric(b$bmsy)
  b$fao_id   <- as.numeric(as.character(b$fao_id))
  b$year     <- as.numeric(as.character(b$year))
  
  # area data for saup to rgn conversion
  a = SelectLayersData(layers, layer='snk_fis_proparea_saup2rgn', narrow=T) %.%
    select(
      saup_id   = id_num,
      rgn_id    = category,
      prop_area = val_num)
  a$prop_area <- as.numeric(a$prop_area)
  a$saup_id   <- as.numeric(as.character(a$saup_id))
  a$rgn_id    <- as.numeric(as.character(a$rgn_id))
  
  # ------------------------------------------------------------------------
  # STEP 1. Merge the species status data with catch data
  #     AssessedCAtches: only taxa with catch status data
  # -----------------------------------------------------------------------
  AssessedCatches <- join(b, c, 
                          by=c("stock_id", "year"), type="inner")
  
  # b,c by stock_id
  
  # include only taxa with species-level data
  AssessedCatches <- AssessedCatches[as.numeric(AssessedCatches$TaxonKey)>=600000, ]
  AssessedCatches$penalty <- 1
  
  # ------------------------------------------------------------------------
  # STEP 2. Estimate status data for catch taxa without species status
  #     UnAssessedCatches: taxa with catch status data
  # -----------------------------------------------------------------------  
  UnAssessedCatches <- c[!(c$year %in% AssessedCatches$year &
                             c$stock_id %in% AssessedCatches$stock_id), ]
  
  # 2a.  Join UnAssessedCatches data to the b_bmsy summaries for each FAO/Year
  
  # Average status data for assessed stocks by FAO region for each year. 
  # This is used as the starting estimate for unassesed stocks
  # Here, the Median b_bmsy was chosen for TaxonKey >= 600000 
  # and Min b_bmsy for TaxonKey < 600000
  #  *************NOTE *****************************
  #  Using the minimum B/BMSY score as an starting point
  #  for the estimate of B/BMSY for unassessed taxa not
  #  identified to species level is very conservative.
  #  This is a parameter that can be changed.
  #  ***********************************************
  b_summary <- ddply(b, .(fao_id, year), summarize,
                     Medianb_bmsy=quantile(as.numeric(bmsy), probs=c(0.5)), 
                     Minb_bmsy=min(as.numeric(bmsy)))
  
  UnAssessedCatches <- join(UnAssessedCatches, b_summary, by=c("fao_id", "year"),
                            type="left", match="all")
  # 2b.  Create a penalty variable based on taxa level:
  UnAssessedCatches$TaxonPenaltyCode <- substring(UnAssessedCatches$TaxonKey,1,1)
  
  # 2c. Create a penalty table for taxa not identified to species level
  #  *************NOTE *****************************
  #  In some cases, it may make sense to alter the 
  #  penalty for not identifying fisheries catch data to
  #  species level.
  #  ***********************************************
  penaltyTable <- data.frame(TaxonPenaltyCode=1:6, 
                             penalty=c(0.01, 0.1, 0.25, 0.5, 0.75, 1))
  # 2d.Merge with data
  UnAssessedCatches <- join(UnAssessedCatches, penaltyTable, by="TaxonPenaltyCode")
  
  # ------------------------------------------------------------------------
  # STEP 3. Calculate score for all taxa based on status (b/bmsy) and taxa
  # -----------------------------------------------------------------------
  
  #  *************NOTE *****************************
  #  These values can be altered
  #  ***********************************************
  alpha <- 0.5
  beta <- 0.25
  lowerBuffer <- 0.95
  upperBuffer <- 1.05
    
  ## Function to calculate score for different scenarios:
  score <- function(data, variable){
    #data <- AssessedCatches
    #variable <- "bmsy"
    ifelse(data[ ,variable]*data[, "penalty"]<lowerBuffer,
           data[ ,variable]*data[, "penalty"],
           ifelse(data[ ,variable]*data[, "penalty"]>upperBuffer,
                  ifelse(1-alpha*(data[ ,variable]*data[, "penalty"]
                                  -upperBuffer)>beta,
                         1-alpha*(data[ ,variable]*data[, "penalty"]-upperBuffer),beta),
                  1))
  }
  
  AssessedCatches$score <- score(data=AssessedCatches, variable="bmsy")
  
  # Median is used to calculate score for species with Taxon 6 coding
  UnAssessedCatchesT6 <- subset(UnAssessedCatches, penalty==1)
  UnAssessedCatchesT6$score <- score(UnAssessedCatchesT6, "Medianb_bmsy")
  
  UnAssessedCatches <- subset(UnAssessedCatches, penalty!=1)
  UnAssessedCatches$score <- score(UnAssessedCatches, "Minb_bmsy")
  
  AllScores <- rbind(AssessedCatches[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")],
                  UnAssessedCatchesT6[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")],
                  UnAssessedCatches[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")])
    
  # ------------------------------------------------------------------------
  # STEP 4. Calculate status for each saup_id region
  # -----------------------------------------------------------------------
  
  # 4a. To calculate the weight (i.e, the relative catch of each stock per saup_id),
  # the mean catch of taxon i is divided by the   
  # sum of mean catch of all species in region r, which is calculated as: 
  
  smc <- ddply(.data = AllScores, .(year, saup_id), summarize, 
               SumCatch = sum(catch))   
  AllScores<-join(AllScores,smc,by=c("year","saup_id"))  
  AllScores$wprop<-AllScores$catch/AllScores$SumCatch 
  
  
  #  4b. The "score" and "weight" values per taxon per SAUP region are used to  
  #    calculate a geometric weighted mean across taxa for each saup_id region
  geomMean <- ddply(.data = AllScores, .(saup_id, year), summarize, status_saup = prod(score^wprop)) 
  
  # ------------------------------------------------------------------------
  # STEP 5. Convert status from saup spatial scale to OHI spatial scale  
  # -----------------------------------------------------------------------
  # In many cases the ohi reporting regions are comprised of multiple saup regions.
  # To correct for this, the proportion of each saup area of the total area of the 
  # OHI region was calculated. This was used to calculate Status from the Status_saup.
  # This type of adjustment is omitted if the data were collected at the same spatial 
  # scale as the collecting region.
  
  # Join region names/ids to Geom data
  geomMean <- join(a, geomMean, type="inner", by="saup_id") # merge km2 of shelf area with status results
  
  # weighted mean scores
  #StatusData <- ddply(.data = geomMean, .(rgn_id, year), summarize, Status = round(sum(status_saup*prop_area)*100))
  StatusData <- ddply(.data = geomMean, .(rgn_id, year), summarize, Status = sum(status_saup*prop_area))
  
  # 2013 status is based on 2011 data (most recent data)
  status = StatusData %.%
    filter(year==status_year) %.%
    mutate(
      score     = round(Status*100),
      dimension = 'status') %.%
    select(region_id=rgn_id, dimension, score)
    
    
  # ------------------------------------------------------------------------
  # STEP 6. Calculate trend  
  # -----------------------------------------------------------------------
  # NOTE: Status is rounded to 2 digits before trend is 
  # calculated in order to match OHI 2013 results (is this what we want to do?)
  trend = ddply(StatusData, .(rgn_id), function(x){
    mdl = lm(Status ~ year, data=x)
    data.frame(
      score     = round(coef(mdl)[['year']] * 5, 2),
      dimension = 'trend')}) %.%
    select(region_id=rgn_id, dimension, score)
  # %.% semi_join(status, by='rgn_id')
  
  # assemble dimensions
  scores = rbind(status, trend) %.% mutate(goal='FIS')
  return(scores)  
}

MAR = function(layers, status_years=2005:2011){  
  # layers used: mar_harvest_tonnes, mar_harvest_species, mar_sustainability_score, mar_coastalpopn_inland25mi, mar_trend_years
  
  harvest_tonnes = rename(
    SelectLayersData(layers, layers='mar_harvest_tonnes', narrow=T),
    c('id_num'='rgn_id', 'category'='species_code', 'year'='year', 'val_num'='tonnes'))
  harvest_species = rename(
    SelectLayersData(layers, layers='mar_harvest_species', narrow=T),
    c('category'='species_code', 'val_chr'='species'))
  sustainability_score = rename(
    SelectLayersData(layers, layers='mar_sustainability_score', narrow=T),
    c('id_num'='rgn_id', 'category'='species', 'val_num'='sust_coeff'))
  popn_inland25mi = rename(
    SelectLayersData(layers, layers='mar_coastalpopn_inland25mi', narrow=T),
    c('id_num'='rgn_id', 'year'='year', 'val_num'='popsum'))
  trend_years = rename(
    SelectLayersData(layers, layers='mar_trend_years', narrow=T),
    c('id_num'='rgn_id', 'val_chr'='trend_yrs'))
  
  # merge and cast harvest with sustainability
  #harvest_species$species = as.character(harvest_species$species)
  rky = dcast(merge(merge(harvest_tonnes, 
                          harvest_species, all.x=TRUE, by=c('species_code')),
                    sustainability_score, all.x=TRUE, by=c('rgn_id', 'species')),
              rgn_id + species + species_code + sust_coeff ~ year, value.var='tonnes', mean, na.rm=T); head(rky)
  rky = harvest_tonnes %.%
    merge(harvest_species     , all.x=TRUE, by='species_code') %.%
    merge(sustainability_score, all.x=TRUE, by=c('rgn_id', 'species')) %.%
    dcast(rgn_id + species + species_code + sust_coeff ~ year, value.var='tonnes', mean, na.rm=T)
  
  # smooth each species-country time-series using a running mean with 4-year window, excluding NAs from the 4-year mean calculation
  # TODO: simplify below with dplyr::group_by()
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
  scores = status %.%
    select(region_id = rgn_id,
           score     = status) %.%
    mutate(dimension='status') %.%
    rbind(
      trend %.%
        select(region_id = rgn_id,
               score     = trend) %.%
        mutate(dimension='trend')) %.%
    mutate(goal='MAR')
  return(scores)
  # NOTE: some differences to www2013 are due to 4_yr species only previously getting trend calculated to 4 years (instead of 5)
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
  r.trend = ddply(subset(ry, year >= year_min), .(region_id), function(x)
    {
      if (length(na.omit(x$status))>1) {
        # use only last valid 5 years worth of status data since year_min
        d = data.frame(status=x$status, year=x$year)[tail(which(!is.na(x$status)), 5),]
        trend = coef(lm(status ~ year, d))[['year']] / 100
      } else {
        trend = NA
      }
      return(data.frame(trend=trend))
    })
  
  # return scores
  scores = r.status %.%
    select(region_id, score=status) %.%
    mutate(dimension='status') %.%
    rbind(
      r.trend %.%
        select(region_id, score=trend) %.%
        mutate(dimension='trend')) %.%
    mutate(goal='AO') # dlply(scores, .(dimension), summary)
  return(scores)  
}


NP = function(scores, layers, 
              status_year=2008, 
              trend_years = list('corals'=2003:2007,'ornamentals'=2003:2007,'shells'=2003:2007,
                                 'fish_oil'=2004:2008,'seaweeds'=2004:2008,'sponges'=2004:2008)){
  # 2013: NP(layers, status_year=2009, trend_years = list('corals'=2004:2008,'ornamentals'=2004:2008,'shells'=2004:2008, 'fish_oil'=2005:2009,'seaweeds'=2005:2009,'sponges'=2005:2009))
  # 2012: NP(layers, status_year=2008, trend_years = list('corals'=2003:2007,'ornamentals'=2003:2007,'shells'=2003:2007, 'fish_oil'=2004:2008,'seaweeds'=2004:2008,'sponges'=2004:2008))
    
  # layers
  lyrs = list('rky' = c('rnky_np_harvest_relative'    = 'H'),
              'rk'  = c('rnk_np_sustainability_score' = 'S',
                        'rnk_np_weights_combo'        = 'w'))
  lyr_names = sub('^\\w*\\.', '', names(unlist(lyrs))) 
  
  # cast data
  D = SelectLayersData(layers, layers=lyr_names)
  rky = rename(dcast(D, id_num + category + year ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['rky']]))),
               c('id_num'='region_id', 'category'='product', lyrs[['rky']]))
  rk  = rename(dcast(D, id_num + category ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['rk']]))),
               c('id_num'='region_id', 'category'='product', lyrs[['rk']]))
  
  # get FIS status
  r = scores %.%
    filter(goal=='FIS' & dimension=='status') %.%
    select(region_id, fis_status=score)
  
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
  scores = r.status %.%
    select(region_id, score=status) %.%
    mutate(dimension='status') %.%
    rbind(
      r.trend %.%
        select(region_id, score=trend) %.%
        mutate(dimension='trend')) %.%
    mutate(goal='NP') %.%
    rbind(scores)
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

LIV.ECO.2012n = function(){

  #   ld.csv=layers_data.csv, 
  #   LIV.status.csv = file.path(dir.results, sprintf('LIV_status_%s.csv', sfx.scenario)),
  #   LIV.trend.csv  = file.path(dir.results, sprintf('LIV_trend_%s.csv' , sfx.scenario)),
  #   ECO.status.csv = file.path(dir.results, sprintf('ECO_status_%s.csv', sfx.scenario)),
  #   ECO.trend.csv  = file.path(dir.results, sprintf('ECO_trend_%s.csv' , sfx.scenario)),
  #   liv_adj_year=2009, eco_adj_min_year=2000
  
  
  # database connection
  pg = dbConnect(dbDriver("PostgreSQL"), host='neptune.nceas.ucsb.edu', dbname='ohi_nature2012', user='bbest') # assumes password in ~/.pgpass
  dbSendQuery(pg, 'SET search_path TO global_li, global; SET ROLE TO ohi;')
  
  # TODO: remove Brazil entries
  # TODO: i_eco_rev_adj_gdp -> cnky_eco_rev_whence in functions.R
  # i_eco_rev_adj_gdp     country_id,year,value_num            Economies model variable  revenue adjustment (GDP)	                   6F	  USD	    ECO	6F_eco_rev_adj_gdp.csv
  
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
  
  # QUICK HACK to consume Nature 2012 results as a placeholder
  d = read.csv(file.path(root.data ,'model/GL-NCEAS-LayersDisaggregated_v2013a/data/rgn_results_2012n_long.csv'), na.strings=''); head(d)
  
  
  
}

LIV.ECO.2013 = function(layers){
    
  # DEBUG: initialize -----
  
  # arguments
  yr = 2013
  
  library(devtools)
  library(RPostgreSQL)
  
  #wd = '~/Code/ohicore'
  #setwd(wd)
  load_all()
  
  #library(RCurl)
  library(dplyr)
  
  # paths
  dir_data  = '/Volumes/data_edit'
  dir_local = '/Volumes/local_edit'
  
  # db for comparison
  pg = dbConnect(dbDriver("PostgreSQL"), host='neptune.nceas.ucsb.edu', dbname='ohi_global2013', user='bbest') # assumes password in ~/.pgpass
  dbSendQuery(pg, 'SET search_path TO global_li, global; SET ROLE TO ohi;')
    
  # inputs
  status_model_curref = read.csv(file.path(dir_data,  sprintf('model/GL-NCEAS-Livelihoods_2012/data_%d/global_li_status_model_curref.csv', yr)), na.strings='') # head(status_model_curref)
  ppp                 = read.csv(file.path(dir_data, 'model/GL-NCEAS-Livelihoods_v2013a/data/country_gdppcppp_2013a.csv'), na.strings='') # head(ppp); dim(ppp); summary(ppp)
  
  
  # model... ----
  
  # status_model_combined: jobs, rev
  status_jobs_rev = status_model_curref %.%
    filter(ref_base_value != 0 & ref_adj_value != 0 & metric %in% c('jobs', 'rev')) %.%
    group_by(metric, iso3166) %.%
    summarise(
      score    = (sum(cur_base_value, na.rm=T) / sum(ref_base_value, na.rm=T)) / (mean(cur_adj_value, na.rm=T) / mean(ref_adj_value, na.rm=T)),
      n_sector = n()) %.%
    arrange(metric, iso3166)
  
  # status_model_combined: wage t0
  t0 = status_model_curref %.%
    filter(metric=='wage' & ref_base_value != 0 & ref_adj_value != 0) %.%
    mutate(w_prime_i = (cur_base_value / ref_base_value) / (cur_adj_value / ref_adj_value)) %.%
    select(metric, iso3166, sector, w_prime_i) %.%
    group_by(metric, iso3166) %.%
    summarise(w_prime  = mean(w_prime_i, na.rm=T),
              n_sector = n()) %.%
    arrange(metric, iso3166)
  
  # status_model_combined: wage p
  p = ppp %.%
    rename(c(ISO3166='iso3166',YEAR='year',VALUE='value')) %.%
    arrange(iso3166, year) %.%
    group_by(iso3166) %.%
    summarise(year      = last(year),
              ppp_value = last(value)) %.%
    filter(!is.na(ppp_value)) %.%
    arrange(iso3166)
  
  # status_model_combined: wage t2 = t0 + p
  t2 = t0 %.%
    merge(p, by='iso3166') %.%
    mutate(score = w_prime * ppp_value) %.%
    select(metric, iso3166, score, n_sector) %.%
    arrange(metric, iso3166)
  
  # status_model_combined: wage
  max_wage_score = max(t2$score, na.rm=T)
  status_wage = t2 %.%
    mutate(score = score / max_wage_score)
  
  # status_model_combined: wage union with jobs, rev
  status_model_combined = ungroup(status_jobs_rev) %.%
    rbind(status_wage)
  
  # status_score R
  status_score = status_model_combined %.%
    # liv
    dcast(iso3166 ~ metric, value.var='score') %.%
    group_by(iso3166) %.%
    mutate(
      value     = mean(c(jobs, wage), na.rm=T),
      component = 'livelihood') %.%
    select(iso3166, component, value) %.%
    ungroup() %.% 
    arrange(iso3166, component, value) %.%
    # eco
    rbind(status_model_combined %.%
            filter(metric=='rev') %.%
            mutate(
              value     = score,
              component = 'economy') %.%
            select(iso3166, component, value)) %.%
    # order
    filter(!is.na(value)) %.%
    arrange(iso3166, component) %.%
    # clamp
    mutate(score = pmin(value, 1))
  
  # DEBUG: status_score compare ---
  status_score_pg = dbGetQuery(pg, "SELECT * FROM status_score ORDER BY iso3166, component")
  head(status_score   ); dim(status_score   ); summary(status_score   )
  head(status_score_pg); dim(status_score_pg); summary(status_score_pg)
  
  # TODO NEXT: 
  #  * aggregate country_2012 to region_2012
  #    - original: /Volumes/local_edit/src/model/global2013/livelihoods/status
  #    - new: aggregate_by_country_weighted(), eg above
  #  * disaggregate region_2012 to region_2013
  #    - /Volumes/data_edit/model/GL-NCEAS-LayersDisaggregated_v2013a/digest_disaggregate.R
  
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
  scores.LE = scores %.% 
    filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend')) %.% # did old use likely future?
    dcast(region_id + dimension ~ goal, value.var='score') %.%
    mutate(score = rowMeans(cbind(ECO, LIV), na.rm=T)) %.%
    select(region_id, dimension, score) %.%
    mutate(goal  = 'LE')

  # rbind to all scores
  scores = scores %.%
    rbind(scores.LE)  
  
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
  r.trend = ddply(subset(r.yrs, year %in% trend_years), .(region_id), function(x){
    data.frame(
      trend = min(1, max(0, 5 * coef(lm(pct_pa ~ year, data=x))[['year']])))})      
  
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
  
#  browser()
  
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