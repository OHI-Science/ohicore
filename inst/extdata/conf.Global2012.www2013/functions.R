Setup = function(){
  
  extra.packages.required = c('zoo') # zoo for MAR()
  
  # install packages if needed
  for (p in extra.packages.required){
    if (!suppressWarnings(library(p, character.only=T, logical.return=T))){
      cat(sprintf('\n\nInstalling %s...\n', p))
      install.packages(p)
      require(p, character.only=T)
    }
  }
}

FIS = function(layers, status_year=2010){
  
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

MAR = function(layers, status_years=2004:2010){  
  
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
  #browser()
  
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
    mutate(fis_status = score / 100) %.%
    select(region_id, fis_status)
  
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
  scores.NP = r.status %.%
    select(region_id, score=status) %.%
    mutate(dimension='status') %.%
    rbind(
      r.trend %.%
        select(region_id, score=trend) %.%
        mutate(dimension='trend')) %.%
    mutate(goal='NP')
  return(scores.NP)  
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


TR.old = function(layers){
  
  # scores
  return(cbind(rename(SelectLayersData(layers, layers=c('rn_tr_status'='status','rn_tr_trend'='trend'), narrow=T),
                      c(id_num='region_id', layer='dimension', val_num='score')), 
               data.frame('goal'='TR')))
}

TR = function(layers, year_max){
  
  # formula:
  #   E = Ed / (L - (L*U))
  #   Xtr = E * S 
  # 
  # Ed = Direct employment in tourism (tr_jobs_tourism): ** this has not been gapfilled. We thought it would make more sense to do at the status level.
  # L = Total labor force (tr_jobs_total) 2013: max(year)=2011; 2012: max(year)=2010
  # U = Unemployment (tr_unemployment) 2013: max(year)=2011; 2012: max(year)=2010 
  # so E is tourism  / employed
  # S = Sustainability index (tr_sustainability)
  #
  # based on model/GL-NCEAS-TR_v2013a: TRgapfill.R, TRcalc.R...
  # spatial gapfill simply avg, not weighted by total jobs or country population?
  
  # DEBUG
  #   library(devtools); load_all()
  #   yr=2013
  #   scenario=sprintf('Global%d.www2013', yr)
  #   conf = ohicore::Conf(sprintf('inst/extdata/conf.%s', scenario))
  #   layers     = Layers(layers.csv = sprintf('inst/extdata/layers.%s.csv', scenario), 
  #                       layers.dir = sprintf('inst/extdata/layers.%s'    , scenario))
  #   year_max = 2011 # for 2013; 2010 for 2012
  
  # get regions
  rgns = layers$data[[conf$config$layer_region_labels]] %.%
    select(rgn_id, rgn_label = label)
  
  # merge layers and calculate score
  d = layers$data[['tr_jobs_tourism']] %.%
    select(rgn_id, year, Ed=count) %.%
    arrange(rgn_id, year) %.%
    merge(
      layers$data[['tr_jobs_total']] %.%
        select(rgn_id, year, L=count),
      by=c('rgn_id','year'), all=T) %.%
    merge(
      layers$data[['tr_unemployment']] %.%
        select(rgn_id, year, U=percent) %.%
        mutate(U = U/100),
      by=c('rgn_id','year'), all=T) %.%    
    merge(
      layers$data[['tr_sustainability']] %.%
        select(rgn_id, S=score),
      by=c('rgn_id'), all=T)  %.%
    mutate(
      E   = Ed / (L - (L * U)),
      Xtr = E * S ) %.%
    merge(rgns, by='rgn_id') %.%
    select(rgn_id, rgn_label, year, Ed, L, U, S, E, Xtr)
  
  # compare with pre-gapfilled data
  if (!file.exists( sprintf('inst/extdata/reports%d.www2013', yr))) dir.create(sprintf('inst/extdata/reports%d.www2013', yr))
  
  o = read.csv('/Volumes/data_edit/model/GL-NCEAS-TR_v2013a/raw/TR_status_pregap_Sept23.csv', na.strings='') %.%
    melt(id='rgn_id', variable.name='year', value.name='Xtr_o') %.%
    mutate(year = as.integer(sub('x_TR_','', year, fixed=T))) %.%
    arrange(rgn_id, year)
  
  vs = o %.%
    merge(
      expand.grid(list(
        rgn_id = rgns$rgn_id,
        year   = 2006:2011)),
      by=c('rgn_id', 'year'), all=T) %.%
    merge(d, by=c('rgn_id','year')) %.%
    mutate(Xtr_dif = Xtr - Xtr_o) %.% 
    select(rgn_id, rgn_label, year, Xtr_o, Xtr, Xtr_dif, E, Ed, L, U, S) %.%
    arrange(rgn_id, year)
  write.csv(vs, sprintf('inst/extdata/reports%d.www2013/tr-%d_0-pregap-vs_details.csv', yr, yr), row.names=F, na='')
  
  vs_rgn = vs %.%
    group_by(rgn_id) %.%
    summarize(
      n_notna_o   = sum(!is.na(Xtr_o)),
      n_notna     = sum(!is.na(Xtr)),
      dif_avg     = mean(Xtr, na.rm=T) - mean(Xtr_o, na.rm=T),
      Xtr_2011_o  = last(Xtr_o),
      Xtr_2011    = last(Xtr),
      dif_2011    = Xtr_2011 - Xtr_2011_o) %.%
    filter(n_notna_o !=0 | n_notna!=0) %.%
    arrange(desc(abs(dif_2011)), Xtr_2011, Xtr_2011_o)
  write.csv(vs_rgn, sprintf('inst/extdata/reports%d.www2013/tr-%d_0-pregap-vs_summary.csv', yr, yr), row.names=F, na='')
  
  # get georegions for gapfilling
  georegions = layers$data[['rnk_rgn_georegions']] %.%
    dcast(rgn_id ~ level, value.var='georgn_id')
  
  # setup data for georegional gapfilling (remove Antarctica rgn_id=213) # load_all()
  d_g = gapfill_georegions(
    d %.%
      filter(rgn_id!=213) %.%
      select(rgn_id, year, Xtr),
    georegions)
  write.csv(attr(d_g, 'gapfill_georegions'), sprintf('inst/extdata/reports%d.www2013/tr-%d_1-gapfilling.csv', yr, yr), row.names=F, na='')
  
  # filter: limit to 5 intervals (6 years worth of data)
  #   NOTE: original 2012 only used 2006:2010 whereas now we're using 2005:2010
  d_g_f = d_g %.%
    filter((year <= year_max) & (year >= (year_max - 5)) )
  
  # rescale for
  #   status: 95 percentile value across all regions and filtered years
  #   trend: use the value divided by max bc that way it's rescaled but not capped to a lower percentile (otherwise the trend calculated for regions with capped scores, i.e. those at or above the percentile value, would be spurious)
  Xtr_95  = quantile(d_g_f$Xtr, probs=0.95, na.rm=T)
  Xtr_max = max(d_g_f$Xtr, na.rm=T)
  d_g_f_r = d_g_f %.%    
    mutate(
      Xtr_r95  = ifelse(Xtr / Xtr_95 > 1, 1, Xtr / Xtr_95), # rescale to 95th percentile, cap at 1
      Xtr_rmax = Xtr / Xtr_max )                            # rescale to max value   
  write.csv(d_g_f_r, sprintf('inst/extdata/reports%d.www2013/tr-%d_2-filtered-rescaled.csv', yr, yr), row.names=F, na='')
  
  # calculate trend
  d_t = d_g_f_r %.%
    arrange(year, rgn_id) %.%
    group_by(rgn_id) %.%
    do(mod = lm(Xtr_rmax ~ year, data = .)) %>%
    do(data.frame(
      rgn_id = .$rgn_id,
      dimension = 'trend',
      score = max(min(coef(.$mod)[['year']] * 5, 1), -1)))
  
  # get status (as last year's value)
  d_s = d_g_f_r %.%
    arrange(year, rgn_id) %.%
    group_by(rgn_id) %.%
    summarize(
      dimension = 'status',
      score = last(Xtr_r95) * 100)
  
  # bind rows
  d_b = rbind(d_t, d_s) %.%
    mutate(goal = 'TR')  
  
  # assign NA for uninhabitated islands
  unpopulated = layers$data[['rny_le_popn']] %.%
    group_by(rgn_id) %.%
    filter(count==0) %.%
    select(rgn_id)  
  d_b_u = mutate(d_b, score = ifelse(rgn_id %in% unpopulated$rgn_id, NA, score))  
  
  # replace North Korea value with 0
  d_b_u$score[d_b_u$rgn_id == 21] = 0
  
  # final scores
  scores = d_b_u %.%
    select(region_id=rgn_id, goal, dimension, score)
  
  # compare with original scores
  csv_o = '/Volumes/data_edit/git-annex/Global/NCEAS-OHI-Scores-Archive/scores/scores.Global2013.www2013_2013-10-09.csv'
  o = read.csv(csv_o, na.strings='NA', row.names=1) %.% 
    filter(goal %in% c('TR') & dimension %in% c('status','trend') & region_id!=0) %.% 
    select(goal, dimension, region_id, score_o=score)
  
  vs = scores %.%
    merge(o, all=T, by=c('goal','dimension','region_id')) %.%
    merge(
      rgns %.%
        select(region_id=rgn_id, region_label=rgn_label), 
      all.x=T) %.%
    mutate(
      score_dif    = score - score_o,
      score_notna  = is.na(score)!=is.na(score_o)) %.%  
    filter(abs(score_dif) > 0.01 | score_notna == T) %.%
    arrange(desc(dimension), desc(abs(score_dif))) %.%
    select(dimension, region_id, region_label, score_o, score, score_dif)
  
  # output comparison
  write.csv(vs, sprintf('inst/extdata/reports%d.www2013/tr-%d_3-scores-vs.csv', yr, yr), row.names=F, na='')
  
  
  return(scores)
}


LIV = function(layers){
#browser()  
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

  # replacing 2012 scores for ECO and LIV with 2013 data (email Feb 28, Ben H.)
  # ECO: Eritrea (just this one country)
  # LIV: Eritrea, Anguilla, Bermuda, Egypt, Ghana, Indonesia, Iceland, Saint Kitts, 
  #      Sri Lanka, Brunei, Malaysia, Trinidad & Tobago, and Taiwan
  
  # replacement data and region names
  scores_2013 <- read.csv("inst/extdata/scores.Global2013.www2013.csv")  
  rgns = SelectLayersData(layers, layers='rtk_rgn_labels', narrow=T) %.%
    select(region_id=id_num, label=val_chr) %.%
    arrange(label)
    
  # ECO
  ECO_rgn_id_replace = subset(rgns, label=='Eritrea', 'region_id', drop=T)
  scores = scores %.%
    filter(!(goal=='ECO' & dimension=='score' & region_id==ECO_rgn_id_replace)) %.%
    rbind(
      scores_2013 %.%
        filter(goal=='ECO' & dimension=='score' & region_id==ECO_rgn_id_replace))
    
  # LIV
  LIV_rgns_label_replace = c('Eritrea','Anguilla','Bermuda','Egypt','Ghana','Indonesia','Iceland','Saint Kitts and Nevis','Sri Lanka','Brunei','Malaysia','Trinidad and Tobago','Taiwan')
  LIV_rgns_id_replace = subset(rgns, label %in% LIV_rgns_label_replace, 'region_id', drop=T)
  stopifnot(length(LIV_rgns_label_replace)==length(LIV_rgns_id_replace))
  scores = scores %.%
    filter(!(goal=='LIV' & dimension=='score' & region_id %in% LIV_rgns_id_replace)) %.%
    rbind(
      scores_2013 %.%
        filter(goal=='LIV' & dimension=='score' & region_id %in% LIV_rgns_id_replace))

  # calculate LE scores
  scores.LE = scores %.% 
    filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend','score','future')) %.%
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
  # 2013: LSP(layers, status_year=2012, trend_years=2005:2009)
  # 2012: LSP(layers, status_year=2011, trend_years=2004:2008)  
    
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
  #r.trend = ddply(subset(r.yrs, year %in% trend_years), .(region_id), summarize,
  #                annual = lm(pct_pa ~ year)[['coefficients']][['year']],
  #                trend = min(1, max(0, 5 * annual)))
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