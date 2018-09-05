## functions.R.
## Each OHI goal model is a separate R function. The function name is the 2- or 3- letter code for each goal or subgoal; for example, FIS is the Fishing subgoal of Food Provision (FP).

## These models originate from Global OHI assessments (ohi-global), and should be tailored to represent the charastics, priorities, and data in your assessment area.

FIS <- function(layers){

  ## read in layers
  ## catch data
  c  <- layers$data$fis_meancatch %>%
    select(region_id = rgn_id, year, stock_id_taxonkey, catch = mean_catch)

  ## b_bmsy data
  b  <- layers$data$fis_b_bmsy %>%
    select(region_id = rgn_id, stock_id, year, bbmsy)

  ## set data year for assessment
  data_year <- max(c$year)


  # The following stocks are fished in multiple regions and have high b/bmsy values
  # Due to the underfishing penalty, this actually penalizes the regions that have the highest
  # proportion of catch of these stocks.  The following corrects this problem:
  #  filter(b, stock_id %in% c('Katsuwonus_pelamis-71', 'Clupea_harengus-27', 'Trachurus_capensis-47'))

  high_bmsy <- c('Katsuwonus_pelamis-71', 'Clupea_harengus-27', 'Trachurus_capensis-47', 'Sardinella_aurita-34', 'Scomberomorus_cavalla-31')

  b <- b %>%
    mutate(bbmsy = ifelse(stock_id %in% high_bmsy, 1, bbmsy))


  # separate out the stock_id and taxonkey:
  c <- c %>%
    mutate(stock_id_taxonkey = as.character(stock_id_taxonkey)) %>%
    mutate(taxon_key = stringr::str_sub(stock_id_taxonkey, -6, -1)) %>%
    mutate(stock_id = substr(stock_id_taxonkey, 1, nchar(stock_id_taxonkey)-7)) %>%
    mutate(catch = as.numeric(catch)) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(region_id = as.numeric(as.character(region_id))) %>%
    mutate(taxon_key = as.numeric(as.character(taxon_key))) %>%
    select(region_id, year, stock_id, taxon_key, catch)

  # general formatting:
  b <- b %>%
    mutate(bbmsy = as.numeric(bbmsy)) %>%
    mutate(region_id = as.numeric(as.character(region_id))) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(stock_id = as.character(stock_id))


  ####
  # STEP 1. Calculate scores for Bbmsy values
  ####
  #  *************NOTE *****************************
  #  These values can be altered
  #  ***********************************************
  alpha <- 0.5
  beta <- 0.25
  lowerBuffer <- 0.95
  upperBuffer <- 1.05

  b$score = ifelse(b$bbmsy < lowerBuffer, b$bbmsy,
                   ifelse (b$bbmsy >= lowerBuffer & b$bbmsy <= upperBuffer, 1, NA))
  b$score = ifelse(!is.na(b$score), b$score,
                   ifelse(1 - alpha*(b$bbmsy - upperBuffer) > beta,
                          1 - alpha*(b$bbmsy - upperBuffer),
                          beta))


  ####
  # STEP 1. Merge the b/bmsy data with catch data
  ####
  data_fis <- c %>%
    left_join(b, by=c('region_id', 'stock_id', 'year')) %>%
    select(region_id, stock_id, year, taxon_key, catch, bbmsy, score)


  ###
  # STEP 2. Estimate scores for taxa without b/bmsy values
  # Median score of other fish in the region is the starting point
  # Then a penalty is applied based on the level the taxa are reported at
  ###

  ## this takes the median score within each region
  data_fis_gf <- data_fis %>%
    group_by(region_id, year) %>%
    mutate(Median_score = quantile(score, probs=c(0.5), na.rm=TRUE)) %>%
    ungroup()

  ## this takes the median score across all regions (when no stocks have scores within a region)
  data_fis_gf <- data_fis_gf %>%
    group_by(year) %>%
    mutate(Median_score_global = quantile(score, probs=c(0.5), na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(Median_score = ifelse(is.na(Median_score), Median_score_global, Median_score)) %>%
    select(-Median_score_global)

  #  *************NOTE *****************************
  #  In some cases, it may make sense to alter the
  #  penalty for not identifying fisheries catch data to
  #  species level.
  #  ***********************************************

  penaltyTable <- data.frame(TaxonPenaltyCode=1:6,
                             penalty=c(0.1, 0.25, 0.5, 0.8, 0.9, 1))

  data_fis_gf <- data_fis_gf %>%
    mutate(TaxonPenaltyCode = as.numeric(substring(taxon_key, 1, 1))) %>%
    left_join(penaltyTable, by='TaxonPenaltyCode') %>%
    mutate(score_gf = Median_score * penalty) %>%
    mutate(score_gapfilled = ifelse(is.na(score), "Median gapfilled", "none")) %>%
    mutate(score = ifelse(is.na(score), score_gf, score))


  gap_fill_data <- data_fis_gf %>%
    mutate(gap_fill = ifelse(is.na(penalty), "none", "median")) %>%
    select(region_id, stock_id, taxon_key, year, catch, score, gap_fill) %>%
    filter(year == data_year)

  status_data <- data_fis_gf %>%
    select(region_id, stock_id, year, catch, score)


  ###
  # STEP 4. Calculate status for each region
  ###

  # 4a. To calculate the weight (i.e, the relative catch of each stock per region),
  # the mean catch of taxon i is divided by the
  # sum of mean catch of all species in region/year

  status_data <- status_data %>%
    group_by(year, region_id) %>%
    mutate(SumCatch = sum(catch)) %>%
    ungroup() %>%
    mutate(wprop = catch/SumCatch)

  status_data <- status_data %>%
    group_by(region_id, year) %>%
    summarize(status = prod(score^wprop)) %>%
    ungroup()

  ###
  # STEP 5. Get yearly status and trend
  ###

  status <-  status_data %>%
    filter(year==data_year) %>%
    mutate(
      score     = round(status*100, 1),
      dimension = 'status') %>%
    select(region_id, score, dimension)


  # calculate trend

  trend_years <- (data_year-4):(data_year)
  first_trend_year <- min(trend_years)

  trend <- status_data %>%
    filter(year %in% trend_years) %>%
    group_by(region_id) %>%
    do(mdl = lm(status ~ year, data=.),
       adjust_trend = .$status[.$year == first_trend_year]) %>%
    summarize(region_id,
              score = round(coef(mdl)['year']/adjust_trend * 5, 4),
              dimension = 'trend') %>%
    ungroup() %>%
    mutate(score = ifelse(score > 1, 1, score)) %>%
    mutate(score = ifelse(score < (-1), (-1), score))

  # assemble dimensions
  scores <- rbind(status, trend) %>%
    mutate(goal='FIS') %>%
    data.frame()

  return(scores)
}

MAR <- function(layers){

  ## read in layers
  harvest_tonnes <- layers$data$mar_harvest_tonnes %>%
    select(region_id = rgn_id, taxa_code, year, tonnes)

  sustainability_score <- layers$data$mar_sustainability_score %>%
    select(region_id = rgn_id, taxa_code, sust_coeff)

  popn_inland25mi <- layers$data$mar_coastalpopn_inland25mi %>%
    select(region_id = rgn_id, year, popsum) %>%
    mutate(popsum = popsum + 1)  # so 0 values do not cause errors when logged

  ## set data year for assessment
  data_year <- 2014

  ## combine layers
  rky <-  harvest_tonnes %>%
    left_join(sustainability_score, by = c('region_id', 'taxa_code'))

  # fill in gaps with no data
  rky <- spread(rky, year, tonnes)
  rky <- gather(rky, "year", "tonnes", 4:68) # ncol(rky)


  # 4-year rolling mean of data
  m <- rky %>%
    mutate(year = as.numeric(as.character(year))) %>%
    group_by(region_id, taxa_code, sust_coeff) %>%
    arrange(region_id, taxa_code, year) %>%
    mutate(sm_tonnes = zoo::rollapply(tonnes, 4, mean, na.rm=TRUE, partial=TRUE)) %>%
    ungroup()

  # smoothed mariculture harvest * sustainability coefficient
  m <- m %>%
    mutate(sust_tonnes = sust_coeff * sm_tonnes)


  # aggregate all weighted timeseries per region, and divide by coastal human population
  ry = m %>%
    group_by(region_id, year) %>%
    summarize(sust_tonnes_sum = sum(sust_tonnes, na.rm=TRUE)) %>%  #na.rm = TRUE assumes that NA values are 0
    left_join(popn_inland25mi, by = c('region_id','year')) %>%
    mutate(mar_pop = sust_tonnes_sum / popsum) %>%
    ungroup()


  # get reference quantile based on argument years
  ref_95pct_data <- ry %>%
    filter(year <= data_year)

  ref_95pct <- quantile(ref_95pct_data$mar_pop, 0.95, na.rm=TRUE)

  # identify reference region_id
  ry_ref = ref_95pct_data %>%
    arrange(mar_pop) %>%
    filter(mar_pop >= ref_95pct)
  message(sprintf('95th percentile for MAR ref pt is: %s\n', ref_95pct))
  message(sprintf('95th percentile region_id for MAR ref pt is: %s\n', ry_ref$region_id[1]))

  ry = ry %>%
    mutate(status = ifelse(mar_pop / ref_95pct > 1,
                           1,
                           mar_pop / ref_95pct))
  status <- ry %>%
    filter(year == data_year) %>%
    select(region_id, status) %>%
    mutate(status = round(status*100, 2))

  trend_years <- (data_year-4):(data_year)
  first_trend_year <- min(trend_years)

  # get MAR trend
  trend = ry %>%
    group_by(region_id) %>%
    filter(year %in% trend_years) %>%
    filter(!is.na(popsum)) %>%
    do(mdl = lm(status ~ year, data=.),
       adjust_trend = .$status[.$year == first_trend_year]) %>%
    summarize(region_id, trend = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
    ungroup()

  trend <- trend %>%
    mutate(trend = ifelse(trend>1, 1, trend)) %>%
    mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
    mutate(trend = round(trend, 4)) %>%
    select(region_id = region_id, score = trend) %>%
    mutate(dimension = "trend")

  # return scores
  scores = status %>%
    select(region_id, score = status) %>%
    mutate(dimension='status') %>%
    rbind(trend) %>%
    mutate(goal='MAR')

  return(scores)
}


FP = function(layers, scores){

  ## read in layers for weights
  w <-  layers$data$fp_wildcaught_weight %>%
    select(region_id = rgn_id, year, w_FIS = w_fis)

  # scores
  s <- scores %>%
    filter(goal %in% c('FIS', 'MAR')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    left_join(w, by="region_id")  %>%
    mutate(w_MAR = 1 - w_FIS) %>%
    mutate(weight = ifelse(goal == "FIS", w_FIS, w_MAR))


  ## Some warning messages due to potential mismatches in data:
  # NA score but there is a weight
  tmp <- filter(s, goal=='FIS' & is.na(score) & (!is.na(w_FIS) & w_FIS!=0) & dimension == "score")
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a FIS weight but no score: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  tmp <- filter(s, goal=='MAR' & is.na(score) & (!is.na(w_MAR) & w_MAR!=0) & dimension == "score")
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a MAR weight but no score: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  # score, but the weight is NA or 0
  tmp <- filter(s, goal=='FIS' & (!is.na(score) & score > 0) & (is.na(w_FIS) | w_FIS==0) & dimension == "score" & region_id !=0)
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a FIS score but no weight: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  tmp <- filter(s, goal=='MAR' & (!is.na(score) & score > 0) & (is.na(w_MAR) | w_MAR==0) & dimension == "score" & region_id !=0)
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a MAR score but no weight: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  s <- s  %>%
    group_by(region_id, dimension) %>%
    summarize(score = weighted.mean(score, weight, na.rm=TRUE)) %>%
    mutate(goal = "FP") %>%
    ungroup() %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
}


AO = function(layers){

  ## placeholder since no data
  Sustainability=1.0

  ## read in layers
  ## access
  access <- layers$data$ao_access %>%
    select(region_id=rgn_id, year, access=value)

  ## need
  need <- layers$data$ao_need %>%
    select(region_id=rgn_id, year, need=value)

  ## combine data
  d <- need %>%
    left_join(access, by=c("region_id", "year"))

  ## set data year for assessment
  data_year <- max(d$year)


  ## model
  d <- d %>%
    mutate(Du = (1 - need) * (1 - access)) %>%
    mutate(status = (1 - Du) * Sustainability)

  # status
  r.status <- d %>%
    filter(year==data_year) %>%
    select(region_id, status) %>%
    mutate(status=status*100)

  # trend

  trend_years <- (data_year-4):(data_year)
  adj_trend_year <- min(trend_years)

  r.trend = d %>%
    group_by(region_id) %>%
    do(mdl = lm(status ~ year, data=., subset=year %in% trend_years),
       adjust_trend = .$status[.$year == adj_trend_year]) %>%
    summarize(region_id, trend = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
    ungroup() %>%
    mutate(trend = ifelse(trend>1, 1, trend)) %>%
    mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
    mutate(trend = round(trend, 4))

  # return scores
  scores = r.status %>%
    select(region_id, score=status) %>%
    mutate(dimension='status') %>%
    rbind(
      r.trend %>%
        select(region_id, score=trend) %>%
        mutate(dimension='trend')) %>%
    mutate(goal='AO')

  return(scores)
}

NP <- function(scores, layers){

  ## read in layers
  r_cyanide <- layers$data$np_cyanide %>% # cyanide & blast used to calculate risk variable
    select(region_id = rgn_id, score)

  r_blast   <- layers$data$np_blast %>%
    select(region_id = rgn_id, score)

  hab_rocky <- layers$data$hab_rockyreef_extent %>%
    select(region_id = rgn_id, habitat, year, km2)

  hab_coral <- layers$data$hab_coral_extent %>% # to calculate exposure variable
    select(region_id = rgn_id, habitat, year, km2)

  ## set data year for assessment
  data_year <- 2013


  ### FIS status for fish oil sustainability
  # FIS_status <- read.csv('scores.csv')%>%  ## this is for troubleshooting
  FIS_status   <-  scores %>%
    filter(goal == 'FIS' & dimension == 'status') %>%
    select(region_id, score)


  ###########################################################.
  ### Here I define five main sub-functions.  The main script that
  ### actually calls these functions is at the very end of the NP section.
  ###   np_rebuild_harvest
  ###   np_calc_exposure
  ###   np_calc_risk
  ###   np_calc_sustainability
  ###   np_calc_scores

  np_rebuild_harvest <- function(layers) {
    ### Reassembles NP harvest information from separate data layers:
    ### [rgn_name  rgn_id  product  year  tonnes  tonnes_rel  prod_weight]
    #########################################.

    ## load data from layers dataframe
    rgns <- layers$data$rgn_labels %>%
      select(region_id = rgn_id, type, label)

    h_tonnes <- layers$data$np_harvest_tonnes %>%
      select(region_id = rgn_id, product, year, tonnes)

    h_tonnes_rel <- layers$data$np_harvest_tonnes_relative %>%
      select(region_id = rgn_id, product, year, tonnes_rel)

    h_w <- layers$data$np_harvest_product_weight %>%
      select(region_id = rgn_id, product, weight)

    # merge harvest in tonnes and usd
    np_harvest <- h_tonnes %>%
      full_join(
        h_tonnes_rel,
        by=c('region_id', 'product', 'year')) %>%
      left_join(
        h_w %>%
          select(region_id, product, prod_weight = weight),
        by=c('region_id', 'product')) %>%
      left_join(
        rgns %>%
          select(region_id, rgn_name=label),
        by='region_id') %>%
      select(
        rgn_name, region_id, product, year,
        tonnes, tonnes_rel, prod_weight)

    return(np_harvest)
  }


  np_calc_exposure <- function(np_harvest, hab_extent, FIS_status) {
    ### calculates NP exposure based on habitats (for corals, seaweeds,
    ### ornamentals, shells, sponges).
    ### Returns the first input data frame with a new column for exposure:
    ### [region_id rgn_name product year tonnes tonnes_rel prod_weight exposure]
    #########################################.

    ### Determine Habitat Areas for Exposure
    ### extract habitats used
    hab_coral <- hab_coral %>%
      select(region_id, km2)
    hab_rocky   <- hab_rocky %>%
      select(region_id, km2)

    ### area for products having single habitats for exposure
    area_single_hab <- bind_rows(
      # corals in coral reef
      np_harvest %>%
        filter(product == 'corals') %>%
        left_join(
          hab_coral %>%
            filter(km2 > 0) %>%
            select(region_id, km2), by = 'region_id'),
      ### seaweeds in rocky reef
      np_harvest %>%
        filter(product == 'seaweeds') %>%
        left_join(
          hab_rocky %>%
            filter(km2 > 0) %>%
            select(region_id, km2), by = 'region_id'))

    ### area for products in both coral and rocky reef habitats: shells, ornamentals, sponges
    area_dual_hab <- np_harvest %>%
      filter(product %in% c('shells', 'ornamentals','sponges')) %>%
      left_join(
        hab_coral %>%
          filter(km2 > 0) %>%
          select(region_id, coral_km2 = km2),
        by = 'region_id') %>%
      left_join(
        hab_rocky %>%
          filter(km2 > 0) %>%
          select(region_id, rocky_km2 = km2),
        by = 'region_id') %>%
      rowwise() %>%
      mutate(
        km2 = sum(c(rocky_km2, coral_km2), na.rm = TRUE)) %>%
      filter(km2 > 0)

    ### Determine Exposure
    ### exposure: combine areas, get tonnes / area, and rescale with log transform
    np_exp <-
      bind_rows(
        area_single_hab,
        area_dual_hab %>%
          select(-rocky_km2, -coral_km2)) %>%
      mutate(
        expos_raw = ifelse(tonnes > 0 & km2 > 0, (tonnes / km2), 0)) %>%
      group_by(product) %>%
      mutate(
        expos_prod_max = (1 - .35)*max(expos_raw, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(
        exposure = (log(expos_raw + 1) / log(expos_prod_max + 1)),
        exposure = ifelse(exposure > 1, 1, exposure)) %>%
      select(-km2, -expos_raw, -expos_prod_max)
    ### clean up columns

    gap_fill <- np_exp %>%
      mutate(gap_fill = ifelse(is.na(exposure), "prod_average", 0)) %>%
      select(region_id, product, year, gap_fill)


    ### add exposure for countries with (habitat extent == NA)
    np_exp <- np_exp %>%
      group_by(product) %>%
      mutate(mean_exp = mean(exposure, na.rm = TRUE)) %>%
      mutate(exposure = ifelse(is.na(exposure), mean_exp, exposure)) %>%
      select(-mean_exp) %>%
      ungroup() %>%
      mutate(product = as.character(product))


    return(np_exp)
  }

  np_calc_risk <- function(np_exp, r_cyanide, r_blast) {
    ### calculates NP risk based on:
    ###   ornamentals:      risk = 1 if blast or cyanide fishing
    ###   corals:           risk = 1 for all cases
    ###   shells, sponges:  risk = 0 for all cases
    ###   others:           risk = NA?
    ### Returns a data frame of risk, by product, by region:
    ###
    #########################################.

    ### Determine Risk

    ### risk for ornamentals set to 1 if blast or cyanide fishing present, based on Nature 2012 code
    ###  despite Nature 2012 Suppl saying Risk for ornamental fish is set to the "relative intensity of cyanide fishing"
    risk_orn <- r_cyanide %>%
      filter(!is.na(score) & score > 0) %>%
      select(region_id, cyanide = score) %>%
      merge(
        r_blast %>%
          filter(!is.na(score) & score > 0) %>%
          select(region_id, blast = score),
        all = TRUE) %>%
      mutate(ornamentals = 1)

    ### risk as binary
    np_risk <-
      ### fixed risk: corals (1), sponges (0) and shells (0)
      data.frame(
        region_id  = unique(np_harvest$region_id),
        corals  = 1,
        sponges = 0,
        shells  = 0) %>%
      ### ornamentals
      left_join(
        risk_orn %>%
          select(region_id, ornamentals),
        by = 'region_id')  %>%
      mutate(
        ornamentals = ifelse(is.na(ornamentals), 0, ornamentals)) %>%
      gather(product, risk, -region_id) %>%
      mutate(product = as.character(product))

    return(np_risk)
  }

  np_calc_sustainability <- function(np_exp, np_risk) {
    ### calculates NP sustainability coefficient for each natural product, based
    ### on (1 - mean(c(exposure, risk))).  Returns first input dataframe with
    ### new columns for sustainability coefficient, and sustainability-adjusted
    ### NP product_status:
    ### [region_id  rgn_name  product  year  prod_weight  sustainability  product_status]
    #########################################.

    ### join Exposure (with harvest) and Risk
    np_sust <- np_exp %>%
      left_join(
        np_risk,
        by = c('region_id', 'product')) %>%
      rowwise() %>%
      mutate(sustainability = 1 - mean(c(exposure, risk), na.rm = TRUE))

    ### add in fish_oil sustainability based on FIS scores calculated above:
    ### add fish_oil (no exposure calculated, sustainability is based on FIS score only, and not exposure/risk components)
    fish_oil_sust <-   FIS_status %>%
      mutate(sustainability = score / 100) %>%
      mutate(sustainability = ifelse(is.na(sustainability), 0, sustainability)) %>%
      select(region_id, sustainability)

    np_sus_fis_oil <- np_harvest %>%
      filter(product=='fish_oil') %>%
      mutate(exposure = NA) %>%
      mutate(risk = NA) %>%
      left_join(fish_oil_sust, by='region_id') %>%
      mutate(product = as.character(product))

    np_exp <- np_sust %>%
      bind_rows(np_sus_fis_oil)


    ### calculate rgn-product-year status
    np_sust <- np_sust %>%
      mutate(product_status = tonnes_rel * sustainability) %>%
      filter(rgn_name != 'DISPUTED') %>%
      select(-tonnes, -tonnes_rel, -risk, -exposure) %>%
      ungroup()

    return(np_sust)
  }

  np_calc_scores <- function(np_sust, data_year) {
    ### Calculates NP status for all production years for each region, based
    ### upon weighted mean of all products produced.
    ### From this, reports the most recent year as the NP status.
    ### Calculates NP trend for each region, based upon slope of a linear
    ### model over the past six years inclusive (five one-year intervals).
    ### Returns data frame with status and trend by region:
    ### [goal   dimension   region_id   score]
    #########################################.

    ### Calculate status, trends
    ### aggregate across products to rgn-year status, weighting by usd_rel
    np_status_all <- np_sust %>%
      filter(!is.na(product_status) & !is.na(prod_weight)) %>%
      select(rgn_name, region_id, year, product, product_status, prod_weight) %>%
      group_by(region_id, year) %>%
      summarize(status = weighted.mean(product_status, prod_weight)) %>%
      filter(!is.na(status)) %>% # 1/0 produces NaN
      ungroup()

    ### get current status
    np_status_current <- np_status_all %>%
      filter(year == data_year & !is.na(status)) %>%
      mutate(
        dimension = 'status',
        score     = round(status,4) * 100) %>%
      select(region_id = region_id, dimension, score)
    stopifnot(
      min(np_status_current$score, na.rm = TRUE) >= 0,
      max(np_status_current$score, na.rm = TRUE) <= 100)

    ### trend

    trend_years <- (data_year-4):(data_year)

    adj_trend_year <- min(trend_years)

    r.trend = np_status_all %>%
      filter(year %in% trend_years) %>%
      unique() %>%
      group_by(region_id) %>%
      do(mdl = lm(status ~ year, data=.),
         adjust_trend = .$status[.$year == adj_trend_year]) %>%
      summarize(region_id, score = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
      ungroup() %>%
      mutate(score = ifelse(score>1, 1, score)) %>%
      mutate(score = ifelse(score<(-1), (-1), score)) %>%
      mutate(score = round(score, 4)) %>%
      mutate(dimension = "trend") %>%
      select(region_id = region_id, score, dimension)

    ### return scores
    np_scores <- np_status_current %>%
      full_join(r.trend, by=c('region_id', 'dimension', 'score')) %>%
      mutate(goal = 'NP') %>%
      select(goal, dimension, region_id, score) %>%
      arrange(goal, dimension, region_id)

    return(np_scores)
  }

  ##########################################.
  ### Natural Products main starts here:

  np_harvest <- np_rebuild_harvest(layers)
  np_exp     <- np_calc_exposure(np_harvest, hab_extent, FIS_status)
  np_risk    <- np_calc_risk(np_exp, r_cyanide, r_blast)
  np_sust    <- np_calc_sustainability(np_exp, np_risk)
  np_scores  <- np_calc_scores(np_sust, data_year)


  return(np_scores)
}


CS <- function(layers){

  ## read in layers
  extent_lyrs <- c('hab_mangrove_extent', 'hab_seagrass_extent', 'hab_saltmarsh_extent')
  health_lyrs <- c('hab_mangrove_health', 'hab_seagrass_health', 'hab_saltmarsh_health')
  trend_lyrs <- c('hab_mangrove_trend', 'hab_seagrass_trend', 'hab_saltmarsh_trend')

  # get data together. Note: SelectData2() is a function defined at the bottom of this script
  extent <- SelectData2(extent_lyrs) %>%
    filter(!(habitat %in% c("mangrove_inland1km", "mangrove_offshore"))) %>%
    filter(scenario_year == max(data_year)) %>%
    filter(!is.na(rgn_id)) %>%
    select(region_id = rgn_id, habitat, extent=km2) %>%
    mutate(habitat = as.character(habitat))

  health <- SelectData2(health_lyrs) %>%
    filter(!(habitat %in% c("mangrove_inland1km", "mangrove_offshore"))) %>%
    filter(scenario_year == max(data_year)) %>%
    filter(!is.na(rgn_id)) %>%
    select(region_id = rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))

  trend <- SelectData2(trend_lyrs)%>%
    filter(!(habitat %in% c("mangrove_inland1km", "mangrove_offshore"))) %>%
    filter(scenario_year == max(data_year)) %>%
    filter(!is.na(rgn_id)) %>%
    select(region_id = rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))

  ## join layer data
  d <-  extent %>%
    full_join(health, by=c("region_id", "habitat")) %>%
    full_join(trend, by=c("region_id", "habitat"))

  ## set ranks for each habitat
  habitat.rank <- c('mangrove'         = 139,
                    'saltmarsh'        = 210,
                    'seagrass'         = 83)

  ## limit to CS habitats and add rank
  d <- d %>%
    mutate(
      rank = habitat.rank[habitat],
      extent = ifelse(extent==0, NA, extent))

  # status
  status <- d %>%
    filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
    group_by(region_id) %>%
    summarize(
      score = pmin(1, sum(rank * health * extent, na.rm=TRUE) / (sum(extent * rank, na.rm=TRUE)) ) * 100,
      dimension = 'status') %>%
    ungroup()

  # if no score, assign NA
  if ( nrow(status) == 0 ){
    status <- dplyr::bind_rows(
      status,
      d %>%
        group_by(region_id) %>%
        summarize(
          score = NA,
          dimension = 'status'))
  }

  # trend

  trend <- d %>%
    filter(!is.na(rank) & !is.na(trend) & !is.na(extent)) %>%
    group_by(region_id) %>%
    summarize(
      score = sum(rank * trend * extent, na.rm=TRUE) / (sum(extent*rank, na.rm=TRUE)),
      dimension = 'trend') %>%
    ungroup()

  # if no score, assign NA
  if ( nrow(trend) == 0 ){
    trend <- dplyr::bind_rows(
      trend,
      d %>%
        group_by(region_id) %>%
        summarize(
          score = NA,
          dimension = 'trend'))
  }

  ## combine scores
  scores_CS <- rbind(status, trend)  %>%
    mutate(goal = 'CS') %>%
    select(goal, dimension, region_id, score)

  ## create weights file for pressures/resilience calculations
  weights <- extent %>%
    filter(extent > 0) %>%
    mutate(rank = habitat.rank[habitat]) %>%
    mutate(extent_rank = extent*rank) %>%
    mutate(layer = "element_wts_cs_km2_x_storage") %>%
    select(rgn_id=region_id, habitat, extent_rank, layer)

  ## if no weights, assign placeholder
  if ( nrow(weights) == 0 ){
    weights <- dplyr::bind_rows(
      weights,
      d %>%
        group_by(region_id, habitat) %>%
        summarize(
          extent_rank = 1)) %>%
      mutate(layer = "element_wts_cs_km2_x_storage")
  }

  ## overwrite this layer with the calculated weights
  layers$data$element_wts_cs_km2_x_storage <- weights


  # return scores
  return(scores_CS)
}



CP <- function(layers){

  ## read in layers
  extent_lyrs <- c('hab_mangrove_extent', 'hab_seagrass_extent', 'hab_saltmarsh_extent', 'hab_coral_extent', 'hab_seaice_extent')
  health_lyrs <- c('hab_mangrove_health', 'hab_seagrass_health', 'hab_saltmarsh_health', 'hab_coral_health', 'hab_seaice_health')
  trend_lyrs <- c('hab_mangrove_trend', 'hab_seagrass_trend', 'hab_saltmarsh_trend', 'hab_coral_trend', 'hab_seaice_trend')

  # get data together. Note: SelectData2() is a function defined at the bottom of this script
  extent <- SelectData2(extent_lyrs) %>%
    filter(!(habitat %in% "seaice_edge")) %>%
    filter(scenario_year == max(data_year)) %>%
    filter(!is.na(rgn_id)) %>%
    select(region_id = rgn_id, habitat, extent=km2) %>%
    mutate(habitat = as.character(habitat))

  health <- SelectData2(health_lyrs) %>%
    filter(!(habitat %in% "seaice_edge")) %>%
    filter(scenario_year == max(data_year)) %>%
    filter(!is.na(rgn_id)) %>%
    select(region_id = rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))

  trend <- SelectData2(trend_lyrs) %>%
    filter(!(habitat %in% "seaice_edge")) %>%
    filter(scenario_year == max(data_year)) %>%
    filter(!is.na(rgn_id)) %>%
    select(region_id = rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))

  ## sum mangrove_offshore + mangrove_inland1km = mangrove to match with extent and trend
  mangrove_extent <- extent %>%
    filter(habitat %in% c('mangrove_inland1km','mangrove_offshore'))

  if (nrow(mangrove_extent) > 0){
    mangrove_extent <- mangrove_extent %>%
      group_by(region_id) %>%
      summarize(extent = sum(extent, na.rm = TRUE)) %>%
      mutate(habitat='mangrove') %>%
      ungroup()
  }

  extent <- extent %>%
    filter(!habitat %in% c('mangrove','mangrove_inland1km','mangrove_offshore')) %>%  #do not use all mangrove
    rbind(mangrove_extent)  #just the inland 1km and offshore

  ## join layer data
  d <-  extent %>%
    full_join(health, by=c("region_id", "habitat")) %>%
    full_join(trend, by=c("region_id", "habitat"))

  ## set ranks for each habitat
  habitat.rank <- c('coral'            = 4,
                    'mangrove'         = 4,
                    'saltmarsh'        = 3,
                    'seagrass'         = 1,
                    'seaice_shoreline' = 4)

  ## limit to CP habitats and add rank
  d <- d %>%
    filter(habitat %in% names(habitat.rank)) %>%
    mutate(
      rank = habitat.rank[habitat],
      extent = ifelse(extent==0, NA, extent))


  # status
  status <- d %>%
    filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
    group_by(region_id) %>%
    summarize(score = pmin(1, sum(rank * health * extent, na.rm=TRUE) /
                             (sum(extent * rank, na.rm=TRUE)) ) * 100) %>%
    mutate(dimension = 'status') %>%
    ungroup()

  # if no score, assign NA
  if ( nrow(status) == 0 ){
    status <- dplyr::bind_rows(
      status,
      d %>%
        group_by(region_id) %>%
        summarize(
          score = NA,
          dimension = 'status'))
  }

  # trend
  d_trend <- d %>%
    filter(!is.na(rank) & !is.na(trend) & !is.na(extent))

  if (nrow(d_trend) > 0 ){
    trend <- d_trend %>%
      group_by(region_id) %>%
      summarize(
        score = sum(rank * trend * extent, na.rm=TRUE) / (sum(extent*rank, na.rm=TRUE)),
        dimension = 'trend')
  } else { # if no trend score, assign NA
    trend <- d %>%
      group_by(region_id) %>%
      summarize(
        score = NA,
        dimension = 'trend')
  }

  ## finalize scores_CP
  scores_CP <- rbind(status, trend)  %>%
    mutate(goal = 'CP') %>%
    select(goal, dimension, region_id, score)


  ## create weights file for pressures/resilience calculations

  weights <- extent %>%
    filter(extent > 0) %>%
    mutate(rank = habitat.rank[habitat]) %>%
    mutate(extent_rank = extent*rank) %>%
    mutate(layer = "element_wts_cp_km2_x_protection") %>%
    select(rgn_id=region_id, habitat, extent_rank, layer)

  ## if no weights, assign placeholder
  if ( nrow(weights) == 0 ){
    weights <- dplyr::bind_rows(
      weights,
      d %>%
        group_by(region_id, habitat) %>%
        summarize(
          extent_rank = 1))
  }

  ## overwrite this layer with the calculated weights
  layers$data$element_wts_cp_km2_x_protection <- weights

  # return scores
  return(scores_CP)


}

TR <- function(layers) {

  ## formula:
  ##  E   = Ep                         # Ep: % of direct tourism jobs. tr_jobs_pct_tourism.csv
  ##  S   = (S_score - 1) / (7 - 1)    # S_score: raw TTCI score, not normalized (1-7). tr_sustainability.csv
  ##  Xtr = E * S

  pct_ref <- 90

  ## read in layers
  tourism  <- layers$data$tr_jobs_pct_tourism %>%
    select(region_id = rgn_id, year, Ep)

  sustain <- layers$data$tr_sustainability %>%
    select(region_id = rgn_id, year, S_score)

  ## combine data
  tr_data  <- full_join(tourism, sustain, by = c('region_id', 'year'))

  ## set data year for assessment
  data_year <- 2017

  tr_model <- tr_data %>%
    mutate(
      E   = Ep,
      S   = (S_score - 1) / (7 - 1), # scale score from 1 to 7.
      Xtr = E * S )


  ### Calculate status based on quantile reference (see function call for pct_ref)
  tr_model <- tr_model %>%
    group_by(year) %>%
    mutate(Xtr_q = quantile(Xtr, probs = pct_ref/100, na.rm = TRUE)) %>%
    mutate(status  = ifelse(Xtr / Xtr_q > 1, 1, Xtr / Xtr_q)) %>% # rescale to qth percentile, cap at 1
    ungroup()


  ## reference points
  ref_point <- tr_model %>%
    filter(year == data_year) %>%
    select(Xtr_q) %>%
    unique() %>%
    data.frame() %>%
    .$Xtr_q

  # get status
  tr_status <- tr_model %>%
    filter(year == data_year) %>%
    select(region_id = region_id, score = status) %>%
    mutate(score = score*100) %>%
    mutate(dimension = 'status')


  trend_data <- tr_model %>%
    filter(!is.na(status))

  trend_years <- (data_year-4):(data_year)

  adj_trend_year <- min(trend_years)

  tr_trend = trend_data %>%
    group_by(region_id) %>%
    do(mdl = lm(status ~ year, data=.),
       adjust_trend = .$status[.$year == adj_trend_year]) %>%
    summarize(region_id, score = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
    ungroup() %>%
    mutate(score = ifelse(score>1, 1, score)) %>%
    mutate(score = ifelse(score<(-1), (-1), score)) %>%
    mutate(score = round(score, 4)) %>%
    mutate(dimension = "trend") %>%
    select(region_id, score, dimension)

  # bind status and trend by rows
  tr_score <- bind_rows(tr_status, tr_trend) %>%
    mutate(goal = 'TR')


  # return final scores
  scores = tr_score %>%
    select(region_id, goal, dimension, score)

  return(scores)
}


LIV_ECO <- function(layers, subgoal){

  ## read in layers
  le_gdp <- layers$data$le_gdp  %>%
    select(region_id = rgn_id, year, gdp_usd = usd)

  le_wages <- layers$data$le_wage_sector_year %>%
    select(region_id = rgn_id, year, sector, wage_usd = value)

  le_jobs <- layers$data$le_jobs_sector_year %>%
    select(region_id = rgn_id, year, sector, jobs = value)

  le_workforce_size <- layers$data$le_workforcesize_adj %>%
    select(region_id = rgn_id, year, jobs_all = jobs)

  le_unemployment <- layers$data$le_unemployment %>%
    select(region_id = rgn_id, year, pct_unemployed = percent)


  # multipliers from Table S10 (Halpern et al 2012 SOM)
  multipliers_jobs = data.frame('sector' = c('tour','cf', 'mmw', 'wte','mar'),
                                'multiplier' = c(1, 1.582, 1.915, 1.88, 2.7))

  # calculate employment counts
  le_employed = le_workforce_size %>%
    left_join(le_unemployment, by = c('region_id', 'year')) %>%
    mutate(proportion_employed = (100 - pct_unemployed) / 100,
           employed            = jobs_all * proportion_employed)

  # reworded from SOM p.26-27
  #reference point for wages is the reference region (r) with the highest average wages across all sectors.
  #Reference points for jobs (j) and revenue (e) employ a moving baseline. The two metrics (j, e) are calculated
  #as relative values: the value in the current year (or most recent year), c, relative to the value in a recent
  #moving reference period, r, defined as 5 years prior to c. This reflects an implicit goal of maintaining coastal
  #livelihoods and economies (L&E) on short time scales, allowing for decadal or generational shifts in what people
  #want and expect for coastal L&E. The most recent year c must be 2000 or later in order for the data to be included.

  liv =
    # adjust jobs
    le_jobs %>%
    left_join(multipliers_jobs, by = 'sector') %>%
    mutate(jobs_mult = jobs * multiplier) %>%  # adjust jobs by multipliers
    left_join(le_employed, by= c('region_id', 'year')) %>%
    mutate(jobs_adj = jobs_mult * proportion_employed) %>% # adjust jobs by proportion employed
    left_join(le_wages, by=c('region_id','year','sector')) %>%
    arrange(year, sector, region_id)

  # LIV calculations ----

  # LIV status
  liv_status = liv %>%
    filter(!is.na(jobs_adj) & !is.na(wage_usd))
  # check if no concurrent wage data
  if (nrow(liv_status)==0){
    liv_status = liv %>%
      dplyr::select(region_id) %>%
      group_by(region_id) %>%
      summarize(
        goal      = 'LIV',
        dimension = 'status',
        score     = NA)
    liv_trend = liv %>%
      dplyr::select(region_id) %>%
      group_by(region_id) %>%
      summarize(
        goal      = 'LIV',
        dimension = 'trend',
        score     = NA)
  } else {
    liv_status = liv_status %>%
      filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
      arrange(region_id, year, sector) %>%
      # summarize across sectors
      group_by(region_id, year) %>%
      summarize(
        # across sectors, jobs are summed
        jobs_sum  = sum(jobs_adj, na.rm=T),
        # across sectors, wages are averaged
        wages_avg = mean(wage_usd, na.rm=T)) %>%
      group_by(region_id) %>%
      arrange(region_id, year) %>%
      mutate(
        # reference for jobs [j]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
        jobs_sum_first  = first(jobs_sum),
        # original reference for wages [w]: target value for average annual wages is the highest value observed across all reporting units
        # new reference for wages [w]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
        wages_avg_first = first(wages_avg)) %>%
      # calculate final scores
      ungroup() %>%
      mutate(
        x_jobs  = pmax(-1, pmin(1,  jobs_sum / jobs_sum_first)),
        x_wages = pmax(-1, pmin(1, wages_avg / wages_avg_first)),
        score   = mean(c(x_jobs, x_wages), na.rm=T) * 100) %>%
      # filter for most recent year
      filter(year == max(year, na.rm=T)) %>%
      # format
      select(region_id, score) %>%
      mutate(
        goal      = 'LIV',
        dimension = 'status')

    # LIV trend
    # From SOM p. 29: trend was calculated as the slope in the individual sector values (not summed sectors)
    # over the most recent five years...
    # with the average weighted by the number of jobs in each sector
    # ... averaging slopes across sectors weighted by the revenue in each sector

    # get trend across years as slope of individual sectors for jobs and wages
    liv_trend = liv %>%
      filter(!is.na(jobs_adj) & !is.na(wage_usd)) %>%
      filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
      # get sector weight as total jobs across years for given region
      arrange(region_id, year, sector) %>%
      group_by(region_id, sector) %>%
      mutate(
        weight = sum(jobs_adj, na.rm=T)) %>%
      # reshape into jobs and wages columns into single metric to get slope of both with one do() call
      reshape2::melt(id=c('region_id','year','sector','weight'), variable='metric', value.name='value') %>%
      mutate(
        sector = as.character(sector),
        metric = as.character(metric)) %>%
      # get linear model coefficient per metric
      group_by(metric, region_id, sector, weight) %>%
      do(mdl = lm(value ~ year, data=.)) %>%
      summarize(
        metric = metric,
        weight = weight,
        region_id = region_id,
        sector = sector,
        sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
      arrange(region_id, metric, sector) %>%
      # get weighted mean across sectors per region-metric
      group_by(metric, region_id) %>%
      summarize(
        metric_trend = weighted.mean(sector_trend, weight, na.rm=T)) %>%
      # get mean trend across metrics (jobs, wages) per region
      group_by(region_id) %>%
      summarize(
        score = mean(metric_trend, na.rm=T)) %>%
      # format
      mutate(
        goal      = 'LIV',
        dimension = 'trend') %>%
      dplyr::select(
        goal, dimension,
        region_id,
        score)
  }


  # ECO calculations ----
  eco = le_gdp %>%
    mutate(
      rev_adj = gdp_usd,
      sector = 'gdp') %>%
    # adjust rev with national GDP rates if available. Example: (rev_adj = gdp_usd / ntl_gdp)
    dplyr::select(region_id, year, sector, rev_adj)

  # ECO status
  eco_status = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
    # across sectors, revenue is summed
    group_by(region_id, year) %>%
    summarize(
      rev_sum  = sum(rev_adj, na.rm=T)) %>%
    # reference for revenue [e]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
    arrange(region_id, year) %>%
    group_by(region_id) %>%
    mutate(
      rev_sum_first  = first(rev_sum)) %>%
    # calculate final scores
    ungroup() %>%
    mutate(
      score  = pmin(rev_sum / rev_sum_first, 1) * 100) %>%
    # get most recent year
    filter(year == max(year, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'status') %>%
    dplyr::select(
      goal, dimension,
      region_id,
      score)

  # ECO trend
  eco_trend = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4 ) %>% # 5 year trend
    # get sector weight as total revenue across years for given region
    arrange(region_id, year, sector) %>%
    group_by(region_id, sector) %>%
    mutate(
      weight = sum(rev_adj, na.rm=T)) %>%
    # get linear model coefficient per region-sector
    group_by(region_id, sector, weight) %>%
    do(mdl = lm(rev_adj ~ year, data=.)) %>%
    summarize(
      weight = weight,
      region_id = region_id,
      sector = sector,
      sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
    # get weighted mean across sectors per region
    group_by(region_id) %>%
    summarize(
      score = weighted.mean(sector_trend, weight, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'trend') %>%
    dplyr::select(
      goal, dimension,
      region_id,
      score)

  # report LIV and ECO scores separately
  if (subgoal=='LIV'){
    d = rbind(liv_status, liv_trend)
  } else if (subgoal=='ECO'){
    d = rbind(eco_status, eco_trend)
  } else {
    stop('LIV_ECO function only handles subgoal of "LIV" or "ECO"')
  }
  return(d)

}


LE = function(scores, layers){

  # calculate LE scores
  scores_LE = scores %>%
    dplyr::filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend','score','future')) %>%
    tidyr::spread(key = goal, value = score) %>%
    dplyr::mutate(score = rowMeans(cbind(ECO, LIV), na.rm=TRUE)) %>%
    dplyr::select(region_id, dimension, score) %>%
    dplyr::mutate(goal  = 'LE')

  # rbind to all scores
  scores = scores %>%
    rbind(scores_LE)

  # return scores
  return(scores)
}


ICO = function(layers){

  ## read in layers
  ico <- layers$data$ico_spp_iucn_status %>%
    select(region_id = rgn_id, sciname, iucn_cat = category, year) %>%
    mutate(sciname  = as.character(sciname),
           iucn_cat = as.character(iucn_cat))

  ## set data year for assessment
  data_year <- max(ico$year)

  # lookup for weights status
  #  LC <- "LOWER RISK/LEAST CONCERN (LR/LC)"
  #  NT <- "LOWER RISK/NEAR THREATENED (LR/NT)"
  #  T  <- "THREATENED (T)" treat as "EN"
  #  VU <- "VULNERABLE (V)"
  #  EN <- "ENDANGERED (E)"
  #  LR/CD <- "LOWER RISK/CONSERVATION DEPENDENT (LR/CD)" treat as between VU and NT
  #  CR <- "VERY RARE AND BELIEVED TO BE DECREASING IN NUMBERS"
  #  DD <- "INSUFFICIENTLY KNOWN (K)"
  #  DD <- "INDETERMINATE (I)"
  #  DD <- "STATUS INADEQUATELY KNOWN-SURVEY REQUIRED OR DATA SOUGHT"

  w.risk_category <- data.frame(
    iucn_cat = c('LC', 'NT', 'CD', 'VU', 'EN', 'CR', 'EX', 'DD'),
    risk_score = c(0,  0.2,  0.3,  0.4,  0.6,  0.8,  1, NA)) %>%
    mutate(status_score = 1-risk_score) %>%
    mutate(iucn_cat = as.character(iucn_cat))

  ####### status
  # STEP 1: take mean of subpopulation scores
  r.status_spp <- ico %>%
    left_join(w.risk_category, by = 'iucn_cat') %>%
    group_by(region_id, sciname, year) %>%
    summarize(spp_mean = mean(status_score, na.rm=TRUE)) %>%
    ungroup()

  # STEP 2: take mean of populations within regions
  r.status <- r.status_spp %>%
    group_by(region_id, year) %>%
    summarize(score = mean(spp_mean, na.rm=TRUE)) %>%
    ungroup()

  ####### trend
  trend_years <- c(data_year:(data_year - 9)) # trend based on 10 years of data, due to infrequency of IUCN assessments
  adj_trend_year <- min(trend_years)


  r.trend <- r.status %>%
    group_by(region_id) %>%
    do(mdl = lm(score ~ year, data=., subset=year %in% trend_years),
       adjust_trend = .$score[.$year == adj_trend_year]) %>%
    summarize(region_id,
              trend = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
    ungroup() %>%
    mutate(trend = ifelse(trend>1, 1, trend)) %>%
    mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
    mutate(trend = round(trend, 4)) %>%
    select(region_id, score = trend) %>%
    mutate(dimension = "trend")


  ####### status
  r.status <- r.status %>%
    filter(year == data_year) %>%
    mutate(score = score * 100) %>%
    mutate(dimension = "status") %>%
    select(region_id, score, dimension)


  # return scores
  scores <-  rbind(r.status, r.trend) %>%
    mutate('goal'='ICO') %>%
    select(goal, dimension, region_id, score) %>%
    data.frame()

  return(scores)

}


LSP <- function(layers){

  ## read in layers
  ## total offshore/inland areas
  inland <- layers$data$rgn_area_inland1km %>%
    select(region_id = rgn_id, area_inland1km = area)

  offshore <- layers$data$rgn_area_offshore3nm %>%
    select(region_id = rgn_id, area_offshore3nm = area)

  ## total protected areas
  inland_prot <- layers$data$lsp_prot_area_inland1km %>%
    select(region_id = rgn_id, cmpa = a_prot_1km, year)
  offshore_prot <- layers$data$lsp_prot_area_offshore3nm %>%
    select(region_id = rgn_id, cp = a_prot_3nm, year)


  ## set data year for assessment
  data_year <- max(inland_prot$year)
  trend_years = (data_year-4):data_year


  ## set reference points
  ref_pct_cmpa <- 30
  ref_pct_cp   <- 30

  ## combine data
  r <- left_join(inland, offshore, by = c('region_id'))

  ry <- left_join(inland_prot, offshore_prot, by = c('region_id', 'year')) %>%
    select(region_id, year, cmpa, cp)

  # fill in time series for all regions
  r.yrs <- expand.grid(region_id = unique(ry$region_id),
                       year = unique(ry$year)) %>%
    left_join(ry, by=c('region_id', 'year')) %>%
    arrange(region_id, year) %>%
    mutate(cp= ifelse(is.na(cp), 0, cp),
           cmpa = ifelse(is.na(cmpa), 0, cmpa)) %>%
    mutate(pa  = cp + cmpa)

  # get percent of total area that is protected for inland1km (cp) and offshore3nm (cmpa) per year
  # and calculate status score
  r.yrs <- r.yrs %>%
    full_join(r, by="region_id") %>%
    mutate(pct_cp    = pmin(cp   / area_inland1km   * 100, 100),
           pct_cmpa  = pmin(cmpa / area_offshore3nm * 100, 100),
           prop_protected    = ( pmin(pct_cp / ref_pct_cp, 1) + pmin(pct_cmpa / ref_pct_cmpa, 1) ) / 2) %>%
    filter(!is.na(prop_protected))

  # extract status based on specified year
  r.status = r.yrs %>%
    filter(year==data_year) %>%
    select(region_id, status=prop_protected) %>%
    mutate(status=status*100) %>%
    select(region_id, score = status) %>%
    mutate(dimension = "status")

  # calculate trend

  adj_trend_year <- min(trend_years)

  r.trend =   r.yrs %>%
    group_by(region_id) %>%
    do(mdl = lm(prop_protected ~ year, data=., subset=year %in% trend_years),
       adjust_trend = .$prop_protected[.$year == adj_trend_year]) %>%
    summarize(region_id, trend = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
    ungroup() %>%
    mutate(trend = ifelse(trend>1, 1, trend)) %>%
    mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
    mutate(trend = round(trend, 4)) %>%
    select(region_id, score = trend) %>%
    mutate(dimension = "trend")


  # return scores
  scores = bind_rows(r.status, r.trend) %>%
    mutate(goal = "LSP")
  return(scores[,c('region_id','goal','dimension','score')])
}

LSP_ex <- function(layers){
  ### an example when there are only data available for the entire assessment area, not for each region.

  ### our assessment area has 8 regions...

  ## total offshore/inland areas
  inland <- layers$data$rgn_area_inland1km %>%
    select(region_id = rgn_id, area_inland1km = area); inland
  offshore <- layers$data$rgn_area_offshore3nm %>%
    select(region_id = rgn_id, area_offshore3nm = area); offshore

  ## total protected
  inland_prot <- layers$data$lsp_prot_area_inland1km %>%
    select(region_id = rgn_id, cmpa = a_prot_1km, year); inland_prot
  offshore_prot <- layers$data$lsp_prot_area_offshore3nm %>%
    select(region_id = rgn_id, cp = a_prot_3nm, year); offshore_prot


  ### ...but let's pretend we only had data for the entire assessment area and not those regions:

  inland   <- data_frame(region_id = 0, area_in = 175147)
  offshore <- data_frame(region_id = 0, area_off = 621887.8)
  inland_prot   <- data_frame(region_id = 0, cmpa = 15369.5)
  offshore_prot <- data_frame(region_id = 0, cp = 30635.50)

  ## combine to one dataframe
  lsp_data <- inland %>%
    left_join(inland_prot) %>%
    left_join(offshore) %>%
    left_join(offshore_prot)

  ### you can calculate the status for just this overall assessment area, and then apply the status score to all the regions so that the Toolbox can calculate the pressures/resilience for those regions.

  ## model
  status_0 <- lsp_data %>%
    dplyr::mutate(score = area_in/cp + area_off/cmpa)

  ## apply to all regions
  status <- data_frame(region_id = 1:8,
                       score  = status_0$score) %>%
    mutate(goal = "LSP",
           dimension = "status") %>%
    arrange(goal, dimension, region_id, score)

}


SP <- function(scores){

  ## to calculate the four SP dimesions, average those dimensions for ICO and LSP
  s <- scores %>%
    filter(goal %in% c('ICO','LSP'),
           dimension %in% c('status', 'trend', 'future', 'score')) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>%
    ungroup() %>%
    arrange(region_id) %>%
    mutate(goal = "SP") %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
}


CW <- function(layers){

  ## read in layers
  pressure_lyrs <- c('po_pathogens', 'po_nutrients_3nm', 'po_chemicals_3nm', 'po_trash')
  trend_layers  <- c('cw_chemical_trend', 'cw_nutrient_trend', 'cw_trash_trend', 'cw_pathogen_trend')

  d_pressures <- rbind(layers$data$po_pathogens,
                       layers$data$po_nutrients_3nm,
                       layers$data$po_chemicals_3nm,
                       layers$data$po_trash) %>%
    select(region_id = rgn_id, year, pressure_score, layer)

  d_trends <- rbind(layers$data$cw_chemical_trend,
                    layers$data$cw_nutrient_trend,
                    layers$data$cw_trash_trend,
                    layers$data$cw_pathogen_trend) %>%
    select(region_id = rgn_id, year, trend, layer)

  ### function to calculate geometric mean:
  geometric.mean2 <- function (x, na.rm = TRUE) {
    if (is.null(nrow(x))) {
      exp(mean(log(x), na.rm = TRUE))
    }
    else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }

  ## calculations
  d_pressures <- d_pressures %>%
    mutate(pressure = 1 - pressure_score) %>%  # invert pressures
    group_by(region_id) %>%
    summarize(score = geometric.mean2(pressure, na.rm=TRUE)) %>% # take geometric mean
    mutate(score = score * 100) %>%
    mutate(dimension = "status") %>%
    ungroup()

  d_trends <- d_trends %>%
    mutate(trend = -1 * trend)  %>%  # invert trends
    group_by(region_id) %>%
    summarize(score = mean(trend, na.rm = TRUE)) %>%
    mutate(dimension = "trend") %>%
    ungroup()


  # return scores
  scores = rbind(d_pressures, d_trends) %>%
    mutate(goal = "CW") %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()

  return(scores)
}


HAB = function(layers){

  ## set data year for assessment
  data_year <- 2014

  ## read in layers
  extent_lyrs <- c('hab_mangrove_extent', 'hab_seagrass_extent', 'hab_saltmarsh_extent',
                   'hab_coral_extent', 'hab_seaice_extent', 'hab_softbottom_extent')
  health_lyrs <- c('hab_mangrove_health', 'hab_seagrass_health', 'hab_saltmarsh_health',
                   'hab_coral_health', 'hab_seaice_health', 'hab_softbottom_health')
  trend_lyrs <- c('hab_mangrove_trend', 'hab_seagrass_trend', 'hab_saltmarsh_trend',
                  'hab_coral_trend', 'hab_seaice_trend', 'hab_softbottom_trend')

  # get data together. Note: SelectData2() is a function defined at the bottom of this script
  extent <- SelectData2(extent_lyrs) %>%
    filter(scenario_year == data_year) %>%
    select(region_id = rgn_id, habitat, extent=km2) %>%
    mutate(habitat = as.character(habitat))

  health <- SelectData2(health_lyrs) %>%
    filter(scenario_year == data_year) %>%
    select(region_id = rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))

  trend <- SelectData2(trend_lyrs) %>%
    filter(scenario_year == data_year) %>%
    select(region_id = rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))

  # join and limit to HAB habitats
  d <- health %>%
    full_join(trend, by = c('region_id', 'habitat')) %>%
    full_join(extent, by = c('region_id', 'habitat')) %>%
    filter(habitat %in% c('coral','mangrove','saltmarsh','seaice_edge','seagrass','soft_bottom')) %>%
    mutate(w  = ifelse(!is.na(extent) & extent > 0, 1, NA)) %>%
    filter(!is.na(w))

  if(sum(d$w %in% 1 & is.na(d$trend)) > 0){
    warning("Some regions/habitats have extent data, but no trend data.  Consider estimating these values.")
  }

  if(sum(d$w %in% 1 & is.na(d$health)) > 0){
    warning("Some regions/habitats have extent data, but no health data.  Consider estimating these values.")
  }


  ## calculate scores
  status <- d %>%
    group_by(region_id) %>%
    filter(!is.na(health)) %>%
    summarize(
      score = pmin(1, sum(health) / sum(w)) * 100,
      dimension = 'status') %>%
    ungroup()

  # if no score, assign NA
  if ( nrow(status) == 0 ){
    status <- dplyr::bind_rows(
      status,
      d %>%
        group_by(region_id) %>%
        summarize(
          score = NA,
          dimension = 'status'))
  }

  trend <- d %>%
    group_by(region_id) %>%
    filter(!is.na(trend)) %>%
    summarize(
      score =  sum(trend) / sum(w),
      dimension = 'trend')  %>%
    ungroup()

  # if no score, assign NA
  if ( nrow(trend) == 0 ){
    trend <- dplyr::bind_rows(
      trend,
      d %>%
        group_by(region_id) %>%
        summarize(
          score = NA,
          dimension = 'trend'))
  }

  scores_HAB <- rbind(status, trend) %>%
    mutate(goal = "HAB") %>%
    select(region_id, goal, dimension, score)

  ## create weights file for pressures/resilience calculations
  weights<- extent %>%
    filter(habitat %in% c('seagrass',
                          'saltmarsh',
                          'mangrove',
                          'coral',
                          'seaice_edge',
                          'soft_bottom')) %>%
    filter(extent > 0) %>%
    mutate(boolean = 1) %>%
    mutate(layer = "element_wts_hab_pres_abs") %>%
    select(rgn_id=region_id, habitat, boolean, layer)

  layers$data$element_wts_hab_pres_abs <- weights


  # return scores
  return(scores_HAB)
}

SPP = function(layers){

  ## read in layers and format
  scores <- rbind(layers$data$spp_status, layers$data$spp_trend) %>%
    mutate(goal = 'SPP') %>%
    mutate(dimension = ifelse(layer=="spp_status", "status", "trend")) %>%
    mutate(score = ifelse(dimension == 'status', score*100, score)) %>%
    select(region_id=rgn_id, goal, dimension, score)

  return(scores)
}

BD = function(scores){

  d <- scores %>%
    filter(goal %in% c('HAB', 'SPP')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>%
    mutate(goal = 'BD') %>%
    data.frame()

  # return all scores
  return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}

PreGlobalScores = function(layers, conf, scores){

  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow = TRUE)

  # limit to just desired regions and global (region_id==0)
  scores = subset(scores, region_id %in% c(rgns[,'id_num'], 0))

  # apply NA to Antarctica
  id_ant = subset(rgns, val_chr=='Antarctica', id_num, drop = TRUE)
  scores[scores$region_id==id_ant, 'score'] = NA

  return(scores)
}

FinalizeScores = function(layers, conf, scores){

  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow = TRUE)

  # add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       region_id = c(rgns[,'id_num'], 0),
                       dimension = c('pressures','resilience','status','trend','future','score'),
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors = FALSE); head(d)
  d = subset(d,
             !(dimension %in% c('pressures','resilience','trend') & region_id==0) &
               !(dimension %in% c('pressures','resilience','trend', 'status') & goal=='Index'))
  scores = merge(scores, d, all = TRUE)[,c('goal','dimension','region_id','score')]

  # order
  scores = arrange(scores, goal, dimension, region_id)

  # round scores
  scores$score = round(scores$score, 2)

  return(scores)
}


## Helper functions ----

# function to link data and scenario years based on
# conf/scenario_data_years.csv information

get_data_year <- function(layer_nm, layers=layers) { #layer_nm="le_wage_cur_base_value"

  all_years <- conf$scenario_data_years %>%
    mutate(scenario_year= as.numeric(scenario_year),
           data_year = as.numeric(data_year)) %>%
    filter(layer_name %in% layer_nm) %>%
    select(layer_name, scenario_year, year=data_year)


  layer_vals <- layers$data[[layer_nm]]

  layers_years <- all_years %>%
    left_join(layer_vals, by="year") %>%
    select(-layer)

  names(layers_years)[which(names(layers_years)=="year")] <- paste0(layer_nm, "_year")

  return(layers_years)
}


# useful function for compiling multiple data layers
# only works when the variable names are the same across datasets
# (e.g., coral, seagrass, and mangroves).
# it relies on get_data_year(), a function defined immediately above.
SelectData2 <- function(layer_names){
  data <- data.frame()
  for(e in layer_names){ # e="le_jobs_cur_base_value"
    data_new <- get_data_year(layer_nm=e, layers=layers)
    names(data_new)[which(names(data_new) == paste0(e, "_year"))] <- "data_year"
    data <- rbind(data, data_new)
  }
  return(data)
}

