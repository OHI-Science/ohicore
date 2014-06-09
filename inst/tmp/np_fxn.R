# debug ----
library(plyr)
library(dplyr)

dir_out = 'inst/tmp/np'
dir.create(dir_out, showWarnings=F)

rgns = read.csv('inst/extdata/layers.Global2013.www2013/rgn_labels.csv') %.% 
  rename(c('label'='rgn_name'))

# arguments ----
yr_max = 2011 # 2014: year_max=2011 # 2013: year_max=2010 # 2012: year_max=2009
harvest_peak_buffer = 0.35

# read in layers ----

# harvest in tonnes and dollars
h_tonnes = read.csv('../ohiprep/Global/FAO-Commodities_v2011/data/FAO-Commodities_v2011_tonnes.csv', na.strings='')
h_usd    = read.csv('../ohiprep/Global/FAO-Commodities_v2011/data/FAO-Commodities_v2011_usd.csv', na.strings='')

# add checks for previous dealings that don't seem necessary in latest FAO commodities layer prep (2014-06-09)
# previosly differing max(year) per region, product
stopifnot(nrow(group_by(h_tonnes, product, rgn_id) %.% summarize(year_max = max(year)) %.% filter(year_max!=yr_max)) == 0)
stopifnot(nrow(group_by(h_usd   , product, rgn_id) %.% summarize(year_max = max(year)) %.% filter(year_max!=yr_max)) == 0)

# show where NAs usd vs tonnes
h_na = merge(h_tonnes, h_usd, all=T) %.%
  filter(year <= yr_max) %.% 
  filter(is.na(usd) | is.na(tonnes)) %.% 
  mutate(na = ifelse(is.na(usd), 'usd', 'tonnes'))
addmargins(table(h_na %.% select(year, na)))
# tonnes    usd 
#    691    214

# merge harvest and filter by yr_max per scenario
h = merge(h_tonnes, h_usd, all=T) %.%
  filter(year <= yr_max)

# get max per region, product
h = h %.%
  group_by(rgn_id, product) %.%
  mutate(
    # calculate max, peak and relative
    usd_max      = max(usd),
    usd_peak     = usd_max  * (1 - harvest_peak_buffer),
    usd_rel      = ifelse(usd >= usd_peak, 1, usd / usd_peak),
    tonnes_max   = max(tonnes),
    tonnes_peak  = tonnes_max  * (1 - harvest_peak_buffer),
    tonnes_rel   = ifelse(tonnes >= tonnes_peak, 1, tonnes / tonnes_peak)) %.%
  ungroup() %.%
  mutate(
    # assign w_p_t based on tonnes_rel, unless NA then use usd_rel
    w            = ifelse(!is.na(tonnes_rel), tonnes_rel, usd_rel)) %.%    
    )

# for now, skipping smoothing done in PLoS 2013
    
  

group_by(h, rgn_id) %.%
  summarize(n_product = n_distinct(product))
  


  select(rgn_id, product, year, tonnes, tonnes, tonnes_max, tonnes_peak, usd_rel)
    tonnes_max  = max(tonnes),
    
    tonnes_peak = tonnes_max * (1 - harvest_peak_buffer),
     %.%
  head()
  

# buffer w/in 35% of peak


# log-transform

# ? converted from converted from nominal dollars as reported by FAO ("observed measure unit - US Dollar") into constant 2008 USD using CPI adjustment data (Sahr 2011 - http://oregonstate.edu/cla/polisci/sahr/sahr).


#   # DEBUG
#   library(devtools); load_all()
#   yr=2012; year_max = 2010 # yr=2013; year_max = 2011
#   scenario=sprintf('Global%d.www2013', yr)
#   conf = ohicore::Conf(sprintf('inst/extdata/conf.%s', scenario))
#   layers     = Layers(layers.csv = sprintf('inst/extdata/layers.%s.csv', scenario), 
#                       layers.dir = sprintf('inst/extdata/layers.%s'    , scenario))

# function ----
NP.new = function(scores, layers, 
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
