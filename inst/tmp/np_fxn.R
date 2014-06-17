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

# cyanide fishing
r_cyanide = read.csv('../ohiprep/Global/WRI-ReefsAtRisk_v2013/data/gl_thr_poison_3nm_rgn2013.csv', na.strings='')
r_blast   = read.csv('../ohiprep/Global/WRI-ReefsAtRisk_v2013/data/gl_thr_blast_3nm_rgn2013.csv', na.strings='')

# add checks for previous dealings that don't seem necessary in latest FAO commodities layer prep (2014-06-09)
# previosly differing max(year) per region, product
stopifnot(nrow(group_by(h_tonnes, product, rgn_id) %.% summarize(year_max = max(year)) %.% filter(year_max!=yr_max)) == 0)
stopifnot(nrow(group_by(h_usd   , product, rgn_id) %.% summarize(year_max = max(year)) %.% filter(year_max!=yr_max)) == 0)

# merge harvest in tonnes and usd
h = merge(
  h_tonnes, 
  h_usd,
  by=c('rgn_name','rgn_id','product','year'), 
  all=T) %.%
  filter(year <= yr_max) %>%
  arrange(rgn_id, product, year) %>%
  group_by(rgn_id, product)

# show where NAs usd vs tonnes
h_na = h %.% 
  filter(is.na(usd) | is.na(tonnes)) %.% 
  mutate(var_na = ifelse(is.na(usd), 'usd', 'tonnes'))
addmargins(table(ungroup(h_na) %.% select(year, var_na)))
# tonnes    usd 
#    694    214

# handle NA mismatch b/n tonnes and usd with correlative model
m_tonnes = h  %>%
  mutate(tonnes_nas   = sum(is.na(tonnes))) %>%
  filter(tonnes_nas > 0 & !is.na(usd) & !is.na(tonnes)) %>%
  do(mdl = lm(tonnes ~ usd, data=.)) %>%
  summarize(
    rgn_id   = rgn_id,
    product  = factor(levels(h$product)[product], levels(h$product)),
    usd_ix0  = coef(mdl)['(Intercept)'],
    usd_coef = coef(mdl)['usd'])
m_usd = h %>%
  mutate(usd_nas = sum(is.na(usd))) %>%
  filter(usd_nas > 0 & !is.na(usd) & !is.na(tonnes)) %>%
  do(mdl = lm(usd ~ tonnes, data=.)) %>%
  summarize(
    rgn_id      = rgn_id,
    product     = factor(levels(h$product)[product], levels(h$product)),
    tonnes_ix0  = coef(mdl)['(Intercept)'],
    tonnes_coef = coef(mdl)['tonnes'])
h = h %>%
  left_join(m_tonnes, by=c('rgn_id','product')) %>%
  mutate(
    tonnes_mdl  = usd_ix0 + usd_coef * usd,
    tonnes_orig = tonnes,
    tonnes      = ifelse(is.na(tonnes), tonnes_mdl, tonnes)) %>%
  left_join(m_usd, by=c('rgn_id','product')) %>%
  mutate(
    usd_mdl  = tonnes_ix0 + tonnes_coef * tonnes,
    usd_orig = usd,
    usd      = ifelse(is.na(usd), usd_mdl, usd))

# relativize harvest
h = h %>%
  mutate(    
    tonnes_max  = max(tonnes, na.rm=T),
    tonnes_peak = tonnes_max  * (1 - harvest_peak_buffer),
    tonnes_rel  = ifelse(tonnes >= tonnes_peak, 1, tonnes / tonnes_peak),
    usd_max     = max(usd, na.rm=T),
    usd_peak    = usd_max  * (1 - harvest_peak_buffer),
    usd_rel     = ifelse(usd >= usd_peak, 1, usd / usd_peak),
    # swap one with other if still NA even after correlative gapfilling above
    tonnes_rel_orig = tonnes_rel,
    usd_rel_orig    = usd_rel,
    tonnes_rel      = ifelse(is.na(tonnes_rel), usd_rel   , tonnes_rel), # 109 rows
    usd_rel         = ifelse(is.na(usd_rel)   , tonnes_rel, usd_rel))    #  99 rows

# report whether region gapfilled for any product-year per var at either correlative or swap stages
h_g = h %>%
  # per region-product-year
  mutate(
    tonnes_gapfilled = ifelse( (is.na(tonnes_orig) & !is.na(tonnes_rel)) | (tonnes_rel_orig != tonnes_rel), T, F),
    usd_gapfilled    = ifelse( (is.na(usd_orig)    & !is.na(usd_rel)   ) | (usd_rel_orig    != usd_rel)   , T, F)) %>%
  # per region
  group_by(rgn_id) %>%
  summarize(
    tonnes_gapfilled = ifelse(sum(tonnes_gapfilled) > 0, T, F),
    usd_gapfilled    = ifelse(sum(usd_gapfilled) > 0, T, F))

# debug output
write.csv(h  , 'inst/tmp/np/np_harvest_gapfilled_data.csv', row.names=F, na='')
write.csv(h_g, 'inst/tmp/np/np_harvest_gapfilled_summary.csv', row.names=F, na='')


#    H = ifelse(!is.na(tonnes_rel), tonnes_rel,    usd_rel), # H: harvest yield
#    w = ifelse(!is.na(   usd_rel),    usd_rel, tonnes_rel)) # w: proportional peak value
# for now, skipping smoothing done in PLoS 2013
  
# S: sustainability of harvest
read.csv('../ohiprep/Global/WRI-ReefsAtRisk_v2013/data/gl_thr_poison_3nm_rgn2013.csv') %.% head()

# risk for ornamentals set to 1 if blast or cyanide fishing present
#  based on Nature 2012 code
#  despite Nature 2012 Suppl saying R for ornamental fish is set to the "relative intensity of cyanide fishing"
r_orn = r_cyanide %.%
  filter(!is.na(score) & score > 0) %.%
  select(rgn_id, cyanide=score) %.%  
  merge(
    r_blast %.%
      filter(!is.na(score) & score > 0) %.%
      select(rgn_id, blast=score),
    all=T) %.%
  mutate(
    ornamentals = 1)

# get FIS status for fish_oil S
FIS_status = read.csv('inst/extdata/scores.Global2013.www2013.csv') %.% 
  filter(goal=='FIS' & dimension=='status')

# risk
R = 
  # fixed
  data.frame(
    rgn_id  = rgns$rgn_id,
    corals  = 1,
    sponges = 0,
    shells  = 0) %.%
  # ornamentals
  left_join(
    r_orn %.%
      select(rgn_id, ornamentals),
    by = 'rgn_id') %.%
  mutate(
    ornamentals = ifelse(is.na(ornamentals), 0, ornamentals)) %.%
  # fish_oil
  left_join(
    FIS_status %.%
      mutate(
        fish_oil = score/100) %.%
      select(
        rgn_id=region_id, fish_oil),
    by='rgn_id') %.%
  mutate(
    fish_oil = ifelse(is.na(fish_oil), 0, fish_oil))

S = 1 - mean(c(E,R), na.rm=T)

  


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
NP.old = function(scores, layers, 
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
