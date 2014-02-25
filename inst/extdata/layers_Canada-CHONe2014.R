library(plyr)
library(dplyr)

wd = '~/Documents/ohigit/ohicore/inst/extdata'
setwd(wd)

# create new layers.csv, and layers folder for Canada-CHONe2014 ####################
file.copy('layers.Global2013.www2013.csv', 'layers.Canada-CHONe2014.csv', overwrite = T)
file.copy('scores.Global2013.www2013.csv', 'scores.Canada-CHONe2014.csv', overwrite = T)
fl=list.files('layers.Global2013.www2013')
file.copy(paste('layers.Global2013.www2013/',fl,sep = ""),paste('layers.Canada-CHONe2014/',fl,sep = ""), overwrite = T)

# get species 
spp_ico = read.csv('rawdata.Canada-CHONe2014/ICO/iconic_species.csv', stringsAsFactors=F); head(spp_ico)
spp_sara = read.csv('rawdata.Canada-CHONe2014/ICO/SARA_sp.csv', stringsAsFactors=F); head(spp_sara)


# TODO: alter weights to something sensible, like IUCN analogue for status weights

# read in weights
spp_range_weights = read.csv('rawdata.Canada-CHONe2014/ICO/spp_range_weights.csv', stringsAsFactors=F); head(spp_range_weights)

# create weights for species category. Can't just use category because averaging multiple ranges
paste(unique(spp_sara$COSEWIC_Status), collapse="'=0.,'")
spp_status_weights = c('Not Assessed'    = 0,
                       'Non-active'      = 0,
                       'Data Deficient'  = 0,
                       'Not at Risk'     = 0,
                       'Special Concern' = 0.2,
                       'Threatened'      = 0.4,
                       'Endangered'      = 0.6,
                       'Extinct'         = 1,
                       'Extirpated'      = 1)
spp_status_weights = data.frame(COSEWIC_Status = names(spp_status_weights),
                                status_weight  = spp_status_weights, stringsAsFactors=F)

# get status for iconics
spp_ico_sara = spp_ico %.%
  inner_join(spp_sara, by='Scientific_name') %.%
  inner_join(spp_range_weights, by='Range') %.%
  inner_join(spp_status_weights, by='COSEWIC_Status') %.%
  rename(c(Scientific_name='sciname')) %.%
  group_by(sciname) %.%
  summarise(
    value = weighted.mean(x=status_weight, w=range_weight),
    rgn_id        = 218)
  
###
# create new ico_spp_extinction_status now with extinction risk as numeric value and not categorical with these Canada values swapped in
ico_spp_status = read.csv('layers.Global2013.www2013/ico_spp_extinction_status.csv', stringsAsFactors=F); head(ico_spp_status)

# lookup for weights status
w.risk_category = c('LC' = 0,
                    'NT' = 0.2,
                    'VU' = 0.4,
                    'EN' = 0.6,
                    'CR' = 0.8,
                    'EX' = 1)
# set value
ico_spp_status$value = w.risk_category[ico_spp_status$category]; head(ico_spp_status)

# inject Canada values
ico_spp_status = ico_spp_status %.%
  filter(rgn_id != 218) %.%
  select(rgn_id, sciname, value) %.%
  rbind(spp_ico_sara)
  
# write out new layer
write.csv(ico_spp_status, 'layers.Canada-CHONe2014/ico_spp_extinction_status_value_Canada-CHONe.csv', na='', row.names=F)

# read layers.csv
layers = read.csv('layers.Canada-CHONe2014.csv', na.strings='', stringsAsFactors=F); head(layers); 

# alter fields for this updated layer
i = which(layers$layer=='rnk_ico_spp_extinction_status_value')
layers$layer[i]     = 'rnk_ico_spp_extinction_status_value'
layers$name[i]      = 'SARA extinction risk value for iconic species'
layers$units[i]     = 'value'
layers$filename[i]  = 'ico_spp_extinction_status_value_Canada-CHONe.csv'
layers$fld_value[i] = 'value'

# write back updated layers.csv
write.csv(layers, 'layers.Canada-CHONe2014.csv', na='', row.names=F)

