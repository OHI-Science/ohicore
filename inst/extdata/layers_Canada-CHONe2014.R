library(plyr)
library(dplyr)

wd = '~/Documents/ohigit/ohicore/inst/extdata'
setwd(wd)

# create new layers.csv, and layers folder for Canada-CHONe2014 ####################
file.copy('layers.Global2013.www2013.csv', 'layers.Canada-CHONe2014.csv', overwrite = T)
file.copy('scores.Global2013.www2013.csv', 'scores.Canada-CHONe2014.csv', overwrite = T)
fl=list.files('layers.Global2013.www2013')
file.copy(paste('layers.Global2013.www2013/',fl,sep = ""),paste('layers.Canada-CHONe2014/',fl,sep = ""), overwrite = T)
fl=list.files('conf.Global2013.www2013')
file.copy(paste('conf.Global2013.www2013/',fl,sep = ""),paste('conf.Canada-CHONe2014/',fl,sep = ""), overwrite = T)
fl=list.files('spatial.www2013')
file.copy(paste('spatial.www2013/',fl,sep = ""),paste('spatial.Canada-CHONe2014/',fl,sep = ""), overwrite = T)

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


########################################## replace WGI with CWI #############################################################

CIW=read.csv('rawdata.Canada-CHONe2014/CIW/CIW-GDP-Domains-1994-2010.csv', stringsAsFactors=F)

# extrapolate CIW and GDP
fitCIW=lm(CIW~Year,data=CIW)
fitGDP=lm(GDP~Year,data=CIW)

# create CIW layers
ciw=data.frame(cbind(rgn_id=1:250,score=0.5))
ciw[218,2]=(coef(fitCIW)[2]*2013+coef(fitCIW)[1])/(coef(fitGDP)[2]*2013+coef(fitGDP)[1])
ciw2=data.frame(cbind(rgn_id=ciw$rgn_id,score=1-ciw$score))

# write out new CIW layers
write.csv(ciw, 'layers.Canada-CHONe2014/rgn_wb_cwi_2013_rescaled.csv', na='', row.names=F)
write.csv(ciw2, 'layers.Canada-CHONe2014/rgn_wb_cwi_2013_rescaled_inverse.csv', na='', row.names=F)

# alter fields for this updated layer
layers = read.csv('layers.Canada-CHONe2014.csv', na.strings='', stringsAsFactors=F); head(layers); 

i = which(layers$layer=='ss_wgi')
layers$layer[i]     = 'ss_cwi'
layers$name[i]      = 'Hardship of Canadians indicated with the CWI'
layers$units[i]     = 'pressure score'
layers$filename[i]  = 'rgn_wb_cwi_2013_rescaled_inverse.csv'
layers$fld_value[i] = 'score'
layers$val_min[i] = 0
layers$val_max[i] = 1


i = which(layers$layer=='wgi_all')
layers$layer[i]     = 'cwi_all'
layers$name[i]      = 'Wellbeing of Canadians indicated with the CWI'
layers$units[i]     = 'resilience score'
layers$filename[i]  = 'rgn_wb_cwi_2013_rescaled.csv'
layers$fld_value[i] = 'score'
layers$val_min[i] = 0
layers$val_max[i] = 1

# write back updated layers.csv
write.csv(layers, 'layers.Canada-CHONe2014.csv', na='', row.names=F)

# update pressure/resilience matrices and resilience weights
rw=read.csv('conf.Canada-CHONe2014/resilience_weights.csv', stringsAsFactors=F)
rw$layer[rw$layer=='wgi_all']='cwi_all'
write.csv(rw,'conf.Canada-CHONe2014/resilience_weights.csv',row.names=FALSE)

pm=read.csv('conf.Canada-CHONe2014/pressures_matrix.csv', stringsAsFactors=F)
names(pm)[names(pm)=='ss_wgi']='ss_cwi'
write.csv(pm,'conf.Canada-CHONe2014/pressures_matrix.csv',row.names=FALSE,na="")

rm=read.csv('conf.Canada-CHONe2014/resilience_matrix.csv', stringsAsFactors=F)
names(rm)[names(rm)=='wgi_all']='cwi_all'
rm$cwi_all='cwi_all'
write.csv(rm,'conf.Canada-CHONe2014/resilience_matrix.csv',row.names=FALSE)


##################################### Aboriginal Needs ################################################################################
source("rawdata.Canada-CHONe2014/AN/AN_timeseries.R")

# copies modified functions.R in rawdata.Canada-CHONe2014/ and to conf.Canada-CHONe2014/functions.R 
file.copy('rawdata.Canada-CHONe2014/functions.R', 'conf.Canada-CHONe2014/functions.R', overwrite = T)

# rename Artisinal Opportunities to Aboriginal Needs
goals = read.csv('conf.Canada-CHONe2014/goals.csv', stringsAsFactors=F); head(goals); 

i = which(goals$goal=='AO')
goals$goal[i]     = 'AN'
goals$name[i]      = 'Aboriginal Needs'
goals$name_flower[i]      = '           Aboriginal Needs'
goals$description[i]     = 'This goal captures The extent to which Canada’s Aboriginals are able to access ocean resources for subsistence, social and ceremonial purposes'
goals$preindex_function[i]  = 'AN(layers)'

# write back updated goals.csv
write.csv(goals, 'conf.Canada-CHONe2014/goals.csv', na='', row.names=F)

# copy rgn_an_timeseries
file.copy('rawdata.Canada-CHONe2014/AN/AN_timeseries.csv', 'layers.Canada-CHONe2014/rgn_an_timeseries.csv', overwrite = T)


# alter fields for this updated layer
layers = read.csv('layers.Canada-CHONe2014.csv', na.strings='', stringsAsFactors=F); head(layers); 

# remove unnecessary layer
i = which(layers$layer!='rn_ao_access')
layers     = layers[i,]

# alter layer
i = which(layers$layer=='rny_ao_need')
layers$targets[i]   = 'AN'
layers$layer[i]     = 'rny_an_timeseries'
layers$name[i]      = 'Timeseries of Aboriginal Needs Status'
layers$citation[i]  = ''
layers$filename[i]  = 'rgn_an_timeseries.csv'
layers$val_min[i] = 0
layers$val_max[i] = 1
# write back updated layers.csv
write.csv(layers, 'layers.Canada-CHONe2014.csv', na='', row.names=F)

pm=read.csv('conf.Canada-CHONe2014/pressures_matrix.csv', stringsAsFactors=F)
pm$goal[pm$goal=='AO']     = 'AN'
write.csv(pm,'conf.Canada-CHONe2014/pressures_matrix.csv',row.names=FALSE,na="")

rm=read.csv('conf.Canada-CHONe2014/resilience_matrix.csv', stringsAsFactors=F)
rm$goal[rm$goal=='AO']     = 'AN'
write.csv(rm,'conf.Canada-CHONe2014/resilience_matrix.csv',row.names=FALSE)

# create Aboriginal needs layers

