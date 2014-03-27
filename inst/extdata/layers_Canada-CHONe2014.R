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

########################################## add carbon storage habitats #############################################################
pres=read.csv('layers.Canada-CHONe2014/hab_habitat_presence.csv', na.strings='', stringsAsFactors=F);head(pres);
ext=read.csv('layers.Canada-CHONe2014/hab_habitat_extent.csv', na.strings='', stringsAsFactors=F);head(ext);
health=read.csv('layers.Canada-CHONe2014/hab_habitat_health_disaggregatedNature2012.csv', na.strings='', stringsAsFactors=F);head(health);
trend=read.csv('layers.Canada-CHONe2014/hab_habitat_trend_disaggregatedNature2012.csv', na.strings='', stringsAsFactors=F);head(trend);

# inject permafrost and clathrates
#presence
newpres=data.frame(cbind(rgn_id=1:278,habitat=rep(c('clathrates','permafrost'),each=278),boolean=0))
newpres$boolean[newpres$rgn_id==218]=1
pres=rbind(pres,newpres)

#extent
ext=rbind(ext,data.frame(cbind(rgn_id=218,habitat=c('clathrates','permafrost'),km2=c(2517375.704,819702.5225))))

# calculate temperature based on CO2
co2 = read.csv("~/Documents/ohigit/ohicore/inst/extdata/rawdata.Canada-CHONe2014/HAB/CO2.csv")

# set baseline to pre-industrial levels (280 ppm)
co2$anomaly = co2$mean-280

#calculate health
co2$health = 1-co2$anomaly/(550-280)

#health
max_year = max(co2$year)
status = co2$health[which(co2$year==max_year)]
health = rbind(health,data.frame(cbind(rgn_id=218,habitat=c('clathrates','permafrost'),health=c(status,status))))

#trend
d = data.frame(status=co2$health, year=co2$year)[tail(which(!is.na(co2$health)), 20),]
t = lm(status ~ year, d)$coefficients[['year']]
trend = rbind(trend,data.frame(cbind(rgn_id=218,habitat=c('clathrates','permafrost'),trend=c(t,t))))

# write out modified layers
write.csv(pres,'layers.Canada-CHONe2014/hab_habitat_presence.csv', na='', row.names=F)
write.csv(ext,'layers.Canada-CHONe2014/hab_habitat_extent.csv', na='', row.names=F)
write.csv(health,'layers.Canada-CHONe2014/hab_habitat_health_disaggregatedNature2012.csv', na='', row.names=F)
write.csv(trend,'layers.Canada-CHONe2014/hab_habitat_trend_disaggregatedNature2012.csv', na='', row.names=F)


########################################## Change iconic species #############################################################
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


##################################### weights from survey ################################################################################

aveRank=read.csv('rawdata.Canada-CHONe2014/weights/aveRank.csv')

# rename Artisinal Opportunities to Aboriginal Needs
goals = read.csv('conf.Canada-CHONe2014/goals.csv', stringsAsFactors=F); head(goals); 

#calculate inverse of average rank
aveRank$V1=10-aveRank$V1

#calculate weights
goals$weight[goals$goal=='FP']     = aveRank$V1[aveRank$X=="FoodProvision"]/sum(aveRank$V1)*10
goals$weight[goals$goal=='FIS']     = aveRank$V1[aveRank$X=="FoodProvision"]/sum(aveRank$V1)/2*10
goals$weight[goals$goal=='MAR']     = aveRank$V1[aveRank$X=="FoodProvision"]/sum(aveRank$V1)/2*10
goals$weight[goals$goal=='AN']     = aveRank$V1[aveRank$X=="AboriginalNeeds"]/sum(aveRank$V1)*10
goals$weight[goals$goal=='NP']     = aveRank$V1[aveRank$X=="NaturalProducts"]/sum(aveRank$V1)*10
goals$weight[goals$goal=='CS']     = aveRank$V1[aveRank$X=="CarbonStorage"]/sum(aveRank$V1)*10
goals$weight[goals$goal=='CP']     = aveRank$V1[aveRank$X=="CoastalProtection"]/sum(aveRank$V1)*10
goals$weight[goals$goal=='TR']     = aveRank$V1[aveRank$X=="TourismRecreation"]/sum(aveRank$V1)*10
goals$weight[goals$goal=='LE']     = aveRank$V1[aveRank$X=="CoastalLivelihoods"]/sum(aveRank$V1)*10
goals$weight[goals$goal=='LIV']     = aveRank$V1[aveRank$X=="CoastalLivelihoods"]/sum(aveRank$V1)/2*10
goals$weight[goals$goal=='ECO']     = aveRank$V1[aveRank$X=="CoastalLivelihoods"]/sum(aveRank$V1)/2*10
goals$weight[goals$goal=='SP']     = aveRank$V1[aveRank$X=="IconicPlacesSPecies"]/sum(aveRank$V1)*10
goals$weight[goals$goal=='ICO']     = aveRank$V1[aveRank$X=="IconicPlacesSPecies"]/sum(aveRank$V1)/2*10
goals$weight[goals$goal=='LSP']     = aveRank$V1[aveRank$X=="IconicPlacesSPecies"]/sum(aveRank$V1)/2*10
goals$weight[goals$goal=='CW']     = aveRank$V1[aveRank$X=="CleanWaters"]/sum(aveRank$V1)*10
goals$weight[goals$goal=='BD']     = aveRank$V1[aveRank$X=="Biodiversity"]/sum(aveRank$V1)*10
goals$weight[goals$goal=='HAB']     = aveRank$V1[aveRank$X=="Biodiversity"]/sum(aveRank$V1)/2*10
goals$weight[goals$goal=='SPP']     = aveRank$V1[aveRank$X=="Biodiversity"]/sum(aveRank$V1)/2*10



# write back updated goals.csv
write.csv(goals, 'conf.Canada-CHONe2014/goals.csv', na='', row.names=F)
