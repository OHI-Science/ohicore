rm(list=ls())


source('../R/SelectLayers.R')
source('../R/ParseLayers.R')

source('../R/TransformSpatialScheme.R')
source('../R/ParseSpatialSchemes.R')

source('../R/CalculateStatusComponent.R')
source('../R/CalculatePressuresComponent.R')
source('../R/CalculateResilienceComponent.R')

source('../R/CalculateSubgoal.R')

source('../R/Halpern2012Data.R')




layers.list <- Layers('data/fullpath.layers.navigation.csv')
schemes.list <- SpatialSchemes('data/schemes_navigation.csv')


data = SelectLayers(layers.list, mode='target', target='FIS', cast=T)
names(data) = gsub('country_id', 'country', names(data))

data$region = NA


tmp = TransformSpatialScheme(schemes.list, data, 'region', c('country'), c('i_fis_bt', 'i_fis_mmsy','i_fis_tc'))
