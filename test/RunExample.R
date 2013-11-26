rm(list=ls())


source('../R/SelectLayers.R')
source('../R/TransformSpatialScheme.R')

source('../R/Layers.R')
source('../R/SpatialSchemes.R')



source('../R/CalculateStatusComponent.R')
source('../R/CalculatePressuresComponent.R')
source('../R/CalculateResilienceComponent.R')
source('../R/CalculateSubgoal.R')


source('../R/Halpern2012Data.R')




layers.list <- Layers('data/fullpath.layers.navigation.csv')
schemes.list <- SpatialSchemes('data/schemes_navigation.csv')


status.data = SelectLayers(layers.list, mode='layers', cast=T,
                           layers=c('i_fis_bt', 'i_fis_mmsy', 'i_fis_tc'),
                           alternate.layer.names = c('Bt', 'mMSY', 'Tc'),
                           expand.time.invariant = T)


status = CalculateStatusComponent(status.data, Halpern2012.FP.FIS, s.name='country_id')






