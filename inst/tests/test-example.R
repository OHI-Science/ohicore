
test_that('example data loads OK', {
  layers <- layers.Global2012.Nature2012ftp
  #schemes.list <- SpatialSchemes(system.file('extdata', 'schemes_navigation.csv', package='ohicore'))
  #schemes.list <- SpatialSchemes(system.file('inst','extdata', 'schemes_navigation.csv', package='ohicore')) # for loading in dev mode
  # TODO: resolve error: "cannot open file 'data/mat1.csv'" from local reference to mat1.csv 
  
  status.data = SelectLayers(layers, mode='layers', cast=T,
                             layers=c('i_fis_bt'='Bt', 'i_fis_mmsy'='mMSY', 'i_fis_tc'='Tc'),
                             expand.time.invariant = T)

  status.data = SelectLayers(layers, mode='target', cast=T,
                             target='FIS',
                             expand.time.invariant = T)
  
  status = CalculateStatusComponent(status.data, Halpern2012.FP.FIS, s.name='country_id')
})

# rm(list=ls())
# 
# 
# source('../R/SelectLayers.R')
# source('../R/TransformSpatialScheme.R')
# 
# source('../R/Layers.R')
# source('../R/SpatialSchemes.R')
# 
# 
# 
# source('../R/CalculateStatusComponent.R')
# source('../R/CalculatePressuresComponent.R')
# source('../R/CalculateResilienceComponent.R')
# source('../R/CalculateSubgoal.R')
# 
# 
# source('../R/Halpern2012Data.R')
# 
# 
# 
# 
# layers.list <- Layers('data/fullpath.layers.navigation.csv')
# schemes.list <- SpatialSchemes('data/schemes_navigation.csv')
# 
# 
# status.data = SelectLayers(layers.list, mode='layers', cast=T,
#                            layers=c('i_fis_bt', 'i_fis_mmsy', 'i_fis_tc'),
#                            alternate.layer.names = c('Bt', 'mMSY', 'Tc'),
#                            expand.time.invariant = T)
# 
# 
# eco.pressures.data = SelectLayers(layers.list, mode='layers', cast=F,
#                                   layers=c('po_chemicals_1', 'po_chemicals_1_coastal', 
#                                            'po_chemicals_2', 'po_chemicals_2_coastal', 
#                                            'po_chemicals_3', 'po_chemicals_3_coastal', 
#                                            'po_nutrients', 'po_nutrients_3nm',
#                                            'hd_subtidal_hb', 'hd_subtidal_sb', 
#                                            'hd_intertidal', 'sp_alien', 'sp_genetic',
#                                            'fp_com_hb', 'fp_com_lb', 'fp_art_hb',
#                                            'fp_art_lb', 'fp_art_lb_3nm'))
# 
# soc.pressures.data = SelectLayers(layers.list, mode='layers', cast=F,
#                                   layers=c('wgi_all'))
# 
# 
# eco.weights = data.frame('layer.id'=c('po_chemicals_1', 'po_chemicals_1_coastal', 
#                                       'po_chemicals_2', 'po_chemicals_2_coastal', 
#                                       'po_chemicals_3', 'po_chemicals_3_coastal', 
#                                       'po_nutrients', 'po_nutrients_3nm',
#                                       'hd_subtidal_hb', 'hd_subtidal_sb', 
#                                       'hd_intertidal', 'sp_alien', 'sp_genetic',
#                                       'fp_com_hb', 'fp_com_lb', 'fp_art_hb',
#                                       'fp_art_lb', 'fp_art_lb_3nm'),
#                          'weight'=c(1,1,1,1,1,1,1,1,2,2,1,1,1,3,1,2,1,1))
# 
# eco.weights$layer.id = as.character(eco.weights$layer.id)
# 
# 
# 
# n.eco.pressures.data = plyr::join(eco.pressures.data, eco.weights, by = 'layer.id')
# 
# 
# 
# status = CalculateStatusComponent(status.data, Halpern2012.FP.FIS, s.name='country_id')
# pressures = CalculatePressuresComponent(n.eco.pressures.data, soc.pressures.data,
#                                         c.name = 'layer.id', s.name='region_id')
# 
# 
# names(status)[1] = 'country'
# print(head(status))
# 
# TransformSpatialScheme(schemes.list, status, 'region', 'country', NULL)
