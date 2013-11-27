
test_that('example data loads OK', {
  layers <- layers.Nature2012ftp
  schemes.list <- SpatialSchemes(system.file('extdata', 'schemes_navigation.csv', package='ohicore'))
  
  status.data = SelectLayers(layers, mode='layers', cast=T,
                             layers=c('i_fis_bt', 'i_fis_mmsy', 'i_fis_tc'),
                             alternate.layer.names = c('Bt', 'mMSY', 'Tc'),
                             expand.time.invariant = T)
  
  status = CalculateStatusComponent(status.data, Halpern2012.FP.FIS, s.name='country_id')
})