rm(list=ls())

source('../R/ParseLayersNavigation.R')
source('../R/ParseLayers.R')

source('../R/CalculateResilienceComponent.R')

source('../R/ParseSpatialSchemesNavigation.R')
source('../R/ParseSpatialSchemes.R')



source('../R/calc.Subgoal.R')
source('../R/calc.Goal.R')
source('../R/ConvertSpatialScheme.R')
source('../R/Halpern2012.R')
source('../R/subcalc.Pressures.R')
source('../R/subcalc.Status.R')




layers.navigation <- ParseLayersNavigation('data/mod_layers_navigation.csv')
layers.navigation$filepath <- gsub('(.*)', 'data/layers/\\1', layers.navigation$filepath)
layers.list <- ParseLayers(layers.navigation)


focus <- layers.list[layers.list$target %in% c('HAB'), ]
focus.df <- reshape2::dcast(plyr::rbind.fill(focus$layer.data),
                           region_id ~ layer_id,
                           value.var = 'value')


tmp <- ParseSpatialSchemesNavigation('data/schemes_navigation.csv')
tmp2 <- ParseSpatialSchemes(tmp)


print (head(focus.df))
print (focus[, -7])


print (tmp2)
