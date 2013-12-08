# TODO: move equtions as latex into goals.csv or seperate txt file for easier editing
#   equations.csv = file.path(dir.conf,'equations.csv')
# TODO: add weights to internal layers used by resilience_matrix.csv: c('fishing-v1'=2,'habitat-combo'=2,'species-diversity-3nm'=1,'wgi-all'=1). see ohi::ohi.model.resilience.matrix(b, w)

# config.R set by ohi::launchApp(config.R)

# # required directories ----
# dir.data      = '~/ohi_tbx/data/global_2013a'
# dir.conf      = dirname(config.R)
# dir.scenario  = dirname(dir.conf)
# dir.scenarios = dirname(dir.scenario)
# dir.results   = file.path(dir.scenario, 'results')
# 
# # required files ----
# functions.R            = file.path(dir.conf, 'functions.R')
# goals.csv              = file.path(dir.conf, 'goals_2013a.csv')
# layers_navigation.csv  = file.path(dir.conf, 'layers_navigation_2013a.csv')
# pressures_matrix.csv   = file.path(dir.conf, 'pressures_matrix.csv')
# resilience_matrix.csv  = file.path(dir.conf, 'resilience_matrix.csv')
# resilience_weights.csv = file.path(dir.conf, 'resilience_weights.csv')
# 
# # outputs ----
# layers_data.csv              = file.path(dir.data,     'layers_data.csv')
# regions_goals.csv            = file.path(dir.results,  'regions_goals.csv')
# regions_goals_dimensions.csv = file.path(dir.data,     'regions_goals_dimensions.csv')

# layers ----
layers_id_fields = c('rgn_id','cntry_key','saup_id') # for Nature 2012: c('region_id','country_id','saup_id')
layer_regions = 'rnk_rgn_global'

# pressures & resilience matrices ----
# components describe the layer and level with which to aggregate resilience and pressures matrices for goals with categories
resilience_components = list('NP'  = c('layer'='rnk_np_weights_combo'       , 'level'='region_id-category'),  # old: rnk_np_product_weight
                             'CS'  = c('layer'='rnk_cs_habitat_extent'      , 'level'='region_id'),
                             'CP'  = c('layer'='rnk_cp_habitat_extent_rank' , 'level'='region_id'),           # old: rnk_cp_habitat_extent
                             'HAB' = c('layer'='rnk_hab_presence'           , 'level'='region_id'))
pressures_components  = list('NP'  = c('layer'='rnk_np_weights_combo'       , 'level'='region_id-category'),
                             'CS'  = c('layer'='rnk_cs_habitat_extent'      , 'level'='region_id'),
                             'CP'  = c('layer'='rnk_cp_habitat_extent_rank' , 'level'='region_id'),
                             'LIV' = c('layer'='rnk_le_sector_weight'       , 'level'='region_id'),
                             'ECO' = c('layer'='rnk_le_sector_weight'       , 'level'='region_id'),
                             'HAB' = c('layer'='rnk_hab_presence'           , 'level'='region_id'))
# TODO: inspect whether rnk_cs_habitat_extent is OK for CS given range of 0.82 to 39491, NOT 0 to 1
pressures_categories = list(environmental=c('po','hd','fp','sp','cc'), social='ss')
resilience_categories = c('environmental', 'regulatory', 'social')