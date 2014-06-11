##' Read or Write Scenario
##' 
##' Read or write all the necessary elements, ie "scenario", of the Ocean Health Index.
##' 
##' @aliases WriteScenario ReadScenario
##' @param scenario list of (conf, layers, scores, spatial, dir)
##' @param scenario.R code to source and set scenario
##' @return Returns scenario
##' @seealso Conf, Layers, scores
##' @keywords ohi
##' @export WriteScenario ReadScenario
##' @name ReadWriteScenario
WriteScenario = function(
  scenario = list(
    conf   = ohicore::conf.Global2013.www2013, 
    layers = ohicore::layers.Global2013.www2013, 
    scores = ohicore::scores.Global2013.www2013,
    spatial = system.file('extdata/spatial.www2013', package='ohicore'),
    #spatial = system.file('inst/extdata/spatial.www2013', package='ohicore'),
    dir    = path.expand('~/myohi/scenario.Global2013.www2013')),
  make_all_launchApp = F
  ) {
  
  
# load_all('~/Code/ohicore'); load_all('~/Code/ohigui')
# if (!require(devtools)) install.packages('devtools'); require(devtools); install_github('rCharts','bbest'); install_github('ohicore','bbest'); install_github('ohigui','bbest')
# load_all('~/Code/rCharts'); load_all('~/Code/ohicore'); setwd('~/Code/ohigui'); load_all(); shiny::runApp(appDir=system.file('inst/shiny_app', package='ohigui'))
  
  # paths
  dir_scenario = path.expand(scenario$dir)
  dir_conf     = file.path(dir_scenario, 'conf')
  dir_layers   = file.path(dir_scenario, 'layers')
  csv_layers   = sprintf('%s.csv', dir_layers)
  csv_scores   = file.path(dir_scenario, 'scores.csv') 
  dir_spatial   = file.path(dir_scenario, 'spatial')
  r_scenario   =  file.path(dir_scenario, 'scenario.R') 
  sh_app       =  file.path(dir_scenario, 'run_app.sh') 

  # scenario dir
  dir.create(dir_scenario, recursive=T, showWarnings=F)
  
  # conf object
  scenario$conf$write(dir_conf)

  # layers object
  scenario$layers$write(csv_layers, dir_layers)
  
  # scores csv
  write.csv(scenario$scores, csv_scores, na='', row.names=F)
  
  # spatial dir
  dir.create(dir_spatial, recursive=T, showWarnings=F)
  for (f in list.files(scenario$spatial, full.names=T)){ # f = list.files(scenario$spatial, full.names=T)[1]
    file.copy(f, file.path(dir_spatial, basename(f)))
  }
  
  # scenario
  cat('require(ohicore)',
      '',
      sprintf('wd = "%s"',  dir_scenario),
      'scenario=list(',
      '  conf   = ohicore::Conf(file.path(wd, "conf")),',
      '  layers = ohicore::Layers(file.path(wd, "layers.csv"), file.path(wd, "layers")),',
      '  scores = read.csv(file.path(wd, "scores.csv"), na.strings=""),',
      '  spatial = file.path(wd, "spatial"),',
      '  dir    = wd)',
      sep='\n', file=file.path(dir_scenario, 'scenario.R'))  
  
  # launch app files
  cat('require(methods)',
      'require(ohicore)',
      'require(ohigui)',
      '',
      'wd = getwd()',
      'ohigui::launchApp(scenario=list(',
      '  conf   = ohicore::Conf("conf"),',
      '  layers = ohicore::Layers("layers.csv", "layers"),',
      '  scores = read.csv("scores.csv", na.strings=""),',
      '  spatial = file.path(wd, "spatial"),',
      '  dir    = wd), launch.browser=T)',
      sep='\n', file=file.path(dir_scenario, 'launchApp_code.R'))
  
  if (.Platform$OS.type == 'windows' | make_all_launchApp){
    Rscript = sprintf('%s/Rscript.exe', dirname(Sys.which('R')) )
    cat('REM on Microsoft Windows (adjust the path to R.exe as needed)',
        sprintf('%s "%%CD%%\\launchApp_code.R"', Rscript),
        'PAUSE',
        sep='\n', file=file.path(dir_scenario, 'launchApp.bat'))
  }
  if (!.Platform$OS.type == 'windows' | make_all_launchApp){
    cat('#!/bin/bash',
        'cd "$(dirname "$BASH_SOURCE")" || {',
        '  echo "Error getting script directory" >&2',
        '  exit 1',
        '}',
        'Rscript --vanilla launchApp_code.R', 
        sep='\n', file=file.path(dir_scenario, 'launchApp.command'))
    Sys.chmod(file.path(dir_scenario, 'launchApp.command'), mode = "0777", use_umask = TRUE)    
  }
  
}

ReadScenario = function(scenario.R) {  
  source(scenario.R)  
}