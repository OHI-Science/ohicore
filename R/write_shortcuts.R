##' Write shortcuts
##' 
##' Write shortcuts to launch the application.
##' 
##' @aliases WriteScenario ReadScenario
##' @param dir_scenario per \code{\link{launch_app}}
##' @param all_os for all operating systems, ie Mac if on Windows, or Windows if on Mac
##' @keywords ohi
##' @export
write_shortcuts = function(dir_scenario=getwd(), all_os = F) {
  
  # scenario dir
  stopifnot(file.exists(dir_scenario))
  stopifnot(file.exists(sprintf('%s/conf'      , dir_scenario)))
  stopifnot(file.exists(sprintf('%s/layers'    , dir_scenario)))
  stopifnot(file.exists(sprintf('%s/layers.csv', dir_scenario)))
  stopifnot(file.exists(sprintf('%s/spatial'   , dir_scenario)))
    
  # launch_app_code.R
  cat('require(methods)',
      'require(ohicore)',
      'launch_app()',
      sep='\n', file=file.path(dir_scenario, 'launch_app_code.R'))
  
  # launch_app.bat (Win)
  if (.Platform$OS.type == 'windows' | all_os){
    Rscript = sprintf('%s/Rscript.exe', dirname(Sys.which('R')) )
    cat('REM on Microsoft Windows (adjust the path to R.exe as needed)',
        sprintf('%s "%%CD%%\\launch_app_code.R"', Rscript),
        'PAUSE',
        sep='\n', file=file.path(dir_scenario, 'launch_app.bat'))
  }
  
  # launchApp.command (Mac)
  if (!.Platform$OS.type == 'windows' | all_os){
    cat('#!/bin/bash',
        'cd "$(dirname "$BASH_SOURCE")" || {',
        '  echo "Error getting script directory" >&2',
        '  exit 1',
        '}',
        'Rscript --vanilla launch_app_code.R', 
        sep='\n', file=file.path(dir_scenario, 'launch_app.command'))
    Sys.chmod(file.path(dir_scenario, 'launch_app.command'), mode = "0777", use_umask = TRUE)    
  }
  
  
  # calculate_scores.R
  cat("# presume that working directory in current scenario directory, eg:",
      "# setwd('~/github/ohi-global/eez2013')",
      "",
      "# load conf",
      "conf = Conf('conf')",
      "",
      "# run checks on layers",
      "CheckLayers('layers.csv', 'layers', flds_id=conf$config$layers_id_fields)",
      "",
      "# load layers",
      "layers = Layers('layers.csv', 'layers')",
      "",
      "# calculate scores",
      "scores = CalculateAll(conf, layers, debug=F)",
      "write.csv(scores, 'scores.csv', na='', row.names=F)",
      sep='\n', file=file.path(dir_scenario, 'calculate_scores.R'))
  
}