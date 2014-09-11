#' Write shortcuts
#' 
#' Write shortcuts to launch the application.
#' 
#' @aliases WriteScenario ReadScenario
#' @param dir_scenario per \code{\link{launch_app}}
#' @param os_files has possible values 0,1 or 2 to determine the files output. See Details.
#' @details The following files are generated based on the value of \code{os_files}:
#' \itemize{
#'   \item{\emph{0}: the files not dependant on operating system are output: launch_app_code.R, calculate_scores.R.}
#'   \item{\emph{1}: in addition to 0, the shortcut for only the running operating system is output: launch_app.bat for Windows, launchApp.command for Mac.}
#'   \item{\emph{2}: in addition to 0, shortcuts for both operating systems are output: launch_app.bat for Windows, launchApp.command for Mac.}
#' }
#' If this function with the \code{os=2} argument is run on a Mac, then the launch_app.bat is unlikely to match the R path on a Windows machine.
#' Whereas, the launchApp.command shortcut on a Mac should still work even if generated on a Windows machine. 
#' @keywords ohi
#' @export
write_shortcuts = function(dir_scenario=getwd(), os_files = 1) {
  
  # scenario dir
  stopifnot(file.exists(dir_scenario))
  stopifnot(file.exists(sprintf('%s/conf'      , dir_scenario)))
  stopifnot(file.exists(sprintf('%s/layers'    , dir_scenario)))
  stopifnot(file.exists(sprintf('%s/layers.csv', dir_scenario)))
  stopifnot(file.exists(sprintf('%s/spatial'   , dir_scenario)))
  
  
  # launch_app_code.R
  cat('require(methods)',
      'suppressWarnings(require(ohicore))',
      'launch_app()',
      sep='\n', file=file.path(dir_scenario, 'launch_app_code.R'))
  
  # calculate_scores.R
  cat("# load required libraries",
      "suppressWarnings(require(ohicore))",
      "",
      "# set working directory to the scenario directory, ie containing conf and layers directories",
      sprintf("setwd('%s')", normalizePath(dir_scenario)),
      "",
      "# load scenario configuration",
      "conf = Conf('conf')",
      "",
      "# run checks on scenario layers",
      "CheckLayers('layers.csv', 'layers', flds_id=conf$config$layers_id_fields)",
      "",
      "# load scenario layers",
      "layers = Layers('layers.csv', 'layers')",
      "",
      "# calculate scenario scores",
      "scores = CalculateAll(conf, layers, debug=F)",
      "write.csv(scores, 'scores.csv', na='', row.names=F)",
      sep='\n', file=file.path(dir_scenario, 'calculate_scores.R'))
  
  # launch_app.bat (Win)
  if ( (.Platform$OS.type == 'windows' & os_files==1) | (os_files == 2) ) {
    Rscript = sprintf('%s/Rscript.exe', dirname(Sys.which('R')) )
    cat('REM on Microsoft Windows (adjust the path to R.exe as needed)',
        sprintf('%s "%%CD%%\\launch_app_code.R"', Rscript),
        'PAUSE',
        sep='\n', file=file.path(dir_scenario, 'launch_app.bat'))
  }
  
  # launchApp.command (Mac)
  if ( (!.Platform$OS.type == 'windows' & os_files==1) | (os_files == 2) ){
    cat('#!/bin/bash',
        'cd "$(dirname "$BASH_SOURCE")" || {',
        '  echo "Error getting script directory" >&2',
        '  exit 1',
        '}',
        'Rscript --vanilla launch_app_code.R', 
        sep='\n', file=file.path(dir_scenario, 'launch_app.command'))
    Sys.chmod(file.path(dir_scenario, 'launch_app.command'), mode = "0777", use_umask = TRUE)
  }
  
}