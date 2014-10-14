#' Launch the scneario comparison browser application
#'
#' This function launches the OHI Scenario Comparison application into a web browser.
#' You'll need to have the https://github.com/ropensci/git2r library installed.
#'
#' @import shiny ggvis markdown yaml
#' @export launch_cmp
launch_cmp = function(launch.browser=T, port=NULL, display.mode='normal', ...){  
  
  dir_app = system.file('shiny_app', package='ohicore')
  if (!file.exists(dir_app))  dir_app =  system.file('inst/compare_app', package='ohicore')
  
  shiny::runApp(appDir=dir_app, launch.browser=launch.browser, quiet=quiet, port=port, display.mode=display.mode, ...)
}