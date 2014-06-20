#' Launch the browser application
#'
#' This function launches the OHI application into a web browser with the scenario data laoded.
#'
#' @param dir_scenario path to the scenario directory
#' @param ... arguments passed to \code{shiny::runApp}
#' @details The dir_scenario is expected to have the following files and directories (*/), some of which are optional:
#' \itemize{
#'   \item{\emph{conf} - configuration directory. see \code{\link{Conf}} for details.}
#'   \item{\emph{layers}, \emph{layers.csv} - layers.csv registry and layers directory. see \code{\link{Layers}} for details.}
#'   \item{\emph{spatial} - spatial directory, containing the regions_gcs.js. TODO: documentation on this.}
#'   \item{\emph{scores}(optional) - scores output from  \code{\link{CalculateAll}}.}
#' }
#' @keywords app
#' @examples
#' \dontrun{
#' launchApp('~/ohi-global/eez2013')
#' }
#' @import shiny rCharts RJSONIO RColorBrewer
#' @export launch_app
launch_app = function(dir_scenario, debug=F){  
  # library(devtools); load_all(); dir_scenario = '~/github/ohi-global/eez2013'
    
  # check for files/directories
  stopifnot(file.exists(sprintf('%s/conf'      , dir_scenario)))
  stopifnot(file.exists(sprintf('%s/layers'    , dir_scenario)))
  stopifnot(file.exists(sprintf('%s/layers.csv', dir_scenario)))
  stopifnot(file.exists(sprintf('%s/spatial'   , dir_scenario)))
  
  # make objects global in scope
  conf         <<- Conf(sprintf('%s/conf', dir_scenario))
  layers       <<- Layers(
    layers.csv = sprintf('%s/layers.csv' , dir_scenario),
    layers.dir = sprintf('%s/layers'     , dir_scenario))  
  if (file.exists(sprintf('%s/scores.csv', dir_scenario))){
    scores <<- read.csv(sprintf('%s/scores.csv'   , dir_scenario))
  } else {
    scores <<- NULL # TODO: handle NULL scores
  }  
  dir_spatial  <<- sprintf('%s/spatial'  , dir_scenario)
  dir_scenario <<- dir_scenario
  
  dir_app = system.file('shiny_app', package='ohicore')

  # update path for devtools load_all() mode
  if (!file.exists(dir_app))  dir_app =  system.file('inst/shiny_app', package='ohicore')
  
  if (debug) options(shiny.trace=TRUE)
  
  shiny::runApp(appDir=dir_app, ...) # shiny::runApp(appDir=dir_app)
}