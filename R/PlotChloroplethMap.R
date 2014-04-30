#' Plot Chloropleth Map
#' 
#' map the percent of residents who are white (or black, or hispanic, or asian) for each county in the United States
#' 
#' @param var  a variable from the counties.RDS data set
#' @param color  any character string you see in the output of colors()
#' @param legend.title  A character string
#' @param max  A parameter for controlling shade range (defaults to 100)
#' @param min	A parameter for controlling shade range (defaults to 0)
#'
#' @return Returns a data.frame with the input data, a likely future
#' status and OHI score, containing columns: status (x), trend (t), resilience (r), 
#' pressure (p), future status (xF) and goal score (score).
#'
#' @details Note: percent map is designed to work with the counties data set
#' It may not work correctly with other data sets if their row order does 
#' not exactly match the order in which the maps package plots counties
#' @keywords ohi map chloropleth
#' @examples
#' \dontrun{
#' 
#' ## When you run percent_map it will plot the counties data as a choropleth map
#' counties <- readRDS("census-app/data/counties.RDS")
#' percent_map(counties$white, "darkgreen", "% white")
#' } 
#' @export
PlotChloroplethMap <- function(var, color, legend.title, min = 0, max = 100) {
  
  require(maps)
  require(mapproj)
  
  counties_rds = system.file('data/counties.RDS', package='ohicore') # system.file(package='ohicore')
  counties <- readRDS(counties_rds)
  
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
    include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]

  # plot choropleth map
  map("county", fill = TRUE, col = fills, 
    resolution = 0, lty = 0, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
    lty = 1, lwd = 1, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min, " % or less"),
    paste0(min + inc, " %"),
    paste0(min + 2 * inc, " %"),
    paste0(min + 3 * inc, " %"),
    paste0(max, " % or more"))
  
  legend("bottomleft", 
    legend = legend.text, 
    fill = shades[c(1, 25, 50, 75, 100)], 
    title = legend.title)
}