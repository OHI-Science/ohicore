# see global.R

#cat(ls(), file='~/Downloads/ohicore_ui_ls.txt'); system('open ~/Downloads/ohicore_ui_ls.txt')

#---- customize for leaflet map
customHeaderPanel <- function(title,windowTitle=title){
  tagList(tags$head(
    tags$title(windowTitle),
    #tags$link(rel="stylesheet", type="text/css", href="/css/tree.css"),
    tags$script(src='spatial/regions_gcs.js')))}      # assume regions geojson variable set by spatial/regions_gcs.js
      
#---- define ui
shinyUI(bootstrapPage(div(class='container-fluid',             # alternate to: pageWithSidebar                          
                          
  # Put this script in the header, and use the HTML() function to make
  # sure that characters like '<' don't get escaped.
  tags$head(tags$script(HTML('

    // debug message
    Shiny.addCustomMessageHandler("layer_fld_category",
      function(message) {
        alert(JSON.stringify(message));
      }
    );'))), 
    
  div(class= "row-fluid", customHeaderPanel("OHI App")), # alternate to: headerPanel  
   
  absolutePanel(
    top=0, right=0, height=10, width=200, fixed=F,
    div(#style="padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFEE;",
      HTML(markdownToHTML(fragment.only=TRUE, text=c(
        sprintf('Scenario: %s/%s', basename(dirname(normalizePath(dir_scenario))), basename(dir_scenario))))))), 

  div(
    class = "row-fluid",
    uiOutput('ui_tabsetpanel'))
  
)))