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
        sprintf('Scenario: %s/%s<br><font color="gray">Last updated: %s</font>', basename(dirname(normalizePath(dir_scenario))), basename(dir_scenario), last_updated )))))), 

  div(
    class = "row-fluid",
    tabsetPanel(id='tabsetFunction',
      tabPanel(
        'Data', value='data', 
        conditionalPanel(
          condition="input.tabsetFunction == 'data'",
          sidebarPanel(
            id='data-sidebar',
            selectInput(
              'sel_type', 
              label='1. Choose variable type:', 
              choices=c('Input Layer'='Layer', 'Output Score'='Score'), 
              selected='Score', multiple=F, selectize=T),
            conditionalPanel(
              "input.sel_type == 'Layer'",
              selectInput(
                'sel_layer_target',
                label    = '2. Choose target (goal, pressures, resilience or spatial):',
                selected = sel_layer_target_choices[1],
                choices  = sel_layer_target_choices, multiple=F, selectize=T),
              selectInput(
                'sel_layer', 
                label    = '3. Choose layer:',
                selected = sel_layer_choices[1],
                choices  = sel_layer_choices, multiple=F, selectize=T),          
              conditionalPanel(
                "input.sel_layer_category != 'NA'",
                selectInput(
                  'sel_layer_category',
                  label    = '4. Choose category:',
                  choices  = sel_layer_category_choices,
                  selected = sel_layer_category_choices[1])),          
              conditionalPanel(
                "input.sel_layer_year != 'NA'",
                selectInput(
                  'sel_layer_year', 
                  label    = '5. Choose year:',
                  choices  = sel_layer_year_choices,
                  selected = sel_layer_year_choices[1]))),        
            conditionalPanel(
              "input.sel_type == 'Score'",
              selectInput(
                'sel_score_target',
                label='2. Choose target (index or goal):', 
                choices=sel_score_target_choices, 
                selected='Index'),
              selectInput(
                'sel_score_dimension', 
                label='3. Choose dimension:', 
                choices=sel_score_dimension_choices,
                selected='score')),        
            p(htmlOutput('var_description')),
            verbatimTextOutput(outputId="var_details")),
          
          # TODO: use Select2 combo boxes and search field, see https://github.com/mostly-harmless/select2shiny
          
          mainPanel(
            id='data-main',
            tabsetPanel(
              id='tabsetMap',          
              tabPanel(
                'Map',
                value='data-map', 
                mapOutput('map_container')),
              tabPanel(
                'Histogram', 
                value='data-histogram', 
                plotOutput('histogram')),
              #tabPanel('Summary',   value='data-summary',   verbatimTextOutput('summary')),                     
              tabPanel('Table',     value='data-table', dataTableOutput('table'))))))
      ))
  )))