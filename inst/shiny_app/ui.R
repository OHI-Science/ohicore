# see global.R

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
    Shiny.addCustomMessageHandler("layer_fld_category",
      function(message) {
        alert(JSON.stringify(message));
      }
    );
  '))),                          
  
  div(class= "row-fluid", customHeaderPanel("OHI App")), # alternate to: headerPanel  
   
  absolutePanel(
    top=0, right=0, height=10, width=200, fixed=F,
    div(#style="padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFEE;",
      HTML(markdownToHTML(fragment.only=TRUE, text=c(
        sprintf('Scenario: %s/%s', basename(dirname(normalizePath(dir_scenario))), basename(dir_scenario))))))), 

  div(class = "row-fluid", tabsetPanel(id='tabsetFunction',    # alternate to: mainPanel                                                                             
    tabPanel('Data', value='data', conditionalPanel(condition="input.tabsetFunction == 'data'",
      sidebarPanel(id='data-sidebar',
        selectInput('sel_type', label='1. Choose variable type:', 
                    choices=c('Input Layer'='Layer', 'Output Score'='Score'), 
                    selected='Score', multiple=F, selectize=T),
        conditionalPanel("input.sel_type == 'Layer'",
          selectInput('sel_layer_target' , 
                      label    = '2. Choose target (goal, pressures, resilience or spatial):',
                      selected = sel_layer_target_choices[1],
                      choices  = sel_layer_target_choices, multiple=F, selectize=T),
          selectInput('sel_layer', 
                      label    = '3. Choose layer:',
                      selected = sel_layer_choices[1],
                      choices  = sel_layer_choices, multiple=F, selectize=T),
          conditionalPanel("input.sel_layer_category != 'NA'",
            selectInput('sel_layer_category', 
                        label    = '4. Choose category:', 
                        choices  = sel_layer_category_choices, 
                        selected = sel_layer_category_choices[1])),
          conditionalPanel("input.sel_layer_year != 'NA'",
            selectInput('sel_layer_year', 
                        label    = '5. Choose year:', 
                        choices  = sel_layer_year_choices, 
                        selected = sel_layer_year_choices[1]))),          
        conditionalPanel("input.sel_type == 'Score'",
          selectInput('sel_score_target'   , label='2. Choose target (index or goal):' , choices=sel_score_target_choices   , selected='Index'),        
          selectInput('sel_score_dimension', label='3. Choose dimension:'              , choices=sel_score_dimension_choices, selected='score')),              
        p(htmlOutput('var_description')),
        verbatimTextOutput(outputId="var_details")),
        # TODO: use Select2 combo boxes and search field, see https://github.com/mostly-harmless/select2shiny                                                    
                                                    
      mainPanel(id='data-main',
        tabsetPanel(id='tabsetMap',
          tabPanel('Map',       value='data-map', mapOutput('map_container')), 
          tabPanel('Histogram', value='data-histogram', plotOutput('histogram')),
          #tabPanel('Summary',   value='data-summary',   verbatimTextOutput('summary')),                     
          tabPanel('Table',     value='data-table', dataTableOutput('table')))))),
                                       
    tabPanel('Goals', value='goals', 
       sidebarPanel(id='goal-sidbar', style='overflow:auto; height:850px; width:200px',                     
        strong('Food Provision:'),
          sliderInput("MAR","Mariculture:"                 , min=0, max=smax, value=0.5, step=0.1),br(),
          sliderInput("FIS","Fisheries:"                   , min=0, max=smax, value=0.5, step=0.1),br(),     
        sliderInput("AO",strong("Artisanal Opportunity:")  , min=0, max=smax, value=1, step=0.1),br(),
        sliderInput("NP",strong("Natural Products:")       , min=0, max=smax, value=1, step=0.1),br(),
        sliderInput("CS",strong("Carbon storage:")         , min=0, max=smax, value=1, step=0.1),br(),
        sliderInput("CP",strong("Coastal protection:")     , min=0, max=smax, value=1, step=0.1),br(),       
        sliderInput("TR",strong("Tourism & Recreation:")   , min=0, max=smax, value=1, step=0.1),br(),
        strong('Coastal Livelihoods & Economies:'),
          sliderInput("LIV","Livelihoods:"                 , min=0, max=smax, value=0.5, step=0.1),
          sliderInput("ECO","Economies:"                   , min=0, max=smax, value=0.5, step=0.1),br(),       
        strong('Sense of Place'),
          sliderInput("ICO","Iconic species:"              , min=0, max=smax, value=0.5, step=0.1),
          sliderInput("LSP","Lasting special places:"      , min=0, max=smax, value=0.5, step=0.1),br(),
        sliderInput("CW",strong("Clean waters:")           , min=0, max=smax, value=1, step=0.1),br(),
        strong('Biodiversity'),
          sliderInput("HAB","Habitats:"                    , min=0, max=smax, value=0.5, step=0.1),
          sliderInput("SPP","Species:"                     , min=0, max=smax, value=0.5, step=0.1)
       ),
       mainPanel(id='goal-main', style='overflow:auto; height:850px',
         plotOutput('aster', ))),
     
#     tabPanel('Paths', value='paths', 
#       includeHTML('tree_body.html')),
    
    tabPanel('Calculate', value='configure',
      p('Scenario path exists:', verbatimTextOutput(outputId='dir_scenario_exists')),
      conditionalPanel(condition='output.dir_scenario_exists == "FALSE"',
        textInput('dir_scenario', 'Scenario directory to output:', value=dir_scenario),
        actionButton('btn_write','Write to disk')),
      conditionalPanel(condition='output.dir_scenario_exists == "TRUE"',
             #uiOutput('sel_scenario_dir'), # generates dir_conf              
             #verbatimTextOutput(outputId="txt_conf_summary"),
             p('Scenario path', verbatimTextOutput(outputId="show_dir_scenario")),
             actionButton('btn_calc','Calculate'),
             verbatimTextOutput(outputId="txt_calc_summary"))),
             
    tabPanel('Report', value='report', 
             conditionalPanel(condition='output.dir_scenario_exists == "TRUE"',
               p('Reports directory:', verbatimTextOutput(outputId='dir_reports')),
               textInput('txt_report_fn', 'Report filename to output:', value='report.html')),
             br('Include:'),
             checkboxInput('ck_flowers'    , 'Flowers'   , value = T),
             checkboxInput('ck_tables'     , 'Tables'    , value = T),
             br('Options:'),
             checkboxInput('ck_open_html'  , 'Open in new window', value = T),
             checkboxInput('ck_global_only', 'Global only (vs all regions which takes time)', value = T),
             checkboxInput('ck_overwrite'  , 'Overwrite existing figures', value = F),
#             br('Not yet implemented...'),
#              uiOutput('sel_compare'), # generates dir_conf              
#              checkboxInput('ck_maps'       , 'Maps'      , value = F),
#              checkboxInput('ck_histograms' , 'Histograms', value = F),
#              checkboxInput('ck_equations'  , 'Equations' , value = F),             
#              checkboxInput('ck_paths'      , 'Paths'     , value = F),             
             conditionalPanel(condition='output.dir_scenario_exists == "TRUE"',
               actionButton('btn_report','Generate Report'),
               verbatimTextOutput(outputId="txt_report_summary")),
             conditionalPanel(condition='output.dir_scenario_exists == "FALSE"',
               p('You must write this scenario to the filesystem (see Calculate tab) before generating a report.')))
             
  )) # end tabsetFunction
    
)))