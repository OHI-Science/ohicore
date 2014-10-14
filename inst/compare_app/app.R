require(shiny)
require(git2r)  # need latest: devtools::install_github('ropensci/git2r')
require(stringr)
require(ggvis)
require(dplyr)
require(ohicore)

#devtools::load_all()
#options(error = browser); options(error = utils::recover); options(error = NULL)

ui <- shinyUI(fluidPage(  
  sidebarLayout(
    sidebarPanel(
      textInput('dir_repo', 'Repository:', value = '~/github/ohi-global'),
      textInput('csv_rgns', 'Regions csv:', value='~/github/ohi-global/eez2013/layers/rgn_labels.csv'),
      textInput('csv_goals', 'Goals csv:', value='~/github/ohi-global/eez2013/conf/goals.csv'),
      uiOutput('ui_a_csv'),
      uiOutput('ui_a_commit'),
      uiOutput('ui_b_csv'),
      uiOutput('ui_b_commit')),
#       sliderInput("n", "Number of points", min = 1, max = nrow(mtcars),
#                   value = 10, step = 1),
#      uiOutput("ui_plot")),
    mainPanel(
      tabsetPanel(
        tabPanel(
          'Plot',
          htmlOutput('ggvis_plot')),
        tabPanel(
          'Table',
          dataTableOutput('table')))))
      #plotOutput("distPlot")))
))

server <- function(input, output, session) {
  
  # reactive drop-downs of csv's based on dir_repo  
  csv_paths = reactive({
    p = list.files(input$dir_repo, 'scores\\.csv$', recursive=T)
    return(p)
  })  
  
  output$ui_a_csv <- renderUI({
    selectInput(
      'a_csv', 'A - scores.csv', 
      choices = csv_paths(),
      selected = 1)})

  output$ui_b_csv <- renderUI({
    selectInput(
      'b_csv', 'B - scores.csv', 
      choices = csv_paths(),
      selected = 1)})
  
  # reactive drop-down of commits based on dir_repo
  repo_commits = reactive({
    k = commits(repository(input$dir_repo), topological=F, time=T, reverse=F)
    kn = setNames(
      sapply(k, function(x) x@sha),
      sapply(k, function(x) sprintf('%s: %s...', when(x), str_sub(x@message, 1, 25))))    
    kn
  })
  
  output$ui_a_commit <- renderUI({
    selectInput(
      'a_commit', 'A - commit', 
      choices = repo_commits(),
      selected = 1)})
  
  output$ui_b_commit <- renderUI({
    selectInput(
      'b_commit', 'B - commit', 
      choices = repo_commits(),
      selected = 2)})
  
  # reactive data: difference of A and B
  data <- reactive({
    
    # # debug
    # input = list()
    # input$dir_repo = '~/github/ohi-global'
    # input$csv_rgns = '~/github/ohi-global/eez2013/layers/rgn_labels.csv'
    # input$a_commit = kn[1]
    # input$b_commit = kn[2]
    # input$a_csv    = 'eez2013/scores.csv' # p[3]
    # input$b_csv    = 'eez2013/scores.csv' # p[3]
    # input$csv_rgns = 'eez2013/layers/rgn_labels.csv'
    
    #if (!'a_csv' %in% names(input)) return(reactive({NA}))
    g_csv = isolate(sprintf('%s/conf/goals.csv', dirname(input$a_csv)))
    
    # read in data
    a = read_git_csv(input$dir_repo, input$a_commit, input$a_csv, stringsAsFactors=F)
    b = read_git_csv(input$dir_repo, input$b_commit, input$b_csv, stringsAsFactors=F)
    r = read.csv(input$csv_rgns, stringsAsFactors=F)
    g = read.csv(input$csv_goals, stringsAsFactors=F) %>% arrange(order_color)
    
    # check column names
    stopifnot(names(a) == c('goal','dimension','region_id','score'))
    stopifnot(names(b) == c('goal','dimension','region_id','score'))
    stopifnot(c('rgn_id','label') %in% names(r))
    
    # merge
    d = 
      base::merge(
        a, b, 
        by=c('goal','dimension','region_id'), 
        suffixes=c('.a','.b')) %>%
      rename(rgn_id=region_id) %>%
      mutate(
        score.dif = score.a - score.b,
        score.na = is.na(score.a)!=is.na(score.b)) %>%      
      left_join(
        rbind_list(
          r %>%
            select(rgn_id, rgn_name=label),
          data.frame(rgn_id=0, rgn_name='GLOBAL')),
        by='rgn_id') %>%
      # filter(abs(score_dif) > 0.01 | score_na == T) %>%
      arrange(rgn_id!=0, goal!='Index', dimension!='score', goal, desc(dimension), desc(abs(score.dif)), is.na(score.a), is.na(score.b)) %>%
      select(goal, dimension, rgn_id, rgn_name, score.a, score.b, score.dif) %>%
      mutate(
        goal      = factor(goal, c('Index', g$goal)),
        dimension = factor(dimension, c('score','status','trend','pressure','resilience','future')),
        id        = row_number())
    
    d
  })
  
    
  # ggvis plot ----
  
  output$ggvis_plot <- renderUI({
    ggvisOutput("plot1")
  })

  hover_tip <- function(x) {
    if(is.null(x)) return(NULL)
    row = data()[data()$id == x$id, ]
    return(with(row, sprintf('%s (%d): %g<br />%s %s<br />score.a: %g<br />score.b: %g', rgn_name, rgn_id, score.dif, goal, dimension, score.a, score.b)))
  }

  observe({
    
    if (all(!is.null(input$a_csv), !is.null(input$b_csv), !is.null(input$a_commit), !is.null(input$b_commit))){  
      
      data %>%
        #filter(!is.na(score.dif) & dimension=='score') %>%
        filter(!is.na(score.dif)) %>%
        ggvis(~goal, ~score.dif) %>% 
        #layer_boxplots(pars = list(outpch=NA)) %>%
        layer_points(key := ~id, fill = ~factor(dimension)) %>%    
        group_by(goal) %>%    
        add_tooltip(hover_tip, 'hover') %>%
        bind_shiny("plot1")    
     
      # table ----       
      output$table <- renderDataTable(
        data(),
        options = list(pageLength = 10)
      )
    
    }
  })

  
}

print(shinyApp(ui = ui, server = server))
