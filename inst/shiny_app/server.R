# see global.R

# # reactiveValues ----
# values = reactiveValues()
# dir_scenarios = dirname(dir_scenario)
# values$dirs_scenario <- grep('^scenario\\.', list.dirs(path=dir_scenarios, recursive=F), value=T)
#dirs_scenario = reactiveValues()
#sel_layer_choices = reactiveValues()

# shinyServer ----
# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {
  
  # scenario directories. monitor filesystem every 5 seconds for folders in dir.conf
  observe({
    invalidateLater(5 * 1000, session)  # 5 seconds, in milliseconds
    dirs_scenario <<- grep('^[^\\.]', basename(list.dirs(path=dir_scenarios, recursive=F)), value=T)
  })
  
  # select layer ----
  observe({
    sel_layer_choices = with(subset(layer_targets, target==input$sel_layer_target), 
                             setNames(layer, layer_label))    
    updateSelectInput(session, 'sel_layer', 
                      label='3. Choose layer:', 
                      choices=sel_layer_choices, 
                      selected=ifelse(length(sel_layer_choices)>0, 
                                      sel_layer_choices[1], 
                                      NULL))    
  }, priority=1)
  
  # select layer category ----
  observe({
    lyr = input$sel_layer
    lyr_fld_category = subset(layers$meta, layer==lyr, fld_category, drop=T)
    if (is.na(lyr_fld_category)){
      sel_layer_category_choices = NA
    } else {
      d = layers$data[[lyr]]
      sel_layer_category_choices = sort(as.character(unique(d[[lyr_fld_category]])))
    }
    updateSelectInput(session,  'sel_layer_category', 
                      label    = sprintf('4. Choose %s category:', lyr_fld_category),  
                      choices  = sel_layer_category_choices)
  }, priority=2)
  
  # select layer year ----
  observe({
    # reactives
    lyr          = input$sel_layer
    lyr_category = input$sel_layer_category
    
    lyr_fld_year = subset(layers$meta, layer==lyr, fld_year, drop=T)
    # TODO: with Layers > NP > rnky_np_harvest_relative, getting "Error in names(choices) <- choiceNames : attempt to set an attribute on NULL", should be fixed in future based on https://groups.google.com/forum/?pli=1#!topic/shiny-discuss/K7chwrMCvkU
    if (is.na(lyr_fld_year)){
      sel_layer_year_choices = NA
    } else {
      d = ohicore::SelectLayersData(layers, layers=lyr, narrow=T) # layers$data[[lyr]]            
      if ('category' %in% names(d) && lyr_category != 'NA'){ # !is.na(input$sel_layer_category & input$sel_layer_category!='')){
        d = subset(d, category==lyr_category)
      }
      sel_layer_year_choices = as.character(sort(unique(d$year), decreasing=T))
    }
    updateSelectInput(session,  'sel_layer_year', 
                      label    = '5. Choose year:',  
                      choices  = sel_layer_year_choices,
                      selected = sel_layer_year_choices[1])
  }, priority=3)
  
  # Layers: GetVar() ----
  GetVar <- reactive({
    v = list()
    #browser('GetVar top', expr=input$sel_layer=='mar_harvest_tonnes')
    if (input$sel_type == 'Layer'){
      # reactives
      lyr          = input$sel_layer
      lyr_category = input$sel_layer_category
      lyr_year     = suppressWarnings(as.integer(input$sel_layer_year))
      lyr_target   = input$sel_layer_target
      
      # data and name
      d = ohicore::SelectLayersData(layers, layers=lyr, narrow=T)
      fld_id_num = subset(layers$meta, layer==lyr, fld_id_num, drop=T)
      if (!is.na(fld_id_num) && fld_id_num=='rgn_id'){
        d = rename(d, c('id_num'='rgn_id'))
        v$fld_id = 'rgn_id'
      } else {
        v$fld_id = ifelse(!is.na(fld_id_num), fld_id_num, subset(layers$meta, layer==lyr, fld_id_chr, drop=T))
      }
  
      x = lyr_label = subset(layer_targets, layer==lyr & target==lyr_target, layer_label, drop=T)
      if ('category' %in% names(d)){
        if (!lyr_category %in% d$category){
          lyr_category = sort(unique(d$category))[1]
          updateSelectInput(session, 'sel_layer_category', selected=lyr_category)
        }
        d = subset(d, category==lyr_category)
        x = sprintf('%s : %s', x, lyr_category)
      }
      
      if ('year' %in% names(d)){
        if (!lyr_year %in% na.omit(d$year)){
          lyr_year = max(d$year, na.rm=T)
        }
        d = subset(d, year==lyr_year)
        x = sprintf('%s : %s', x, as.character(lyr_year))
      }
      
      v$data  = d
      v$name  = x
      attr(v$name, 'target') = lyr_target
      v$description = paste0('<b>',lyr_label,'</b>:', 
                             #renderMarkdown(text=subset(layers$meta, layer==lyr, description, drop=T)))
                             #markdownToHTML(text=subset(layers$meta, layer==lyr, description, drop=T), options=c('fragment_only')))
                             renderMarkdown(file=NULL, text=as.character(subset(layers$meta, layer==lyr, description, drop=T))))
      v$details = ''
      m = subset(layers$meta, layer==input$sel_layer)
      v$layer = lyr
      v$meta  = m
      for (f in names(m)){
        s = ifelse(is.numeric(m[[f]]), sprintf('%0.4g', m[[f]]), as.character(m[[f]]))
        v$details = paste0(v$details, sprintf('%s: %s\n', f, s))
      }
    } else if (input$sel_type == 'Score') {
      # reactives
      g = input$sel_score_target    # strsplit(v$name, ' - ')[[1]][1]
      m = input$sel_score_dimension # strsplit(v$name, ' - ')[[1]][2]
        
      v$name = sprintf('%s : %s', g, m)
      attr(v$name, 'target') = g
      attr(v$name, 'dimension') = m
      v$data = plyr::rename(subset(scores, goal==g & dimension==m, c(region_id, score)), c('region_id'='rgn_id', 'score'='val_num'))
      v$description = paste0('<b>', names(sel_score_target_choices[sel_score_target_choices==g]),'</b>: <em>',
                            ifelse(g=='Index',
                                   conf$config$index_description,
                                   as.character(subset(conf$goals, goal == g, description, drop=T))),
                            '</em>\n<br>\n',
                            '<b>', m,'</b>: <em>',conf$config$dimension_descriptions[[m]], '</em>')
      v$details = ''      
    }
    v$summary = sprintf('%s\n\n  count, not NA: %s\n  min: %0.4g\n  mean: %0.4g\n  max: %0.4g\n\n', v$name, length(na.omit(v$data$val_num)), signif(min(v$data$val_num, na.rm=T), 4), mean(v$data$val_num, na.rm=T), max(v$data$val_num, na.rm=T))    
    return(v)
  })
  
  
  

  # Data: info
  output$var_description = renderUI({ HTML(GetVar()$description) })
  
  output$var_details = renderPrint({ v = GetVar(); cat(v$summary, '\n\n', v$details) })
  
  # Data: Map ----
  output$map_container <- renderMap({
      v = GetVar()
      #browser('Data: Map', expr=input$sel_layer=='mar_harvest_tonnes')
      PlotMap(v=v, 
              lon=ifelse(is.null(conf$config$map_lon), 0, conf$config$map_lon), 
              lat=ifelse(is.null(conf$config$map_lat), 0, conf$config$map_lat), 
              zoom=ifelse(is.null(conf$config$map_zoom), 2, conf$config$map_zoom))
    }, html_sub = c('"features": "#! regions !#",' = '"features": regions,'))
  
  
  # Layers: histogram ----
  output$histogram <- renderPlot({
    library(ggplot2)
    
    # get input data
    v = GetVar()
    bin.width = diff(range(v$data[,'val_num'], na.rm=T))/30
    
    # Histogram overlaid with kernel density curve
    p = ggplot(na.omit(v$data), aes(x=val_num))
    p = p + geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                           binwidth=bin.width,
                           colour="black", fill="white") +
      geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
    print(p)
  })  
  
  # Layers: table ----
  output$table <- renderDataTable({
    d = rename(GetVar()$data, c('val_num'='value'))
    
    # HACK: assuming has rgn_id in layer
    d = merge(d, rgn_names, all.x=T)
    d = cbind(d[,c('rgn_id','rgn_name')], d[,!names(d) %in% c('layer','rgn_id','rgn_name'), drop=F])
    d
  })

  # Goals: aster ----
  output$aster <- renderPlot(width=800, height=850, {
    
    # get rescaled weights from slider inputs
    wts = get_wts(input)
    goals.wts = names(wts)
    cols.wts = cols.goals.all[goals.wts]
    
    # get data from results, so far assuming first line of results/regions_goals.csv
    # TODO: add dropdowns for: 1) different regions, 2) different schemes (ie weights)
    x = subset(scores, dimension=='score' & region_id==0)
    scores.wts  = x$score[match(names(wts), as.character(x$goal))] # regions_goals[1, goals.wts]
    index.score = weighted.mean(scores.wts, wts)
    
    # plot aster
    aster(lengths=scores.wts,
          max.length=100,
          widths=wts,
          disk=0.4,
          main='Global',
          fill.col=cols.wts,
          center=round(index.score),
          labels=paste(goals.wts, round(scores.wts), sep='\n'),
          label.cex=1.0,
          label.offset=0.1,
          cex=2.2,
          cex.main=2.5)
    
  })
  
  # Calculate: write ----
  output$show_dir_scenario = renderText({ cat(dir_scenario) })
  observe({
    input$btn_write
    output$dir_scenario_exists = renderText({file.exists(input$dir_scenario)})
    output$show_dir_scenario   = renderText({input$dir_scenario})
    if (input$btn_write == 0){
      state_btn_write <<- 0
      #output$dir_scenario_exists = as.character()
    } else if (input$btn_write != state_btn_write){
      #output$dir_scenario_exists = as.character(file.exists(dir_scenario))
      isolate({
        state_btn_write <<- input$btn_write
        dir_scenario <<- input$dir_scenario
        ohicore::WriteScenario(
          scenario = list(
            conf = conf, 
            layers = layers, 
            scores = scores,
            spatial = dir_spatial,
            dir    = dir_scenario))
      })
      # = file.exists(dir_scenario)
    }
  })

#   # Calculate: sel_scenario_dir ----
#   output$sel_scenario_dir <- renderUI({
#     selectInput("dir_scenario", sprintf("Select scenario (from %s):", dir_scenarios), 
#                 values$dirs_scenario, selected=basename(dir_scenario))
#   })
   
#   # Calculate: txt_conf_summary ----
#   output$txt_conf_summary <- renderPrint({
#     cat('Scenario:', input$dir_scenario,'\n')
#     if (is.null(input$dir_scenario)){
#       config.R = file.path(dir_scenarios, 'conf','config.R')  
#     } else {
#       config.R = file.path(dir_scenarios, input$dir_scenario, 'conf','config.R')  
#     }    
#     #config.check(config.R)
#     #config.summary(config.R)
#   })
   
   # Calculate: txt_calc_summary ----
    observe({      
      input$btn_calc      
      if (input$btn_calc == 0){
        state_btn_calc <<- 0
        output$txt_calc_summary <- renderText({''})
      } else if (input$btn_calc != state_btn_calc){
        isolate({
          state_btn_calc <<- input$btn_calc
          output$txt_calc_summary <- renderText({
            
            #browser('server.R -- Calculate: txt_calc_summary')
            
            # set scenario vars (previously in scenario.R)
            scenario=list(
              conf    = ohicore::Conf(file.path(dir_scenario, "conf")),
              layers  = ohicore::Layers(file.path(dir_scenario, "layers.csv"), file.path(dir_scenario, "layers")),
              scores  = read.csv(file.path(dir_scenario, "scores.csv"), na.strings=""),
              spatial = file.path(dir_scenario, "spatial"),
              dir     = dir_scenario)
            layers <<- scenario$layers
            conf   <<- scenario$conf
            
            cat(sprintf('Checking layers: %s)\n  having conf/Config.R/layers_id_fields: %s', file.path(dir_scenario, 'layers.csv'),paste(conf$config$layers_id_fields, collapse=', ')))
            cat(sprintf('   layers: %s)\n', file.path(dir_scenario, 'layers.csv')))
            
            # check and reload layers
            CheckLayers(file.path(dir_scenario, 'layers.csv'), file.path(dir_scenario, 'layers'), conf$config$layers_id_fields)
            scenario$layers = layers <<- ohicore::Layers(file.path(dir_scenario, 'layers.csv'), file.path(dir_scenario, 'layers'))            
            
            # calculate scores
            setwd(dir_scenario)
            scores <<- ohicore::CalculateAll(scenario$conf, scenario$layers, debug=F)
            write.csv(scores, 'scores.csv', na='', row.names=F)
            
            sprintf('Scores calculated and output to: %s', file.path(dir_scenario, 'scores.csv'))
          })
        })
        
      }
    })  
  
  # Report: sel_compare ----
  output$sel_compare <- renderUI({
    selectInput("dir_compare", "Compare with scenario:",                 
                c('[None]',setdiff(dirs_scenario, basename(dir_scenario))), 
                selected='[None]')
  })
  
  # Report: btn_report ----
  output$dir_reports  = renderText({ file.path(dir_scenario, 'reports') })
  observe({      
    input$btn_report      
    if (input$btn_report == 0){
      state_btn_report <<- 0
      output$txt_report_summary <- renderText({''})
    } else if (input$btn_report != state_btn_report){
      isolate({
        state_btn_report <<- input$btn_report
        ohicore::ReportScores(scenario = list(
                    conf = conf, 
                    layers = layers, 
                    scores = scores,
                    spatial = ifelse(file.exists(system.file('extdata/spatial.www2013', package='ohicore')),
                                           system.file('extdata/spatial.www2013', package='ohicore'),
                                           system.file('inst/extdata/spatial.www2013', package='ohicore'))),
                    directory = file.path(dir_scenario,'reports'),
                    filename = input$txt_report_fn, 
                    open_html=input$ck_open_html,
                    do_flowers=input$ck_flowers, do_tables=input$ck_tables,
                    overwrite=input$ck_overwrite, global_only=input$ck_global_only, 
                    # TODO: implement...                         
                    do_maps=input$ck_maps, do_histograms=input$ck_histograms, do_equations=input$ck_equations, do_paths=input$ck_paths, 
                    debug=F) 
        output$txt_report_summary <- renderText({
          #browser()
          cat('success')
        })
      })
      
    }
  })  
  

#    output$txt_calc_summary <- renderText({
#      #cat('input$btn_conf_calc:',input$btn_conf_calc,'\n')
#      
#      if (input$btn_calc == 0 & length(input$dir_scenario)>0){
#        return('')
#        btn_calc_counter <<- 0
#      } else {
#        btn_calc_counter = btn_calc_counter + 1
#        # outiside if and inside isolat()?
#        return(c('Scenario:',input$dir_scenario,'\n',
#                 'btn_calc_counter:',as.character(btn_calc_counter),
#                 'TODO: integrate with execution of sequence of functions and display calculated summary.\n'))     
#      }
#    })
  

})

#                                                       tabPanel('Goals', value='goals', 
#                                                                sidebarPanel(id='goal-sidbar', style='overflow:auto; height:850px; width:200px',                     
#                                                                             strong('Food Provision:'),
#                                                                             sliderInput("MAR","Mariculture:"                 , min=0, max=smax, value=0.5, step=0.1),br(),
#                                                                             sliderInput("FIS","Fisheries:"                   , min=0, max=smax, value=0.5, step=0.1),br(),     
#                                                                             sliderInput("AO",strong("Artisanal Opportunity:")  , min=0, max=smax, value=1, step=0.1),br(),
#                                                                             sliderInput("NP",strong("Natural Products:")       , min=0, max=smax, value=1, step=0.1),br(),
#                                                                             sliderInput("CS",strong("Carbon storage:")         , min=0, max=smax, value=1, step=0.1),br(),
#                                                                             sliderInput("CP",strong("Coastal protection:")     , min=0, max=smax, value=1, step=0.1),br(),       
#                                                                             sliderInput("TR",strong("Tourism & Recreation:")   , min=0, max=smax, value=1, step=0.1),br(),
#                                                                             strong('Coastal Livelihoods & Economies:'),
#                                                                             sliderInput("LIV","Livelihoods:"                 , min=0, max=smax, value=0.5, step=0.1),
#                                                                             sliderInput("ECO","Economies:"                   , min=0, max=smax, value=0.5, step=0.1),br(),       
#                                                                             strong('Sense of Place'),
#                                                                             sliderInput("ICO","Iconic species:"              , min=0, max=smax, value=0.5, step=0.1),
#                                                                             sliderInput("LSP","Lasting special places:"      , min=0, max=smax, value=0.5, step=0.1),br(),
#                                                                             sliderInput("CW",strong("Clean waters:")           , min=0, max=smax, value=1, step=0.1),br(),
#                                                                             strong('Biodiversity'),
#                                                                             sliderInput("HAB","Habitats:"                    , min=0, max=smax, value=0.5, step=0.1),
#                                                                             sliderInput("SPP","Species:"                     , min=0, max=smax, value=0.5, step=0.1)
#                                                                ),
#                                                                mainPanel(id='goal-main', style='overflow:auto; height:850px',
#                                                                          plotOutput('aster', ))),
#                                                       
#                                                         tabPanel('Paths', value='paths', 
#                                                           includeHTML('tree_body.html')),

