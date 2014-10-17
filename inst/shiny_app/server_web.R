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
  

})