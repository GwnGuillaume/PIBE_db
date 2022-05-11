server <- function(input, output, session) {
  
  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                      PIBE PROJECT PRESENTATION                      ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################
  
  ##=======================
  ##  PIBE project logo   =
  ##=======================
  pibe_logo = "https://www.anr-pibe.com/sites/pibe/files/anr-pibe-logo.gif"
  output$pibe_logo<-renderText({c('<img src="', pibe_logo, '">')})
  
  ##================================
  ##  PIBE project html document   =
  ##================================
  output$pibe_project_html <- renderUI({includeHTML(path = file.path("www", "html", "pibe_project_presentation.html"))})
  
  ############################################################################
  ############################################################################
  ###                                                                      ###
  ###                              SENSOR MAP                              ###
  ###                                                                      ###
  ############################################################################
  ############################################################################
  
  ##===================================
  ##  Sensor map marker click event   =
  ##===================================
  observeEvent(input$map_marker_click, { 
    p <- input$map_marker_click
    print(p)
  })
  
  ##==========================
  ##  Sensor map rendering   =
  ##==========================

  output$map_tab_sensors <- renderLeaflet({
    map_sensors_tab
    # leaflet(data = sensors) %>% 
    #   addTiles() %>% 
    #   addCircleMarkers(lng = centre_point[1], lat = centre_point[2]) %>%
    #   setView(lng = centre_point[1], lat = centre_point[2], zoom = 14) %>%
    #   addMarkers(data = sensors, lng = ~lon, lat = ~lat, icon = ~my_icons[mast], group = ~mast,
    #              popup = ~sapply(mast, sensor_popup))
  })
  
  observe({
    input$map_tiles
    if (input$map_tiles == 'Stamen.Toner') {
      leafletProxy(mapId = "map_tab_sensors", data = sensors) %>%
        addProviderTiles(providers$Stamen.Toner)
      leafletProxy(mapId = "map_tab_visu", data = sensors) %>%
        addProviderTiles(providers$Stamen.Toner)
    } else if (input$map_tiles == 'CartoDB.Positron') {
      leafletProxy(mapId = "map_tab_sensors", data = sensors) %>%
        addProviderTiles(providers$CartoDB.Positron)
      leafletProxy(mapId = "map_tab_visu", data = sensors) %>%
        addProviderTiles(providers$CartoDB.Positron)
    } else if (input$map_tiles == 'GeoportailFrance') {
      leafletProxy(mapId = "map_tab_sensors", data = sensors) %>%
        addProviderTiles(providers$GeoportailFrance.ignMaps)
      leafletProxy(mapId = "map_tab_visu", data = sensors) %>%
        addProviderTiles(providers$GeoportailFrance.ignMaps)
    } else if (input$map_tiles == 'Esri.NatGeoWorldMap') {
      leafletProxy(mapId = "map_tab_sensors", data = sensors) %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap)
      leafletProxy(mapId = "map_tab_visu", data = sensors) %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap)
    }
  })
  
  observe({
    input$acoustic_sensors
    sensors <- sensors %>% mutate(color_selected = case_when(name %in% input$acoustic_sensors ~ "green",
                                                             name %notin% input$acoustic_sensors ~ "red"))
    sensors <- sensors %>% mutate(color_icon = case_when(color_selected == "green" ~ 'green_pin',
                                                         color_selected != "green" ~ "black_pin"))
    if(isTRUE(input$selected_sensors)){
      leafletProxy(mapId = "map_tab_sensors", data = sensors) %>%
      clearMarkers() %>%   ## clear previous markers
      addMarkers(data = sensors, lng = ~lon, lat = ~lat, icon = ~my_icons2[color_icon], group = ~color_icon,
                 popup = ~sapply(mast, sensor_popup))
    } else {
      leafletProxy(mapId = "map_tab_sensors", data = sensors) %>%
        clearMarkers() %>%   ## clear previous markers
        addMarkers(data = sensors, lng = ~lon, lat = ~lat, icon = ~my_icons[mast], group = ~color_icon,
                   popup = ~sapply(mast, sensor_popup))
    }
    if(isTRUE(input$selected_sensors)){
      leafletProxy(mapId = "map_tab_visu", data = sensors) %>%
        clearMarkers() %>%   ## clear previous markers
        addMarkers(data = sensors, lng = ~lon, lat = ~lat, icon = ~my_icons2[color_icon], group = ~color_icon,
                   popup = ~sapply(mast, sensor_popup))
    } else {
      leafletProxy(mapId = "map_tab_visu", data = sensors) %>%
        clearMarkers() %>%   ## clear previous markers
        addMarkers(data = sensors, lng = ~lon, lat = ~lat, icon = ~my_icons[mast], group = ~color_icon,
                   popup = ~sapply(mast, sensor_popup))
    }
  })
  
  ##======================
  ##  Table of sensors   =
  ##======================
  output$table_sensors_tab_map <- renderDataTable(sensors, options = list(paging = TRUE, pageLength =  10))
  
  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                            DATA DOWNLOAD                            ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################

  ##===================================================
  ##  Update the values of the dates and times input  =
  ##===================================================  
  observe({
    updateDateInput(session, 
                    "start_date", 
                    value = input$start_date_download)
  })
  observe({
    updateTimeInput(session, 
                    "start_time", 
                    value = strptime(input$start_time_download, "%H:%M"))
  })
  observe({
    updateDateInput(session,
                    "end_date",
                    value = input$end_date_download)
  })
  observe({
    updateTimeInput(session, 
                    "end_time", 
                    value = strptime(input$end_time_download, "%H:%M"))
  })
  ##=========================================================================
  ##  Enable or disable buttons to select acoustic sensors, indicators and  =
  ##  frequencies according to the scientific_domain choice
  ##=========================================================================
  observe({
    if (input$scientific_domain == 'acoustique' || input$scientific_domain == 'both') {
      shinyjs::enable("acoustic_sensors")
      shinyjs::enable("acoustic_indicators")
      shinyjs::enable("acoustic_frequencies")
    } else {
      shinyjs::disable("acoustic_sensors")
      shinyjs::disable("acoustic_indicators")
      shinyjs::disable("acoustic_frequencies")
    }
  })
  
  # ##====================================
  # ##  Data download folder selection   =
  # ##====================================
  # volumes = c(data = paste(getwd(), "www", "data", sep = .Platform$file.sep), Desktop = path.expand("~"))
  # shinyDirChoose(input, "downloadFolder",
  #                roots = volumes,
  #                defaultRoot = volumes[1], 
  #                filetypes=c('', 'csv'), session = session)
  # resultFolder <- reactive(input$downloadFolder)
  # output$resultFolder = renderText({ parseDirPath(volumes, resultFolder()) })
  
  ##======================================
  ##  Data download from remote server   =
  ##======================================
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("data-", Sys.Date(), ".csv")
    },
    content = function(file) {
      code_txt <- downloading_page(input)
      html_fp = paste(getwd(), "www", "html", "download.html", sep = .Platform$file.sep)
      write.table(code_txt, file=html_fp, quote = FALSE, col.names = FALSE, row.names = FALSE)
      showModal(modalDialog(
        includeHTML(html_fp), 
        footer = tagList(actionButton(inputId = 'cancelDownload', label = 'Cancel'), modalButton("Ok")),  # modalButton("Cancel"), 
        size = "m", 
        easyClose = TRUE))
      on.exit(removeModal())
      start_time_download = strftime(as.character(input$start_time_download), format = "%H:%M")
      end_time_download = strftime(as.character(input$end_time_download), format = "%H:%M")
      download <- get_data(conn, input$start_date_download, start_time_download, input$end_date_download, end_time_download, input$time_sampling, input$scientific_domain, input$acoustic_sensors, input$acoustic_indicators, input$acoustic_frequencies)
      # data <- data.frame(sapply(download$data, unlist))
      print("\t -< data downloaded")
      print("Writing data")
      write.csv(download$data, file = file, row.names = FALSE)
      print("\t -< data written")
    }
  )
  
  observeEvent(input$cancelDownload, {
    removeModal()
  })

  ##========================
  ##  Acoustic data plot   =
  ##========================
  
  # observe({
  #   cat("\ninput$file value:\n\n")
  #   print(input$file)
  # })
  
  data <- reactiveValues(
    filename = init$selection$csv_in_filename, 
    filepath = init$selection$csv_in_filepath, 
    csv = init$selection$csv_in_raw, 
    plot = init$selection$csv_in_plot, 
    time_slider.min = init$selection$start_date_time,
    time_slider.max = init$selection$end_date_time
  )
  
  observeEvent(input$data_file, {
    infile <- input$data_file
    print('INFILE')
    print(infile)
    if (identical(infile$name, "sample.csv")) {    # User has not uploaded a file yet
      return(list(filename = infile$name, 
                  filepath = init$selection$csv_in_filepath, 
                  csv <- init$selection$csv_in_file_data, 
                  plot <- init$selection$csv_in_plot, 
                  time_slider.min = init$selection$start_date_time,
                  time_slider.max = init$selection$end_date_time))
    }
    the_data_path = infile$datapath
    the_csv_data = read_csv(file = the_data_path, col_names = TRUE)
    timeslider_min = the_csv_data$start_time[1]
    timeslider_max = the_csv_data$start_time[length(the_csv_data$start_time)]
    data$filename <- infile$name
    data$filepath <- the_data_path
    data$csv <- the_csv_data
    data$plot <- format_data_for_app(the_csv_data)
    data$time_slider.min <- timeslider_min
    data$time_slider.max <- timeslider_max
  })
  
  observeEvent(input$data_file, {
    updateSliderInput(session, "time_slider",
                    min = as.POSIXct(x = data$time_slider.min, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC"),
                    max = as.POSIXct(x = data$time_slider.max, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC"),
                    value =  c(as.POSIXct(x = data$time_slider.min, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC"),
                               as.POSIXct(x = data$time_slider.max, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC")), 
                    timeFormat = "%d/%m %H:%M")
  })
  
  output$acoustic_plot_tab_data <- renderPlotly({
    data$plot$timestamp <- as.POSIXct(data$plot$timestamp, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC")
    nb_days = as.double(difftime(data$plot$timestamp[length(data$plot$timestamp)], data$plot$timestamp[1]))
    if(nb_days <= 5){
      date_breaker = "1 day"
    } else {
      date_breaker = "2 day"
    }
    p <- ggplot(data = data$plot, aes_string(x='timestamp', y='value')) +
      geom_line() +
      scale_x_datetime(labels = date_format(format = "%y/%m/%d\n%H:%M"), timezone = "UTC",
                       breaks = date_breaks(date_breaker),
                       # breaks=as.POSIXct(seq(from = as.POSIXct(x = input$time_slider[[1]], format = "%Y-%m-%d %H:%M:%S"),
                       #                       to = as.POSIXct(x = input$time_slider[[2]], format = "%Y-%m-%d %H:%M:%S"),
                       #                       by = (24*3600))),
                       limits = c(as.POSIXct(x = input$time_slider[[1]], format = "%Y-%m-%d_%H:%M:%OS"),
                                  as.POSIXct(x = input$time_slider[[2]], format = "%Y-%m-%d_%H:%M:%OS"))) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      labs(title = paste("Filename:", data$filename, sep = " "), x ="Time", y = "Sound level [dB(A)]")
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    if (input$jitter)
      p <- p + geom_jitter()
    
    if (input$smooth)
      p <- p + geom_smooth()
    
    height <- session$clientData$output_p_height
    width <- session$clientData$output_p_width
    ggplotly(p, height = height, width = width)
  })
  
  ##============================
  ##  Table of acoustic data   =
  ##============================
  output$table_acoustic_tab_data <- renderDataTable(data$plot, options = list(paging = TRUE, pageLength =  10))
    
  ############################################################################
  ############################################################################
  ###                                                                      ###
  ###                          DATA VISUALIZATION                          ###
  ###                                                                      ###
  ############################################################################
  ############################################################################
  
  ##========================
  ##  Time slider update   =
  ##========================
  # change_time_slider <- reactive({
  #   print("rrrrrrrr")
  #   print(start_time_download)
  #   print(paste(input$start_date_download, input$start_time_download, sep = " "))
  #   the_date = as.Date(x = input$start_date_download, format = "%Y-%m-%d")
  #   update(as.POSIXct(x = input$start_time_download, format = "%H:%M"), 
  #          year = year(the_date), 
  #          month = month(the_date), 
  #          mday = day(the_date))
  # })
  
  ##=======================
  ##  Date range update   =
  ##=======================
  # observe({
  #   the_start_date_time <- format_datetime_from_time_slider(input$start_date_download, input$start_time_download)
  #   the_end_date_time <- format_datetime_from_time_slider(input$end_date_download, input$end_time_download)
  #   # Control the value, min, max, and step.
  #   # Step size is 2 when input value is even; 1 when value is odd.
  #   updateSliderInput(session, "time_slider", 
  #                     min = as.POSIXct(x = the_start_date_time, format = "%Y-%m-%d %H:%M:%S CET"),
  #                     max = as.POSIXct(x = the_end_date_time, format = "%Y-%m-%d %H:%M:%S CET"),
  #                     value =  c(as.POSIXct(x = the_start_date_time, format = "%Y-%m-%d %H:%M:%S CET"), 
  #                                as.POSIXct(x = the_end_date_time, format = "%Y-%m-%d %H:%M:%S CET")), )
  # })
  #
  # observeEvent(input$time_slider, {
  #   print(paste0("Date range for time_slider from ", input$start_date_download, " to ", input$end_date_download))
  #   reactdelay <- 1 # Avoid chain reaction
  #   start_date_time.download = as.POSIXct(dataset$start_time[1], format = "%Y-%m-%d %H:%M:%S UTC")
  #   start_date_time.input =  as.POSIXct(paste(input$start_date_download, format(input$start_time_download, "%H:%M"), sep = " "))
  #   end_date_time.input =  as.POSIXct(paste(input$end_date_download, format(input$end_time_download, "%H:%M"), sep = " "))
  #   updateDateRangeInput(session,
  #                        "daterange",
  #                        start = start_date_time.input,
  #                        end = end_date_time.input)
  # })
  
  ##==================
  ##  Data refresh   =
  ##==================
  # values <- reactiveValues(default = 0)
  # observeEvent(input$refresh,{
  #   values$default <- input$refresh
  # })
  
  ##==========================
  ##  Sensor map rendering   =
  ##==========================
  output$map_tab_visu <- renderLeaflet({
    map_visu_tab
    # leaflet(data = sensors) %>% 
    #   addTiles() %>% 
    #   setView(lng = centre_point[1], lat = centre_point[2], zoom = 14) %>%
    #   addMarkers(data = sensors, lng = ~lon, lat = ~lat, icon = ~my_icons[mast], group = ~mast,
    #              popup = ~sapply(mast, sensor_popup))
  })
  
  ##============================
  ##  Table of acoustic data   =
  ##============================
  
  output$table_acoustic_tab_visu <- renderDataTable(data_app)
  
  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                            MISCELLANEOUS                            ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################
  output$histo <- renderPlot({
    set.seed(122)
    histdata <- rnorm(500)
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  ##==========================================================
  ##  Reuse the same output binding on different tab panels  =
  ##==========================================================
  options<-callModule(modal.options,"options1",options=if(exists("options")){options}else{reactiveValues(check1=FALSE)})
  options<-callModule(modal.options,"options2",options=if(exists("options")){options}else{reactiveValues(check1=FALSE)})
  output$out1<-output$out2<-renderText({options$check1})
} # End sever
