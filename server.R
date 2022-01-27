server <- function(input, output) {
  
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
  
  ##==========================================
  ##  Sensor map marker click popup window   =
  ##==========================================
  sensor_popup <- function(mast) {
    if(mast=="LIDAR") {
      sensor_url = "https://www.zxlidars.com/wind-lidars/zx-300/"
      img_url <- "https://www.nrgsystems.com/assets/images-products/ZX-1__FitWzEyMDAsODAwXQ.jpg"
    } else {
      sensor_url = "https://adresse_bidon/"
      img_url = "https://uxwing.com/wp-content/themes/uxwing/download/07-web-app-development/image-not-found.png"
    }
    html_txt <- paste0(sep = "<br/>",
                       "<b><a href=", sensor_url, ">", mast, "</a></b>",
                       "<i> <img src = ", img_url, " width = 100> </i>")
    return(html_txt)
  }
  
  ##==========================
  ##  Sensor map rendering   =
  ##==========================
  gray_pin <- makeIcon(iconUrl = "www/img/pin_icon_gray.svg",
                       iconWidth = 20, iconHeight = 30)
  black_pin <- makeIcon(iconUrl = "www/img/pin_icon_black.svg",
                        iconWidth = 20, iconHeight = 30)
  green_pin <- makeIcon(iconUrl = "www/img/pin_icon_green.svg",
                        iconWidth = 20, iconHeight = 30)
  my_icons <- iconList(
    'acoustic' = black_pin,
    'EDF' = black_pin,
    'LIDAR' = black_pin,
    'Sonic 1' = gray_pin,
    'Sonic 1' = gray_pin
  )
  my_icons2 <- iconList(
    'green_pin' = green_pin,
    'black_pin' = black_pin
  )
  output$map_tab_sensors <- renderLeaflet({
    leaflet(data = sensors) %>% 
      addTiles() %>% 
      setView(lng = centre_point[1], lat = centre_point[2], zoom=16) %>%
      addMarkers(data = sensors, lng = ~lon, lat = ~lat, icon = ~my_icons[mast], group = ~mast,
                 popup = ~sapply(mast, sensor_popup))
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
    input$selected_sensors
    sensors <- sensors %>% mutate(color_selected = case_when(name %in% input$sensors ~ "green",
                                                             name %notin% input$sensors ~ "red"))
    sensors <- sensors %>% mutate(color_icon = case_when(color_selected == "green" ~ 'green_pin',
                                                         color_selected != "green" ~ "black_pin"))
    if (input$selected_sensors) {
      leafletProxy(mapId = "map_tab_sensors", data = sensors) %>%
        clearMarkers() %>%   ## clear previous markers
        addMarkers(data = sensors, lng = ~lon, lat = ~lat, icon = ~my_icons2[color_icon], group = ~color_icon,
                   popup = ~sapply(mast, sensor_popup))
      leafletProxy(mapId = "map_tab_visu", data = sensors) %>%
        clearMarkers() %>%   ## clear previous markers
        addMarkers(data = sensors, lng = ~lon, lat = ~lat, icon = ~my_icons2[color_icon], group = ~color_icon,
                   popup = ~sapply(mast, sensor_popup))
    } else {
      leafletProxy(mapId = "map_tab_sensors", data = sensors) %>%
        clearMarkers() %>%   ## clear previous markers
        addMarkers(data = sensors, lng = ~lon, lat = ~lat, icon = ~my_icons[mast], group = ~mast,
                   popup = ~sapply(mast, sensor_popup))
      leafletProxy(mapId = "map_tab_visu", data = sensors) %>%
        clearMarkers() %>%   ## clear previous markers
        addMarkers(data = sensors, lng = ~lon, lat = ~lat, icon = ~my_icons[mast], group = ~mast,
                   popup = ~sapply(mast, sensor_popup))
    }
  })
  
  ##======================
  ##  Table of sensors   =
  ##======================
  output$table_sensors_tab_map <- renderDataTable(dataset, options = list(paging = TRUE, pageLength =  10))
  
  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                            DATA DOWNLOAD                            ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################
  
  ##======================================
  ##  Data download from remote server   =
  ##======================================
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(getwd(), "/data/", "data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      code_txt <- downloading_page(input)
      html_fp = paste(getwd(), "www", "html", "download.html", sep="/")
      write.table(code_txt, file=html_fp, quote = FALSE, col.names = FALSE, row.names = FALSE)
      showModal(modalDialog(includeHTML(html_fp), footer="Cancel", size = "m", easyClose = TRUE))
      on.exit(removeModal())
      data <- download_data(conn, input)
      write.csv(data, file = file, row.names = FALSE)
    }
  )

  
  ##========================
  ##  Acoustic data plot   =
  ##========================
  output$acoustic_plot_tab_data <- renderPlotly({
    data_path = paste(getwd(), "www", "csv", sep = "/")
    if (values$default == 0) {
      file_path = paste(data_path, "sample.csv", sep = "/")
    } else if (values$default >= 1) {
      data_paths = list.files(path = data_path, full.names=TRUE)
      file_path = data_paths[which(str_detect(data_paths, "sample"))]
      # file_path = data_paths[which(file.info(data_paths)$ctime==max(file.info(data_paths)$ctime))]
    }
    print(paste("\n Loading file", file_path, sep = " "))
    dataset <- read_csv(file = file_path, col_names = TRUE)
    p <- ggplot(dataset, aes_string(x='timestamp', y='value')) +
      geom_line() +
      scale_x_datetime(labels = date_format(format = "%y/%m/%d\n%H:%M"),
                       breaks=date_breaks("1 day"),
                       # breaks=as.POSIXct(seq(from = as.POSIXct(x = input$time_slider[[1]], format = "%Y-%m-%d %H:%M:%S"),
                       #                       to = as.POSIXct(x = input$time_slider[[2]], format = "%Y-%m-%d %H:%M:%S"),
                       #                       by = (24*3600))),
                       limits = c(as.POSIXct(x = input$time_slider[[1]], format = "%Y-%m-%d %H:%M:%S"),
                                  as.POSIXct(x = input$time_slider[[2]], format = "%Y-%m-%d %H:%M:%S"))) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      # scale_x_discrete(breaks=unique(df_M$variable),  labels=c("Ambystoma mexicanum",  "Daubentonia madagascariensis", "Psychrolutes marcidus")) +
      xlab(label = "Time") + ylab(label = "Sound level [dB(A)]")
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    if (input$jitter)
      p <- p + geom_jitter()
    
    if (input$smooth)
      p <- p + geom_smooth()
    
    print(p)
  })

  
  ##============================
  ##  Table of acoustic data   =
  ##============================
  output$table_acoustic_tab_data <- renderDataTable(dataset, options = list(paging = TRUE, pageLength =  10))
    
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
  change_time_slider <- reactive({
    update(as.POSIXct(x = input$start_time, format = "%Y-%m-%d %H:%M:%S"), year = year(as.Date(x = input$start_date, format = "%Y-%m-%d")), month = month(as.Date(x = input$start_date, format = "%Y-%m-%d")), mday = day(as.Date(x = input$start_date, format = "%Y-%m-%d")))
  })
  
  ##=======================
  ##  Date range update   =
  ##=======================
  observeEvent(input$time_slider, {
    reactdelay <- 1 # Avoid chain reaction
    the_start_date = as.Date(x = input$start_date, format = "%Y-%m-%d")
    the_start_date_time = update(as.POSIXct(x = input$start_time, format = "%Y-%m-%d %H:%M:%S"), year = year(the_start_date), month = month(the_start_date), mday = day(the_start_date))
    if (difftime(the_start_date_time, change_time_slider()) > reactdelay) {
      change_daterange(the_start_date_time)
      updateDateRangeInput(session,
                           "daterange",
                           start = input$time_slider[[1]],
                           end = input$time_slider[[2]])
    }
  })
  
  ##==================
  ##  Data refresh   =
  ##==================
  values <- reactiveValues(default = 0)
  observeEvent(input$refresh,{
    values$default <- input$refresh
  })
  
  ##========================
  ##  Acoustic data plot   =
  ##========================
  output$acoustic_plot_tab_data <- renderPlotly({
    data_path = paste(getwd(), "www", "csv", sep = "/")
    if (values$default == 0) {
      file_path = paste(data_path, "sample.csv", sep = "/")
    } else if (values$default >= 1) {
      data_paths = list.files(path = data_path, full.names=TRUE)
      file_path = data_paths[which(str_detect(data_paths, "sample"))]
      # file_path = data_paths[which(file.info(data_paths)$ctime==max(file.info(data_paths)$ctime))]
    }
    print(paste("\n Loading file", file_path, sep = " "))
    dataset <- read_csv(file = file_path, col_names = TRUE)
    p <- ggplot(dataset, aes_string(x='timestamp', y='value')) +
      geom_line() +
      scale_x_datetime(labels = date_format(format = "%y/%m/%d\n%H:%M"),
                       breaks=date_breaks("1 day"),
                       # breaks=as.POSIXct(seq(from = as.POSIXct(x = input$time_slider[[1]], format = "%Y-%m-%d %H:%M:%S"),
                       #                       to = as.POSIXct(x = input$time_slider[[2]], format = "%Y-%m-%d %H:%M:%S"),
                       #                       by = (24*3600))),
                       limits = c(as.POSIXct(x = input$time_slider[[1]], format = "%Y-%m-%d %H:%M:%S"),
                                  as.POSIXct(x = input$time_slider[[2]], format = "%Y-%m-%d %H:%M:%S"))) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      # scale_x_discrete(breaks=unique(df_M$variable),  labels=c("Ambystoma mexicanum",  "Daubentonia madagascariensis", "Psychrolutes marcidus")) +
      xlab(label = "Time") + ylab(label = "Sound level [dB(A)]")
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    if (input$jitter)
      p <- p + geom_jitter()
    
    if (input$smooth)
      p <- p + geom_smooth()
    
    print(p)
  })
  
  ##==========================
  ##  Sensor map rendering   =
  ##==========================
  output$map_tab_visu <- renderLeaflet({
    leaflet(data = sensors) %>% 
      addTiles() %>% 
      setView(lng = centre_point[1], lat = centre_point[2], zoom=16) %>%
      addMarkers(data = sensors, lng = ~lon, lat = ~lat, icon = ~my_icons[mast], group = ~mast,
                 popup = ~sapply(mast, sensor_popup))
  })
  
  ##============================
  ##  Table of acoustic data   =
  ##============================
  
  output$table_acoustic_tab_visu <- renderDataTable(dataset)
  
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