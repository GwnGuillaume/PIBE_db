#' server.R
#'
#' @title Shiny server function for PIBE project presentation
#' @description This function defines the server logic for the PIBE project presentation Shiny app.
#' @param input The input values from the Shiny app interface.
#' @param output Reactive expressions and outputs to update in the Shiny app.
#' @param session The Shiny session object.
#' @details This server function includes sections for PIBE project presentation, session timer,
#' connection to the server, sensor map, data download, data visualization, and miscellaneous.
#' @examples 
#' server(input, output, session)
server <- function(input, output, session) {
  #' 
  #' PIBE PROJECT PRESENTATION
  #' 
  #' PIBE project logo
  pibe_logo <- "https://www.anr-pibe.com/sites/pibe/files/anr-pibe-logo.gif"
  output$pibe_logo<-renderText({c('<img src="', pibe_logo, '">')})
  #' 
  #' PIBE project html document
  output$pibe_project_html <- renderUI({includeHTML(path = file.path(dirname(getwd()), "www", "html", "pibe_project_presentation.html"))})
  output$pibe_project_WP2 <- renderUI({includeHTML(path = file.path(dirname(getwd()), "www", "html", "pibe_project_WP2.html"))})
  output$pibe_project_partners <- renderUI({includeHTML(path = file.path(dirname(getwd()), "www", "html", "pibe_project_partners.html"))})
  #' 
  #' Session timer
  session_start_datetime <- as.POSIXct(format(Sys.time()), tz = "UTC")
  session_timer <- reactiveTimer(intervalMs = 1000, session = getDefaultReactiveDomain())
  secs_timer <- sessions_secs_timer(start_datetime = session_start_datetime)
  session_counter_i <- secs_timer
  session_counter <- reactive({
    session_timer()
    on.exit(session_counter_i <<- round(x = as.double(sessions_secs_timer(start_datetime = session_start_datetime)), digits = 0))
    session_counter_i
  })
  #' 
  #' Session counter
  output$session_counter <- renderText(paste0("Session time: ",
                                              stri_replace_all_fixed(str = paste0(seconds_to_period(session_counter())),
                                                                     pattern = c("H", "M", "S"),
                                                                     replacement = c("h",  "min", "s"),
                                                                     vectorize_all = FALSE)))
  #' 
  #' REMOTE SERVER CONNECTION
  #' 
  #' Close shiny app and window
  observe({
    if (input$closeApp > 0){
      stopApp()
      }
  })
  #' 
  #' Connection details to the Elasticsearch engine
  elastic_conn <- connect(host = es_params$host, port = es_params$port, user = es_params$user, pwd = es_params$pwd, 
                          path=es_params$path, transport_schema = es_params$transport_schema, cainfo = es_params$cainfo)
  #' 
  #' Ping connection
  cluster_ping <-  reactive({ping_database(conn = elastic_conn)})
  #' 
  activeShards <- reactiveVal(0)
  #' 
  #' Connection Button
  observeEvent(eventExpr = input$connectionButton, handlerExpr = {
    cluster_ping <- ping_database(conn = elastic_conn)
    if(str_detect(string = cluster_ping$message, pattern = "Successful")){
      #' 
      #' Cluster health
      cluster_health = get_elastic_health(conn = elastic_conn)
      shinyalert(title = "Connection successful !", text = "Successfully connected to database.", 
                 type = "success", closeOnClickOutside = TRUE)
      output$connect_button_style <- renderUI({tags$head(tags$style(".connect_button{background-color: #c7c7c7;}"))})
      output$conn_bsTooltip <- renderUI({bsTooltip(id = "connectionButton", title = "You are connected to the database.", 
                                                   placement = "bottom", trigger = "hover", options = list(container = "body"))})
      output$server_start_datetime <- renderText({paste0("Starting the server at ", 
                                                         strftime(x = as.POSIXct(as.integer(cluster_health$epoch), 
                                                                                 origin = "1970-01-01", tz = "UTC"), 
                                                                  format = "%Y-%m-%d %H:%M"))})
      output$pending_tasks <- renderText({paste0("Number of pending tasks: ", cluster_health$pending_tasks)})
      output$max_task_wait_time <- renderText({paste0("Longest task pending time: ", cluster_health$max_task_wait_time)})
      output$active_shards_percent <- renderText({cluster_health$active_shards_percent})
      output$server_status <- renderText({cluster_health$status})
      activeShards(as.double(sub("%", "", cluster_health$active_shards_percent)))
    } else if(str_detect(string = cluster_ping$message, pattern = "Failed")){
      shinyalert(title = "Connection failed", text = "Please retry to connect.", 
                 type = "error", closeOnClickOutside = TRUE)
      output$connect_button_style <- renderUI({tags$head(tags$style(".connect_button{background-color: #27ae60;}"))})
      output$conn_bsTooltip <- renderUI({bsTooltip(id = "connectionButton", title = "Connection failed. Please retry to connect.", 
                                                   placement = "bottom", trigger = "hover", options = list(container = "body"))})
      output$server_start_datetime <- renderText({"Not connected to server"})
      output$pending_tasks <- renderText({"No info on pending tasks"})
      output$max_task_wait_time <- renderText({"No info on longest task pending time"})
      output$active_shards_percent <- renderText({"0%"})
      output$server_status <- renderText({"Unknown status"})
      activeShards(0)
    }
  }, 
  ignoreNULL = FALSE)
  #' 
  output$active_shards <- renderUI(taskItem(text = 'Cluster health', activeShards(), color = "green"))
  #' Server health
  # serverStartDatetime <- reactive({
  #   server_start <- as.POSIXct(as.integer(pibe_cluster_health["epoch"]), origin = "1970-01-01", tz = "UTC")
  #   server_start_text <- strftime(x = server_start, format = "%Y-%m-%d %H:%M")
  #   return(server_start)
  # })
  # output$server_start_datetime <- renderText(paste0("Starting the server at ", serverStartDatetime()))
  # output$pending_tasks <- renderText(paste0("Number of pending tasks: ", pibe_cluster_health["pending_tasks"]))
  # output$max_task_wait_time <- renderText(paste0("Longest task pending time: ", pibe_cluster_health["max_task_wait_time"]))
  # output$active_shards_percent <- renderText(as.numeric(sub("%", "", pibe_cluster_health["active_shards_percent"])))
  # output$server_status <- renderText({paste0(pibe_cluster_health["status"])})
  #' 
  #' SENSOR MAP
  #' 
  #' Locations of sensors
  sensors_filename <- paste(dirname(getwd()), "www", "csv", "sensors_info.csv", sep = .Platform$file.sep)
  sensors_info <- get_meteo_sensors_info(sensors_filename = sensors_filename)
  sensors_info <- init_sensors_color_icons(sensors_info)          # Initialize colors and icons of sensors
  sensors_info <- set_sensors_url_img(sensors_df = sensors_info)  # Set sensors url and image
  
  #' Reference sensor point
  ref_point_coords <- data.frame(latitude = sensors_info$lat[which(sensors_info$system == "mast")[1]], 
                                 longitude = sensors_info$lon[which(sensors_info$system == "mast")[1]])
  
  #' Set coordinates relative to mast
  sensors_info$longitude[which(sensors_info$system == "mast")] = 0
  sensors_info$longitude[which(sensors_info$system != "mast")] = sensors_info$longitude[which(sensors_info$system != "mast")] - ref_point_coords$longitude
  sensors_info$lon <- sensors_info$longitude
  sensors_info$latitude[which(sensors_info$system == "mast")] = 0
  sensors_info$latitude[which(sensors_info$system != "mast")] = sensors_info$latitude[which(sensors_info$system != "mast")] - ref_point_coords$latitude
  sensors_info$lat <- sensors_info$latitude
  
  sensors_info <- st_as_sf(x = sensors_info, 
                           coords = c("longitude", "latitude"), 
                           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", 
                           na.fail = FALSE)
  sensors_info <- sensors_info[!sf::st_is_empty(sensors_info$geometry), ] %>%   # Remove rows with empty geometry
    st_set_geometry(NULL)  # Remove geometry
  sensors_info <- st_as_sf(x = sensors_info, 
                           coords = c("longitude", "latitude"), 
                           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", 
                           na.fail = FALSE)
  
  #' Wind turbines location
  wind_turbines_info <- get_wind_turbine_relative_coords(ref_point_coords)
  wind_turbines_info$color_icon <- 'wind_turbine_pin'
  #' 
  #' Sensor map marker click event
  observeEvent(eventExpr = input$map_marker_click, handlerExpr = {
    p <- input$map_marker_click
    print(p)
  })
  #' 
  #' Map rendering on PIBE project presentation tab
  map_sensors_tab <- init_map_of_sensors(sensors = sensors_info, wind_turbines = wind_turbines_info) %>% 
    add_sensors_to_map(sensors = sensors_info) %>% 
    add_wind_turbines_to_map(wind_turbines = wind_turbines_info) %>% 
    add_scale_bar_to_map() %>% 
    add_grid_to_map() %>% 
    add_north_arrow_to_map() %>% 
    addLayersControl(overlayGroups = c("Sensors", "Wind turbines", "Grid"), 
                     position = "bottomleft", 
                     options = layersControlOptions(collapsed = FALSE))
  output$map_tab_sensors <- renderLeaflet({map_sensors_tab})
  #' 
  #' Map rendering on data download tab
  reactive_map <- reactive({
    list(input$selected_sensors, input$acoustic_sensors, input$meteo_sensors, input$show_grid)
  })
  #' 
  observeEvent(eventExpr = reactive_map(), handlerExpr = {
    if(isTRUE(input$selected_sensors)){
      reactive_sensors <- set_sensors_color_icon(sensors_df = sensors_info, 
                                                 selected_sensors = c(input$acoustic_sensors, input$meteo_sensors))
    } else {
      reactive_sensors <- init_sensors_color_icons(sensors_info)
    }
    leafletProxy(mapId = "map_tab_visu", data = sensors_info) %>% 
      clearMarkers() %>%   ## clear previous markers
      add_sensors_to_map(sensors = reactive_sensors) %>% 
      add_wind_turbines_to_map(wind_turbines = wind_turbines_info) %>% 
      add_scale_bar_to_map() %>% 
      add_north_arrow_to_map()
    if(isTRUE(input$show_grid)){
      leafletProxy(mapId = "map_tab_visu", data = reactive_sensors) %>% 
        add_grid_to_map()
    } else {
      leafletProxy(mapId = "map_tab_visu", data = reactive_sensors) %>% 
        clearGroup("Grid") %>% 
        add_grid_to_map_without_labels()
    }
    leafletProxy(mapId = "map_tab_visu", data = reactive_sensors) %>% 
      addLayersControl(overlayGroups = c("Sensors", "Wind turbines", "Grid"), 
                       position = "bottomleft", 
                       options = layersControlOptions(collapsed = FALSE))
  })
  #' 
  #' Reset map view (centre of dataframe)
  observeEvent(eventExpr = input$reset_view_map_tab_visu, handlerExpr = {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Sight reset')
    geodf <- rbind(sensors_info[which(sensors_info$lon != 0. & sensors_info$lat != 0.), c("lon", "lat", "geometry")], 
                   wind_turbines_info[, c("lon", "lat", "geometry")])
    centre_point <- get_geodf_centre_point(geodf = geodf)
    leafletProxy(mapId = "map_tab_visu", data = sensors_info) %>% 
      setView(lng = centre_point$X, lat = centre_point$Y, zoom = 14)
  })
  #' 
  #' Table of sensors
  sensors_tab <- sensors_info %>% 
    select(-c(lat, lon, url, img, color_selected, color_icon)) %>% 
    st_drop_geometry()  # Remove geometry column
  output$table_sensors_tab_map <- renderDataTable(sensors_tab, 
                                                  options = list(paging = TRUE, pageLength =  10)) # rownames = FALSE
  
  #' 
  #' DATA DOWNLOAD
  #' 
  #' Update the values of the dates and times input
  observe({updateDateInput(session, "start_date", value = input$start_date_download)})
  observe({updateTimeInput(session, "start_time", value = strptime(input$start_time_download, "%H:%M"))})
  observe({updateDateInput(session, "end_date", value = input$end_date_download)})
  observe({updateTimeInput(session, "end_time", value = strptime(input$end_time_download, "%H:%M"))})
  
  #' Acoustic variables
  observeEvent(eventExpr = input$acoustic_sensors, handlerExpr = {
    if (is.null(input$acoustic_sensors)) {
      disable('acoustic_indicators')
      removePopover(session, id = "acoustic_indicators")
      addPopover(session, id = "acoustic_indicators",
                 title = "Acoustic indicators",
                 content = "Select at least one acoustic sensor from a1, a2, a3, a4 and a5 to activate the drop-down list.",
                 placement = "top", trigger = "hover",
                 options = list(container = "body"))
      disable('acoustic_frequencies')
      removePopover(session, id = "acoustic_frequencies")
      addPopover(session, id = "acoustic_frequencies",
                 title = "Frequencies",
                 content = "Select at least one acoustic sensor from a1, a2, a3, a4 and a5 to activate the drop-down list.",
                 placement = "top", trigger = "hover",
                 options = list(container = "body"))
    } else {
      enable('acoustic_indicators')
      enable('acoustic_frequencies')
    }
  }, ignoreNULL = FALSE)
  
  #' Meteo variables
  meteoVariables <- reactive(c(input$sound_level_meter_variables, input$lidar_variables, input$sonics_variables))
  
  observeEvent(eventExpr = meteoVariables(), handlerExpr = {
    updated_meteo_variables <- list()
    # Combine the selected variables from all three inputs
    for (meteo_sensor in input$meteo_sensors) {
      if (str_starts(string = meteo_sensor, pattern = "a")){
        updated_meteo_variables[[meteo_sensor]] <- input$sound_level_meter_variables  # paste(meteo_sensor, input$sound_level_meter_variables, sep = ".", collapse = " ")
      } else if (str_starts(string = meteo_sensor, pattern = "lidar")){
        updated_meteo_variables[[meteo_sensor]] <- input$lidar_variables
      } else if (str_starts(string = meteo_sensor, pattern = "s")){
        updated_meteo_variables[[meteo_sensor]] <- input$sonics_variables
      }
    }
    # Combine names and values
    combined_list <- mapply(function(name, values) paste(name, values, sep = "."), 
                                                   names(updated_meteo_variables), 
                                                   updated_meteo_variables, 
                                                   SIMPLIFY = FALSE)
    # Flatten the list
    meteo_vars <- as.character(unlist(combined_list))
    # Update the input$meteo_variables
    shinyjs::runjs(sprintf("Shiny.setInputValue('meteo_variables', %s);", toJSON(meteo_vars)))
  })
  
  observeEvent(eventExpr = input$meteo_sensors, handlerExpr = {
    if (is.null(input$meteo_sensors) | 
        length(get_sensors_names_starting_with(sensors = input$meteo_sensors, names_starting_with = "a")) == 0) {
      disable('sound_level_meter_variables')
    } else if (length(get_sensors_names_starting_with(sensors = input$meteo_sensors, names_starting_with = "a")) > 0) {
      enable('sound_level_meter_variables')
      removePopover(session, id = "sound_level_meter_variables")
      addPopover(session, id = "sound_level_meter_variables",
                 title = HTML("Meteo variable(s) near sound level meters"),
                 content = "Select one or multiple variables (Maj+click). Press ctrl+click to deselect a variable",
                 placement = "bottom", trigger = "hover",
                 options = list(container = "body"))
    }
    if (is.null(input$meteo_sensors) | 
        length(get_sensors_names_starting_with(sensors = input$meteo_sensors, names_starting_with = "lidar")) == 0) {
      disable('lidar_variables')
    } else if (length(get_sensors_names_starting_with(sensors = input$meteo_sensors, names_starting_with = "lidar")) > 0) {
      enable('lidar_variables')
      removePopover(session, id = "lidar_variables")
      addPopover(session, id = "lidar_variables",
                 title = HTML("Meteo variable(s) from lidar"),
                 content = "Select one or multiple variables (Maj+click). Press ctrl+click to deselect a variable",
                 placement = "bottom", trigger = "hover",
                 options = list(container = "body"))
      
    }
    if (is.null(input$meteo_sensors) | 
        length(get_sensors_names_starting_with(sensors = input$meteo_sensors, names_starting_with = "s")) == 0) {
      disable('sonics_variables')
    } else if (length(get_sensors_names_starting_with(sensors = input$meteo_sensors, names_starting_with = "s")) > 0) {
      enable('sonics_variables')
      removePopover(session, id = "sonics_variables")
      addPopover(session, id = "sonics_variables",
                 title = HTML("Meteo variable(s) from sonic anemometers"),
                 content = "Select one or multiple variables (Maj+click). Press ctrl+click to deselect a variable",
                 placement = "bottom", trigger = "hover",
                 options = list(container = "body"))
    }
  }, 
  ignoreNULL = TRUE)
  
  # Download data button
  selectedSensors <- reactive(c(input$acoustic_sensors, input$meteo_sensors))
  
  observeEvent(eventExpr = selectedSensors(), handlerExpr = {
    if(is.null(selectedSensors())){
      disabled(uiOutput('downloadDataButton'))
      output$download_data_button_style <- renderUI({tags$head(tags$style(".download_data_button{background-color: #c7c7c7;}"))})
      output$download_bsTooltip <- renderUI({bsTooltip(id = "downloadDataButton",
                                                       title = "Select at least one acoustic or meteorological sensor",
                                                       placement = "bottom", trigger = "hover",
                                                       options = list(container = "body"))})
    } else {
      enable('downloadDataButton')
      output$download_data_button_style <- renderUI({tags$head(tags$style(".download_data_button{background-color: #27ae60;}"))})
      output$download_bsTooltip <- renderUI({bsTooltip(id = "downloadDataButton", 
                                                       title = "Download selected data", 
                                                       placement = "bottom", trigger = "hover", 
                                                       options = list(container = "body"))})
    }
  }, ignoreNULL = FALSE)
  
  acouscticsSensorSelections <- reactive(list(sensors = input$acoustic_sensors, 
                                              indicators = input$acoustic_indicators, 
                                              frequencies = input$acoustic_frequencies))
  meteoSensorsSelections <- reactive(list(
    sonos = list(sensors = input$meteo_sensors[str_starts(string = input$meteo_sensors, pattern = "a")],
                 variables =input$sound_level_meter_variables), 
    lidar = list(sensors = input$meteo_sensors[str_starts(string = input$meteo_sensors, pattern = "lidar")],
                 variables =input$lidar_variables), 
    sonics = list(sensors = input$meteo_sensors[str_starts(string = input$meteo_sensors, pattern = "s")],
                  variables =input$sonics_variables)))
  
  completeSensorSelection <- reactive(list(acoustic = acouscticsSensorSelections(), 
                                           meteo = meteoSensorsSelections()))
  
  observeEvent(eventExpr = completeSensorSelection(), handlerExpr = {
    is.not.null <- function(x) !is.null(x)
    selected_sensors <- completeSensorSelection()
    for (scientific_field in names(selected_sensors)) {
      if(str_equal(scientific_field, "acoustic")){
        selected_sensor_names <- selected_sensors[[scientific_field]]$sensors
        selected_sensor_variables <- str_remove(string = names(selected_sensors[[scientific_field]]), 
                                                pattern = "sensors")[2:length(names(selected_sensors[[scientific_field]]))]
        # Acoustic sensor(s) selected
        sensors_selected <- (!is.null(selected_sensor_names))
        # Acoustic indicator(s) and frequency(ies) well selected
        variables_selected <- (all(sapply(selected_sensors[[scientific_field]][selected_sensor_variables], is.not.null)))
        # variables_selected <- (all(!is.null(selected_sensors[[scientific_field]][selected_sensor_variables])))
        # No acoustic indicator(s) and frequency(ies) selected
        variables_not_selected <- (all(is.null(selected_sensors[[scientific_field]][selected_sensor_variables])))
        # Acoustic indicator(s) or frequency(ies) selected
        variables_even_incomplete <- (any(!is.null(selected_sensors[[scientific_field]][selected_sensor_variables])))
        # Acoustic sensor(s), indicator(s) and frequency(ies) well selected
        sensors_and_variables_well_selected <- (isTRUE(sensors_selected) & isTRUE(variables_selected))
        # No acoustic sensor nor indicator and frequency selected
        no_sensors_nor_variables_selected <- (isFALSE(sensors_selected) & isTRUE(variables_not_selected))
        # Acoustic sensor(s) selected but no indicator and/or frequency selected
        sensors_but_no_variables_selected <- (isTRUE(sensors_selected) & isTRUE(variables_even_incomplete))
        # No acoustic sensor selected where as indicator and/or frequency are
        no_sensors_whereas_variables_selected <- (isFALSE(sensors_selected) & isTRUE(variables_even_incomplete))
        if(isTRUE(sensors_and_variables_well_selected) | isTRUE(no_sensors_nor_variables_selected)){
          valid_input_test <- TRUE
        } else if(isTRUE(sensors_but_no_variables_selected)){
          valid_input_test <- FALSE
          bs_title <- paste0("Select at least one indicator and one frequency for sensor(s) ", paste(selected_sensor_names, collapse = '","'))
          break
        } else if(isTRUE(no_sensors_whereas_variables_selected)){
            valid_input_test <- FALSE
            bs_title <- paste0("Select at least one acoustic sensor")
            break
        }
      } else if(str_equal(scientific_field, "meteo")){
        for (meteo_sensor_name in names(selected_sensors[[scientific_field]])){
          selected_sensor_names <- selected_sensors[[scientific_field]][[meteo_sensor_name]]$sensors
          selected_sensor_variables <- str_remove(string = names(selected_sensors[[scientific_field]][[meteo_sensor_name]]), 
                                                  pattern = "sensors")[2:length(names(selected_sensors[[scientific_field]][[meteo_sensor_name]]))]
          # Meteo sensor(s) selected
          sensors_selected <- (!is_null(selected_sensor_names) & (!identical(selected_sensor_names, character(0))))
          # Meteo variables selected
          variables_selected <- (!is.null(selected_sensors[[scientific_field]][[meteo_sensor_name]][[selected_sensor_variables]]))
            
          # Meteo sensor(s), indicator(s) and frequency(ies) well selected
          sensors_and_variables_well_selected <- (isTRUE(sensors_selected) & isTRUE(variables_selected))
          # No meteo sensor nor variables selected
          no_sensors_nor_variables_selected <- (isFALSE(sensors_selected) & isFALSE(variables_selected))
          # Meteo sensor selected but no variables selected
          sensors_but_no_variables_selected <- (isTRUE(sensors_selected) & isFALSE(variables_selected))
          # No sensor selected but variables selected
          no_sensors_nor_variables_selected <- (isFALSE(sensors_selected) & isTRUE(variables_selected)) 
            
          if(isTRUE(sensors_and_variables_well_selected) | isTRUE(no_sensors_nor_variables_selected)){
            valid_input_test <- TRUE
          } else if(isTRUE(sensors_but_no_variables_selected)){
            valid_input_test <- FALSE
            bs_title <- paste0("Select at least one meteo variable for sensor(s) ", paste(selected_sensor_names, collapse = '","'))
            break
          } else if(isTRUE(no_sensors_nor_variables_selected)){
            valid_input_test <- FALSE
            bs_title <- paste0("Select at least one meteo sensor for variable(s) ", paste(selected_sensor_variables, collapse = '","'))
            break
          }
        }
      }
    }
    if(isTRUE(valid_input_test)){
      enable('downloadDataButton')
      output$download_data_button_style <- renderUI({tags$head(tags$style(".download_data_button{background-color: #27ae60;}"))})
      output$download_bsTooltip <- renderUI({bsTooltip(id = "downloadDataButton", 
                                                       title = "Download selected data", 
                                                       placement = "bottom", trigger = "hover", 
                                                       options = list(container = "body"))})
    } else {
      disabled(uiOutput('downloadDataButton'))
      output$download_data_button_style <- renderUI({tags$head(tags$style(".download_data_button{background-color: #c7c7c7;}"))})
      output$download_bsTooltip <- renderUI({bsTooltip(id = "downloadDataButton",
                                                       title = bs_title,
                                                       placement = "bottom", trigger = "hover",
                                                       options = list(container = "body"))})
    }
  }, 
  ignoreNULL = TRUE)
  
  output$downloadDataButton <- downloadHandler(
    filename = function() {
      filename <- paste0("data-", Sys.Date(), ".csv")
      assign("download_filename", filename, envir = .GlobalEnv)  # Store the filename in the global environment
      filename
    },
    content = function(file) {
      showModal(modalDialog(
        title = "Downloading data",
        footer = tagList(
          actionButton("cancelDownloadData", "Cancel")
        ),
        div(
          id = "progress-container",
          style = "width: 80%; margin: 0 auto;",
          tags$div(
            id = "progress-bar",
            style = "width: 0%; height: 20px; background-color: #4CAF50;",
            ""
          ),
          tags$div(
            id = "progress-text",
            style = "margin-top: 10px; text-align: center;",
            "Downloading data..."
          )
        )
      ))
      
      # Enable shinyjs to disable the cancel button during loading
      shinyjs::enable("cancelDownloadData")
      
      start_time_download <- strftime(as.character(input$start_time_download), format = "%H:%M")
      end_time_download <- strftime(as.character(input$end_time_download), format = "%H:%M")
      
      downloading_time <- system.time(invisible(0))
      
      # on.exit({
      #   shinyjs::disable("cancelDownloadData")
      # })
      # code_txt <- downloading_page(input)
      # html_fp = paste(dirname(getwd()), "www", "html", "download.html", sep = .Platform$file.sep)
      # write.table(code_txt, file=html_fp, quote = FALSE, col.names = FALSE, row.names = FALSE)
      # showModal(modalDialog(
      #   includeHTML(html_fp),
      #   footer = tagList(actionButton(inputId = 'cancelDownload', label = 'Cancel'), modalButton("Ok")),  # modalButton("Cancel"),
      #   size = "m",
      #   easyClose = TRUE))
      # on.exit(removeModal())
      start_time_download = strftime(as.character(input$start_time_download), format = "%H:%M")
      end_time_download = strftime(as.character(input$end_time_download), format = "%H:%M")
      downloading_time <- system.time(invisible(0))
      time_result <- system.time({
        download <- get_data(conn = elastic_conn,
                             start_date = input$start_date_download, start_time = start_time_download,
                             end_date = input$end_date_download, end_time = end_time_download,
                             time_sampling = input$time_sampling, scientific_domain = input$scientific_domain,
                             acoustic_sensors = input$acoustic_sensors, acoustic_indicators = input$acoustic_indicators, acoustic_frequencies = input$acoustic_frequencies,
                             meteo_sensors = input$meteo_sensors, meteo_variables = input$meteo_variables)
      })
      #'
      # Use JavaScript to update progress
      session$sendCustomMessage(type = "downloadProgress", message = list(progress = 1))
      shinyjs::disable("cancelDownloadData")
      removeModal()
      
      #' Calculating download time
      cat("\n \t -> data downloaded in", format_elapsed_time(time_result["elapsed"]), "\n")
      print("Writing data")
      #'
      #' Full path of output file
      data_filepath <- file.path(file.path(dirname(getwd()), "www", "data"), download_filename)
      write.csv(download, file = data_filepath, row.names = FALSE, na="")
      print("\t -< data written")
    }
  )
  # Observer for canceling the download
  observeEvent(input$cancelDownloadData, handlerExpr = {
    shinyjs::disable("cancelDownloadData")
    removeModal()
    # You may want to add code here to cancel the download operation if applicable
  })
  
  #' 
  #' Acoustic data plot
  # observe({
  #   cat("\ninput$file value:\n\n")
  #   print(input$file)
  # })
  
  mydata <- reactiveValues(
    filename = NULL,
    filepath = NULL,
    raw_data = NULL,
    plot = NULL,
    time_slider.min = NULL,
    time_slider.max = NULL,
    downloading_time = NULL
  )
  
  get_latest_file <- function(path, ext = "csv") {
    files <- list.files(path = path, pattern = paste0("*.", ext), full.names = TRUE)
    latest_file <- files[which.max(fs::file_info(files)$birth_time)]
    return(latest_file)
  }
  
  latest_data_file_path <- reactiveVal(get_latest_file(paste(dirname(getwd()), "www", "data", sep = .Platform$file.sep)))
  
  shinyFileChoose(input, "fileSelection", roots = c(wd = getwd()), filetypes = c("", ".csv"))
  
  observeEvent(eventExpr = input$sidebarid, handlerExpr = {
    # Check if a file is loaded when no reactive dataframe is defined
    if (input$sidebarid == "visualization" & is.null(mydata$raw_data)) {
      showModal(modalDialog(
        title = "Data Alert",
        paste0("You don't have any data. Do you want to load the most recent file (", basename(latest_data_file_path()), ") or choose another file?"),
        footer = tagList(
          actionButton("loadRecentFile", "Load Most Recent File"),
          actionButton("chooseFile", "Choose File")
        )
      ))
    }
  })
  
  observeEvent(eventExpr = input$loadRecentFile, handlerExpr = {
    # Implement the logic to load the most recent file here
    showModal(modalDialog(
      title = "Loading...",
      paste0("Loading the most recent file ", basename(latest_data_file_path()), "..."),
      footer = NULL
    ))
    current_time <- Sys.time()
    mydata$filename <- basename(latest_data_file_path())
    mydata$filepath <- latest_data_file_path()
    mydata$raw_data <- read_csv(mydata$filepath, col_names = TRUE)
    elapsed_time <- Sys.time() - current_time
    mydata$downloading_time <- as.double(elapsed_time)
    mydata$plot <- format_data_for_app(mydata$raw_data)
    mydata$time_slider.min <- mydata$raw_data$timestamp[1]
    mydata$time_slider.max <- mydata$raw_data$timestamp[nrow(mydata$raw_data)]
    output$result <- renderText("File loaded!")
    shinyjs::disable("loadDataButton")  # Disabling the button to prevent multiple clicks
    removeModal()
  })
  
  observeEvent(eventExpr = input$chooseFile, handlerExpr = {
    # Implement the logic to choose another file here
    showModal(modalDialog(
      title = "Choose File",
      fileInput("fileSelectionModal", "Choose a *.CSV file"),
      footer = tagList(
        actionButton("loadSelectedFile", "Load Selected File"),
        actionButton("cancelChooseFile", "Cancel")
      )
    ))
  })
  
  observeEvent(eventExpr = input$loadSelectedFile, handlerExpr = {
    # React to changes in the selected file
    req(input$fileSelectionModal)
    showModal(modalDialog(
      title = "Loading...",
      paste0("Loading the selected file ", input$fileSelectionModal$name, "..."),
      footer = NULL
    ))
    current_time <- Sys.time()
    mydata$filename <- basename(input$fileSelectionModal$datapath)
    mydata$filepath <- input$fileSelectionModal$datapath
    mydata$raw_data <- read_csv(mydata$filepath, col_names = TRUE)
    elapsed_time <- Sys.time() - current_time
    mydata$downloading_time <- as.double(elapsed_time)
    mydata$plot <- format_data_for_app(mydata$raw_data)
    mydata$time_slider.min <- mydata$raw_data$timestamp[1]
    mydata$time_slider.max <- mydata$raw_data$timestamp[nrow(mydata$raw_data)]
    output$result <- renderText("File loaded!")
    removeModal()
  })
  
  
  observeEvent(eventExpr = input$cancelChooseFile, handlerExpr = {
    showModal(modalDialog(
      title = "Data Alert",
      paste0("You don't have any data. Do you want to load the most recent file (", basename(latest_data_file_path()), ") or choose another file?"),
      footer = tagList(
        actionButton("loadRecentFile", "Load Most Recent File"),
        actionButton("chooseFile", "Choose File")
      )
    ))
  })
  
  observeEvent(eventExpr = input$loadDataButton, handlerExpr = {
    # This event is triggered when the user clicks "Load Data"
    # Implement the logic to load the data here
    showModal(modalDialog(
      title = "Loading...",
      paste0("Loading the most recent file ", basename(latest_data_file_path()), "..."),
      footer = NULL
    ))
    shinyjs::disable("loadDataButton")  # Disabling the button to prevent multiple clicks
    removeModal()
  })
  
  unique_triplets <- reactive({
    return(unique_field_sensor_variable_triplets(df = mydata$plot))
  })
  
  #' 
  observeEvent(eventExpr = input$data_file, handlerExpr = {
    infile <- input$data_file
    print('INFILE')
    print(infile)
    if (identical(infile$name, "sample.csv")) {    # User has not uploaded a file yet
      return(list(filename = infile$name, 
                  filepath = init$selection$csv_in_filepath, 
                  raw_data <- init$selection$csv_in_file_data, 
                  plot <- init$selection$csv_in_plot, 
                  time_slider.min = init$selection$start_date_time,
                  time_slider.max = init$selection$end_date_time,
                  downloading_time <- datasample.init$downloading_time))
    }
    the_data_path <- infile$datapath
    current_time <- Sys.time()
    the_csv_data <- read_csv(file = the_data_path, col_names = TRUE)
    elapsed_time <- Sys.time() - current_time
    timeslider_min <- the_csv_data$start_time[1]
    timeslider_max <- the_csv_data$start_time[length(the_csv_data$start_time)]
    mydata$filename <- infile$name
    mydata$filepath <- the_data_path
    mydata$raw_data <- the_csv_data
    mydata$plot <- format_data_for_app(the_csv_data)
    mydata$time_slider.min <- timeslider_min
    mydata$time_slider.max <- timeslider_max
    mydata$downloading_time <- as.double(elapsed_time)
  })
  #' 
  downloadingTime <- reactive({
    dw_time <- round(mydata$downloading_time, digits = 2)
    return(dw_time)
  })
  output$downloading_time <- renderText(paste0("Data loaded in ", downloadingTime(),  " s"))
  #' 
  observeEvent(eventExpr = input$data_file, handlerExpr = {
    updateSliderInput(session, "time_slider",
                    min = as.POSIXct(x = mydata$time_slider.min, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC"),
                    max = as.POSIXct(x = mydata$time_slider.max, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC"),
                    value =  c(as.POSIXct(x = mydata$time_slider.min, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC"),
                               as.POSIXct(x = mydata$time_slider.max, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC")), 
                    timeFormat = "%d/%m %H:%M")
  })
  #' 
  scatter_plot_axis_vars <- reactive({
    if(is.null(mydata$plot)){
      x_var <- ""
      y_var <- ""
    } else if(!is.null(mydata$plot)){
      if(input$scatter_plot_x_axis == ""){
        x_var <- unique_triplets()$field.sensor.var[1]
      } else {
        x_var <- input$scatter_plot_x_axis
      }
      if(input$scatter_plot_y_axis == ""){
        y_var <- unique_triplets()$field.sensor.var[2]
      } else {
        y_var <- input$scatter_plot_y_axis
      }
    }
    return(list(x = x_var, y = y_var))
  })
  
  observeEvent(eventExpr = mydata$plot, handlerExpr = {
    updateSelectInput(session, inputId = "scatter_plot_x_axis", label = "x-axis variable", 
                      selected = scatter_plot_axis_vars()$x, choices = unique_triplets()$field.sensor.var)
    updateSelectInput(session, inputId = "scatter_plot_y_axis", label = "y-axis variable", 
                      selected = scatter_plot_axis_vars()$y, choices = unique_triplets()$field.sensor.var)
  })
  
  split_field_sensor_var_name <- function(field_sensor_var_name){
    c(field, sensor, var)  %<-% str_split_fixed(string = field_sensor_var_name, pattern = "[.]", n=3)
    return(list(field = field, 
                sensor = sensor, 
                var = var))
  }
  
  
  plotdata <- reactive({
    c(x_field, x_sensor, x_var)  %<-% split_field_sensor_var_name(scatter_plot_axis_vars()$x)
    c(y_field, y_sensor, y_var)  %<-% split_field_sensor_var_name(scatter_plot_axis_vars()$y)
    df <- as.data.frame(mydata$plot[c(which(mydata$plot$field == x_field & mydata$plot$sensor == x_sensor & mydata$plot$variable == x_var), 
                                      which(mydata$plot$field == y_field & mydata$plot$sensor == y_sensor & mydata$plot$variable == y_var)), ])
    return(df)
  })
  #' 
  output$scatter_plot <- renderPlotly({
    if (is.null(mydata$plot)){
      return(NULL)
    }
    df <- plotdata()
    c(x_field, x_sensor, x_var)  %<-% split_field_sensor_var_name(scatter_plot_axis_vars()$x)
    c(y_field, y_sensor, y_var)  %<-% split_field_sensor_var_name(scatter_plot_axis_vars()$y)
    df$xx <- df[which(df$field == x_field & df$sensor == x_sensor & df$variable == x_var), "value"]
    df$yy <- df[which(df$field == y_field & df$sensor == y_sensor & df$variable == y_var), "value"]
    df$timeseries <- as.POSIXct(x = df[which(df$field == x_field & df$sensor == x_sensor & df$variable == x_var), "timestamp"], 
                                format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC")
    tile_font <- list(family = "Courier New", size = 14, color = "black")
    plot <- plot_ly(data = df, type = 'scatter', mode = 'markers') %>% 
      add_trace(x = ~xx, 
                y = ~yy, 
                hovertext = ~paste0("<B>", timeseries, "</B><br>", 
                                    str_to_title(x_field), " sensor ", x_sensor, ": ", x_var, "=", xx, "<br>", 
                                    str_to_title(y_field), " sensor ", y_sensor, ": ", y_var, "=", yy),
                hoverinfo = "text",
                marker = list(color = input$scatter_plot_color), 
        showlegend = FALSE) %>% 
      layout(xaxis = list(title=paste0("<b>", x_var, "</b>"), font = tile_font), 
             yaxis = list(title=paste0("<b>", y_var, "</b>"), font = tile_font))
    plot
  })
  #' 
  output$plot_data <- renderPlotly({
    if (is.null(mydata$plot)) {
      return(NULL)
    }
    mydata$plot$timestamp <- as.POSIXct(mydata$plot$timestamp, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC")
    # nb_days <- as.double(difftime(mydata$plot$timestamp[length(mydata$plot$timestamp)], mydata$plot$timestamp[1]))
    # if (nb_days <= 5) {
    #   date_breaker <- "1 day"
    # } else {
    #   date_breaker <- "2 day"
    # }
    # subplot_list <- list()
    # for (sensor_val in unique(mydata$plot$sensor)) {
    #   for (field_val in unique(mydata$plot$field)) {
    #     subset_data <- mydata$plot[mydata$plot$sensor == sensor_val & mydata$plot$field == field_val, ]
    #     subplot <- plot_ly(subset_data, x = ~timestamp, y = ~value, color = ~variable, type = 'scatter', mode = 'lines') %>%
    #       layout(
    #         xaxis = list(
    #           title = "Time",
    #           tickformat = "%Y-%m-%d_%H:%M:%OS UTC", #"%y/%m/%d\n%H:%M",
    #           color = ~variable, colors = "Set2", 
    #           tickmode = 'linear',
    #           tick0 = input$time_slider[[1]],  #'2000-01-01',
    #           dtick = 'M1',
    #           range = as.POSIXct(c(input$time_slider[[1]], input$time_slider[[2]]), format = "%Y-%m-%d_%H:%M:%OS"),
    #           showline = TRUE,
    #           showgrid = FALSE,
    #           showticklabels = TRUE,
    #           zeroline = FALSE
    #         ),
    #         yaxis = list(title = "Sound level [dB(A)]"),
    #         hovermode = 'x',
    #         showlegend = TRUE
    #       )
    #     # Add field and sensor annotations
    #     subplot <- subplot %>%
    #       layout(annotations = list(
    #         list(x = 0.5, y = 1.1, text = field_val, showarrow = FALSE, xref='paper', yref='paper', xanchor='center', yanchor='bottom'),
    #         list(x = -0.1, y = 0.5, text = sensor_val, showarrow = FALSE, xref='paper', yref='paper', xanchor='right', yanchor='middle')
    #       ))
    #     subplot_list[paste(sensor_val, field_val, sep = "_")] <- subplot
    #   }
    # }
    # subplot(subplot_list, nrows = length(unique(mydata$plot$sensor)), margin = 0.05)
    
    
    
    # ax <- list(title = "Time", tickformat = "%Y-%m-%d_%H:%M:%OS UTC", 
    #            range = as.POSIXct(c(input$time_slider[[1]], input$time_slider[[2]]), format = "%Y-%m-%d_%H:%M:%OS"), 
    #            zeroline = FALSE, showline = FALSE, showticklabels = FALSE)
    
    # Acoustic and of meteo sensors dataframes
    acoustic_sensors_df <- data.frame(acoustic = rep(1, length(unique(mydata$plot$sensor[mydata$plot$field == 'acoustic']))), 
                                      row.names = unique(mydata$plot$sensor[mydata$plot$field == 'acoustic']))
    meteo_sensors_df <- count_common_prefix_elements(meteo_sensors, unique(mydata$plot$sensor[mydata$plot$field == 'meteo']))
    
    # Ajuster la structure de acoustic_sensors_df avec les noms de colonnes de meteo_sensors_df
    merge_sensors_dataframes <- function(df1, df2) {
      df <- rbind.fill(df1, df2)
      df[is.na(df)] <- 0
      row.names(df) <- c(rownames(df1), rownames(df2))
      return(df)
    }
    
    # Merge dataframes
    sensors_df <- merge_sensors_dataframes(acoustic_sensors_df, meteo_sensors_df)
    # Maximum number of non zeroes among all columns
    max_non_nulls_cells_by_columns <- max(apply(sensors_df, 2, function(col) sum(col != 0)))
    # subplot_list <- list()
    subplot_list <- vector(mode = "list", length = max_non_nulls_cells_by_columns*ncol(sensors_df))
    # subplot_list <- as.data.frame(matrix(NA, nrow = max_non_nulls_cells_by_columns, ncol = ncol(sensors_df)))
    # names(subplot_list) <- colnames(sensors_df)
    for(idcol in seq(1, ncol(sensors_df))){
      if(names(sensors_df)[idcol] %in% mydata$plot$field){  # Acoustic data
        subset_data <- mydata$plot[mydata$plot$field == names(sensors_df)[[idcol]],]
      } else {  # Meteo data
        subset_data <- mydata$plot[mydata$plot$sensor == rownames(sensors_df)[sensors_df[, idcol] != 0],]
      }
      if(length(subset_data) > 0){
        the_sensors <- unique(subset_data$sensor)
        nb_sensors <- length(the_sensors)
        id_cols <- seq(idcol, nb_sensors*ncol(sensors_df), ncol(sensors_df))
        for(id_sensor in seq(1, nb_sensors)){
          subsubset_data <- subset_data[subset_data$sensor == the_sensors[id_sensor], ]
          subfig <- plot_ly(subsubset_data, type = 'scatter', mode = 'lines', fill = 'tonexty') %>%
            add_trace(x = ~timestamp, y = ~value, name = ~variable, color = ~variable, 
                      text = paste0(subsubset_data$variable), 
                      # text = paste0("Sensor ", subsubset_data$sensor, ": ", subsubset_data$variable), 
                      hovertemplate = paste('<b>%{text}:</b> %{y:.2f}<br>',  #'<b>%{xaxis.title.text}:</b> %{x}<br>'
                                            '<extra></extra>')) %>%
            layout(title =str_to_upper(the_sensors[id_sensor]),
                   legend=list(title=list(text=~variable)), 
                   xaxis = list(title = "Time", tickformat = "%d/%m/%Y\n%H:%M", 
                                range = as.POSIXct(c(input$time_slider[[1]], input$time_slider[[2]]), format = "%Y-%m-%d_%H:%M:%OS"), 
                                zeroline = FALSE, showline = FALSE), 
                   yaxis = list(title = paste0(unique(subsubset_data$variable), collapse = ", ")), 
                   hovermode = 'x unified', 
                   legend_traceorder="normal")
          subplot_list[[id_cols[id_sensor]]] <- subfig
        }
        # Add empty plot if no more sensor(s)
        nb_rows_with_data <- sum(sensors_df[, idcol] != 0, na.rm = TRUE)
        if(nb_rows_with_data != max_non_nulls_cells_by_columns){
          nb_empty_rows = max_non_nulls_cells_by_columns - nb_rows_with_data
          for(id_empty in seq(idcol+(max_non_nulls_cells_by_columns-nb_empty_rows)*ncol(sensors_df), 
                              idcol+nb_empty_rows*ncol(sensors_df), ncol(sensors_df))){
            subfig <- plotly_empty(type = 'scatter', mode = 'lines')
            subplot_list[[id_empty]] <- subfig
          }
        }
      }
    }
    fig <- subplot(subplot_list, nrows = max_non_nulls_cells_by_columns, titleY = TRUE, titleX = TRUE) %>%
      layout(xaxis = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'),
             yaxis = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'),
             plot_bgcolor='#e5ecf6')
    if(length(unique(mydata$plot$field)) == 2){
      annotations = list(list(x = 0.225, y = 1.0, font = list(size = 10), text = str_to_title(unique(mydata$plot$field)[1]),
                              xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE),
                         list(x = 0.775, y = 1, font = list(size = 10), text = str_to_title(unique(mydata$plot$field)[2]),
                              xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE))
    } else {
      annotations = list(list(x = 0.5, y = 1.0, font = list(size = 10), text = str_to_title(unique(mydata$plot$field)),
                              xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE))
    }
    fig <- fig %>% layout(annotations = annotations) %>%
      layout(hovermode = "x unified")
    options(warn = -1)
    fig
    
    # VALID SUBPLOT TEST
    # df2 <- as.data.frame(matrix(round(runif(n=8*100, min=1, max=20), 0), nrow=100))
    # df2$x <- seq(1, 100)
    # subplot_list2 <- vector(mode = "list", length = 8)
    # for(idd in seq(1, length(names(df2))-1)){
    #   print(idd)
    #   if(idd != 7){
    #     subfig2 <- plot_ly(df2, type = 'scatter', mode = 'lines', fill = 'tonexty') %>%
    #       add_trace(x = ~x, y = df2[, paste0("V", idd)], name = paste0("V", idd))
    #   } else {
    #     print('ok')
    #     subfig2 <- plotly_empty(type = 'scatter', mode = 'lines')
    #   }
    #   subplot_list2[[idd]] <- subfig2
    # }
    # fig <- subplot(subplot_list2, nrows = 2, titleY = TRUE, titleX = TRUE) %>% 
    #   layout(xaxis = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'),
    #          yaxis = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'),
    #          plot_bgcolor='#e5ecf6')
    # fig
    

    
    
    
    #' #'
    #' if (input$jitter)
    #'   p <- p + geom_jitter()
    #' #' 
    #' if (input$smooth)
    #'   p <- p + geom_smooth()
    #' #' 
    #' height <- session$clientData$output_p_height
    #' width <- session$clientData$output_p_width
    #' ggplotly(p, height = height, width = width, )
  })
  #' 
  #' Table of data
  # myTableContainer <- reactive({
  #   browser()
  #   df <- unique_triplets()
  #   # Create columns
  #   tab_acoustic_sensors <- unique(df[df$field == "acoustic", "sensor"])
  #   tab_meteo_sensors <- unique(df[df$field == "meteo", "sensor"])
  #   tab_acoustic_vars <- unique(df[df$field == "acoustic", "variable"])
  #   tab_meteo_vars <- unique(df[df$field == "meteo", "variable"])
  #   htmltools::withTags(table(
  #     class = 'display',
  #     # Header with scientific fields (Acoustic and/or Meteo)
  #     thead(
  #       tr(
  #         th(),
  #         th(colspan = length(tab_acoustic_sensors)*length(tab_acoustic_vars), 'Acoustic', class = "dt-center"),
  #         th(colspan = length(tab_meteo_sensors)*length(tab_meteo_vars), 'Meteo', class = "dt-center")
  #       ),
  #       # Sub-header with sensors
  #       tr(
  #         th(),
  #         mapply(tab_headers_row, c(length(tab_acoustic_sensors), length(tab_meteo_sensors)), c(tab_acoustic_sensors, tab_meteo_sensors)),
  #         lapply(X = c(tab_acoustic_sensors, tab_meteo_sensors), 
  #                tab_sensors_row, 
  #                nb_vars = c(length(tab_acoustic_vars), length(tab_meteo_vars)))),
  #       tr(
  #         th(),
  #         lapply(c(rep(tab_acoustic_vars, length(tab_acoustic_sensors)), rep(tab_meteo_vars, length(tab_meteo_sensors))), th)
  #       )
  #     )
  #   ))
  # })
  # 
  # tab_headers_row <- function(extent, char){
  #   th(colspan = extent, char, class = "dt-center")
  # }
  # 
  # output$table_acoustic_tab_data <- DT::renderDataTable({
  #   browser()
  #   if (is.null(mydata$plot)){
  #     return(NULL)
  #   }
  #   DT::datatable(mydata$plot, container = myTableContainer,
  #                 options = list(dom = "t", ordering = FALSE,
  #                                columnDefs = list(list(className = "dt-center", targets = "_all"))
  #                 ))
  # })
  output$table_acoustic_tab_data <- renderDataTable(
    mydata$plot, options = list(paging = TRUE, pageLength =  10))
  #' 
  #' DATA VISUALIZATION
  #' 
  #' facetColumns <- reactive({
  #'   choices <- c(None='.', str_remove_all(names(mydata$plot), c("timestamp|value")))
  #'   return(choices)
  #' })
  #' #' 
  #' output$facet_columns <- renderUI({
  #'   selectInput('facet_col', 'Facet Column', facetColumns())
  #' })
  #' 
  colorPlot <- reactive({
    choices <- c('None', str_remove_all(names(mydata$plot), c("timestamp|value")))
    return(choices)
  })
  output$color_plot <- renderUI({
    selectInput('color', label = 'Color', colorPlot())
  })
  #' 
  #' Time slider update
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
  #' 
  #' Date range update
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
  #' 
  # observeEvent(eventExpr = input$time_slider, handlerExpr = {
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
  #' 
  #' Data refresh
  # values <- reactiveValues(default = 0)
  # observeEvent(eventExpr = input$refresh, handlerExpr = {
  #   values$default <- input$refresh
  # })
  #' 
  #' Sensor map rendering
  sensors_visu_map <- set_sensors_color_icon(sensors_df = sensors_info, selected_sensors = c(init$selection$acoustic_sensors, init$selection$meteo_sensors))
  map_visu_tab <- init_map_of_sensors(sensors = sensors_visu_map, wind_turbines = wind_turbines_info) %>% 
    add_sensors_to_map(sensors = sensors_visu_map) %>% 
    add_wind_turbines_to_map(wind_turbines = wind_turbines_info) %>% 
    add_scale_bar_to_map() %>% 
    add_grid_to_map() %>% 
    add_north_arrow_to_map() %>% 
    addLayersControl(overlayGroups = c("Sensors", "Wind turbines", "Grid"), 
                     position = "bottomleft", 
                     options = layersControlOptions(collapsed = FALSE)
    )
  output$map_tab_visu <- renderLeaflet({map_visu_tab})
  #' 
  #' Table of acoustic data
  output$table_acoustic_tab_visu <- renderDataTable(mydata$plot)
  #' 
  #' MISCELLANEOUS
  #' 
  output$histo <- renderPlot({
    set.seed(122)
    histdata <- rnorm(500)
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  #' 
  #' Reuse the same output binding on different tab panels
  options<-callModule(modal.options,"options1",options=if(exists("options")){options}else{reactiveValues(check1=FALSE)})
  options<-callModule(modal.options,"options2",options=if(exists("options")){options}else{reactiveValues(check1=FALSE)})
  output$out1<-output$out2<-renderText({options$check1})
  
  # session$onSessionEnded(function() {
  #   browser()
  #   shinyjs::reset()
  # })
  
} # End sever