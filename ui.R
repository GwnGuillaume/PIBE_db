dashboardPage(
  
  ############################################################################
  ############################################################################
  ###                                                                      ###
  ###                                HEADER                                ###
  ###                                                                      ###
  ############################################################################
  ############################################################################
  
  dashboardHeader(
    ##===========
    ##  Title   =
    ##===========
    title = 'PIBE Database Management Dashboard',
    tags$li(class = "dropdown",
            tags$a(href='https://www.anr-pibe.com/en', 
                   target="_blank", 
                   tags$img(height = "20px", 
                            alt = "PIBE Logo", 
                            src = "https://www.anr-pibe.com/sites/pibe/files/anr-pibe-logo.gif"))),
    ##==================
    ##  Message menu   =
    ##==================
    dropdownMenuOutput('messageMenu'),
    ##===================
    ##  Notifications   =
    ##===================
    dropdownMenu(type = 'notifications',
                 notificationItem(text = '5 new users today', icon('users')),
                 notificationItem(text = '12 items delivered', 
                                  icon('truck'), status = 'success'),
                 notificationItem(text = sprintf('Data loaded in %sms', downloading_time), #'Server load at 86%', 
                                  icon = icon('history'), 
                                  status = 'warning')),
    ##===========
    ##  Tasks   =
    ##===========
    dropdownMenu(type = 'tasks',
                 badgeStatus = 'success',
                 taskItem(value = server_health$active_shards_percent_as_number, color = server_health$status, 'Cluster health'),
                 taskItem(value = 17, color = 'aqua', 'Project X'),
                 taskItem(value = 75, color = 'yellow', 'Server deployment'),
                 taskItem(value = 80, color = 'red', 'Overall project'))),
  
  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                               SIDEBAR                               ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################
  
  dashboardSidebar(
    tags$style(HTML(".sidebar-menu li a { font-size: 20px; }")), 
    width = 350,
    sidebarMenu(
      id = "sidebarid",
      ##==================
      ##  PIBE project   =
      ##==================
      menuItem("PIBE project", tabName = "pibe_project", icon = icon("info")),
      ##=================
      ##  Sensors map   =
      ##=================
      menuItem("Sensor map", tabName = "sensor_map", icon = icon("map-marked-alt")),
      conditionalPanel('input.sidebarid == "sensor_map"',
                       checkboxInput('selected_sensors', 'Highlight selected'),
                       selectInput("map_tiles", "Tiles", choices = tiles, selected = 'Default', multiple = FALSE, size = 3, selectize = FALSE)),
      ##===================
      ##  Data download   =
      ##===================
      menuItem("Data download", tabName = "data_download", icon = icon("calendar")),
      ##========================
      ##  Data visualization   =
      ##========================
      menuItem("Visualization", tabName = "visualization", icon = icon("image")),
      conditionalPanel('input.sidebarid == "visualization"',
                       fileInput('data_file', 'Choose file', accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                       splitLayout(cellWidths = c("50%", "50%"),
                                   checkboxInput('jitter', 'Jitter'),
                                   checkboxInput('smooth', 'Smooth')),
                       selectInput('facet_row', label = 'Facet Row', choices = init$all_choices$facet_row),  # c(None='.', names(data_app))
                       selectInput('facet_col', 'Facet Column', c(None='.', str_remove(string = names(data_app), pattern = c("timestamp", "value")))),
                       selectInput('color', label = 'Color', choices = c('None', str_remove(string = names(data_app), pattern = c("timestamp", "value"))))))), 
  # TODO: reactive value for choices with names(data_app)
  
  ############################################################################
  ############################################################################
  ###                                                                      ###
  ###                                 BODY                                 ###
  ###                                                                      ###
  ############################################################################
  ############################################################################
  
  dashboardBody(
    
    ##=============
    ##  CSS file  =
    ##=============
    tags$link(rel = "stylesheet", type = "text/css", href = paste("html", "custom.css", sep = .Platform$file.sep)),
    
    tabItems(
      ##===============================
      ##  PIBE project presentation   =
      ##===============================
      tabItem(tabName = "pibe_project",
              # Show html file presenting the PIBE project
              uiOutput(outputId = "pibe_project_html")),
      ##================
      ##  Sensor map   =
      ##================
      tabItem(tabName = "sensor_map",
        h2("Sensor map"),
        fluidRow(
          ##..............
          ##  Sensor map  
          ##..............
          leafletOutput("map_tab_sensors"),
          ##.........................
          ##  Table of sensors 
          ##.........................
          box(title = "Table of sensors", status = "primary", solidHeader = TRUE, collapsible = TRUE, width=12, collapsed = FALSE,
              dataTableOutput('table_sensors_tab_map'))
        )
      ),
      ##===================
      ##  Data download   =
      ##===================
      tabItem(tabName = "data_download",
          box(title = "Dates and time sampling", width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE, 
              column(width = 8, 
                     splitLayout(cellWidths = c("30%", "70%"),
                                 dateInput("start_date_download", label = "from",
                                           value = init$selection$start_date, format = "yyyy/mm/dd"),
                                 timeInput("start_time_download", label = "at", 
                                           value = strptime(init$selection$start_time, "%H:%M"), seconds = FALSE)),
                     splitLayout(cellWidths = c("30%", "70%"),
                                 dateInput("end_date_download", label = "to", 
                                           value = init$selection$end_date, format = "yyyy/mm/dd"),
                                 timeInput("end_time_download", label = "at", 
                                           value = strptime(init$selection$end_time, "%H:%M"), seconds = FALSE))
              ),
              column(width = 4, 
                     radioButtons("time_sampling", label = "Time sampling", inline = FALSE, 
                                  selected = init$selection$time_sampling,  choices = init$all_choices$time_samplings))
          ),
          box(title = "Field(s), sensor(s) and indicator(s)", width=6, status = "primary", solidHeader = TRUE, collapsible = TRUE, 
              prettyRadioButtons("scientific_domain", label = "Field(s)", 
                                 choiceNames = init$all_choices$scientific_domain.names, 
                                 choiceValues = init$all_choices$scientific_domain.values,  
                                 selected = "acoustique", inline = TRUE),
              splitLayout(cellWidths = c("33%", "33%", "33%"),
                          checkboxGroupInput("acoustic_sensors", label = "Acoustic sensor(s)", 
                                             choices = init$all_choices$acoustic_sensors, 
                                             selected = init$selection$acoustic_sensors),
                          checkboxGroupInput("acoustic_indicators", label = "Acoustic indicator(s)", 
                                             choices = init$all_choices$acoustic_indicators, 
                                             selected = init$selection$acoustic_indicators),
                          selectInput("acoustic_frequencies", "Frequencies", 
                                      choices = init$all_choices$acoustic_frequencies, 
                                      selected = init$selection$acoustic_frequencies, 
                                      multiple = TRUE, size = 5, selectize = FALSE))
          ),
          box(title = "Sensor map", status = "primary", solidHeader = TRUE, collapsible = TRUE, width=6,
              leafletOutput("map_tab_visu")),
          fluidRow(
            column(width = 6, align="center",
                   downloadButton("downloadData", label = "Download", style = "color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px;"))
            # box(title = "Download selected dataset", width=4, status = "primary", solidHeader = TRUE, collapsible = TRUE, 
            #     # column(width = 6, 
            #     #        shinyDirButton("downloadFolder", label = 'Select folder', 
            #     #                       title = 'Please select a destination folder', icon = icon("folder-open"))),
            #     column(width = 6, 
            #            downloadButton("downloadData", label = "Download")))
            # # box(title = "data paths:", 
            # #     h3("view"),
            # #     verbatimTextOutput("resultFolder"), br(),
            # #     h3("info"),
            # #     verbatimTextOutput("info"))
          )
      ),
      ##===================
      ##  Visualization   =
      ##===================
      tabItem(tabName = "visualization",
        fluidRow(
          ##..............................
          ##  Acoustic data visualization 
          ##..............................
          box(title = "Acoustic data visualization", width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("acoustic_plot_tab_data", width = "100%") %>% withSpinner(color="#0dc5c1")),
        wellPanel(
          fluidRow(
            box(title = "Viewing control panel", width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                sliderInput("time_slider", label = "Viewing time slider", width = "100%",
                            min = as.POSIXct(x = init$selection$start_date_time, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC"),
                            max = as.POSIXct(x = init$selection$end_date_time, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC"),
                            value =  c(as.POSIXct(x = init$selection$start_date_time, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC"), 
                                       as.POSIXct(x = init$selection$end_date_time, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC")), 
                            timeFormat = "%d/%m %H:%M")
                # useShinyjs(),
                # extendShinyjs(text = jscode, functions = c("winprint")),
                # actionButton("refresh", label = "Refresh data", icon = icon("sync"))
            ),
            box(title = "Table of acoustic data", status = "primary", solidHeader = TRUE, collapsible = TRUE, width=12, collapsed = TRUE,
                dataTableOutput('table_acoustic_tab_data'))
          ),
          )
        )
      )
    ) # End of tabItems
  )
)
