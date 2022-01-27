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
                            src = "https://www.anr-pibe.com/sites/pibe/files/anr-pibe-logo.gif")
                   )
            ),
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
                 notificationItem(text = 'Server load at 86%', 
                                  icon = icon('exclamation-triangle'), 
                                  status = 'warning')),
    ##===========
    ##  Tasks   =
    ##===========
    dropdownMenu(type = 'tasks',
                 badgeStatus = 'success',
                 taskItem(value = server_health$active_shards_percent_as_number, color = server_health$status, 'Cluster health'),
                 taskItem(value = 17, color = 'aqua', 'Project X'),
                 taskItem(value = 75, color = 'yellow', 'Server deployment'),
                 taskItem(value = 80, color = 'red', 'Overall project'))
  ),
  
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
      ##================
      ##  Data query   =
      ##================
      menuItem("Data query", tabName = "data_query", icon = icon("calendar")),
      conditionalPanel('input.sidebarid == "data_query"',
                       splitLayout(cellWidths = c("50%", "50%"),
                                   dateInput("start_date", label = "Date from:", value = start_date, format = "yyyy/mm/dd"),
                                   timeInput("start_time", label = "at", value = strptime(start_date_time, "%H:%M"), seconds = FALSE)),
                       splitLayout(cellWidths = c("50%", "50%"),
                                   dateInput("end_date", label = "Date to:", value = end_date, format = "yyyy/mm/dd"),
                                   timeInput("end_time", label = "at", value = strptime(end_date_time, "%H:%M"), seconds = FALSE)),
                       splitLayout(cellWidths = c("50%", "50%"),
                                   radioButtons("fields", label = "Field(s)", choiceNames = c('acoustic', 'meteo'),  choiceValues = c('acoustique', 'meteo'),  selected = "acoustique", inline = FALSE),
                                   radioButtons("timesampling", label = "Time sampling", inline = FALSE, selected = "1min",  choices = time.samplings)),
                       splitLayout(cellWidths = c("33%", "33%", "33%"),
                                   checkboxGroupInput("sensors", label = "Sensor(s)", choices = acoustic.sensors, selected = c("a1", "a2", "a3", "a4", "a5")),
                                   checkboxGroupInput("indicators", label = "Indicator(s)", choices = acoustic.indicators, selected = "Leq"),
                                   selectInput("frequencies", "Frequencies", choices = acoustic.frequencies, selected = c("A", "Z"), multiple = TRUE, size = 5, selectize = FALSE))),
      ##=================
      ##  Sensors map   =
      ##=================
      menuItem("Sensor map", tabName = "sensor_map", icon = icon("map-marked-alt")),
      conditionalPanel('input.sidebarid == "sensor_map"',
                       checkboxInput('selected_sensors', 'Highlight selected'),
                       selectInput("map_tiles", "Tiles", choices = tiles, selected = 'Default', multiple = FALSE, size = 3, selectize = FALSE)),
      ##========================
      ##  Data visualization   =
      ##========================
      menuItem("Visualization", tabName = "visualization", icon = icon("image")),
      conditionalPanel('input.sidebarid == "visualization"',
                       splitLayout(cellWidths = c("50%", "50%"),
                                   checkboxInput('jitter', 'Jitter'),
                                   checkboxInput('smooth', 'Smooth')),
                       selectInput('facet_row', label = 'Facet Row', choices = c(None='.', str_remove(string = names(dataset), pattern = c("timestamp", "Value")))),  # c(None='.', names(dataset))
                       selectInput('facet_col', 'Facet Column', c(None='.', str_remove(string = names(dataset), pattern = c("timestamp", "value")))),
                       selectInput('color', label = 'Color', choices = c('None', str_remove(string = names(dataset), pattern = c("timestamp", "value")))))
    )), 
  
  ############################################################################
  ############################################################################
  ###                                                                      ###
  ###                                 BODY                                 ###
  ###                                                                      ###
  ############################################################################
  ############################################################################
  
  dashboardBody(
    tabItems(
      ##===============================
      ##  PIBE project presentation   =
      ##===============================
      tabItem(tabName = "pibe_project",
              # Show html file presenting the PIBE project
              uiOutput(outputId = "pibe_project_html")
      ),
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
      ##==================
      ##  Data content   =
      ##==================
      tabItem(tabName = "data_query",
              fluidRow(
                ##..............................
                ##  Acoustic data visualization 
                ##..............................
                box(title = "Acoustic data visualization", width=8, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    plotlyOutput("acoustic_plot_tab_data", width = "100%") %>% withSpinner(color="#0dc5c1")),
                ##..............................
                ##  Sensor map 
                ##..............................
                box(title = "Sensor map", status = "primary", solidHeader = TRUE, collapsible = TRUE, width=4,
                    leafletOutput("map_tab_visu")),
                ##.........................
                ##  Table of acoustic data 
                ##.........................
                box(title = "Table of acoustic data", status = "primary", solidHeader = TRUE, collapsible = TRUE, width=12, collapsed = TRUE,
                    dataTableOutput('table_acoustic_tab_data'))
              ),
              wellPanel(
                fluidRow(
                  box(title = "Viewing control panel", width=8, status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                      sliderInput("time_slider", label = "Viewing time slider", width = "100%",
                                  min = as.POSIXct(x = the_start_date_time, format = "%Y-%m-%d %H:%M:%S CET"),
                                  max = as.POSIXct(x = the_end_date_time, format = "%Y-%m-%d %H:%M:%S CET"),
                                  value =  c(as.POSIXct(x = the_start_date_time, format = "%Y-%m-%d %H:%M:%S CET"), as.POSIXct(x = the_end_date_time, format = "%Y-%m-%d %H:%M:%S CET")), 
                                  timeFormat="%d/%m %H:%M"),
                      useShinyjs(),
                      extendShinyjs(text = jscode, functions = c("winprint")),
                      actionButton("refresh", label = "Refresh data", icon = icon("sync"))),
                  box(title = "Download selected dataset", width=4, status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                      column(width = 6, 
                             shinyDirButton('downloadFolder', label = 'Folder select', title = 'Please select a destination folder', icon = icon("folder-open"))),
                      column(width = 6, 
                             downloadButton("downloadData", label = "Download"))
                  ),
                ))
      ),
      ##===================
      ##  Visualization   =
      ##===================
      tabItem(
        tabName = "visualization",
        fluidRow(
          ##..............................
          ##  Acoustic data visualization 
          ##..............................
          box(title = "Acoustic data visualization", width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
              plotlyOutput("acoustic_plot_tab_visu", width = "100%") %>% withSpinner(color="#0dc5c1")),
          ##.........................
          ##  Table of acoustic data 
          ##.........................
          box(title = "Table of acoustic data", status = "primary", solidHeader = TRUE, collapsible = FALSE, width=12, collapsed = FALSE,
              dataTableOutput('table_acoustic_tab_visu'))
        )
      )
    ) # End of tabItems
  )
)
