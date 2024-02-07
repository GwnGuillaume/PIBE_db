##  Shinydashboard script for PIBE Database Management Dashboard 
##  ----------------------------------------------------------- 
##  This script defines a Shiny dashboard with various tabs for managing and visualizing data related to the PIBE project. 
##  It utilizes Shinydashboard, leaflet, and plotly for interactive elements. 
##  ----------------------------------------------------------- 
##   
##  @section Initializations and loading Libraries 
##  @description Load the required libraries for the dashboard. 
##   
##  ## Initializations 
##  @name Initializations 
##  @description This part initialize the title and logo for the dashboard. 
##  @param title The title of the dashboard. 
##  @param logo The path to the logo image. 
##   
title = 'PIBE Database Management Dashboard' 
logo = '/www/img/anr-pibe-logo.gif' 
##   
##  ## Loading Libraries 
##  @name Loading Libraries 
##  @description This part loads necessary Libraries. 
##   
suppressMessages(library(shiny)) 
suppressMessages(library(shinydashboard)) 
suppressMessages(library(shinyalert)) 
suppressMessages(library(shinyFiles)) 
suppressMessages(library(shinyWidgets)) 
suppressMessages(library(shinyTime)) 
suppressMessages(library(shinyjs)) 
suppressMessages(library(shinyBS)) 
suppressMessages(library(shinycssloaders)) 
suppressMessages(library(shinyobjects)) 
suppressMessages(library(sfheaders)) 
suppressMessages(library(concaveman)) 
suppressMessages(library(fontawesome)) 
suppressMessages(library(plotly)) 
suppressMessages(library(ggplot2)) 
suppressMessages(library(ggh4x)) 
suppressMessages(library(pkgdown)) 
##   
##  @section Shinydashboard UI 
##  @name Shinydashboard UI 
##  @description This section defines the Shinydashboard UI, including the header, sidebar, and body sections. 
##  @seealso [Shinydashboard documentation](https://rstudio.github.io/shinydashboard/) 
##  @seealso [Leaflet documentation](https://rstudio.github.io/leaflet/) 
##  @seealso [Plotly documentation](https://plotly.com/r/shinyapp-dashboards/) 
##   

```{r chunk-name, echo=FALSE}
dashboardPage(  #'  
  #' @title Header Section 
  #' @description This tab presents the PIBE project and the WP2 package. 
  #' @param title The title of the dashboard. 
  #' @param logo The path to the logo image. 
  #' 
  dashboardHeader( 
    #'  
    #' ### Title 
    title = 'PIBE Database Management Dashboard', 
    tags$li(class = "dropdown", 
            tags$a(href = 'https://www.anr-pibe.com/en',  
                   target = "_blank",  
                   tags$img(height = "20px",  
                            alt = "PIBE Logo",  
                            src = "https://www.anr-pibe.com/sites/pibe/files/anr-pibe-logo.gif",  
                            title= 'https://www.anr-pibe.com/en'))), 
    #'  
    #' ### Message menu 
    dropdownMenuOutput('messageMenu'), 
    #'  
    #' ### Notifications 
    dropdownMenu(type = 'tasks', headerText = "",  
                 notificationItem(text = textOutput("session_counter", inline = TRUE), icon = icon('clock')), 
                 notificationItem(text = textOutput("server_start_datetime", inline = TRUE), icon = icon(name = 'clock')), 
                 notificationItem(text = textOutput("pending_tasks", inline = TRUE), icon = icon(name = 'tasks')), 
                 notificationItem(text = textOutput("max_task_wait_time", inline = TRUE), icon = icon(name = 'user-times')), 
                 notificationItem(text = textOutput("active_shards_percent", inline = TRUE), icon = icon(name = 'user-times')), 
                 notificationItem(text = textOutput("downloading_time", inline = TRUE), icon = icon(name = 'history'), status = 'warning') 
                 ), 
    #'  
    #' ### Tasks 
    dropdownMenu(type = 'tasks', 
                 badgeStatus = 'success', headerText = "",  
                 uiOutput(outputId="active_shards") 
                 ), 
    #'  
    #' ### Quit app 
    tags$li(class = "dropdown", 
            tags$button(id = 'closeApp', type = "button", class = "btn action-button", icon("stop-circle"), style = "color: #fff; background-color: red; border-color: #fff", 
                onclick = "setTimeout(function(){window.close();},500);",  # close browser 
                "Close window"), 
            bsTooltip(id = "closeApp", title = "Quit app and close window", placement = "bottom", trigger = "hover", options = list(container = "header")),) 
  ),  # End of dashboardHeader 
  #'  
  #' @title Sidebar Section 
  #' @description This section defines the sidebar of the dashboard. 
  #' @param id The ID of the sidebar. 
  #' @seealso [Shinydashboard documentation](https://rstudio.github.io/shinydashboard/) 
  #'  
  dashboardSidebar( 
    tags$style(HTML(".sidebar-menu li a { font-size: 20px; }")),  
    sidebarMenu( 
      id = "sidebarid", 
      #'  
      #' ### PIBE project 
      menuItem("PIBE project", tabName = "pibe_project", icon = icon("info")), 
      #'  
      #' ### Data download 
      menuItem("Data download", tabName = "data_download", icon = icon("calendar")), 
      #'  
      #' ### Data visualization 
      menuItem("Visualization", tabName = "visualization", icon = icon("image")), 
      conditionalPanel('input.sidebarid == "visualization"', 
                       fileInput('data_file', 'Choose file', accept=c('text/csv', 'text/comma-separated-values,text/plain')), 
                       splitLayout(cellWidths = c("50%", "50%"), 
                                   checkboxInput('jitter', 'Jitter'), 
                                   checkboxInput('smooth', 'Smooth')), 
                       # selectInput('facet_row', label = 'Facet Row', choices = init$all_choices$facet_row), 
                       # uiOutput(outputId = 'facet_columns'), 
                       uiOutput(outputId = 'color_plot') 
                       ))),  
  #'  
  #' @title Body Section 
  #' @description This section defines the body of the dashboard. 
  #' @seealso [Shinydashboard documentation](https://rstudio.github.io/shinydashboard/) 
  #'  
  dashboardBody( 
    useShinyjs(), 
    # Include the external JavaScript file 
    includeScript(paste(getwd(), "www", "custom.js", sep = .Platform$file.sep)), 
    ## CSS file 
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),  # paste("html", "custom.css", sep = .Platform$file.sep)), 
    tags$head(tags$style(HTML("select {margin-top: 0 !important;}"))), # Overrides default top margin in drop-down lists 
    tabItems( 
      #'  
      #' ### PIBE project presentation 
      #' @description This tab presents the PIBE project and the WP2 package. 
      tabItem(tabName = "pibe_project", 
              #' Show html files presenting the PIBE project and the WP2 package 
              fluidRow( 
                column(width = 12, uiOutput(outputId = "pibe_project_html"))),  
              fluidRow( 
                column(width = 6, uiOutput(outputId = "pibe_project_WP2")),  
                column(width = 6, leafletOutput("map_tab_sensors"))),  # Map of sensors 
              #' Table of sensors 
              box(title = "Table of sensors", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12, collapsed = TRUE, 
                  dataTableOutput('table_sensors_tab_map')),  
              fluidRow( 
                column(width = 12, uiOutput(outputId = "pibe_project_partners"))) 
      ), 
      #'  
      #' ### Data download 
      #' @description This tab allows users to download data based on specified criteria. 
      tabItem(tabName = "data_download", 
          box(title = "Dates and time sampling", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,  
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
          box(title = "Field(s), sensor(s) and indicator(s)", width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE,  
              # prettyRadioButtons("scientific_domain", label = "Field(s)",  
              #                    choiceNames = init$all_choices$scientific_domain,  
              #                    choiceValues = init$all_choices$scientific_domain,   
              #                    selected = "acoustic", inline = TRUE), 
              splitLayout(cellWidths = c("33%", "33%", "33%"), 
                          #' #### Acoustic sensor(s) 
                          div(checkboxGroupInput("acoustic_sensors", label = "Acoustic sensor(s)",  
                                                 choices = init$all_choices$acoustic_sensors,  
                                                 selected = init$selection$acoustic_sensors),  
                              bsPopover(id = "acoustic_sensors", title = "Acoustic sensors", 
                                        content = "Select one or multiple sensors", 
                                        placement = "left", 
                                        trigger = "hover", 
                                        options = list(container = "body"))), 
                          #' #### Acoustic indicator(s) 
                          div(checkboxGroupInput("acoustic_indicators", label = "Acoustic indicator(s)", 
                                                 choices = init$all_choices$acoustic_indicators, 
                                                 selected = init$selection$acoustic_indicators), 
                              bsPopover(id = "acoustic_indicators", title = "Acoustic indicators", 
                                        content = "Select one or multiple acoustic indicators", 
                                        placement = "top", 
                                        trigger = "hover", 
                                        options = list(container = "body"))), 
                          #' #### Frequency-ies 
                          div(selectInput("acoustic_frequencies", "Frequencies", 
                                          choices = init$all_choices$acoustic_frequencies, 
                                          selected = init$selection$acoustic_frequencies, 
                                          multiple = TRUE, size = 5, selectize = FALSE), 
                              bsPopover(id = "acoustic_frequencies", title = "Global and third-octave band frequencies", 
                                        content = "Hold down the shift key to select multiple frequencies", 
                                        placement = "top", 
                                        trigger = "hover", 
                                        options = list(container = "body"))) 
              ), 
              # splitLayout(cellWidths = c("33%", "22%", "22%", "22%"), 
              flowLayout( 
                style = "display:flex;align-items:flex-start;",  # Top align titles 
                #' #### Meteo sensor(s) 
                div( 
                  style = "flex: 1; padding-right: 20px;",  # Adjust the padding as needed 
                  checkboxGroupInput("meteo_sensors", label = "Meteo sensor(s)",  
                                     choices = init$all_choices$meteo_sensors,  
                                     selected = init$selection$meteo_sensors),  
                  bsPopover(id = "meteo_sensors", title = "Meteo sensors", 
                            content = "Select one or multiple sensors", 
                            placement = "left", 
                            trigger = "hover", 
                            options = list(container = "body")) 
                ), 
                # Common title for the selectInputs 
                tagList( 
                  HTML("<h5 style='font-family: inherit; font-weight: bold;'>Meteo variable(s) ...</h5>"), 
                  # Container for the three selectInputs 
                  div( 
                    style = "display:flex;align-items:flex-start;",  # Top align selectInputs 
                    # First selectInput with its popover 
                    div( 
                      style = "flex:1;margin-right:10px;",  # Flex layout with margin 
                      disabled( 
                        selectInput("sound_level_meter_variables",  
                                    label = HTML(paste("... near sound level meters",  
                                                       paste0(get_sensors_names_starting_with(sensors = meteo_sensors,  
                                                                                              names_starting_with = "a"),  
                                                              collapse = ", "), sep = " ")),  
                                    choices = meteo_variables$`sound level meter`,  
                                    selected = NULL,  
                                    multiple = TRUE, size = 5, selectize = FALSE, width = '170px')), 
                      bsPopover(id = "sound_level_meter_variables", 
                                title = HTML("Meteo variable(s) near sound level meters"), 
                                content = "Select at least one sensor from a1, a2, a3, a4 and a5 to activate the drop-down list.", 
                                placement = "bottom", trigger = "hover",  
                                options = list(container = "body")) 
                    ), 
                    div( 
                      style = "flex:1;margin-right:10px;", 
                      disabled( 
                        selectInput("lidar_variables",  
                                    label = HTML("... from lidar\n"),  
                                    choices = meteo_variables$lidar,  
                                    selected = NULL,  
                                    multiple = TRUE, size = 5, selectize = FALSE, width = '170px')), 
                      bsPopover(id = "lidar_variables", 
                                title = HTML("Meteo variable(s) from lidar"), 
                                content = "Select lidar to activate the drop-down list.", 
                                placement = "bottom", trigger = "hover",  
                                options = list(container = "body")) 
                    ), 
                    div( 
                      style = "flex:1;margin-right:10px;", 
                      disabled( 
                        selectInput("sonics_variables",  
                                    label = HTML(paste("... from sonic anemometers",  
                                                       paste0(get_sensors_names_starting_with(sensors = meteo_sensors,  
                                                                                              names_starting_with = "s"),  
                                                              collapse = ", "), sep = " ")), 
                                    choices = meteo_variables$sonic,  
                                    selected = NULL,  
                                    multiple = TRUE, size = 5, selectize = FALSE, width = '170px')), 
                      bsPopover(id = "sonics_variables", 
                                title = HTML("Meteo variable(s) from sonic anemometers"), 
                                content = "Select at least one sensor from s1, s2 and s3 to activate the drop-down list.", 
                                placement = "bottom", trigger = "hover",  
                                options = list(container = "body")) 
                    ) 
                  ) 
                ), 
                shinyjs::hidden(textOutput("meteo_variables")) 
                # verbatimTextOutput(outputId = "meteo_variables", placeholder = FALSE) 
              ) 
          ), 
          box(title = "Sensor map", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6, 
              leafletOutput("map_tab_visu"),  
              splitLayout(cellWidths = c("40%", "30%", "30%"),  
                          actionButton("reset_view_map_tab_visu", "Reset view"), 
                          checkboxInput('selected_sensors', 'Highlight selected sensors', value = TRUE),  
                          checkboxInput('show_grid', 'Show/hide grid origin', value = TRUE)) 
              ), 
          fluidRow( 
            column(width = 2, align = "center",  
                   use_hover(), 
                   hover_action_button(inputId = "connectionButton", label = "Connect", icon = icon(name = 'database'), class = "connect_button", 
                                       button_animation = "pulse-grow", icon_animation = "spin"), 
                   tags$head(tags$style(".connect_button{color: #fff} .connect_button{background-color: #c7c7c7;} .connect_button{border-color: #fff;} .connect_button{padding: 5px 14px 5px 14px;} .connect_button{margin: 5px 5px 5px 5px;}")), 
                   uiOutput(outputId = "connect_button_style"), 
                   uiOutput(outputId = "conn_bsTooltip") 
                   ), 
            column(width = 2, align = "center", 
                   use_hover(), 
                   hover_download_button(outputId = "downloadDataButton", label = "Download", class = "download_data_button", 
                                         button_animation = "pop", icon_animation = "spin"), 
                   tags$head(tags$style(".download_data_button{color: #fff} .download_data_button{background-color: #27ae60;} .download_data_button{border-color: #fff;} .download_data_button{padding: 5px 14px 5px 14px;} .download_data_button{margin: 5px 5px 5px 5px;}")), 
                   uiOutput(outputId = "download_data_button_style"), 
                   uiOutput(outputId = "download_bsTooltip"), 
                   useShinyjs(), 
                   ## hide the downloadButton and only display actionButton 
                   conditionalPanel("false", downloadButton("downloadData")) 
                   ), 
            column(width = 2, align = "left", 
                   textOutput("download_button_disable_text") 
            ) 
          ) 
      ), 
      #'  
      #' @title  Visualization 
      #' @description This tab provides visualizations based on the selected data. 
      #'  
      tabItem(tabName = "visualization", 
        fluidRow( 
          #'  
          #' #### Time series 
          #' @description This sub-section provides a time series plot. 
          #'  
          box(title = "Time series", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, 
              plotlyOutput("plot_data", width = "100%") %>% withSpinner(color = "#0dc5c1"),  
              sliderInput("time_slider", label = "Viewing time slider", width = "100%", 
                          min = as.POSIXct(x = init$selection$start_date_time, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC"), 
                          max = as.POSIXct(x = init$selection$end_date_time, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC"), 
                          value =  c(as.POSIXct(x = init$selection$start_date_time, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC"),  
                                     as.POSIXct(x = init$selection$end_date_time, format = "%Y-%m-%d_%H:%M:%OS", tz = "UTC")),  
                          timeFormat = "%d/%m %H:%M")), 
          #'  
          #'  
          #' #### Scatter plot 
          #' @description This sub-section provides a scatter plot. 
          #'  
          wellPanel( 
            fluidRow( 
              box(title = "Scatter plot", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,  
                  column(width = 2,  
                         # uiOutput(outputId = "scatter_plot_x_axis"),  
                         # uiOutput(outputId = "scatter_plot_y_axis"),  
                         selectInput(inputId = "scatter_plot_y_axis", label = "y-axis variable", multiple = FALSE, 
                                     selected = NULL, choices = NULL), 
                         selectInput(inputId = "scatter_plot_x_axis", label = "x-axis variable", multiple = FALSE, 
                                     selected = NULL, choices = NULL), 
                         selectInput(inputId = "scatter_plot_color", label = "color", multiple = FALSE,  
                                     selected = palette()[4], choices = palette())),  
                  column(width = 10,  
                         plotlyOutput("scatter_plot")) 
              ), 
              box(title = "Table of acoustic data", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12, collapsed = TRUE, 
                  dataTableOutput('table_acoustic_tab_data')) 
            ) 
          ) 
        ) 
      ) 
    ) # End of tabItems 
  ), # End of dashboardBody 
) 
```
