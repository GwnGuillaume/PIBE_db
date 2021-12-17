# library(curl)
# set_config(config(ssl_verifypeer = FALSE))
# options(RCurlOptions = list(ssl_verifypeer = FALSE))
# options(rsconnect.check.certificate = FALSE)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyTime)
library(lubridate)
library(hms)
library(ggplot2)
library(elastic)
library(httr)
library(chron)
library(qdapRegex)

source('queries.R')

# As we want to show milliseconds, we can change the global options of RStudio to show more digits
my_options <- options(digits.secs = 3)

# File path for crt file
# crt_file_path = paste(getwd(), "ca/pibe.crt", sep = "/")      # local
crt_file_path = setwd("/srv/shiny-server/PIBE_db/ca/pibe.crt")  # shinyapp.io

# ElasticSearch parameters
es_params <- c(host = "51.178.66.152",
               port = 443,
               transport_schema = "https",
               user = "lecteur",
               pwd = "PVLDn&+6Afnt.",
               path = "elastic",
               crtfile = "pibe.crt",
               cainfo = crt_file_path)

cat(file = stderr(), "Establishing connexion\n")
conn = connect(host = es_params["host"], port = es_params["port"], user = es_params["user"], pwd = es_params["pwd"], path=es_params["path"], transport_schema = es_params["transport_schema"], cainfo = es_params["cainfo"])
cluster_health(conn = conn)


# Data sample for initial ui
# count(conn = conn, index = get_indexes_from_input(es.init.fields, es.init.sensors, es.init.time.sampling))
sample_data_file = paste(getwd(), "Data_sample.csv", sep = "/")
if (file.exists(sample_data_file)){
  es.init.df <- read_csv(file = sample_data_file, col_names = TRUE)
} else {
  start_date = "2020/03/22"
  start_time = "10:00"
  end_date = "2020/03/29"
  end_time = "01:00"
  es.init.fields <- c("acoustique")
  es.init.time.sampling <- c("1min") 
  es.init.sensors <- c("a1", "a2", "a3", "a4", "a5")
  es.init.acoustic.indicators <- c("Leq")
  es.init.acoustic.frequencies <- c("A", "Z")
  es.init.df <- get_data(conn = conn, 
                         start_date = start_date, 
                         start_time = start_time, 
                         end_date = end_date, 
                         end_time = end_time, 
                         fields = es.init.fields, 
                         timesampling = es.init.time.sampling, 
                         sensors = es.init.sensors, 
                         indicators = es.init.acoustic.indicators, 
                         frequencies = es.init.acoustic.frequencies)
}

# Available queries
time.samplings <- c("100ms", "1min", "10min")
acoustic.sensors = c("a1", "a2", "a3", "a4", "a5")
acoustic.indicators <- c("Leq", "L10", "L50", "L90")
acoustic.frequencies <- c("6.3", "8", "10", "12.5", "16", "20", "25", "31.5", "40", "50", "63", "80", "100", "125", "160", "200", "250", "315", "400", "500", "630", "800", "1000",
                          "1250", "1600", "2000", "2500", "3150", "4000", "5000", "6300", "8000", "10000", "12500", "16000", "20000", "A", "Z", "all")
start_date = "2020/03/22"
start_time = "10:00"
# start_date_time = start_time
the_start_date = as.Date(x = start_date, format = "%Y/%m/%d")
the_start_date_time = update(as.POSIXct(x = start_time, format = "%H:%M"), year = year(the_start_date), month = month(the_start_date), mday = day(the_start_date))
start_date_time = format(the_start_date_time, "%H:%M")
start_time = start_date_time
end_date = "2020/03/25"
end_time = "01:00"
# end_date_time = end_time
the_end_date = as.Date(x = end_date, format = "%Y/%m/%d")
the_end_date_time = update(as.POSIXct(x = end_time, format = "%H:%M"), year = year(the_end_date), month = month(the_end_date), mday = day(the_end_date))
end_date_time = format(the_end_date_time, "%H:%M")
end_time = end_date_time
# meteo.sensors = c("a1", "a2", "a3", "a4", "a5")
# meteo.indicators <- c("AtmPress", "Humidity", "Rain", "Temperature", "WindDirection", "WindSpeed")

# write.csv(es.init.df, paste(getwd(), "Data_sample.csv", sep = "/"), row.names = FALSE)

dataset <- es.init.df

dashboardPage(

  dashboardHeader(title = 'PIBE Database Management Dashboard',
                  tags$li(class = "dropdown",
                          tags$a(href='https://www.anr-pibe.com/en', 
                                 target="_blank", 
                                 tags$img(height = "20px", 
                                          alt = "PIBE Logo", 
                                          src = "https://www.anr-pibe.com/sites/pibe/files/anr-pibe-logo.gif")
                          )
                  ),  
                  dropdownMenuOutput('messageMenu'),
                  dropdownMenu(type = 'notifications',
                               notificationItem(text = '5 new users today', icon('users')),
                               notificationItem(text = '12 items delivered', 
                                                icon('truck'), status = 'success'),
                               notificationItem(text = 'Server load at 86%', 
                                                icon = icon('exclamation-triangle'), 
                                                status = 'warning')),
                  dropdownMenu(type = 'tasks',
                               badgeStatus = 'success',
                               taskItem(value = 90, color = 'green', 'Documentation'),
                               taskItem(value = 17, color = 'aqua', 'Project X'),
                               taskItem(value = 75, color = 'yellow', 'Server deployment'),
                               taskItem(value = 80, color = 'red', 'Overall project'))
  ),
  
  ## Sidebar content
  dashboardSidebar(
    width = 290,
    sidebarMenu(
      # Pibe project
      menuItem("Pibe project", tabName = "pibe_project", icon = icon("info")),
      # Sensors map
      menuItem("Sensors map", tabName = "sensors_map", icon = icon("map-marked-alt")),
      # Data
      menuItem("Data query", tabName = "data_query", icon = icon("calendar")),
      splitLayout(cellWidths = c("50%", "50%"),
                  dateInput("start_date", label = "Date from:", value = start_date, format = "yyyy/mm/dd"),
                  timeInput("start_time", label = "at", value = strptime(start_date_time, "%H:%M"), seconds = FALSE)),
      splitLayout(cellWidths = c("50%", "50%"),
                  dateInput("end_date", label = "Date to:", value = end_date, format = "yyyy/mm/dd"),
                  timeInput("end_time", label = "at", value = strptime(end_date_time, "%H:%M"), seconds = FALSE)),
      # Fields and indicators
      menuItem("Fields and indicators", tabName = "fields_indicators", icon = icon("dashboard")),
      splitLayout(cellWidths = c("50%", "50%"),
                  radioButtons("fields", label = "Field(s)", choiceNames = c('acoustic', 'meteo'),  choiceValues = c('acoustique', 'meteo'),  selected = "acoustique", inline = FALSE),
                  radioButtons("timesampling", label = "Time sampling", inline = FALSE, selected = "1min",  choices = time.samplings)),
      splitLayout(cellWidths = c("33%", "33%", "33%"),
                  checkboxGroupInput("sensors", label = "Sensor(s)", choices = acoustic.sensors, selected = c("a1", "a2", "a3", "a4", "a5")),
                  checkboxGroupInput("indicators", label = "Indicator(s)", choices = acoustic.indicators, selected = "Leq"),
                  selectInput("frequencies", "Frequencies", choices = acoustic.frequencies, selected = c("A", "Z"), multiple = TRUE, size = 5, selectize = FALSE)),
      # Visualization
      menuItem("Visualization", tabName = "visualization", icon = icon("image")),
      splitLayout(cellWidths = c("50%", "50%"),
                  checkboxInput('jitter', 'Jitter'),
                  checkboxInput('smooth', 'Smooth')),
      selectInput('facet_row', label = 'Facet Row', choices = names(dataset)),  # c(None='.', names(dataset))
      selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)), size = 2, selectize = FALSE)),
      selectInput('color', label = 'Color', choices = c('None', names(dataset)))
  ), 
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "pibe_project",
              htmlOutput("pibe_logo"),
              strong("PIBE project"),
              p("Project presentation.", style = "font-family: 'times'; font-si16pt"),
              div(em("URL link: "), tags$a(href = "https://www.anr-pibe.com/en", "https://www.anr-pibe.com/en")),
              em("em() creates italicized (i.e, emphasized) text."),
              p("Data are distributed under the", span("licence", style = "color:red"), "..."),
              tags$video(id="video2", type = "video/mp4",src = "https://cerema.cache.ephoto.fr/link/ple18ocjgj4qydc.mp4", controls = "controls")
      ),
      
      # Data tab content
      tabItem(tabName = "data_query",
              h2("Data content"),
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Visualization
      tabItem(tabName = "visualization",
              h2("Data content"),
              fluidRow(
                box(plotOutput("plot", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("time_slider", label = "Time slider", 
                              min = as.POSIXct(x = the_start_date_time, format = "%Y-%m-%d %H:%M:%S CET"), 
                              max = as.POSIXct(x = the_end_date_time, format = "%Y-%m-%d %H:%M:%S CET"), 
                              value =  c(as.POSIXct(x = the_start_date_time, format = "%Y-%m-%d %H:%M:%S CET"), as.POSIXct(x = the_end_date_time, format = "%Y-%m-%d %H:%M:%S CET")))
                )
              )
      )
    )
  )
)


