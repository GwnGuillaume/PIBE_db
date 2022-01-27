library(config)
library(DT)
library(tidyverse)
library(zeallot)
library(lubridate)
library(hms)
library(plotly)
library(ggplot2)
library(scales)
library(elastic)
library(httr)
library(chron)
library(qdapRegex)
library(leaflet)
library(proj4)
library(sf)
library(sfheaders)
library(concaveman)
library(shiny)
library(shinyFiles)
library(shinyWidgets)
library(shinydashboard)
library(shinyTime)
library(shinyjs)
library(shinycssloaders)
library(fontawesome)


###########################################################
###########################################################
###                                                     ###
###                FUNCTIONS AND METHODS                ###
###                                                     ###
###########################################################
###########################################################


##=============================
##  Remote server utilities   =
##=============================

##....................................
##  Download data from remote server  
##....................................
download_data <- function(conn, input){
  the_start_date = as.Date(x = input$start_date, format = "%Y-%m-%d")
  the_start_date_time = update(as.POSIXct(x = input$start_time, format = "%Y-%m-%d %H:%M:%S"), year = year(the_start_date), month = month(the_start_date), mday = day(the_start_date))
  the_end_date = as.Date(x = input$end_date, format = "%Y-%m-%d")
  the_end_date_time = update(as.POSIXct(x = input$end_time, format = "%Y-%m-%d %H:%M:%S"), year = year(the_end_date), month = month(the_end_date), mday = day(the_end_date))
  print("\n Reloading data")
  dataset <- get_data(conn = conn,
                      start_date = format(as.POSIXct(x = input$start_date, format = "%Y-%m-%d"), "%Y/%m/%d"),
                      start_time = format(as.POSIXct(x = the_start_date_time, format = "%Y-%m-%d %H:%M:%S"), "%H:%M"),  # input$start_time,
                      end_date = format(as.POSIXct(x = input$end_date, format = "%Y-%m-%d"), "%Y/%m/%d"),
                      end_time = format(as.POSIXct(x = the_end_date_time, format = "%Y-%m-%d %H:%M:%S"), "%H:%M"),  # input$end_time
                      fields = input$fields,
                      timesampling = input$timesampling,
                      sensors = input$sensors,
                      indicators = input$indicators,
                      frequencies = input$frequencies)
  print("\n \t -> data reloaded")
  return(dataset)
}

##..................................................
##  Remote server query to get data between         
##  start_dt (format = '%Y/%m/%d-%H:%M') and        
##  end_dt (format = '%Y/%m/%d-%H:%M') for selected 
##  indicators (character, ex: c('Leq_A', 'Leq_Z'))  
##..................................................
query_discover_params <- function(start_dt, end_dt, indicators){
  t_beg = format(as.POSIXct(start_dt, format = "%Y/%m/%d-%H:%M"), 
                 format = "%Y-%m-%dT%H:%M:%OSZ")
  t_end = format(as.POSIXct(end_dt, format = "%Y/%m/%d-%H:%M"), 
                 format = "%Y-%m-%dT%H:%M:%OSZ")
  indicators.list = c("@timestamp", indicators)
  indicators.str = paste0('"', paste(indicators.list, collapse = '","'), '"')
  txt_query <- sprintf('{"version": true,
                         "size": 10000,
                         "sort": [{"@timestamp": {"order": "asc",
                                                  "unmapped_type": "boolean"}}],
                         "aggs": {"2": {"date_histogram": {"field": "@timestamp",
                                                           "calendar_interval": "1d",
                                                           "time_zone": "Europe/Paris",
                                                           "min_doc_count": 1}}},
                         "stored_fields": [%s],
                         "script_fields": {},
                         "docvalue_fields": [{"field": "@timestamp",
                                              "format": "date_time"}],
                         "_source": {"excludes": [], "includes":[%s]},
                         "query": {"bool": {"must": [],
                                            "filter": [{"match_all": {}},
                                                       {"range": {"@timestamp": {"gte": "%s",
                                                                                 "lte": "%s",
                                                                                 "format": "strict_date_optional_time"}}}],
                                            "should": [],
                                            "must_not": []}},
                         "highlight": {"pre_tags": ["@kibana-highlighted-field@"],
                                       "post_tags": ["@/kibana-highlighted-field@"],
                                       "fields": {"*": {}},
                                       "fragment_size": 2147483647}}', 
                       indicators.str, indicators.str, t_beg, t_end)
  return(txt_query)
}
##.......................................................................
##  Get the data index list for fields, sensors and time sampling input  
##.......................................................................
get_indexes_from_input <- function(fields, sensors, timesampling) {
  indexes <-str_c(fields, sensors, timesampling, sep = "_")
}

##................................................................................................................
##  Get data from url with the following parameters:                                                              
##  conn: url connection                                                                                          
##  start_date: start date (format "yyyy/mm/dd")                                                                  
##  start_time: start time (format "hh:mn")                                                                       
##  end_date: end_date (format "yyyy/mm/dd")                                                                      
##  end_time: end time (format "hh:mn")                                                                           
##  fields: list of fields -> `c("acoustique", "meteo")`                                                          
##  timesampling: time sampling -> c("100ms", "1min", "10min")                                                    
##  sensors: list of sensors -> c("a1", "a2", "a3", "a4", "a5")                                                   
##  indicators: list of indicators -> c("Leq_A", "Leq_Z")                                                         
##  frequencies: list of frequencies ->  c("6.3", "8", "10", "12.5", "16", "20", "25", "31.5", "40", "50", "63",  
##                                         "80", "100", "125", "160", "200", "250", "315", "400", "500", "630",   
##                                         "800", "1000", "1250", "1600", "2000", "2500", "3150", "4000", "5000", 
##                                         "6300", "8000", "10000", "12500", "16000", "20000", "A", "Z", "all")   
##................................................................................................................
get_data <- function(conn, start_date, start_time, end_date, end_time, fields, timesampling, sensors, indicators, frequencies) {
  # print("get_data")
  # print(paste0("start_date: ", start_date))
  # print(paste0("start_time: ", start_time))
  # print(paste0("end_date: ", end_date))
  # print(paste0("end_time: ", end_time))
  start_date_time = paste(start_date, start_time, sep = "-")
  end_date_time = paste(end_date, end_time, sep = "-")
  indexes <- get_indexes_from_input(fields, sensors, timesampling)
  indicators = paste(indicators, frequencies, sep = "_")
  size = count(conn = conn, index = indexes)
  tmp_data <- Search(conn = conn, index = indexes, body = query_discover_params(start_dt=start_date_time, end_dt=end_date_time, indicators))
  tmp_data <- tmp_data$hits$hits
  df <- data.frame(timestamp = as.POSIXct(character()), sensor = character(), indicators = character(), value = double(), stringsAsFactors = FALSE)
  tmp_timestamp = as.POSIXct(start_date_time, format = "%Y/%m/%d-%H:%M")
  while (tmp_timestamp != as.POSIXct(end_date_time, format = "%Y/%m/%d-%H:%M")) {
    start_date_time = format(x = tmp_timestamp, format = "%Y/%m/%d-%H:%M")
    tmp_data <- Search(conn = conn, index = indexes, body = query_discover_params(start_dt=start_date_time, end_dt=end_date_time, indicators))
    tmp_data <- tmp_data$hits$hits
    for (idt in seq(1, length(tmp_data))) {
      tmp_val = as.numeric(tmp_data[[idt]]$fields[indicators][[1]])
      if(identical(tmp_val, numeric(0))) {
        tmp_val = NA
      }
      tmp_timestamp = as.POSIXct(tmp_data[[idt]]$`_source`$`@timestamp`)
      es.init.index = tmp_data[[idt]]$'_index'
      es.init.sensor <- str_split(es.init.index, pattern = "_", n = 3)[[1]][2]
      df <- df %>% add_row(timestamp = tmp_timestamp,
                           sensor = es.init.sensor,
                           indicators = indicators,
                           value = tmp_val)
    }
  }
  df <- df[-which(duplicated(df)), ]
  return(df)
}


##==================
##  Sensor tools   =
##==================

##.......................................................
##  Data frame creation from csv file with input header  
##.......................................................
sensor_file_to_df <- function(csv_filename, header) {
  tmp <- read.csv(file = csv_filename, header = FALSE, check.names = FALSE, stringsAsFactors = TRUE)
  df <- as.data.frame(t(tmp))
  colnames(df)<- header   # Set column names
  df <- df[-1,]           # Remove first row that is now the
  return(df)
}

##........................................
##  Get sensor information from csv file  
##........................................
get_sensor_info <- function(sensor_filename){
  sensors_tmp <- sensor_file_to_df(csv_filename = sensor_filename, 
                                   header = c("mast", "name", "height (m)", "coord. GPS", "type"))
  sensors_tmp <- sensors_tmp %>% 
    separate(col = "coord. GPS", 
             into = c("latitude", "longitude", "latdeg", "longdeg"), 
             sep = " ", convert = TRUE) %>% 
    mutate(latitude = str_sub(latitude, 1, -2)) %>% 
    mutate(latitude = as.numeric(latitude))  %>% 
    mutate(longitude = as.numeric(longitude)) %>% 
    mutate(latdeg = str_sub(latdeg, 2, -1)) %>% 
    mutate(longdeg = str_sub(longdeg, 1, -2))
  sensors_tmp <- sensors_tmp[!is.na(sensors_tmp$longitude), ]
  sensors_tmp$lon <- as.numeric(sensors_tmp$longitude)
  sensors_tmp$lat <- as.numeric(sensors_tmp$latitude)
  sensors <- st_as_sf(x = sensors_tmp,
                      coords = c("longitude", "latitude"),
                      crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  return(sensors)
}


##==========================
##  HTML documents tools   =
##==========================

##.....................................
##  HTML document for download action  
##.....................................
downloading_page <- function(input){
  html_page <- sprintf('<p><img style="width:201px;height:45px;display: block; margin-left: auto; margin-right: auto;" 
                        src="https://www.anr-pibe.com/sites/pibe/files/anr-pibe-logo.gif">
                        </p><h3 style="text-align: center;">Downloading data ...</p>
                        <p><img style="width:201px;display: block; margin-left: auto; margin-right: auto;" 
                        src="http://northerntechmap.com/assets/img/loading-dog-final.gif">
                        </p><h5 style="text-align: left;"><ul>
                        <li>dates: %s to %s</li>
                        <li>indicators: %s</li>
                        <li>sensors: %s</li>
                        <li>frequencies: %s</li>
                        </ul></h5><hr /> </p>', 
                       input$start_time, input$end_time, 
                       paste(input$indicators, collapse = ", "), 
                       paste(input$sensors, collapse = ", "), 
                       paste(input$frequencies, collapse = ", "))
  return(html_page)
}


##=========================
##  Miscellaneous tools   =
##=========================

##....................................
##  Creation of the %notin% operator  
##....................................
`%notin%` <- Negate(`%in%`)

##.........................................................
##  Reuse the same output binding on different tab panels  
##.........................................................
modal.options.UI<-function(id){
  ns<-NS(id)
  tagList(
    actionButton(ns("showModal"),"Show")
  )
}
modal.options<-function(input,output,session,options){
  modal<-function(ns, options){
    modalDialog(
      size = "s",
      title = "Upload Data",
      easyClose = TRUE,
      fade = TRUE,
      checkboxInput(ns("check1"), "Option 1", value = options$check1),
      footer=tagList(
        actionButton(ns("submit"), "Submit", width = "100%")
      )
    )
  }
  observeEvent(input$showModal,{
    ns <- session$ns
    showModal(modal(ns=ns,options))
  })
  observeEvent(input$submit,{
    options$check1<-input$check1
    removeModal()
  })
  
  return(options)
}


###########################################################################
###########################################################################
###                                                                     ###
###                GLOBAL OPTIONS AND INITIAL PARAMETERS                ###
###                                                                     ###
###########################################################################
###########################################################################


##====================
##  Global options   =
##====================

##.......................
##  Page refresh button  
##.......................
jscode <- "shinyjs.refresh = function() { history.go(0); }"

##..............................................
##  Global options change to show milliseconds  
##..............................................
my_options <- options(digits.secs = 3)

##.........................
##  File path to crt file  
##.........................
# crt_file_path = paste(getwd(), "www/pibe.crt", sep = "/")
# crt_file_path = paste("/srv/shiny-server/PIBE_db", "www/pibe.crt", sep = "/")  # shinyapp.io ?


##=================================
##  Remote server configuration   =
##=================================

##............................
##  ElasticSearch parameters  
##............................
es_params <- config::get(file = paste(getwd(), "www", "server.conf", sep = "/"))

##............................
##  Remote server connection  
##............................
cat(file = stderr(), "| Establishing connexion\n")
conn = connect(host = es_params$host, port = es_params$port, user = es_params$user, pwd = es_params$pwd, path=es_params$path, transport_schema = es_params$transport_schema, cainfo = es_params$cainfo)
server_health <- cluster_health(conn = conn)
cat(file = stderr(), "  -> Connexion ready !\n")


##===================================
##  User interface initialization   =
##===================================

##........................................
##  Data query parameters initialization  
##........................................
start_date = "2020/03/22"
start_time = "10:00"
end_date = "2020/03/29"
end_time = "01:00"
es.init.fields <- c("acoustique")
es.init.time.sampling <- c("1min") 
es.init.sensors <- c("a1", "a2", "a3", "a4", "a5")
es.init.acoustic.indicators <- c("Leq")
es.init.acoustic.frequencies <- c("A", "Z")

##........................................
##  Displayed data sample initialization  
##........................................
# count(conn = conn, index = get_indexes_from_input(es.init.fields, es.init.sensors, es.init.time.sampling))
sample_data_file = paste(getwd(), "www", "csv", "sample.csv", sep = "/")
if (file.exists(sample_data_file)){
  cat(file = stderr(), paste0("| Dowloading initial dataset from ",  sample_data_file, "\n"))
  es.init.df <- read_csv(file = sample_data_file, col_names = TRUE)
} else {
  cat(file = stderr(), paste0("| Dowloading initial dataset from remote server\n"))
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
dataset <- es.init.df
cat(file = stderr(), paste0("  -> Initial dataset ready !"))

##.................................................
##  Selected data query parameters initialization  
##.................................................
time.samplings <- c("100ms", "1min", "10min")
acoustic.sensors = c("a1", "a2", "a3", "a4", "a5")
acoustic.indicators <- c("Leq", "L10", "L50", "L90")
acoustic.frequencies <- c("6.3", "8", "10", "12.5", "16", "20", "25", "31.5", "40", "50", "63", "80", "100", "125", "160", "200", "250", "315", "400", "500", "630", "800", "1000",
                          "1250", "1600", "2000", "2500", "3150", "4000", "5000", "6300", "8000", "10000", "12500", "16000", "20000", "A", "Z", "all")
start_date = "2020/03/22"
start_time = "10:00"
the_start_date = as.Date(x = start_date, format = "%Y/%m/%d")
the_start_date_time = update(as.POSIXct(x = start_time, format = "%H:%M"), year = year(the_start_date), month = month(the_start_date), mday = day(the_start_date))
start_date_time = format(the_start_date_time, "%H:%M")
start_time = start_date_time
end_date = "2020/03/25"
end_time = "01:00"
the_end_date = as.Date(x = end_date, format = "%Y/%m/%d")
the_end_date_time = update(as.POSIXct(x = end_time, format = "%H:%M"), year = year(the_end_date), month = month(the_end_date), mday = day(the_end_date))
end_date_time = format(the_end_date_time, "%H:%M")
end_time = end_date_time
# meteo.sensors = c("a1", "a2", "a3", "a4", "a5")
# meteo.indicators <- c("AtmPress", "Humidity", "Rain", "Temperature", "WindDirection", "WindSpeed")

##...................................
##  Sensors location and map centre  
##...................................
tiles <- c('Default', 'Stamen.Toner', 'CartoDB.Positron', 'GeoportailFrance', 'Esri.NatGeoWorldMap')  # List of tiles for sensor map
sensors <- get_sensor_info(paste(getwd(), "www", "csv", "sensors.csv", sep = "/"))
points_hull <- sensors %>% st_combine() %>% st_cast("POINT") %>% st_sf()
# points_hull.X <- sf::st_as_sf(x = points_hull, coords = c("Xi", "Yi"), crs = 4326)  # longitude
# points_hull.Y <- sf::st_as_sf(x = points_hull, coords = c("Xj", "Yj"), crs = 4326)   # latitude
# nodes <- cbind(points_hull.Y, points_hull.X)
# nodes_unique <- unique(nodes)
# if(nrow(nodes_unique) == 2) {
#   line_from_nodes <- nodes_unique %>% st_combine() %>% st_cast("LINESTRING") %>% st_sf()
#   centre_point <- st_centroid(line_from_nodes) %>% st_coordinates()
# } else {
#   concave_hull <- points_hull %>% concaveman()
#   centre_point <- st_centroid(concave_hull) %>% st_coordinates()
# }
concave_hull <- points_hull %>% concaveman()
centre_point <- st_centroid(concave_hull) %>% st_coordinates()





# # start_dt : format = "%Y/%m/%d-%H:%M"
# # end_dt : format = "%Y/%m/%d-%H:%M"
# query_discover_available_indicators <- function(start_dt, end_dt){
#   t_beg = format(as.POSIXct(start_dt, format = "%Y/%m/%d-%H:%M"), format = "%Y-%m-%dT%H:%M:%OSZ")
#   t_end = format(as.POSIXct(end_dt, format = "%Y/%m/%d-%H:%M"), format = "%Y-%m-%dT%H:%M:%OSZ")
#   txt_query <- sprintf('{"version": true,
#                          "size": 500,
#                          "sort": [{"@timestamp": {"order": "desc",
#                                                   "unmapped_type": "boolean"}}],
#                          "aggs": {"2": {"date_histogram": {"field": "@timestamp",
#                                                            "fixed_interval": "3h",
#                                                            "time_zone": "Europe/Paris","min_doc_count": 1}}},
#                          "stored_fields": ["*"],
#                          "script_fields": {},
#                          "docvalue_fields": [{"field": "@timestamp","format": "date_time"},
#                                              {"field": "alert.createdAt","format": "date_time"},
#                                              {"field": "application_usage_daily.timestamp","format": "date_time"},
#                                              {"field": "canvas-element.@created","format": "date_time"},
#                                              {"field": "canvas-element.@timestamp","format": "date_time"},
#                                              {"field": "canvas-workpad.@created","format": "date_time"},
#                                              {"field": "canvas-workpad.@timestamp","format": "date_time"},
#                                              {"field": "cases-comments.created_at","format": "date_time"},
#                                              {"field": "cases-comments.pushed_at","format": "date_time"},
#                                              {"field": "cases-comments.updated_at","format": "date_time"},
#                                              {"field": "cases-configure.created_at","format": "date_time"},
#                                              {"field": "cases-configure.updated_at","format": "date_time"},
#                                              {"field": "cases-user-actions.action_at","format": "date_time"},
#                                              {"field": "cases.closed_at","format": "date_time"},
#                                              {"field": "cases.created_at","format": "date_time"},
#                                              {"field": "cases.external_service.pushed_at","format": "date_time"},
#                                              {"field": "cases.updated_at","format": "date_time"},
#                                              {"field": "endpoint:user-artifact-manifest.created","format": "date_time"},
#                                              {"field": "endpoint:user-artifact.created","format": "date_time"},
#                                              {"field": "event.end","format": "date_time"},
#                                              {"field": "event.start","format": "date_time"},
#                                              {"field": "fleet-agent-actions.created_at","format": "date_time"},
#                                              {"field": "fleet-agent-actions.sent_at","format": "date_time"},
#                                              {"field": "fleet-agent-events.timestamp","format": "date_time"},
#                                              {"field": "fleet-agents.enrolled_at","format": "date_time"},
#                                              {"field": "fleet-agents.last_checkin","format": "date_time"},
#                                              {"field": "fleet-agents.last_updated","format": "date_time"},
#                                              {"field": "fleet-agents.unenrolled_at","format": "date_time"},
#                                              {"field": "fleet-agents.unenrollment_started_at","format": "date_time"},
#                                              {"field": "fleet-agents.updated_at","format": "date_time"},
#                                              {"field": "fleet-enrollment-api-keys.created_at","format": "date_time"},
#                                              {"field": "fleet-enrollment-api-keys.expire_at","format": "date_time"},
#                                              {"field": "fleet-enrollment-api-keys.updated_at","format": "date_time"},
#                                              {"field": "ingest-agent-policies.updated_at","format": "date_time"},
#                                              {"field": "ingest-package-policies.created_at","format": "date_time"},
#                                              {"field": "ingest-package-policies.updated_at","format": "date_time"},
#                                              {"field": "lens-ui-telemetry.date","format": "date_time"},
#                                              {"field": "siem-detection-engine-rule-status.lastFailureAt","format": "date_time"},
#                                              {"field": "siem-detection-engine-rule-status.lastLookBackDate","format": "date_time"},
#                                              {"field": "siem-detection-engine-rule-status.lastSuccessAt","format": "date_time"},
#                                              {"field": "siem-detection-engine-rule-status.statusDate","format": "date_time"},
#                                              {"field": "siem-ui-timeline-note.created","format": "date_time"},
#                                              {"field": "siem-ui-timeline-note.updated","format": "date_time"},
#                                              {"field": "siem-ui-timeline-pinned-event.created","format": "date_time"},
#                                              {"field": "siem-ui-timeline-pinned-event.updated","format": "date_time"},
#                                              {"field": "siem-ui-timeline.created","format": "date_time"},
#                                              {"field": "siem-ui-timeline.dateRange.end","format": "date_time"},
#                                              {"field": "siem-ui-timeline.dateRange.start","format": "date_time"},
#                                              {"field": "siem-ui-timeline.favorite.favoriteDate","format": "date_time"},
#                                              {"field": "siem-ui-timeline.updated","format": "date_time"},
#                                              {"field": "task.retryAt","format": "date_time"},
#                                              {"field": "task.runAt","format": "date_time"},
#                                              {"field": "task.scheduledAt","format": "date_time"},
#                                              {"field": "task.startedAt","format": "date_time"},
#                                              {"field": "telemetry.lastReported","format": "date_time"},
#                                              {"field": "updated_at","format": "date_time"},
#                                              {"field": "upgrade-assistant-reindex-operation.locked","format": "date_time"},
#                                              {"field": "url.accessDate","format": "date_time"},
#                                              {"field": "url.createDate","format": "date_time"}],
#                          "_source": {"excludes": []},
#                          "query": {"bool": {"must": [],
#                                             "filter": [{"match_all": {}},
#                                                        {"range": {"@timestamp": {"gte": "%s",
#                                                                                  "lte": "%s",
#                                                                                  "format": "strict_date_optional_time"}}}],
#                                             "should": [],
#                                             "must_not": []}},
#                          "highlight": {"pre_tags": ["@kibana-highlighted-field@"],
#                                        "post_tags": ["@/kibana-highlighted-field@"],
#                                        "fields": {"*": {}},
#                                        "fragment_size": 2147483647}}',
#                        t_beg, t_end)
#   return(txt_query)
# }





# query_es<- function(start_dt, end_dt){
#   t_beg = format(as.POSIXct(start_dt, format = "%Y/%m/%d-%H:%M"), format = "%Y-%m-%dT%H:%M:%OSZ")
#   t_end = format(as.POSIXct(end_dt, format = "%Y/%m/%d-%H:%M"), format = "%Y-%m-%dT%H:%M:%OSZ")
#   txt_query <- sprintf(
#     '{"params":{
#       "ignoreThrottled":true,
#       "index":"acoustique_a1_100ms,acoustique_a2_100ms",
#       "body":{
#         "version":true,
#         "size":500,
#         "sort":[{"@timestamp":{"order":"desc","unmapped_type":"boolean"}}],
#         "aggs":{"2":{"date_histogram":{
#           "field":"@timestamp",
#           "calendar_interval":"1d",
#           "time_zone":"Europe/Paris",
#           "min_doc_count":1}}},
#         "stored_fields":["*"],
#         "script_fields":{},
#         "docvalue_fields":[{"field":"@timestamp","format":"date_time"}],
#         "_source":{"excludes":[]},
#         "query":{"bool":{
#           "must":[],
#           "filter":[
#             {"match_all":{}},
#             {"range":{
#               "@timestamp":{
#                 "gte":%s,
#                 "lte":%s,
#                 "format":"strict_date_optional_time"}}}],
#           "should":[],
#           "must_not":[]}},
#         "highlight":{"pre_tags":["@kibana-highlighted-field@"],"post_tags":["@/kibana-highlighted-field@"],"fields":{"*":{}},"fragment_size":2147483647}
#       },
#       "rest_total_hits_as_int":true,
#       "ignore_unavailable":true,
#       "ignore_throttled":true,
#       "preference":1638962038807,
#       "timeout":"30000ms"}
#     }', 
#     t_beg, t_end)
#   return(txt_query)
# }


# query_body <- function(){
#   '{
#     "version": true,
#     "size": 10,
#     "sort": [
#       {
#         "@timestamp": {
#           "order": "desc",
#           "unmapped_type": "boolean"
#         }
#       }
#     ]
#   }'
# }


# query_count <- function(){
#   '{
#     "version": true,
#     "size": 500,
#     "sort": [
#       {
#         "@timestamp": {
#           "order": "desc",
#           "unmapped_type": "boolean"
#         }
#       }
#     ],
#     "aggs": {
#       "2": {
#         "date_histogram": {
#           "field": "@timestamp",
#           "calendar_interval": "1d",
#           "time_zone": "Europe/Paris",
#           "min_doc_count": 1
#         }
#       }
#     },
#     "stored_fields": [
#       "*"
#     ],
#     "script_fields": {},
#     "docvalue_fields": [
#       {
#         "field": "@timestamp",
#         "format": "date_time"
#       }
#     ],
#     "_source": {
#       "excludes": []
#     },
#     "query": {
#       "bool": {
#         "must": [],
#         "filter": [
#           {
#             "match_all": {}
#           },
#           {
#             "range": {
#               "@timestamp": {
#                 "gte": "2020-01-26T23:00:00.000Z",
#                 "lte": "2021-01-02T23:00:00.000Z",
#                 "format": "strict_date_optional_time"
#               }
#             }
#           }
#         ],
#         "should": [],
#         "must_not": []
#       }
#     },
#     "highlight": {
#       "pre_tags": [
#         "@kibana-highlighted-field@"
#       ],
#       "post_tags": [
#         "@/kibana-highlighted-field@"
#       ],
#       "fields": {
#         "*": {}
#       },
#       "fragment_size": 2147483647
#     }
#   }'
# }


# query_discover <- function(){
#   '{
#     "version": true,
#     "size": 500,
#     "sort": [
#       {
#         "@timestamp": {
#           "order": "desc",
#           "unmapped_type": "boolean"
#         }
#       }
#     ],
#     "aggs": {
#       "2": {
#         "date_histogram": {
#           "field": "@timestamp",
#           "calendar_interval": "1d",
#           "time_zone": "Europe/Paris",
#           "min_doc_count": 1
#         }
#       }
#     },
#     "stored_fields": [
#       "*"
#     ],
#     "script_fields": {},
#     "docvalue_fields": [
#       {
#         "field": "@timestamp",
#         "format": "date_time"
#       }
#     ],
#     "_source": {
#       "excludes": []
#     },
#     "query": {
#       "bool": {
#         "must": [],
#         "filter": [
#           {
#             "match_all": {}
#           },
#           {
#             "range": {
#               "@timestamp": {
#                 "gte": "2020-01-26T23:00:00.000Z",
#                 "lte": "2020-03-26T22:00:00.000Z",
#                 "format": "strict_date_optional_time"
#               }
#             }
#           }
#         ],
#         "should": [],
#         "must_not": []
#       }
#     },
#     "highlight": {
#       "pre_tags": [
#         "@kibana-highlighted-field@"
#       ],
#       "post_tags": [
#         "@/kibana-highlighted-field@"
#       ],
#       "fields": {
#         "*": {}
#       },
#       "fragment_size": 2147483647
#     }
#   }'
# }