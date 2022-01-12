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
library(scales)
library(elastic)
library(httr)
library(chron)
library(qdapRegex)
library(leaflet)
library(sf)
library(concaveman)
library(shinyjs)
library(shinycssloaders)

jscode <- "shinyjs.refresh = function() { history.go(0); }"

# Change the global options of RStudio to show milliseconds
my_options <- options(digits.secs = 3)

# File path for crt file
crt_file_path = paste(getwd(), "www/pibe.crt", sep = "/")
# crt_file_path = paste("/srv/shiny-server/PIBE_db", "www/pibe.crt", sep = "/")  # shinyapp.io


# ElasticSearch parameters
es_params <- c(host = "51.178.66.152",
               port = 443,
               transport_schema = "https",
               user = "lecteur",
               pwd = "PVLDn&+6Afnt.",
               path = "elastic",
               crtfile = "pibe.crt",
               cainfo = crt_file_path)

# Connection to remote server
cat(file = stderr(), "Establishing connexion\n")
conn = connect(host = es_params["host"], port = es_params["port"], user = es_params["user"], pwd = es_params["pwd"], path=es_params["path"], transport_schema = es_params["transport_schema"], cainfo = es_params["cainfo"])
server_health <- cluster_health(conn = conn)


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


# # Sensors
# sensors <- get_sensors_info("./input/sensors.csv")
# sensors.point <- st_as_sf(x = sensors,
#                           coords = c("longitude", "latitude"),
#                           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# points_hull <- sensors.point %>% st_combine() %>% st_cast("POINT") %>% st_sf()
# p.X <- sf::st_as_sf(x = points_hull, coords = c("Xi", "Yi"), crs = 4326)
# p.Y<- sf::st_as_sf(x = points_hull, coords = c("Xj", "Yj"), crs = 4326)
# nodes<-cbind(p.Y,p.X) 
# concave_hull <- points_hull %>% concaveman()
# centre_point_df <- st_centroid(concave_hull$polygons) %>% st_transform(4326) %>% st_set_crs(2154)
# centre_point <- centre_point_df[[1]]

dataset <- es.init.df

## Functions

# Download data from remote server
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

# Get sensors information
get_sensors_info <- function(sensors_filename){
  sensors_tmp <- read_csv(file = sensors_filename)
  # the_dates <- read_csv(file = 'Bonneval_data.csv')
  sensors <- data.frame(t(sensors_tmp[-1]))
  colnames(sensors) <- t(sensors_tmp[, 1])
  remove(sensors_tmp)
  sensors <- sensors %>% 
    separate(col = "coord. GPS", into = c("latitude", "longitude", "latdeg", "longdeg"), sep = " ", convert = TRUE) %>% 
    mutate(latitude = str_sub(latitude, 1, -2)) %>% 
    mutate(longitude = as.numeric(longitude))  %>% 
    mutate(longitude = as.numeric(longitude)) 
  row_names = rownames(sensors)
  for(idr in seq(length(row_names))){
    if(isTRUE(startsWith(row_names[idr], prefix = "..."))){
      row_names[idr] <- row_names[idr-1]
    }
  }
  rownames(sensors) <- make.names(row_names, unique = TRUE)
  sensors <- sensors[!is.na(sensors$longitude), ]
  return(sensors)
}

# start_dt : format = "%Y/%m/%d-%H:%M"
# end_dt : format = "%Y/%m/%d-%H:%M"
# indicators : character (ex: `c("Leq_A", "Leq_Z")`)
query_discover_params <- function(start_dt, end_dt, indicators){
  t_beg = format(as.POSIXct(start_dt, format = "%Y/%m/%d-%H:%M"), format = "%Y-%m-%dT%H:%M:%OSZ")
  t_end = format(as.POSIXct(end_dt, format = "%Y/%m/%d-%H:%M"), format = "%Y-%m-%dT%H:%M:%OSZ")
  # print("QUERY")
  # print("\n t_beg")
  # print(t_beg)
  # print("\n t_end")
  # print(t_end)
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

# html text when downloading data
downloading_text <- function(input){
  txt_query <- sprintf('{"<p><img style="display: block; margin-left: auto; margin-right: auto;" src="https://www.anr-pibe.com/sites/pibe/files/anr-pibe-logo.gif?w=101&h=45&mode=crop&scale=both" /></p>
<h1 style="text-align: center;">Downloading data ... &nbsp;<strong>Application</strong></h1>
<hr />
<p style="text-align: left;">indicators: %sTEXT TEXT TEXT TEXT TEXT TEXT TEXT................................................... </p>}', 
                       input$indicators)
  return(txt_query)
}

# start_dt : format = "%Y/%m/%d-%H:%M"
# end_dt : format = "%Y/%m/%d-%H:%M"
query_discover_available_indicators <- function(start_dt, end_dt){
  t_beg = format(as.POSIXct(start_dt, format = "%Y/%m/%d-%H:%M"), format = "%Y-%m-%dT%H:%M:%OSZ")
  t_end = format(as.POSIXct(end_dt, format = "%Y/%m/%d-%H:%M"), format = "%Y-%m-%dT%H:%M:%OSZ")
  txt_query <- sprintf('{"version": true,
                         "size": 500,
                         "sort": [{"@timestamp": {"order": "desc",
                                                  "unmapped_type": "boolean"}}],
                         "aggs": {"2": {"date_histogram": {"field": "@timestamp",
                                                           "fixed_interval": "3h",
                                                           "time_zone": "Europe/Paris","min_doc_count": 1}}},
                         "stored_fields": ["*"],
                         "script_fields": {},
                         "docvalue_fields": [{"field": "@timestamp","format": "date_time"},
                                             {"field": "alert.createdAt","format": "date_time"},
                                             {"field": "application_usage_daily.timestamp","format": "date_time"},
                                             {"field": "canvas-element.@created","format": "date_time"},
                                             {"field": "canvas-element.@timestamp","format": "date_time"},
                                             {"field": "canvas-workpad.@created","format": "date_time"},
                                             {"field": "canvas-workpad.@timestamp","format": "date_time"},
                                             {"field": "cases-comments.created_at","format": "date_time"},
                                             {"field": "cases-comments.pushed_at","format": "date_time"},
                                             {"field": "cases-comments.updated_at","format": "date_time"},
                                             {"field": "cases-configure.created_at","format": "date_time"},
                                             {"field": "cases-configure.updated_at","format": "date_time"},
                                             {"field": "cases-user-actions.action_at","format": "date_time"},
                                             {"field": "cases.closed_at","format": "date_time"},
                                             {"field": "cases.created_at","format": "date_time"},
                                             {"field": "cases.external_service.pushed_at","format": "date_time"},
                                             {"field": "cases.updated_at","format": "date_time"},
                                             {"field": "endpoint:user-artifact-manifest.created","format": "date_time"},
                                             {"field": "endpoint:user-artifact.created","format": "date_time"},
                                             {"field": "event.end","format": "date_time"},
                                             {"field": "event.start","format": "date_time"},
                                             {"field": "fleet-agent-actions.created_at","format": "date_time"},
                                             {"field": "fleet-agent-actions.sent_at","format": "date_time"},
                                             {"field": "fleet-agent-events.timestamp","format": "date_time"},
                                             {"field": "fleet-agents.enrolled_at","format": "date_time"},
                                             {"field": "fleet-agents.last_checkin","format": "date_time"},
                                             {"field": "fleet-agents.last_updated","format": "date_time"},
                                             {"field": "fleet-agents.unenrolled_at","format": "date_time"},
                                             {"field": "fleet-agents.unenrollment_started_at","format": "date_time"},
                                             {"field": "fleet-agents.updated_at","format": "date_time"},
                                             {"field": "fleet-enrollment-api-keys.created_at","format": "date_time"},
                                             {"field": "fleet-enrollment-api-keys.expire_at","format": "date_time"},
                                             {"field": "fleet-enrollment-api-keys.updated_at","format": "date_time"},
                                             {"field": "ingest-agent-policies.updated_at","format": "date_time"},
                                             {"field": "ingest-package-policies.created_at","format": "date_time"},
                                             {"field": "ingest-package-policies.updated_at","format": "date_time"},
                                             {"field": "lens-ui-telemetry.date","format": "date_time"},
                                             {"field": "siem-detection-engine-rule-status.lastFailureAt","format": "date_time"},
                                             {"field": "siem-detection-engine-rule-status.lastLookBackDate","format": "date_time"},
                                             {"field": "siem-detection-engine-rule-status.lastSuccessAt","format": "date_time"},
                                             {"field": "siem-detection-engine-rule-status.statusDate","format": "date_time"},
                                             {"field": "siem-ui-timeline-note.created","format": "date_time"},
                                             {"field": "siem-ui-timeline-note.updated","format": "date_time"},
                                             {"field": "siem-ui-timeline-pinned-event.created","format": "date_time"},
                                             {"field": "siem-ui-timeline-pinned-event.updated","format": "date_time"},
                                             {"field": "siem-ui-timeline.created","format": "date_time"},
                                             {"field": "siem-ui-timeline.dateRange.end","format": "date_time"},
                                             {"field": "siem-ui-timeline.dateRange.start","format": "date_time"},
                                             {"field": "siem-ui-timeline.favorite.favoriteDate","format": "date_time"},
                                             {"field": "siem-ui-timeline.updated","format": "date_time"},
                                             {"field": "task.retryAt","format": "date_time"},
                                             {"field": "task.runAt","format": "date_time"},
                                             {"field": "task.scheduledAt","format": "date_time"},
                                             {"field": "task.startedAt","format": "date_time"},
                                             {"field": "telemetry.lastReported","format": "date_time"},
                                             {"field": "updated_at","format": "date_time"},
                                             {"field": "upgrade-assistant-reindex-operation.locked","format": "date_time"},
                                             {"field": "url.accessDate","format": "date_time"},
                                             {"field": "url.createDate","format": "date_time"}],
                         "_source": {"excludes": []},
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
                       t_beg, t_end)
  return(txt_query)
}

# Get the list of data indexes for queried fields, sensors and time sampling
get_indexes_from_input <- function(fields, sensors, timesampling) {
  indexes <-str_c(fields, sensors, timesampling, sep = "_")
}

# Get data from url with the following parameters:
# conn: url connection
# start_date: start date (format "yyyy/mm/dd")
# start_time: start time (format "hh:mn")
# end_date: end_date (format "yyyy/mm/dd")
# end_time: end time (format "hh:mn")
# fields: list of fields -> `c("acoustique", "meteo")`
# timesampling: time sampling -> c("100ms", "1min", "10min") 
# sensors: list of sensors -> c("a1", "a2", "a3", "a4", "a5")
# indicators: list of indicators -> c("Leq_A", "Leq_Z")
# frequencies: list of frequencies ->  c("6.3", "8", "10", "12.5", "16", "20", "25", "31.5", "40", "50", "63", "80", "100", "125", "160", "200", "250", "315", "400", "500", "630", 
#                                        "800", "1000", "1250", "1600", "2000", "2500", "3150", "4000", "5000", "6300", "8000", "10000", "12500", "16000", "20000", "A", "Z", "all")
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



query_es<- function(start_dt, end_dt){
  t_beg = format(as.POSIXct(start_dt, format = "%Y/%m/%d-%H:%M"), format = "%Y-%m-%dT%H:%M:%OSZ")
  t_end = format(as.POSIXct(end_dt, format = "%Y/%m/%d-%H:%M"), format = "%Y-%m-%dT%H:%M:%OSZ")
  txt_query <- sprintf(
    '{"params":{
      "ignoreThrottled":true,
      "index":"acoustique_a1_100ms,acoustique_a2_100ms",
      "body":{
        "version":true,
        "size":500,
        "sort":[{"@timestamp":{"order":"desc","unmapped_type":"boolean"}}],
        "aggs":{"2":{"date_histogram":{
          "field":"@timestamp",
          "calendar_interval":"1d",
          "time_zone":"Europe/Paris",
          "min_doc_count":1}}},
        "stored_fields":["*"],
        "script_fields":{},
        "docvalue_fields":[{"field":"@timestamp","format":"date_time"}],
        "_source":{"excludes":[]},
        "query":{"bool":{
          "must":[],
          "filter":[
            {"match_all":{}},
            {"range":{
              "@timestamp":{
                "gte":%s,
                "lte":%s,
                "format":"strict_date_optional_time"}}}],
          "should":[],
          "must_not":[]}},
        "highlight":{"pre_tags":["@kibana-highlighted-field@"],"post_tags":["@/kibana-highlighted-field@"],"fields":{"*":{}},"fragment_size":2147483647}
      },
      "rest_total_hits_as_int":true,
      "ignore_unavailable":true,
      "ignore_throttled":true,
      "preference":1638962038807,
      "timeout":"30000ms"}
    }', 
    t_beg, t_end)
  return(txt_query)
}


query_body <- function(){
  '{
    "version": true,
    "size": 10,
    "sort": [
      {
        "@timestamp": {
          "order": "desc",
          "unmapped_type": "boolean"
        }
      }
    ]
  }'
}

query_count <- function(){
  '{
    "version": true,
    "size": 500,
    "sort": [
      {
        "@timestamp": {
          "order": "desc",
          "unmapped_type": "boolean"
        }
      }
    ],
    "aggs": {
      "2": {
        "date_histogram": {
          "field": "@timestamp",
          "calendar_interval": "1d",
          "time_zone": "Europe/Paris",
          "min_doc_count": 1
        }
      }
    },
    "stored_fields": [
      "*"
    ],
    "script_fields": {},
    "docvalue_fields": [
      {
        "field": "@timestamp",
        "format": "date_time"
      }
    ],
    "_source": {
      "excludes": []
    },
    "query": {
      "bool": {
        "must": [],
        "filter": [
          {
            "match_all": {}
          },
          {
            "range": {
              "@timestamp": {
                "gte": "2020-01-26T23:00:00.000Z",
                "lte": "2021-01-02T23:00:00.000Z",
                "format": "strict_date_optional_time"
              }
            }
          }
        ],
        "should": [],
        "must_not": []
      }
    },
    "highlight": {
      "pre_tags": [
        "@kibana-highlighted-field@"
      ],
      "post_tags": [
        "@/kibana-highlighted-field@"
      ],
      "fields": {
        "*": {}
      },
      "fragment_size": 2147483647
    }
  }'
}

query_discover <- function(){
  '{
    "version": true,
    "size": 500,
    "sort": [
      {
        "@timestamp": {
          "order": "desc",
          "unmapped_type": "boolean"
        }
      }
    ],
    "aggs": {
      "2": {
        "date_histogram": {
          "field": "@timestamp",
          "calendar_interval": "1d",
          "time_zone": "Europe/Paris",
          "min_doc_count": 1
        }
      }
    },
    "stored_fields": [
      "*"
    ],
    "script_fields": {},
    "docvalue_fields": [
      {
        "field": "@timestamp",
        "format": "date_time"
      }
    ],
    "_source": {
      "excludes": []
    },
    "query": {
      "bool": {
        "must": [],
        "filter": [
          {
            "match_all": {}
          },
          {
            "range": {
              "@timestamp": {
                "gte": "2020-01-26T23:00:00.000Z",
                "lte": "2020-03-26T22:00:00.000Z",
                "format": "strict_date_optional_time"
              }
            }
          }
        ],
        "should": [],
        "must_not": []
      }
    },
    "highlight": {
      "pre_tags": [
        "@kibana-highlighted-field@"
      ],
      "post_tags": [
        "@/kibana-highlighted-field@"
      ],
      "fields": {
        "*": {}
      },
      "fragment_size": 2147483647
    }
  }'
}