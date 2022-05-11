suppressMessages(library(config))
suppressMessages(library(DT))
suppressMessages(library(tidyverse))
suppressMessages(library(zeallot))
suppressMessages(library(lubridate))
suppressMessages(library(hms))
suppressMessages(library(plotly))
suppressMessages(library(ggplot2))
suppressMessages(library(scales))
suppressMessages(library(elastic))
suppressMessages(library(httr))
suppressMessages(library(chron))
suppressMessages(library(qdapRegex))
suppressMessages(library(leaflet))
suppressMessages(library(proj4))
suppressMessages(library(sf))
suppressMessages(library(sfheaders))
suppressMessages(library(concaveman))
suppressMessages(library(shiny))
suppressMessages(library(shinyFiles))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shinydashboard))
suppressMessages(library(shinyTime))
suppressMessages(library(shinyjs))
suppressMessages(library(shinycssloaders))
suppressMessages(library(fontawesome))
suppressMessages(library(data.table))
suppressMessages(library(tm))


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

##..................................................
##  Text query to download data from remote server  
##..................................................
download_data_body <- function(start_datetime, end_datetime, fields, size=10000){
  # start = list(date = format(as.POSIXct(x = start_datetime, format = "%Y-%m-%dT%H:%M:%OSZ"), "%Y/%m/%d"),
  #              time = format(as.POSIXct(x = start_datetime, format = "%Y-%m-%dT%H:%M:%OSZ"), "%H:%M"))
  # end = list(date = format(as.POSIXct(x = end_datetime, format = "%Y-%m-%dT%H:%M:%OSZ"), "%Y/%m/%d"),
  #            time = format(as.POSIXct(x = end_datetime, format = "%Y-%m-%dT%H:%M:%OSZ"), "%H:%M"))
  # datetimes <- format_datetimes_for_elastic(start_date = start$date,
  #                                           start_time = start$time,
  #                                           end_date = end$date,
  #                                           end_time = end$time)
  datetimes = list(start = start_datetime , end = end_datetime)
  body = sprintf_named('{"version": true,
                         "size": %{SIZE}s,
                         "aggs": {"2": {"date_histogram": {"field": "@timestamp",
                           "calendar_interval": "1d",
                           "time_zone": "Europe/Paris",
                           "min_doc_count": 1}}},
                         "stored_fields": [%{INDICATORS}s],
                         "script_fields": {},
                         "docvalue_fields": [{"field": "@timestamp",
                                              "format": "date_time"}],
                         "_source": {"excludes": [], "includes":["@timestamp"]},
                         "query": {"bool": {"must": [],
                                            "filter": [{"match_all": {}},
                                                       {"range": {"@timestamp": {"gte": "%{START_DATE}s",
                                                                                 "lte": "%{END_DATE}s",
                                                                                 "format": "strict_date_optional_time"}}}],
                                            "should": [],
                                            "must_not": []}},
                         "highlight": {"pre_tags": ["@kibana-highlighted-field@"],
                                       "post_tags": ["@/kibana-highlighted-field@"],
                                       "fields": {"*": {}},
                                       "fragment_size": 2147483647}}', 
                       START_DATE=datetimes$start, END_DATE=datetimes$end, INDICATORS=fields, SIZE=size)
  return(body)
}

# ##....................................
# ##  Download data from remote server  
# ##....................................
download_data <- function(conn, input){
  # init_es = list(start_date = "2020/03/22", 
  #                start_time = "10:00", 
  #                end_date = "2020/03/29", 
  #                end_time = "01:00", 
  #                scientific_domain = c("acoustique"), 
  #                time_sampling = c("1min"), 
  #                acoustic_sensors = c("a1", "a2", "a3", "a4", "a5"), 
  #                acoustic_indicators = c("Leq"), 
  #                acoustic_frequencies = c("A", "Z"))
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
                      time_sampling = input$time_sampling,
                      scientific_domain = input$scientific_domain,
                      acoustic_sensors = input$acoustic_sensors,
                      acoustic_indicators = input$acoustic_indicators,
                      acoustic_frequencies = input$acoustic_frequencies, 
                      size=10000)
  print("\n \t -> data reloaded")
  return(dataset)
}

##..................................................
##  Remote server query to get data between         
##  start_dt (format = '%Y/%m/%d-%H:%M') and        
##  end_dt (format = '%Y/%m/%d-%H:%M') for selected 
##  indicators (character, ex: c('Leq_A', 'Leq_Z'))  
##..................................................
query_discover_params <- function(start_dt, end_dt, acoustic_indicators){
  t_beg = format(as.POSIXct(start_dt, format = "%Y/%m/%d-%H:%M"), 
                 format = "%Y-%m-%dT%H:%M:%OSZ")
  t_end = format(as.POSIXct(end_dt, format = "%Y/%m/%d-%H:%M"), 
                 format = "%Y-%m-%dT%H:%M:%OSZ")
  acoustic_indicators.list = c("@timestamp", acoustic_indicators)
  acoustic_indicators.str = paste0('"', paste(acoustic_indicators.list, collapse = '","'), '"')
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
                       acoustic_indicators.str, acoustic_indicators.str, t_beg, t_end)
  return(txt_query)
}
##.........................................................
##  Sprintf function with input strings formatted by name  
##.........................................................
sprintf_named <- function(fmt, ...) {
  args <- list(...)
  argn <- names(args)
  if(is.null(argn)) return(sprintf(fmt, ...))
  
  for(i in seq_along(args)) {
    if(argn[i] == "") next;
    fmt <- gsub(sprintf("%%{%s}", argn[i]), sprintf("%%%d$", i), fmt, fixed = TRUE)
  }
  
  do.call(sprintf, append(args, fmt, 0))
}
##..............................................................
##  Format start and end datetimes for Elastic Search discovery  
##..............................................................
format_datetimes_for_elastic <- function(start_date, start_time, end_date, end_time) {
  start_date_time = as.POSIXct(paste(start_date, start_time, sep = "-"), format = "%Y/%m/%d-%H:%M")
  end_date_time = as.POSIXct(paste(end_date, end_time, sep = "-"), format = "%Y/%m/%d-%H:%M")
  start_date_time = format(start_date_time, format = "%Y-%m-%dT%H:%M:%OSZ")
  end_date_time = format(end_date_time, format = "%Y-%m-%dT%H:%M:%OSZ")
  datetimes <- list(start = start_date_time, 
                    end = end_date_time)
  return(datetimes)
}##....................................
##  Format start column for csv files  
##.....................................
format_datetimes_for_csv <- function(start_date_times, out_format = "%Y-%m-%d_%H:%M:%OS") {
  in_format = "%Y-%m-%dT%H:%M:%OSZ"
  start_date_times = unlist(start_date_times)
  start_date_times_utc = as.POSIXct(start_date_times, format = in_format, tz = "UTC")
  start_date_times_csv = as.list(strftime(start_date_times_utc, out_format))
  return(start_date_times_csv)
}
##..............................................................
##  Format datetime from date and time for time slider
##..............................................................
format_datetime_from_time_slider <- function(the_date, the_time) {
  the_date_time = as.POSIXct(paste(the_date, format(the_time, format = "%H:%M"), sep = " "))
  return(the_date_time)
}
##..............................................................
##  Get the timesampling unit (min or ms) and (numeric) value
##..............................................................
get_timesampling_unit_and_value <- function(time_sampling){
  is_num <- function(x) grepl("^[-]?[0-9]+[.]?[0-9]*|^[-]?[0-9]+[L]?|^[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",x)
  as_num <- function(x) {
    if (is.null(x)||length(x) == 0) return(x)
    if (class(x)=="list") return(lapply(x, as_num))
    if (is.character(x) & is_num(x)) return(as.numeric(x))
    return(x)
  }
  the_names = c("num", "unit")
  the_values = strsplit(time_sampling, "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE)[[1]]
  timesampling_unit_and_value = mapply(function(the_names, the_values) { the_values }, the_names, the_values, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  return(as_num(timesampling_unit_and_value))
}
##..............................................................................................
##  Get the time increment to subtract to the last downloaded data to ensure data completeness  
##..............................................................................................
get_back_time_overlap <- function(time_sampling){
  timesampling = get_timesampling_unit_and_value(time_sampling)
  back_time = case_when(timesampling$unit == "min" ~ lubridate::minutes(timesampling$num), 
                        timesampling$unit == "ms" ~ lubridate::milliseconds(timesampling$num))
  return(back_time)
}
##.......................................................................
##  Get the data index list for fields, sensors and time sampling input  
##.......................................................................
format_indexes_for_elastic <- function(scientific_domain, acoustic_sensors, time_sampling) {
  indexes <-str_c(scientific_domain, acoustic_sensors, time_sampling, sep = "_")
}
##..............................................
##  Format fields for Elastic Search discovery  
##..............................................
format_fields_for_elastic <- function(acoustic_indicators, acoustic_frequencies) {
  # indicators = paste(acoustic_indicators, acoustic_frequencies, sep = "_")
  indicators = as.vector(outer(acoustic_indicators, acoustic_frequencies, paste, sep="_"))
  indicators.list = c("@timestamp", indicators)
  fields = paste0('"', paste(indicators.list, collapse = '","'), '"')  
  return(fields)
}
##..........................
##  Unlist a list of lists  
##..........................
df_unlist<-function(df){
  df <- as.data.frame(df)
  nr <- nrow(df)
  c.names <- colnames(df)
  lscols <- as.vector(which(apply(df, 2, is.list) == TRUE))
  if(length(lscols) != 0){
    for(i in lscols){
      temp <- as.vector(unlist(df[, i]))
      if(length(temp) != nr){
        adj <- nr-length(temp)
        temp <- c(rep(0, adj), temp)
      }
      df[, i] <- temp
    }
    df <- as.data.frame(df)
    colnames(df) <- c.names
  }
  return(df)
}

##................................................................................................................
##  Get data from url with the following parameters:                                                              
##  conn: url connection                                                                                          
##  start_date: start date (format "yyyy/mm/dd")                                                                  
##  start_time: start time (format "hh:mn")                                                                       
##  end_date: end_date (format "yyyy/mm/dd")                                                                      
##  end_time: end time (format "hh:mn")                                                                           
##  fields: list of fields -> `c("acoustique", "meteo")`                                                          
##  time_sampling: time sampling -> c("100ms", "1min", "10min")                                                    
##  acoustic_sensors: list of acoustic sensors -> c("a1", "a2", "a3", "a4", "a5")                                                   
##  acoustic_indicators: list of acoustic indicators -> c("Leq_A", "Leq_Z")                                                         
##  acoustic_frequencies: list of frequencies ->  c("6.3", "8", "10", "12.5", "16", "20", "25", "31.5", "40", "50", "63",  
##                                                  "80", "100", "125", "160", "200", "250", "315", "400", "500", "630",   
##                                                  "800", "1000", "1250", "1600", "2000", "2500", "3150", "4000", "5000", 
##                                                  "6300", "8000", "10000", "12500", "16000", "20000", "A", "Z", "all")
##  size: size of the data sample (integer, default:10000)
##................................................................................................................
get_data <- function(conn, start_date, start_time, end_date, end_time, time_sampling, scientific_domain, acoustic_sensors, acoustic_indicators, acoustic_frequencies, size=10000) {
  if(!is.character(start_date)){start_date = format(start_date, format = "%Y/%m/%d")}
  print("start_date")
  print(start_date, digits = NULL, quote = FALSE)
  if(!is.character(start_time)){
    start_time = format(update(as.POSIXct(x = start_time, format = "%H:%M"), year = year(start_date), month = month(start_date), mday = day(start_date)), "%H:%M")
  }
  print("start_time")
  print(start_time, digits = NULL, quote = FALSE)
  if(!is.character(end_date)){end_date = format(end_date, format = "%Y/%m/%d")}
  print("end_date")
  print(end_date, digits = NULL, quote = FALSE)
  if(!is.character(end_time)){
    end_time = format(update(as.POSIXct(x = end_time, format = "%H:%M"), year = year(end_date), month = month(end_date), mday = day(end_date)), "%H:%M")
  }
  print("end_time")
  print(end_time, digits = NULL, quote = FALSE)
  indexes = format_indexes_for_elastic(scientific_domain = scientific_domain, acoustic_sensors = acoustic_sensors, time_sampling = time_sampling)
  fields <- format_fields_for_elastic(acoustic_indicators = acoustic_indicators, acoustic_frequencies = acoustic_frequencies)
  datetimes.query <- format_datetimes_for_elastic(start_date, start_time, end_date, end_time)
  datetimes.loop = datetimes.query
  downloading_time = milliseconds(0)
  # Time increment to subtract to the last datetime in the data frame
  back_time = get_back_time_overlap(time_sampling)
  print(indexes, digits = NULL, quote = FALSE)
  print(fields, digits = NULL, quote = FALSE)
  print("Downloading data", digits = NULL, quote = FALSE)
  while (datetimes.loop$start < datetimes.query$end) {
    print(datetimes.loop$start, digits = NULL, quote = FALSE)
    body = download_data_body(start_datetime = datetimes.loop$start,
                              end_datetime = datetimes.loop$end,
                              fields = fields)
    data_sample <- Search(conn = conn,
                          index = indexes,  # "acoustique_*_1min,meteo_a2_sono",
                          body = body,
                          asdf = TRUE,
                          sort = '@timestamp:asc',
                          raw = FALSE,
                          track_total_hits = TRUE,
                          time_zone = 'UTC')
    downloading_time <- downloading_time + milliseconds(data_sample$took)
    # Dataframe of data sample
    data_sample_tmp_df <- as.data.frame(do.call(cbind, data_sample$hits$hits)) %>% select('_index', colnames(data_sample$hits$hits %>% select(contains('fields.')))) %>% 
      setNames(sub("fields.", "", colnames(.))) %>%                                                       # Remove fields before field names
      separate(col = `_index`, into = c('scientific_domain', 'sensor', 'time_sampling'), sep = '_') %>%   # Separate  column `_index` 3 columns ('scientific_domain', 'sensor', 'time_sampling')
      rename(start_time = `@timestamp`) %>%                                                               # Rename column name '@timestamp' as 'start_time'
      relocate(start_time)                                                                                # Relocate datetime column at first column
    # # Dataframe of aggregated data sample
    # aggreg_data_tmp_df <- as.data.frame(do.call(cbind, data_sample$aggregation$`2`$buckets)) %>% select(c('key_as_string', 'doc_count'))
    if(datetimes.loop$start == datetimes.query$start){  # First sample of data
      data_sample_df <- data_sample_tmp_df
      # aggreg_data_df <- aggreg_data_tmp_df
    } else {
      data_sample_df <- rbind(data_sample_df, data_sample_tmp_df)
      # aggreg_data_df <- rbind(aggreg_data_df, aggreg_data_tmp_df)
    }
    # Starting datetime for the download of the next stack of data
    data_sample_last_datetime = data_sample_df[nrow(data_sample_df),]$start_time[[1]]
    # datetimes.loop$start = as.POSIXct(, format = "%Y-%m-%dT%H:%M:%OSZ")
    if (all(data_sample_last_datetime < datetimes.query$end && nrow(data_sample$hits$hits) == size)){ # If TRUE, this is not the required end datetime yet and the last data sample was full 
      datetimes.loop$start = strftime(as.POSIXct(data_sample_last_datetime, format = "%Y-%m-%dT%H:%M:%OSZ") - back_time, format = "%Y-%m-%dT%H:%M:%OSZ")  # Subtract time equivalent to data time sampling to avoid any gap in downloading data (i.e. allow data overlapping)
    } else {
      datetimes.loop$start = data_sample_last_datetime
    }
  }
  # Remove duplicated data in data frame
  data_df = data_sample_df[-which(duplicated(data_sample_df)), ]
  # Format time list
  data_df$start_time = format_datetimes_for_csv(start_date_times = data_df$start_time)
  # Output
  output = list(data = df_unlist(data_df), 
                downloading_time = downloading_time)
  return(output)
}

##................................................................................................................
##  Format data from csv sheet to shiny app dataframe:                                                            
##  dataset: data issued from the csv file                                                                        
##................................................................................................................
format_data_for_app <- function(dataset){
  indicator_names = colnames(dataset[5:ncol(dataset)])
  df_indicators = dataset %>% select(one_of(indicator_names))
  data_app <- data.frame(timestamp = dataset$start_time, 
                         sensor = dataset$sensor, 
                         indicators = rep(names(df_indicators), each = nrow(df_indicators)), 
                         value = unlist(df_indicators), 
                         row.names = NULL)
  return(data_app)
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
  cat(file = stderr(), paste0("\n\t| Get information about sensors from ",  sensor_filename, "\n"))
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
  the_start_date = as.Date(x = input$start_date_download, format = "%Y-%m-%d")
  the_start_time = update(as.POSIXct(x = input$start_time_download , format = "%Y-%m-%d %H:%M:%S CEST"), 
                          year = year(the_start_date), month = month(the_start_date), mday = day(the_start_date))
  the_end_date = as.Date(x = input$end_date_download, format = "%Y-%m-%d")
  the_end_time = update(as.POSIXct(x = input$end_time_download , format = "%Y-%m-%d %H:%M:%S CEST"), 
                          year = year(the_end_date), month = month(the_end_date), mday = day(the_end_date))
  html_page <- sprintf('<p><img style="width:201px;height:45px;display: block; margin-left: auto; margin-right: auto;" 
                        src="https://www.anr-pibe.com/sites/pibe/files/anr-pibe-logo.gif">
                        </p><h3 style="text-align: center;">Downloading data ...</p>
                        <p><img style="width:201px;display: block; margin-left: auto; margin-right: auto;" 
                        src="http://northerntechmap.com/assets/img/loading-dog-final.gif">
                        </p><h5 style="text-align: left;"><ul>
                        <li>dates: %s to %s</li>
                        <li>acoustic indicators: %s</li>
                        <li>acoustic_sensors: %s</li>
                        <li>frequencies: %s</li>
                        </ul></h5><hr /> </p>', 
                       the_start_time, the_end_time, 
                       paste(input$acoustic_indicators, collapse = ", "), 
                       paste(input$acoustic_sensors, collapse = ", "), 
                       paste(input$acoustic_frequencies, collapse = ", "))
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
my_options <- options(digits.secs = 3, quote = TRUE, 
                      shiny.maxRequestSize = 10 * 1024^2)

##=================================
##  Remote server configuration   =
##=================================

##............................
##  ElasticSearch parameters  
##............................
es_params <- config::get(file = paste(getwd(), "www", "server.conf", sep = .Platform$file.sep))

##............................
##  Remote server connection  
##............................
cat(file = stderr(), "\n| Establishing connexion\n")
conn = connect(host = es_params$host, port = es_params$port, user = es_params$user, pwd = es_params$pwd, path=es_params$path, transport_schema = es_params$transport_schema, cainfo = es_params$cainfo)
server_health <- cluster_health(conn = conn)
cat(file = stderr(), "  -> Connexion ready !\n")

##===================================
##  User interface initialization   =
##===================================

##........................................
##  Data query parameters initialization  
##........................................
init_es = list(start_date = "2020/03/22", 
               start_time = "10:00", 
               end_date = "2020/03/25", 
               end_time = "01:00", 
               scientific_domain = c("acoustique"), 
               time_sampling = c("1min"), 
               acoustic_sensors = c("a1", "a2", "a3", "a4", "a5"), 
               acoustic_indicators = c("Leq"), 
               acoustic_frequencies = c("A", "Z"))

##........................................
##  Displayed data sample initialization  
##........................................
# count(conn = conn, index = get_indexes_from_input(es.init.fields, init_es$acoustic_sensors, init_es$time_sampling))
sample_data_file = paste(getwd(), "www", "data", "sample.csv", sep = .Platform$file.sep)
if (file.exists(sample_data_file)){
  cat(file = stderr(), paste0("\n| Dowloading initial dataset from local folder ",  sample_data_file, "\n"))
  datasample.init = list(downloading_time = 0, 
                         data = read_csv(file = sample_data_file, col_names = TRUE))
} else {
  cat(file = stderr(), paste0("\n| Downloading initial dataset from remote server\n"))
  datasample.init <- get_data(conn = conn, 
                              start_date = init_es$start_date, 
                              start_time = init_es$start_time, 
                              end_date = init_es$end_date, 
                              end_time = init_es$end_time, 
                              time_sampling = init_es$time_sampling, 
                              scientific_domain = init_es$scientific_domain, 
                              acoustic_sensors = init_es$acoustic_sensors, 
                              acoustic_indicators = init_es$acoustic_indicators, 
                              acoustic_frequencies = init_es$acoustic_frequencies)
}
dataset <- datasample.init$data
data_app = format_data_for_app(dataset)
downloading_time <- datasample.init$downloading_time
cat(file = stderr(), paste0("  -> Initial dataset ready !\n"))

##.................................................
##  Selected data query parameters initialization  
##.................................................
csv_in_filename = "sample.csv"
csv_in_filepath = paste(getwd(), "www", "data", csv_in_filename, sep = .Platform$file.sep)
csv_in_raw_data = read_csv(file = csv_in_filepath, col_names = TRUE)
csv_in_plot = format_data_for_app(csv_in_raw_data)

init = list(selection = list(downloadFolder = paste(getwd(), "www", "data", sep = .Platform$file.sep),
                             start_date = "2020/03/22",
                             start_time = "10:00",
                             start_date_time =  as.POSIXct("2020/03/22 10:00"),
                             end_date = "2020/03/25",
                             end_time = "01:00",
                             end_date_time =  as.POSIXct("2020/03/25 01:00"),
                             scientific_domain = c("acoustique"),
                             time_sampling = "1min",
                             acoustic_sensors = c("a1", "a2", "a3", "a4", "a5"),
                             acoustic_indicators = c("Leq"),
                             acoustic_frequencies = c("A", "Z"),
                             csv_in_filename = csv_in_filename,
                             csv_in_filepath = csv_in_filepath,
                             csv_in_raw = csv_in_raw_data,
                             csv_in_plot = csv_in_plot),
            all_choices = list(scientific_domain = c("acoustique"),
                               time_samplings = c("100ms", "1min", "10min"),
                               acoustic_sensors = c("a1", "a2", "a3", "a4", "a5"),
                               acoustic_indicators = c("Leq", "L10", "L50", "L90"),
                               acoustic_frequencies = c("6.3", "8", "10", "12.5", "16", "20", "25", "31.5", "40", "50", "63", 
                                                        "80", "100", "125", "160", "200", "250", "315", "400", "500", "630", 
                                                        "800", "1000", "1250", "1600", "2000", "2500", "3150", "4000", "5000", 
                                                        "6300", "8000", "10000", "12500", "16000", "20000", "A", "Z", "all"),
                               facet_row = c(None='.', str_remove(string = names(data_app), pattern = c("timestamp", "Value"))),
                               scientific_domain.names = c('acoustic', 'meteo', 'both'),
                               scientific_domain.values = c('acoustique', 'meteo', 'both')))

##...................................
##  Sensors location and map centre  
##...................................
sensor_filename = paste(getwd(), "www", "csv", "sensors.csv", sep = .Platform$file.sep)
cat(file = stderr(), paste0("\n| Initialization of the map of sensors from file ",  sensor_filename, "\n"))
tiles <- c('Default', 'Stamen.Toner', 'CartoDB.Positron', 'GeoportailFrance', 'Esri.NatGeoWorldMap')  # List of tiles for sensor map
sensors <- get_sensor_info(sensor_filename)
points_hull <- sensors %>% st_combine() %>% st_cast("POINT") %>% st_sf()
concave_hull <- points_hull %>% concaveman()
centre_point <- st_centroid(concave_hull) %>% st_coordinates()
cat(file = stderr(), paste0("  -> Map of sensors ready !\n"))
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
sensor_popup <- function(mast, name) {
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
sensors <- sensors %>% mutate(color_selected = case_when(name %in% init$selection$acoustic_sensors ~ "green",
                                                         name %notin% init$selection$acoustic_sensors ~ "red"))
sensors <- sensors %>% mutate(color_icon = case_when(color_selected == "green" ~ 'green_pin',
                                                     color_selected != "green" ~ "black_pin"))
map_sensors_tab <- leaflet(data = sensors) %>% 
  addTiles() %>%
  setView(lng = centre_point[1], lat = centre_point[2], zoom = 14) %>%
  addMarkers(data = sensors, lng = ~lon, lat = ~lat, icon = ~my_icons[mast], group = ~mast,    # ~my_icons2[color_icon]
             popup = ~sapply(mast, sensor_popup))
map_visu_tab <- leaflet(data = sensors) %>% 
  addTiles() %>%
  setView(lng = centre_point[1], lat = centre_point[2], zoom = 14) %>%
  addMarkers(data = sensors, lng = ~lon, lat = ~lat, icon = ~my_icons[mast], group = ~mast,    # ~my_icons2[color_icon]
             popup = ~sapply(mast, sensor_popup))
