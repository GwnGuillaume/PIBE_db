#' global.R
#' @description Functions and Methods used in the Shinydashboard scripts
#' @details Current date: `r format(Sys.Date(), "%B %e, %Y")`
#' 
#' @name Global Functions
#' 
#' @section libraries: Libraries
#' 
# Clear workspace
rm(list = ls())
# Load libraries
suppressMessages(library(docstring))
suppressMessages(library(config))
suppressMessages(library(DT))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))
suppressMessages(library(zeallot))
suppressMessages(library(lubridate))
suppressMessages(library(hms))
suppressMessages(library(scales))
suppressMessages(library(elastic))
suppressMessages(library(httr))
suppressMessages(library(chron))
suppressMessages(library(qdapRegex))
suppressMessages(library(proj4))
suppressMessages(library(sf))
suppressMessages(library(hover))
suppressMessages(library(data.table))
suppressMessages(library(tm))
suppressMessages(library(readxl))
suppressMessages(library(glue))
suppressMessages(library(stringi))
suppressMessages(library(leaflet))
suppressMessages(library(jsonlite))
suppressMessages(library(htmltools))
suppressMessages(library(fontawesome))
#' 
#' @section functions_server: Remote server connection utilities
#' 
#' This documentation provides details about the remote server connection utilities.
#' 
#' @title Time Elapsed Since Session Started
#' @description Calculates the time elapsed in seconds since the specified session start time.
#' @param start_datetime The timestamp representing the start of the session.
#' @return The time elapsed in seconds since the session started.
#' @details This function takes the start timestamp of a session and calculates the
#'   time elapsed in seconds up to the current moment.
#' @examples
#' start_time <- as.POSIXct("2023-01-01 12:00:00", tz = "UTC")
#' sessions_secs_timer(start_time)
#' @seealso \code{\link{as.POSIXct}}, \code{\link{difftime}}, \code{\link{round}}, \code{\link{str_remove}}
#' @export
sessions_secs_timer <- function(start_datetime){
  current_datetime <- as.POSIXct(format(Sys.time()), tz = "UTC")
  time_elapsed <- difftime(time1 = current_datetime, time2 = start_datetime, units = "secs")
  time_elapsed_secs <- round(x = as.double(str_remove(string = time_elapsed, 
                                                      pattern = "Time elapsed since session starts: ")), 
                             digits = 0)
  return(time_elapsed_secs)
}
#' 
#' @title Get ElasticSearch Health
#' @description Retrieves the health status information from an ElasticSearch connection.
#' @param conn The ElasticSearch connection object.
#' @return A data frame containing various health indicators from ElasticSearch.
#' @details This function sends a request to ElasticSearch to retrieve health status
#'   information and parses the response into a structured data frame.
#' @examples
#' conn <- elastic::connect()
#' get_elastic_health(conn)
#' @seealso \code{\link{elastic::connect}}, \code{\link{elastic::cat_health}}
#' @export
get_elastic_health <- function(conn){
  elastic_health <- elastic::cat_health(conn = conn, parse = TRUE)
  colnames(elastic_health) <- c("epoch", "timestamp", "cluster", "status", "node.total", 
                                "node.data", "shards", "pri", "relo", "init", "unassign", 
                                "pending_tasks", "max_task_wait_time", "active_shards_percent")
  return(elastic_health)
}
#' 
#' @title Ping Database
#' @description Sends a ping request to check the status of the connection to a remote server.
#' @param conn The connection object to the remote server.
#' @return A list containing the ping result and, if successful, a message with connection details.
#' @details This function sends a ping request to the specified remote server and measures
#'   the time taken for the server to respond. If the ping is successful, it returns a list
#'   with the ping result and a message indicating the successful connection.
#' @examples
#' conn <- elastic::connect()
#' ping_database(conn)
#' @seealso \code{\link{elastic::connect}}, \code{\link{milliseconds}}
#' @export
ping_database <- function(conn){
  t_start <- Sys.time()
  ping_db <- tryCatch(expr = elastic::ping(conn = conn), 
                      error = function(e) e)
  time_elapsed <- round(as.numeric(milliseconds(Sys.time() - t_start)), digits = 3)
  if(!"message" %in% names(ping_db)){
    ping_db$message <- paste("Successful connection to localhost port", conn$port, 
                             "after", time_elapsed, "ms: Connexion autorisée", sep = " ")
  }
  return(ping_db)
}
#' Get Elasticsearch Index Information
#'
#' This function extracts index information from the output of elastic::cat_indices().
#'
#' @param conn An Elasticsearch connection object.
#' @return A data frame containing information about each index.
#' @examples
#' \dontrun{
#' conn <- connect("http://your-elasticsearch-host:9200")
#' index_info <- get_indexes_info(conn)
#' }
#' @export
get_indexes_info <- function(conn) {
  # Get raw output from cat_indices
  indexes_info <- tryCatch(elastic::cat_indices(conn = conn, parse = TRUE), error = function(e) NULL)
  if (is.null(indexes_info)) {
    warning("Failed to retrieve index information.")
    return(NULL)
  }
  # Rename columns
  colnames(indexes_info) <- c("status", "health", "index_name", "uuid", "primaries", "replicas", "docs_count", "docs_deleted", "store_size", "pri_store_size")
  # Subdivide column index_name
  split_index <- do.call(rbind, strsplit(as.character(indexes_info$index_name), "-"))
  # Add columns
  indexes_info <- indexes_info %>% 
    mutate(field = split_index[, 1],
           sensor = split_index[, 2],
           timesampling = split_index[, 3])
  return(indexes_info)
}
#' 
#' @title Get a list of sensors for a specific field, optionally avoiding certain sensors.
#' @description This function extracts a list of sensors for a given field from Elasticsearch index information.
#' Optionally, you can provide a list of sensors to avoid.
#' @param indexes_info A data frame containing Elasticsearch index information.
#' @param wanted_field The desired field for which sensors are to be retrieved.
#' @param avoided_sensors A character vector of sensors to avoid (default is NULL).
#' @return A character vector containing the list of sensors for the specified field.
#' @examples
#' get_sensors_for_field(indexes_info, wanted_field = 'acoustic', avoided_sensors = 'extraneous_noise')
#' get_sensors_for_field(indexes_info, wanted_field = 'meteo', avoided_sensors = c('extraneous_noise', 'a1', 'a2', 'a3'))
#' @export
get_sensors_for_field <- function(indexes_info, wanted_field, avoided_sensors = NULL){
  sensors_for_field <- indexes_info %>%
    filter(field == wanted_field & !(sensor %in% avoided_sensors)) %>%
    select(sensor) %>%
    distinct() %>%
    arrange(sensor) %>%
    pull()
  return(sensors_for_field)
}
#' 
#' @title Get Indexes Names for a Specific Field
#' @description This function retrieves the names of indexes for a specified field, excluding the "extraneous_noise" sensor.
#' @param indexes_info A data frame containing information about indexes.
#' @param field The field for which index names are to be retrieved.
#' @return A character vector containing the names of indexes for the specified field.
#' @examples
#' \dontrun{
#' # Assuming indexes_info is a data frame with appropriate structure
#' indexes_names <- get_indexes_names(indexes_info, "acoustic")
#' }
#' @seealso \code{\link{elastic::cat_indices}} for information about the indexes_info data frame structure.
#' @export
get_indexes_names <- function(indexes_info, field){
  indexes_names <- indexes_info[which(indexes_info$field == field & indexes_info$sensor != 'extraneous_noise'),]$index_name
  return(indexes_names)
}
#' 
#' Order Acoustic Indicators
#' @description This function is designed for sorting strings based on the presence of digits.
#' Strings containing digits are considered greater and placed at the end, while
#' strings without digits are considered smaller and placed at the beginning.
#' @param x A character vector of strings to be sorted.
#' @return A numeric vector indicating the order of elements in the sorted array.
#' @seealso order
#' @examples
#' strings <- c("L10", "L50", "L90", "Leq")
#' sorted_strings <- strings[order(sapply(strings, indicators_custom_order), strings)]
#' print(sorted_strings)
#' @export
order_indicators <- function(x) {
  indicators_custom_order <- function(x) {
    if (any(grepl("\\d", x))) {
      return(Inf)
    } else {
      return(-Inf)
    }
  }
  ordered_indicators <- order(sapply(x, indicators_custom_order), x)
  # Return the factor with custom order
  return(factor(x[ordered_indicators], levels = x[ordered_indicators]))
}
#' 
#' Order Frequencies Numerically
#' This function extracts frequencies from a vector of strings containing
#' patterns, such as '100_Hz', and orders them numerically.
#' @title Order Frequencies Numerically
#' @description This function takes a vector of strings containing frequency
#' patterns, such as '100_Hz', and orders them numerically.
#' @param x A vector of strings containing frequency patterns.
#' @return A factor with frequencies ordered numerically.
#' @examples
#' x <- c("10000_Hz", "1000_Hz", "6.3_Hz", "Z", "A")
#' ordered_x <- order_frequencies(x)
#' print(ordered_x)
#' @export
order_frequencies <- function(x) {
  # Extract frequencies from strings
  frequencies <- sapply(strsplit(x, "_"), `[`, 1)
  # Order frequencies numerically
  suppressWarnings({
    ordered_frequencies <- order(as.numeric(frequencies), na.last = FALSE)
  })
  # Return the factor with custom order
  return(factor(x[ordered_frequencies], levels = x[ordered_frequencies]))
}
#' 
#' Order Meteo Variables
order_meteo_variables <- function(x) {
  meteo_variables_custom_order <- function(x) {
    # Extract numeric suffix using regular expression
    suffix <- suppressWarnings(as.numeric(gsub(".*_(\\d+)m", "\\1", x)))
    if (length(suffix) > 0 && !any(is.na(suffix))) {
      # If the string contains numbers, use the numeric suffix for ordering
      order_key <- suffix
    } else {
      # If the string doesn't contain numbers, use a large number to place it at the end
      order_key <- -Inf
    }
    return(order_key)
  }
  ordered_meteo_variables <- order(sapply(x$variables, meteo_variables_custom_order), x$variables)
  # Return the factor with custom order
  return(factor(x$variables[ordered_meteo_variables], levels = x$variables[ordered_meteo_variables]))
}
#'
#' This function is designed for sorting strings based on the presence of digits.
#' Strings containing digits are considered greater and placed at the end, while
#' strings without digits are considered smaller and placed at the beginning.
#' @title Custom sorting function for strings
#' @description This function returns a character vector of strings sorted based on
#' the presence of digits.
#' @param x A character vector of strings to be sorted.
#' @return A character vector of strings sorted based on the presence of digits.
#' @examples
#' strings <- c("L10", "L50", "L90", "Leq")
#' sorted_strings <- custom_order_sort(strings)
#' print(sorted_strings)
#' @export
custom_order_sort <- function(x) {
  if(any(str_detect(string = x, pattern = "Hz"))){  # Frequencies character vector
    ordered_vector <- order_frequencies(x)
  } else {  # Indicators character vector
    ordered_vector <- order_indicators(x)  # order(sapply(x, indicators_custom_order), x)
  }
  return(ordered_vector)
}
#' 
#' Flatten nested mapping structure
#' This function flattens a nested mapping structure by converting it into a
#' flat list. The nested fields are represented by concatenating the field
#' names with a dot (.) separator.
#' @title Flatten nested mapping structure
#' @description This function takes a nested mapping structure and returns a
#' flat list where the field names are concatenated with a dot (.) separator
#' for nested fields.
#' @param mapping A nested mapping structure.
#' @return A flat list representing the mapping structure with nested fields
#' concatenated using a dot (.) separator.
#' @examples
#' nested_mapping <- list(
#'   field1 = "value1",
#'   field2 = list(
#'     nested_field1 = "nested_value1",
#'     nested_field2 = "nested_value2"
#'   ),
#'   field3 = "value3"
#' )
#' flat_mapping <- flatten_mapping(nested_mapping)
#' print(flat_mapping)
#' @export
flatten_mapping <- function(mapping) {
  flat_mapping <- list()
  for (field in names(mapping)) {
    if (is.list(mapping[[field]]) && "properties" %in% names(mapping[[field]])) {
      # Recursive call for nested fields
      nested_mapping <- flatten_mapping(mapping[[field]]$properties)
      # Add nested mapping to the flat_mapping
      flat_mapping <- c(flat_mapping, setNames(nested_mapping, paste0(field, ".", names(nested_mapping))))
    } else {
      # Add non-nested field to the flat_mapping
      flat_mapping[[field]] <- mapping[[field]]
    }
  }
  return(flat_mapping)
}
#' 
#' Get mappings of selected indexes
#' @title Get Indexes Mappings
#' @description This function retrieves the mappings of selected Elasticsearch indexes.
#' @param conn An Elasticsearch connection object.
#' @param indexes A character vector containing the names of the Elasticsearch indexes.
#' @return A list containing the flattened mappings for each index.
#' @examples
#' \dontrun{
#' indexes_mappings <- get_indexes_mappings(conn = elastic_conn, indexes = c("index1", "index2"))
#' }
#' @export
get_indexes_mappings <- function(conn, indexes) {
  mappings_list <- list()
  for (index in indexes) {
    # Retrieve mapping using elastic::mapping_get
    index_mapping <- elastic::mapping_get(conn = conn, index = index)
    # Flatten the mapping structure
    flat_mapping <- flatten_mapping(index_mapping[[index]]$mappings$properties)
    # Keep only fields variables
    vars_names <- names(flat_mapping)
    # Remove "@timestamp" from list of variables
    vars_names <- vars_names[vars_names != "@timestamp"]
    if (str_starts(string = index, pattern = "acoustic")){
      indicators_and_frequencies <- as.data.frame(t(sapply(vars_names, function(var_name) {str_split(var_name, "_", n = 2)[[1]]}))) %>% 
        rename_with(~c("indicators", "frequencies"), c("V1", "V2"))
      # Add the mapping to the list
      mappings_list[[index]] <- list(indicators=custom_order_sort(unique(indicators_and_frequencies$indicators)),
                                     frequencies=custom_order_sort(unique(indicators_and_frequencies$frequencies)))
    } else if (str_starts(string = index, pattern = "meteo")){
      # Check if each element in vars_names starts with any element in acoustic_sensors
      meteo_at_sonos <- all(sapply(tolower(vars_names), function(var_name) {any(startsWith(var_name, tolower(acoustic_sensors)))}))
      meteo_at_lidar <- all(sapply(vars_names, function(var_name) {any(startsWith(var_name, "Lidar"))}))
      if (isTRUE(meteo_at_sonos) | isTRUE(meteo_at_lidar)){
        variables <- str_split_fixed(string = vars_names, pattern = "_", n = 2)[,2]
      } else {
        variables <- vars_names
      }
      # Add the mapping to the list
      mappings_list[[index]] <- list(variables=variables)
    }
  }
  return(mappings_list)
}
#'
#' Get Common Fields/Variables
#' This function takes a list of mappings and finds the common fields/variables
#' @title Get Common Fields/Variables
#' @description This function takes a list of mappings and finds the common fields/variables.
#' @param mappings_list A list of mappings.
#' @return A vector containing the common fields/variables.
#' @examples
#' mappings_list <- list(
#'   c("field_A", "field_B", "field_C"),
#'   c("field_B", "field_C", "field_D"),
#'   c("field_A", "field_B", "field_D")
#' )
#' common_fields <- get_common_fields_variables(mappings_list)
#' print(common_fields)
#' @export
get_common_fields_variables <- function(mappings_list) {
  # Find common columns
  common_fields <- Reduce(intersect, mappings_list)
  return(common_fields)
}
#' 
#' Get Meteo Variables
#' This function extracts meteo sensors and related variables from a list of meteo mappings.
#' @param mappings_list A list of meteo mappings.
#' @return A list containing meteo sensors and their related variables.
#' @examples
#' mappings_list <- list("meteo-a4-10min" = c("Temp", "Hum"), "meteo-s3-1min" = c("Temp", "Wind_Speed"))
#' meteo_variables <- get_meteo_variables(mappings_list)
#' @export
get_meteo_variables <- function(mappings_list){
  meteo_sensors <- list('sonic' = NULL, 'sound level meter' = NULL, 'lidar' = NULL)
  for (sensor_id in seq_along(mappings_list)) {
    #' Meteo sensor
    meteo_sensor <- unique(gsub("meteo-([a-zA-Z0-9]+)-.*", "\\1", names(mappings_list)[sensor_id]))
    #' Meteo sensor name
    if (meteo_sensor %in% c('s1', 's2', 's3')){
      meteo_sensor_name <- 'sonic'
    } else if (meteo_sensor %in% c('a1', 'a2', 'a3', 'a4', 'a5')){
      meteo_sensor_name <- 'sound level meter'
    } else {
      meteo_sensor_name <- meteo_sensor
    }
    #' Meteo variables
    meteo_variables <- order_meteo_variables(mappings_list[[sensor_id]])
    #' Data frame of meteo sensors and related variables
    if (is.null(meteo_sensors[[meteo_sensor_name]])){
      meteo_sensors[[meteo_sensor_name]] <- meteo_variables
    } else {
      meteo_variables_set <- list('previous' = meteo_sensors[[meteo_sensor_name]], 'new' = meteo_variables)
      common_meteo_variables <- get_common_fields_variables(meteo_variables_set)
      meteo_sensors[[meteo_sensor_name]] <- common_meteo_variables
    }
  }
  return(meteo_sensors)
}
#' 
#' Get Sensors Names Starting With
#' This function filters a list of sensor names to include only those starting with a specified pattern.
#' @param sensors A character vector containing sensor names.
#' @param names_starting_with A character string specifying the pattern that sensor names should start with.
#' @return A character vector of sensor names starting with the specified pattern.
#' @examples
#' sensors <- c("a1", "a2", "a3", "b1", "b2", "b3")
#' selected_sensors <- get_sensors_names_starting_with(sensors, "a")
#' @export
get_sensors_names_starting_with <- function(sensors, names_starting_with){
  sensors <- names(which(sapply(sensors, 
                                function(sensor){str_starts(string = sensor, 
                                                            pattern = names_starting_with)})))
  return(sensors)
}
#' 
#' @title Download Data Query Body
#' @description Generates the query body for downloading data from a remote server with timestamp 
#' filtering.
#' @param start_datetime The start datetime for the data request.
#' @param end_datetime The end datetime for the data request.
#' @param fields The fields expected from the database.
#' @param size The maximum number of records to retrieve (default is 10000).
#' @return A JSON-formatted query body for use in querying the remote server.
#' @details This function constructs a JSON-formatted query body based on the provided parameters.
#'   It includes specifications for the time range, aggregation, fields to be retrieved, and other
#'   relevant settings for querying data from a remote server.
#' @examples
#' start_date <- "2023-01-01T00:00:00"
#' end_date <- "2023-01-31T23:59:59"
#' fields_to_retrieve <- c("field1", "field2", "field3")
#' download_data_body(start_date, end_date, fields_to_retrieve)
#' @seealso \code{\link{elastic::search}}, \code{\link{sprintf_named}}
#' @export
download_data_body <- function(start_datetime, end_datetime, fields, size = 10000){
  datetimes <- list(start = start_datetime, end = end_datetime)
  body <- sprintf_named('{"version": true,
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
                                            "filter": [
                                              {"match_all": {}},
                                              {"range": 
                                                {"@timestamp": {"gte": "%{START_DATE}s",
                                                                "lte": "%{END_DATE}s",
                                                                "format": "strict_date_optional_time"}}}],
                                            "should": [],
                                            "must_not": []}},
                         "highlight": {"pre_tags": ["@kibana-highlighted-field@"],
                                       "post_tags": ["@/kibana-highlighted-field@"],
                                       "fields": {"*": {}},
                                       "fragment_size": 2147483647}}',
                       START_DATE = datetimes$start, 
                       END_DATE = datetimes$end, 
                       INDICATORS = cat(fields), 
                       SIZE = size)
  return(body)
}
#' 
#' @title Download Data
#' @description Downloads data from a remote server based on user input.
#' @param conn The connection to the remote server.
#' @param input A list containing user input parameters, including start and end dates, time sampling, 
#' and sensor configurations.
#' @return A dataset containing the downloaded data.
#' @details This function retrieves data from a remote server based on the user's input parameters,
#'   such as start and end dates, time sampling, and sensor configurations. The downloaded data is 
#'   returned as a dataset.
#' @examples
#' conn <- create_connection("http://example.com", port = 9200)
#' user_input <- list(start_date = "2023-01-01", start_time = "08:00:00",
#'                    end_date = "2023-01-31", end_time = "18:00:00",
#'                    time_sampling = "1h", scientific_domain = "physics",
#'                    acoustic_sensors = c("sensor1", "sensor2"),
#'                    acoustic_indicators = c("indicator1", "indicator2"),
#'                    acoustic_frequencies = c(1000, 2000))
#' dataset <- download_data(conn, user_input)
#' @seealso \code{\link{create_connection}}, \code{\link{get_data}}
#' @export
download_data <- function(conn, input){
  the_start_date <- as.Date(x = input$start_date, format = "%Y-%m-%d")
  the_start_date_time <- update(as.POSIXct(x = input$start_time, format = "%Y-%m-%d %H:%M:%S"), 
                                year = year(the_start_date), 
                                month = month(the_start_date), 
                                mday = day(the_start_date))
  the_end_date <- as.Date(x = input$end_date, format = "%Y-%m-%d")
  the_end_date_time <- update(as.POSIXct(x = input$end_time, format = "%Y-%m-%d %H:%M:%S"), 
                              year = year(the_end_date), 
                              month = month(the_end_date), mday = 
                                day(the_end_date))
  print("\n Reloading data")
  dataset <- get_data(conn = conn,
                      start_date = format(as.POSIXct(x = input$start_date, format = "%Y-%m-%d"), 
                                          "%Y/%m/%d"),
                      start_time = format(as.POSIXct(x = the_start_date_time, format = "%Y-%m-%d %H:%M:%S"), 
                                          "%H:%M"),
                      end_date = format(as.POSIXct(x = input$end_date, format = "%Y-%m-%d"), 
                                        "%Y/%m/%d"),
                      end_time = format(as.POSIXct(x = the_end_date_time, format = "%Y-%m-%d %H:%M:%S"), 
                                        "%H:%M"),
                      time_sampling = input$time_sampling,
                      scientific_domain = input$scientific_domain,
                      acoustic_sensors = input$acoustic_sensors,
                      acoustic_indicators = input$acoustic_indicators,
                      acoustic_frequencies = input$acoustic_frequencies, 
                      size=1000)
  return(dataset)
}
#' 
#' @title Formatted sprintf with Named Arguments
#' @description This function extends the base R `sprintf` function by allowing
#'   the use of named arguments in the format string.
#' @param fmt A character string containing the format specification.
#' @param ... Named arguments to be substituted into the format string.
#' @return A character vector resulting from applying the formatted string.
#' @details This function provides a convenient way to use named arguments in a
#'   sprintf-like format string. The named arguments are specified using the
#'   `%{name}` syntax, where `name` corresponds to the name of the argument.
#' @examples
#' fmt <- "Hello, %{name}! You are %{age} years old."
#' sprintf_named(fmt, name = "John", age = 30)
#' @seealso \code{\link{sprintf}}
#' @export
sprintf_named <- function(fmt, ...) {
  args <- list(...)
  argn <- names(args)
  if (is.null(argn)) return(sprintf(fmt, ...))
  
  for (i in seq_along(args)) {
    if (argn[i] == "") next;
    fmt <- gsub(sprintf("%%{%s}", argn[i]), sprintf("%%%d$", i), fmt, fixed = TRUE)
  }
  
  do.call(sprintf, append(args, fmt, 0))
}
#' 
format_datetime_for_ui <- function(the_date, the_time) {
  return(as.POSIXct(paste(the_date, the_time, sep = " ")))
}
#' 
#' @title Format Datetimes for ElasticSearch
#' @description Formats user-provided dates and times into the required format for 
#' querying ElasticSearch.
#' @param start_date The start date provided by the user.
#' @param start_time The start time provided by the user.
#' @param end_date The end date provided by the user.
#' @param end_time The end time provided by the user.
#' @return A list containing formatted start and end datetimes.
#' @details This function takes user-provided start and end dates along with 
#' corresponding start and end times and formats them into the required format 
#' for querying ElasticSearch. The formatted datetimes are returned as a list.
#' @examples
#' start_date <- "2023-01-01"
#' start_time <- "08:00:00"
#' end_date <- "2023-01-31"
#' end_time <- "18:00:00"
#' formatted_datetimes <- format_datetimes_for_elastic(start_date, start_time, end_date, end_time)
#' # Result: List with formatted start and end datetimes.
#' @seealso \code{\link{download_data}}, \code{\link{get_data}}
#' @export
format_datetimes_for_elastic <- function(start_date, start_time, end_date, end_time) {
  start_date_time <- as.POSIXct(paste(start_date, start_time, sep = "-"), format = "%Y/%m/%d-%H:%M")
  end_date_time <- as.POSIXct(paste(end_date, end_time, sep = "-"), format = "%Y/%m/%d-%H:%M")
  start_date_time <- format(start_date_time, format = "%Y-%m-%dT%H:%M:%S")
  end_date_time <- format(end_date_time, format = "%Y-%m-%dT%H:%M:%S")
  datetimes <- list(start = start_date_time, 
                    end = end_date_time)
  return(datetimes)
}
#' 
#' @title Format Datetimes for CSV
#' @description Formats a vector of date-time strings for CSV output.
#' @param start_date_times A vector of date-time strings to be formatted.
#' @param out_format The desired output format for the date-time strings. Defaults to \%Y-\%m-\%d_\%H:\%M:\%OS.
#' @return A list of formatted date-time strings.
#' @details This function takes a vector of date-time strings, interprets them as UTC, 
#' and formats them for CSV output. The user can specify the desired output format using 
#' the `out_format` parameter.
#' @examples
#' date_times <- c("2023-01-01T08:00:00Z", "2023-01-02T12:30:00Z")
#' formatted_dates <- format_datetimes_for_csv(date_times)
#' # Result: List of formatted date-time strings for CSV.
#' @seealso \code{\link{download_data}}, \code{\link{get_data}}
#' @export
format_datetimes_for_csv <- function(start_date_times, out_format = "%Y-%m-%d_%H:%M:%OS") {
  in_format <- "%Y-%m-%dT%H:%M:%OSZ"
  start_date_times <- unlist(start_date_times)
  start_date_times_utc <- as.POSIXct(start_date_times, format = in_format, tz = "UTC")
  start_date_times_csv <- as.list(strftime(start_date_times_utc, out_format))
  return(start_date_times_csv)
}
#' 
#' @title Format Datetime from Time Slider
#' @description Formats a datetime using a date and time from a time slider.
#' @param the_date The date selected from the time slider.
#' @param the_time The time selected from the time slider.
#' @return A POSIXct object representing the formatted datetime.
#' @details This function takes a date and time from a time slider, combines them into a datetime, 
#' and returns the result.
#' @examples
#' selected_date <- "2020-07-19"
#' selected_time <- "14:30"
#' formatted_datetime <- format_datetime_from_time_slider(selected_date, selected_time)
#' # Result: POSIXct object representing the formatted datetime.
#' @seealso \code{\link{download_data}}, \code{\link{get_data}}
#' @export
format_datetime_from_time_slider <- function(the_date, the_time) {
  the_date_time <- as.POSIXct(paste(the_date, format(the_time, format = "%H:%M"), sep = " "))
  return(the_date_time)
}
#' 
#' @title Get Timesampling Unit and Value
#' @description Extracts numeric value and unit from a timesampling string.
#' @param time_sampling A string representing the timesampling, e.g., "1h", "30s", "2d", etc.
#' @return A named list containing numeric value and unit.
#' @details This function parses a timesampling string (e.g., "1h", "30s") and extracts the 
#' numeric value and unit.
#' @examples
#' timesampling_str <- "1h"
#' timesampling_info <- get_timesampling_unit_and_value(timesampling_str)
#' # Result: List with "num" and "unit" components.
#' @seealso \code{\link{format_datetime_from_time_slider}}, \code{\link{download_data}}
#' @export
get_timesampling_unit_and_value <- function(time_sampling){
  is_num <- function(x) grepl("^[-]?[0-9]+[.]?[0-9]*|^[-]?[0-9]+[L]?|^[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",x)
  as_num <- function(x) {
    if (is.null(x)||length(x) == 0) return(x)
    if (class(x)=="list") return(lapply(x, as_num))
    if (is.character(x) & is_num(x)) return(as.numeric(x))
    return(x)
  }
  the_names <- c("num", "unit")
  the_values <- strsplit(time_sampling, "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl = TRUE)[[1]]
  timesampling_unit_and_value <- mapply(function(the_names, the_values) { the_values }, 
                                        the_names, the_values, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  return(as_num(timesampling_unit_and_value))
}
#' 
#' @title Format Acoustic Fields for ElasticSearch Query
#' @description Formats acoustic indicators and frequencies for an ElasticSearch query.
#' @param acoustic_indicators A vector of acoustic indicators.
#' @param acoustic_frequencies A vector of acoustic frequencies.
#' @return A formatted string of fields for the ElasticSearch query.
#' @details This function takes vectors of acoustic indicators and frequencies and generates a 
#' formatted string of fields for an ElasticSearch query.
#' @examples
#' indicators <- c("indicator1", "indicator2")
#' frequencies <- c("freq1", "freq2")
#' fields_str <- format_acoustic_fields_for_elastic(acoustic_indicators = indicators, acoustic_frequencies = frequencies)
#' # Result: A formatted string suitable for ElasticSearch query.
#' @seealso \code{\link{download_data_body}}, \code{\link{get_elastic_health}}
#' @export
format_acoustic_fields_for_elastic <- function(acoustic_indicators, acoustic_frequencies) {
  indicators <- as.vector(outer(acoustic_indicators, acoustic_frequencies, paste, sep = "_"))
  indicators.list <- c("@timestamp", indicators)
  fields <- paste0('"', paste(indicators.list, collapse = '","'), '"')
  return(fields)
}

format_meteo_fields_for_elastic <- function(meteo_variables) {
  lidar_vars <- startsWith(meteo_variables, 'lidar')
  if(any(lidar_vars)){
    meteo_variables[lidar_vars] <- gsub('lidar.', '', meteo_variables[lidar_vars])
    meteo_variables[lidar_vars] <- unlist(lapply(meteo_variables[lidar_vars], 
                                                 function(x) {paste0(str_to_title(str_split_1(string = x, pattern = '_')), 
                                                                     collapse = "_")}))
  }
  sonos_vars <- startsWith(meteo_variables, 'a')
  if(any(sonos_vars)){
    meteo_variables[sonos_vars] <- gsub('a[0-9]+.', '', meteo_variables[sonos_vars])
  }
  sonics_vars <- startsWith(meteo_variables, 's')
  if(any(sonics_vars)){
    meteo_variables[sonics_vars] <- gsub('s[0-9]+.', '', meteo_variables[sonics_vars])
  }
  variables.list <- c("@timestamp", unique(meteo_variables))
  fields <- paste0('"', paste(variables.list, collapse = '","'), '"')
  return(fields)
}

#' 
#' @title Format Elapsed Time
#' @description Formats elapsed time into a human-readable string.
#' @param elapsed_time The elapsed time in seconds.
#' @return A formatted string indicating the elapsed time in minutes and seconds.
#' @details This function takes an elapsed time in seconds and formats it into a human-readable string. 
#' If the elapsed time is greater than or equal to one minute, it is displayed in minutes and seconds; 
#' otherwise, it is displayed in seconds.
#' @examples
#' elapsed_seconds <- 75
#' formatted_time <- format_elapsed_time(elapsed_time = elapsed_seconds)
#' # Result: "1 min 15.00 s"
#' @export
format_elapsed_time <- function(elapsed_time) {
  # Convertir en minutes si le temps écoulé est supérieur à une minute
  if (elapsed_time >= 60) {
    minutes <- floor(elapsed_time / 60)
    seconds <- round(elapsed_time %% 60, 2)
    return(sprintf("%d min %.2f s", minutes, seconds))
  } else {
    # Sinon, rester en secondes
    return(sprintf("%.2f s", elapsed_time))
  }
}
#' 
#' @title Extract Sensor Information from Elasticsearch Index Name
#' @description Extracts sensor information from the given Elasticsearch index name.
#' @param field A character string representing the field.
#' @param time_sampling A character string representing the time sampling.
#' @param index_name A character string representing the Elasticsearch index name.
#' @return The sensor name.
#' @examples
#' # Example Usage:
#' extract_sensor("acoustic-a1-10min", "acoustic", "10min")
#' # Returns: "a1"
#' @export
extract_sensor <- function(index_name, field, time_sampling) {
  removePattern <- paste(field, time_sampling, "-", sep = "|")
  return(gsub(removePattern, "", index_name))
}
#' 
#' @title Convert a Data Column
#' @description This function attempts to convert the values of a data column into different 
#' types (logical, numeric, integer, floating-point, character).
#' @param column_data The data of the column to convert.
#' @return The converted values of the column in the appropriate data type.
#' @details The function identifies non-empty values in the column and then tries to convert 
#' them into logical, numeric, integer, floating-point, or character types based on certain 
#' characteristics of the data.
#' @examples
#' \dontrun{
#' # Example usage
#' data <- c("1", "2", "3", "")
#' result <- convert_column(data)
#' }
#' @export
convert_column <- function(column_data) {
  # Replace Null and NaT with NA
  column_data[unlist(lapply(column_data, is.null))] <- NA
  column_data[unlist(lapply(column_data, function(x) x == "NaT"))] <- NA 
  # Flatten the list
  flattened_data <- character(length = length(column_data))
  flattened_data <- unlist(column_data)
  # Replace "NaT" with "nan
  flattened_data <- str_replace_all(string = flattened_data, pattern = "NaT", replacement = "nan")
  # Identify non-empty values
  non_empty_indices <- which(flattened_data != "" & flattened_data != "nan")
  # 1. Check if all non-empty values can be converted to logical
  if (length(non_empty_indices) > 0 && all(!is.na(tryCatch(as.logical(flattened_data[non_empty_indices]), error = function(e) NA)))) {
    result <- logical(length(flattened_data))
    result[non_empty_indices] <- as.logical(flattened_data[non_empty_indices])
    result[is.na(flattened_data)] <- NA
  } else {
    # 2. Check if all non-empty values can be converted to numeric
    if (length(non_empty_indices) > 0 && all(grepl("^-?\\d*\\.?\\d*$", flattened_data[non_empty_indices]))) {
      # Check if values are between 0 and 1, indicating a ratio or percentage
      if (all(as.numeric(flattened_data[non_empty_indices]) >= 0) && all(as.numeric(flattened_data[non_empty_indices]) <= 1)) {
        result <- numeric(length(flattened_data))
        result[non_empty_indices] <- as.numeric(flattened_data[non_empty_indices]) * 100
        result[is.na(flattened_data)] <- NA
      } else {
        # 3. Check if all non-empty values can be converted to integers
        if (all(!is.na(tryCatch({
          result <- integer(length(flattened_data))
          result[non_empty_indices] <- as.integer(flattened_data[non_empty_indices])
          result[is.na(flattened_data)] <- NA
          if (any(is.na(result))) stop("NAs introduced")
          result
        }, error = function(e) NA)))) {
          # Check if values contain decimals, indicating they should be converted to float
          if (any(grepl("\\.", as.character(flattened_data[non_empty_indices])))) {
            result <- numeric(length(flattened_data))
            result[non_empty_indices] <- as.numeric(flattened_data[non_empty_indices])
            result[is.na(flattened_data)] <- NA
          } else {
            result <- integer(length(flattened_data))
            result[non_empty_indices] <- as.integer(flattened_data[non_empty_indices])
            result[is.na(flattened_data)] <- NA
          }
        } else {
          # 4. Check if all non-empty values can be converted to float
          if (length(non_empty_indices) > 0 
              && 
              all(!is.na(tryCatch(as.numeric(flattened_data[non_empty_indices]), 
                                  error = function(e) NA)))) {
            result <- numeric(length(flattened_data))
            result[non_empty_indices] <- as.numeric(flattened_data[non_empty_indices])
            result[is.na(flattened_data)] <- NA
          } else {
            # 5. Check if all non-empty values can be converted to character
            if (all(!is.na(tryCatch(as.character(flattened_data[non_empty_indices]), 
                                    error = function(e) NA)))) {
              result <- character(length(flattened_data))
              result[non_empty_indices] <- as.character(flattened_data[non_empty_indices])
              result[is.na(flattened_data)] <- NA
            } else {
              # If all conversion attempts fail, return the original column
              result <- flattened_data
              result[is.na(flattened_data)] <- NA
            }
          }
        }
      }
    } else {
      # If all conversion attempts fail, return the original column
      result <- flattened_data
    }
  }
  return(result)
}
#' 
#' @title Convert Columns of a Data Object
#' @description This function converts the "timestamp" column to POSIXct type with the 
#' specified format and attempts to convert other columns into different types.
#' @param df The data object to process.
#' @param timestamp_format The format to use for converting the "timestamp" column.
#' @return The data object with converted columns.
#' @details The function applies the conversion of the "timestamp" column to POSIXct 
#' type with the specified format. Then, it attempts to convert values in other columns 
#' into logical, numeric, integer, floating-point, or character types using the 
#' `convert_column` function.
#' @examples
#' \dontrun{
#' # Example usage
#' data <- data.frame(timestamp = c("2020-07-19 14:00:00"), value = c("1", "2", "3"))
#' result <- convert_columns(data)
#' }
#' @export
convert_columns <- function(df, timestamp_format="%Y-%m-%d %H:%M:%S") {
  df$timestamp <- as.POSIXct(df$timestamp, format = timestamp_format)
  other_columns <- setdiff(names(df), "timestamp")
  df[other_columns] <- lapply(df[other_columns], convert_column)
  return(df)
}

elastic_body <- function(datetimes, fields, size){
  # Define the Elasticsearch query
  requete_elasticsearch <- sprintf_named('{
    "query": {
      "bool": {
        "must": [
          {
            "range": {
              "@timestamp": {
                "gte": "%{START_DATE}s",
                "lt": "%{END_DATE}s"
              }
            }
          }
        ]
      }
    },
    "_source": [%{FIELDS}s],
    "size": %{SIZE}s
  }
  ', START_DATE = datetimes$start, END_DATE = datetimes$end, FIELDS = fields, SIZE = size)
  # Explanation:
  # - "query": Specifies the query section
  #   - "bool": Boolean query for combining multiple conditions
  #     - "must": Specifies conditions that must be satisfied
  #       - "range": Range condition for the "@timestamp" field
  #         - "gte": Greater than or equal to the start date
  #         - "lt": Less than the end date
  # - "_source": Specifies the fields to be included in the response
  # - "size": Specifies the number of results to return per batch (pagination), set to 10000
  return(requete_elasticsearch)
}

elastic_scroll <- function(time_sampling, elastic_body, scientific_domain, indexes){
  # Execute the Elasticsearch query and handle the results (scrolling)
  time_scroll <- str_remove(string = time_sampling, pattern = "in")
  response_content <- Search(conn = elastic_conn, index = indexes, body = elastic_body, time_scroll = time_scroll)
  # Initialize a list to store the results
  all_data_samples <- list()
  # Repeat the retrieval of pages as long as there are results
  while (length(response_content$hits$hits) > 0) {
    # Convert JSON response to a dataframe
    data_sample <- as.data.frame(do.call(rbind, lapply(response_content$hits$hits, `[[`, "_source")))
    data_sample$sensor <- sapply(response_content$hits$hits, 
                                 function(hit) extract_sensor(hit$`_index`, scientific_domain, time_sampling))
    # Add the dataframe to the list
    all_data_samples <- c(all_data_samples, list(data_sample))
    # Get the next page
    response_content <- scroll(conn = elastic_conn, x = response_content$`_scroll_id`)
  }
  # Combine all the results into a single dataframe
  final_data_frame <- do.call(rbind, all_data_samples)
  # Creating the original column @timestamp
  final_data_frame$timestamp <- sapply(final_data_frame$`@timestamp`, 
                                       function(ts) {
                                         timestamp <- as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
                                         format(timestamp, format = "%Y-%m-%d %H:%M:%S")})
  # Remove the original column "@timestamp"
  final_data_frame <- final_data_frame[, !names(final_data_frame) %in% c("@timestamp")]
  # Rename columns (except timestamp and sensor) and order rows according to ascending timestamp
  final_data_frame <- pivot_wider(data = final_data_frame, 
                                  names_from = sensor,
                                  values_from = names(final_data_frame)[! names(final_data_frame) %in% c("timestamp", "sensor")],
                                  names_glue = paste(scientific_domain, "{sensor}.{.value}", sep = ".")) %>%
    arrange(timestamp) %>%   # Sort rows by ascending timestamp
    relocate(sort(names(.)[which(names(.) != "timestamp")]), .after = timestamp)  # Reorder columns by sensor after timestamp column 
  # Convert the dataframe columns into the correct format
  final_data_frame <- convert_columns(final_data_frame)
  return(final_data_frame)
}

collapse_df_field_and_sensor <- function(df, scientific_field, remove_field_sensor_cols = TRUE){
  df['scientific_field'] <- scientific_field
  df <- df %>% unite("field_sensor", c('scientific_field', 'sensor'), sep = "_", remove = remove_field_sensor_cols)
  return(df)
}


# check_sensors_vars_selections <- function(scientific_field, selected_sensors, selected_sensor_names, selected_sensor_variables){
#   if(is.null(selected_sensor_names) & is.null(selected_sensors[[scientific_field]][[selected_sensor_variables]])){
#     # No sensor nor variables selected
#     valid_input_test <- TRUE
#     mess <- "Download selected data"
#   } else if(!is.null(selected_sensor_names) & is.null(selected_sensors[[scientific_field]][[selected_sensor_variables]])){
#     # Sensor selected but no variables selected
#     valid_input_test <- FALSE
#     mess <- paste0("Select at least one ", scientific_field, " variable for sensor(s) ", paste(selected_sensor_names, collapse = '","'))
#     break
#   } else if(is.null(selected_sensor_names) & !is.null(selected_sensors[[scientific_field]][[selected_sensor_variables]])){
#     # No sensor selected but variables are
#     valid_input_test <- FALSE
#     mess <- paste0("Select at least one ", scientific_field, " sensor for variable(s) ", paste(selected_sensor_variables, collapse = '","'))
#     break
#   } else if(!is.null(selected_sensor_names) & !is.null(selected_sensors[[scientific_field]][[selected_sensor_variables]])){
#     # Both sensor and variables selected
#     valid_input_test <- TRUE
#     mess <- "Download selected data"
#   }
#   return(list(result = valid_input_test, bs_title = mess))
# }


#' 
#' @title Retrieve Data from Elasticsearch
#' @description This function retrieves data from Elasticsearch based on specified parameters.
#' @param conn The Elasticsearch connection.
#' @param start_date The start date for data retrieval.
#' @param start_time The start time for data retrieval.
#' @param end_date The end date for data retrieval.
#' @param end_time The end time for data retrieval.
#' @param time_sampling The time sampling interval.
#' @param scientific_domain The scientific domain for data retrieval.
#' @param acoustic_sensors The acoustic sensors for data retrieval.
#' @param acoustic_indicators The acoustic indicators for data retrieval.
#' @param acoustic_frequencies The acoustic frequencies for data retrieval.
#' @param size The number of data samples to retrieve in each request.
#' @return A dataframe containing the retrieved data.
#' @details This function constructs Elasticsearch queries based on the specified parameters 
#' and retrieves data in chunks to handle pagination.
#' @examples
#' \dontrun{
#' # Example usage
#' conn <- elasticsearch::connect(host = "localhost", port = 9200)
#' start_date <- "2023-01-01"
#' start_time <- "12:00:00"
#' end_date <- "2023-01-02"
#' end_time <- "12:00:00"
#' time_sampling <- "1h"
#' scientific_domain <- "domain1"
#' acoustic_sensors <- "sensor1"
#' acoustic_indicators <- c("indicator1", "indicator2")
#' acoustic_frequencies <- c("frequency1", "frequency2")
#' size <- 10000
#' data <- get_data(conn, start_date, start_time, end_date, end_time, time_sampling, 
#' scientific_domain, acoustic_sensors, acoustic_indicators, acoustic_frequencies, size)
#' }
#' @export
get_data <- function(conn, start_date, start_time, end_date, end_time, time_sampling, 
                     scientific_domain, acoustic_sensors, acoustic_indicators, acoustic_frequencies, 
                     meteo_sensors, meteo_variables, size = 10000) {
  if(!is.character(start_date)){start_date <- format(start_date, format = "%Y/%m/%d")}
  print("start_date")
  print(start_date, digits = NULL, quote = FALSE)
  if(!is.character(start_time)){
    start_time <- format(update(as.POSIXct(x = start_time, format = "%H:%M"), 
                                year = year(start_date), 
                                month = month(start_date), 
                                mday = day(start_date)), "%H:%M")
  }
  print("start_time")
  print(start_time, digits = NULL, quote = FALSE)
  if(!is.character(end_date)){end_date <- format(end_date, format = "%Y/%m/%d")}
  print("end_date")
  print(end_date, digits = NULL, quote = FALSE)
  if(!is.character(end_time)){
    end_time <- format(update(as.POSIXct(x = end_time, format = "%H:%M"), 
                              year = year(end_date), 
                              month = month(end_date), 
                              mday = day(end_date)), "%H:%M")
  }
  print("end_time")
  print(end_time, digits = NULL, quote = FALSE)
  if(!is.null(acoustic_sensors)){
    # Download acoustic data
    acoustic_indexes <- paste0('acoustic', "-", acoustic_sensors, "-", time_sampling)
    acoustic_fields <- format_acoustic_fields_for_elastic(acoustic_indicators = acoustic_indicators, 
                                                          acoustic_frequencies = acoustic_frequencies)
    datetimes <- format_datetimes_for_elastic(the_start_date, the_start_time, the_end_date, the_end_time)
    # Download acoustic data
    acoustic_elasticsearch_body <- elastic_body(datetimes = datetimes, fields = acoustic_fields, size = size)
    # Execute the Elasticsearch query and handle the results (scrolling)
    acoustic_df <- elastic_scroll(time_sampling = time_sampling, elastic_body = acoustic_elasticsearch_body, 
                                  scientific_domain = 'acoustic', indexes = acoustic_indexes)
    # acoustic_df <- collapse_df_field_and_sensor(df = acoustic_df, scientific_field = 'acoustic')
  }
  if(!is.null(meteo_sensors)){
    # Download meteo data
    for(meteo_sensor_type in unique(str_extract(meteo_sensors, "^[a-z]+"))){
      cur_meteo_sensors <- meteo_sensors[startsWith(x = meteo_sensors, prefix = meteo_sensor_type)]
      cur_meteo_indexes <- paste0('meteo', "-", cur_meteo_sensors, "-", time_sampling)
      cur_meteo_variables <- meteo_variables[which(startsWith(x = meteo_variables, prefix = meteo_sensor_type))]
      cur_meteo_fields <- format_meteo_fields_for_elastic(meteo_variables=cur_meteo_variables)
      cur_meteo_elasticsearch_body <- elastic_body(datetimes = datetimes, fields = cur_meteo_fields, size = size)
      # Execute the Elasticsearch query and handle the results (scrolling)
      cur_meteo_df <- elastic_scroll(time_sampling = time_sampling, elastic_body = cur_meteo_elasticsearch_body, 
                                     scientific_domain = 'meteo', indexes = cur_meteo_indexes)
      if(!exists('meteo_df') && !is.data.frame('meteo_df')){
        meteo_df <- cur_meteo_df
      } else {
        meteo_df <- left_join(meteo_df, cur_meteo_df, by = 'timestamp')
      }
    }
    # meteo_indexes <- paste0('meteo', "-", meteo_sensors, "-", time_sampling)
    # meteo_fields <- format_meteo_fields_for_elastic(meteo_variables=meteo_variables)
    # meteo_elasticsearch_body <- elastic_body(datetimes = datetimes, fields = meteo_fields, size = size)
    # # Execute the Elasticsearch query and handle the results (scrolling)
    # meteo_df <- elastic_scroll(time_sampling = time_sampling, elastic_body = meteo_elasticsearch_body, 
    #                            scientific_domain = 'meteo', indexes = meteo_indexes)
  }
  if(!is.null(acoustic_sensors) & !is.null(meteo_sensors)){
    # Join acoustic and meteo data in a common dataframe
    final_df <- left_join(acoustic_df, meteo_df, by = 'timestamp')
  } else if(!is.null(acoustic_sensors) & is.null(meteo_sensors)){
    # Acoustic data only
    final_df <- acoustic_df
  } else if(is.null(acoustic_sensors) & !is.null(meteo_sensors)){
    # Meteo data only
    final_df <- meteo_df
  }
  return(final_df)
}
#' @title Unique Field, Sensor, and Variable Triplets
#' @description
#' This function takes a dataframe as input and returns a tibble with unique triplets
#' of "field", "sensor", and "variable". The three values are concatenated into a
#' string with "." as separators.
#' @param df A dataframe containing columns "field", "sensor", and "variable".
#' @return
#' A tibble with unique triplets of "field", "sensor", and "variable". The three
#' values are concatenated into a string with "." as separators.
#' @details
#' The function uses the distinct function to obtain unique combinations of
#' "field", "sensor", and "variable". It then uses mutate with paste to create a
#' new column "field.sensor.var" with the concatenated string.
#' @examples
#' \dontrun{
#' # Create a sample dataframe
#' df <- tibble::tibble(
#'   field = c("acoustic", "acoustic", "meteo"),
#'   sensor = c("a1", "a2", "lidar"),
#'   variable = c("Leq_A", "Leq_B", "Hum")
#' )
#' # Call the function
#' unique_triplets <- unique_field_sensor_variable_triplets(df)
#' }
#' @export
unique_field_sensor_variable_triplets <- function(df){
  unique_triplets <- df %>% 
    distinct(field, sensor, variable) %>% 
    mutate("field.sensor.var" = paste(field, sensor, variable, sep = "."))
  return(unique_triplets)
}
#' 
#' @title Count Common Prefix Elements
#' @description
#'   This function takes a reference set of sensor names and a list of sensor names,
#'   and counts the number of elements in the list that have a common prefix with each
#'   element in the reference set.
#' @details
#'   The function works by converting the reference set of sensor names to a set of unique
#'   prefixes, and then counting the number of elements in the list that share each prefix.
#' @param meteo_sensors A vector of sensor names serving as the reference set.
#'   The function will extract unique prefixes from these names for comparison.
#' @param strings A vector of sensor names to be compared with the reference set.
#' @return
#'   A data frame where each column corresponds to a unique prefix from the
#'   reference set, and each row indicates the count of elements in the list
#'   that share the corresponding prefix.
#' @examples
#' \dontrun{
#'   meteo_sensors <- c("a1", "a2", "a3", "a4", "a5", "lidar", "s1", "s2", "s3")
#'   input_strings <- c("a3", "a4", "lidar", "s1", "s2")
#'   result <- count_common_prefix_elements(meteo_sensors, input_strings)
#'   print(result)
#' }
#'
#' @export
count_common_prefix_elements <- function(meteo_sensors, strings) {
  # Convert meteo_sensors to a reference set by extracting unique prefixes
  reference <- unique(gsub('[[:digit:]]+', '', meteo_sensors))
  # Initialize a list to store counts for each prefix
  count_list <- lapply(reference, function(word) {
    # For each prefix, count the number of elements in strings with a common prefix
    prefix_length <- sapply(strings, function(s) sum(substr(word, 1, nchar(word)) == substr(s, 1, nchar(word))))
    return(prefix_length)
  })
  # Create a data frame from the count list
  result_df <- data.frame(do.call(cbind, count_list))
  # Rename columns
  colnames(result_df) <- reference
  colnames(result_df)[which(names(result_df) == "a")] <- "near_slm"
  colnames(result_df)[which(names(result_df) == "s")] <- "sonics"
  return(result_df)
}
#' 
#' @section functions_shiny_app: Shiny application utilities
#' 
#' @title Format Data for Shiny App
#' @description This function formats a dataset for display in a Shiny app.
#' @param dataset The input dataset containing acoustic sensor data.
#' @return A dataframe suitable for use in a Shiny app.
#' @details This function extracts relevant information from the input dataset to create 
#' a new dataframe with columns for timestamp, sensor, indicator, and corresponding values.
#' @examples
#' \dontrun{
#' # Example usage
#' dataset <- ...  # Replace with your dataset
#' formatted_data <- format_data_for_app(dataset)
#' }
#' @export
format_data_for_app <- function(dataset){
  # Use pivot_longer to gather acoustic and meteo indicators
  df_long <- dataset %>%
    pivot_longer(cols = starts_with("acoustic") | starts_with("meteo"),
                 names_to = "variable",
                 values_to = "value")
  # Create new columns "field", "sensor", and "variable"
  data_app <- df_long %>%
    mutate(field = str_extract(variable, "^[^.]+"),                          # Extract the first word before the first dot
           sensor = str_split(variable, "\\.", simplify = TRUE)[, 2],        # Extract the second word after the first dot
           variable = str_split(variable, "\\.", simplify = TRUE)[, 3]) %>%  # Extract the third word after the second dot
    select(timestamp, field, sensor, variable, value)                        # Reorder the columns
  # # Use pivot_wider to get the final format
  # df_final <- df_long %>%
  #   pivot_wider(names_from = variable, values_from = value)
  
  # indicator_names <- colnames(dataset[5:ncol(dataset)])
  # df_indicators <- dataset %>% select(one_of(indicator_names))
  # data_app <- data.frame(timestamp = dataset$start_time, 
  #                        sensor = dataset$sensor, 
  #                        indicators = rep(names(df_indicators), each = nrow(df_indicators)), 
  #                        value = unlist(df_indicators), 
  #                        row.names = NULL)
  return(data_app)
}
#' 
#' @section functions_sensors: Sensors utilities
#'
#' @title Convert Excel File to Data Frame
#' @description
#' This function converts an Excel file (.xlsx) containing sensor data into a data frame.
#' It takes the path to the Excel file and an optional sheet name as input and returns
#' a data frame representing the sensor data.
#' @param file_path Path to the Excel file (.xlsx) containing sensor data.
#' @param sheet_name Name of the worksheet in the Excel file to extract (optional,
#'        by default, the first sheet will be used).
#' @return A data frame representing the sensor data extracted from the Excel file.
#' @examples
#' # Example usage with the file "sensor_data.xlsx" and the sheet "SensorData"
#' df <- sensor_xlsx_file_to_df(file_path = "sensor_data.xlsx", sheet_name = "SensorData")
#'
#' # Display the first few rows of the data frame
#' head(df)
#' @export
sensor_xlsx_file_to_df <- function(xlsx_filename, sheetname) {
  tmp <-read_xlsx(path = xlsx_filename, sheet = 9, col_names = TRUE, skip = 2, n_max = 156)
  df <- as.data.frame(tmp[, seq(2, ncol(tmp))])   # Set column names
  row.all.na <- apply(df, 1, function(x){all(is.na(x))})
  df <- df[!row.all.na, ]  # Remove rows with NA values only
  return(df)
}
#'
#' @title Get Sensors Information
#' @description
#' This function reads information about acoustic and meteorological sensors from a CSV file and processes it.
#' It extracts sensor details, such as coordinates, latitude, and longitude, from the input file.
#' @param sensors_filename The file path to the CSV file containing sensors information.
#' @return
#' A data frame containing the processed information about sensors, including system names, coordinates, latitude, and longitude.
#' @details
#' The function reads the CSV file, performs data manipulation, and extracts relevant sensor information.
#' It handles cases where coordinates are present in a single column and splits them into latitude and longitude.
#' The resulting data frame is cleaned up to remove missing or irrelevant information.
#' @examples
#' # Example:
#' meteo_data <- get_meteo_sensors_info("path/to/meteo_sensors.csv")
#' head(meteo_data)
#' @importFrom readr read_csv
#' @importFrom tidyr separate
#' @importFrom dplyr mutate
get_meteo_sensors_info <- function(sensors_filename){
  sensors_tmp <- read_csv(file = sensors_filename, locale = readr::locale(encoding = "UTF-8"))
  sensors <- data.frame(t(sensors_tmp[-1]))
  colnames(sensors) <- t(sensors_tmp[, 1])
  remove(sensors_tmp)
  sensors <- sensors %>% 
    separate(col = "coord. GPS", into = c("latitude", "longitude", "latdeg", "longdeg"), sep = " ", convert = TRUE) %>% 
    mutate(latitude = str_sub(latitude, 1, -2)) %>% 
    mutate(longitude = as.numeric(longitude))
  row_names = rownames(sensors)
  for(idr in seq(length(row_names))){
    if(isTRUE(startsWith(row_names[idr], prefix = "..."))){
      row_names[idr] <- row_names[idr-1]
    }
  }
  rownames(sensors) <- make.names(row_names, unique = TRUE)
  sensors <- setNames(cbind(rownames(sensors), sensors, row.names = NULL), 
                      c("system", colnames(sensors)))
  sensors <- st_as_sf(sensors, coords = c('longitude', 'latitude')) %>% 
    mutate(lon = as.numeric(st_coordinates(geometry)[, 1]), 
           lat = as.numeric(st_coordinates(geometry)[, 2])) %>% 
    st_set_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  sensors$longitude <- sensors$lon
  sensors$latitude <- sensors$lat
  sensors <- sensors[, c("system", "field", "measurement", "variable(s)", "height (m)", "lon", "lat", "longitude", "latitude", "type", "model")]
  # Remove rownames with numbered suffix (ex: EDF.1, ...)
  id_multiple_rows_matEDF = grep("EDF", sensors$system)
  sensors[id_multiple_rows_matEDF, ]$system <- c(rep('mast', length(id_multiple_rows_matEDF)))
  id_multiple_rows_Sonic1 = grep("Sonic.1", sensors$system)
  sensors[id_multiple_rows_Sonic1, ]$system <- c(rep('S1', length(id_multiple_rows_Sonic1)))
  id_multiple_rows_Sonic2 = grep("Sonic.2", sensors$system)
  sensors[id_multiple_rows_Sonic2, ]$system <- c(rep('S2', length(id_multiple_rows_Sonic2)))
  id_multiple_rows_Sonic3 = grep("Sonic.3", sensors$system)
  sensors[id_multiple_rows_Sonic3, ]$system <- c(rep('S3', length(id_multiple_rows_Sonic3)))
  id_multiple_rows_LIDAR = grep("LIDAR", sensors$system)
  sensors[id_multiple_rows_LIDAR, ]$system <- c(rep('LIDAR', length(id_multiple_rows_LIDAR)))
  id_multiple_rows_A1 = grep("A1", sensors$system)
  sensors[id_multiple_rows_A1, ]$system <- c(rep('A1', length(id_multiple_rows_A1)))
  id_multiple_rows_A2 = grep("A2", sensors$system)
  sensors[id_multiple_rows_A2, ]$system <- c(rep('A2', length(id_multiple_rows_A2)))
  id_multiple_rows_A3 = grep("A3", sensors$system)
  sensors[id_multiple_rows_A3, ]$system <- c(rep('A3', length(id_multiple_rows_A3)))
  id_multiple_rows_A4 = grep("A4", sensors$system)
  sensors[id_multiple_rows_A4, ]$system <- c(rep('A4', length(id_multiple_rows_A4)))
  id_multiple_rows_A5 = grep("A5", sensors$system)
  sensors[id_multiple_rows_A5, ]$system <- c(rep('A5', length(id_multiple_rows_A5)))
  return(sensors)
}
#' #' 
#' #' @title Read Sensor Data from Excel File
#' #' @description This function reads sensor data from an Excel file and returns a dataframe.
#' #' @param xlsx_filename The path to the Excel file.
#' #' @param sheetname The name or index of the sheet containing the sensor data.
#' #' @return A dataframe containing the sensor data.
#' #' @details This function uses the `read_xlsx` function to read sensor data from a specified 
#' #' sheet in an Excel file. It then removes rows with NA values only and returns the resulting dataframe.
#' #' @examples
#' #' \dontrun{
#' #' # Example usage
#' #' xlsx_filename <- ...  # Replace with your file path
#' #' sheetname <- ...  # Replace with your sheet name or index
#' #' sensor_data <- sensor_xlsx_file_to_df(xlsx_filename, sheetname)
#' #' }
#' #' @export
#' get_sensor_info_xlsx <- function(sensor_filename, sheetname){
#'   cat(file = stderr(), paste0("\n\t| Get information about sensors from ",  sensor_filename, "\n"))
#'   sensors_tmp <- sensor_xlsx_file_to_df(xlsx_filename = sensor_filename, 
#'                                         sheetname = sheetname)
#'   sensors_tmp <- sensors_tmp %>% 
#'     mutate(`date/heure/min début` = format(`date/heure/min début`, format = "%Y-%m-%d %H:%M:%S", usetz = FALSE)) %>% 
#'     separate(col = `Coordonnées GPS`, 
#'              into = c("latitude", "longitude", "latdeg", "longdeg"), 
#'              sep = " ", convert = TRUE) %>% 
#'     mutate(latitude = str_sub(latitude, 1, -2)) %>%   
#'     mutate(latitude = as.numeric(latitude))  %>% 
#'     mutate(longitude = as.numeric(longitude)) %>% 
#'     mutate(latdeg = str_sub(latdeg, 2, -1)) %>%    # remove parenthesis
#'     mutate(longdeg = str_sub(longdeg, 1, -2)) %>%
#'     fill(`date/heure/min début`) %>%               # fill column with previous row value if na 
#'     fill(`date/heure/min fin`) %>% 
#'     fill(capteurs) %>% 
#'     fill(emplacement)
#'   sensors_tmp$name <- ifelse(startsWith(sensors_tmp$`Références (cf protocole exp)`, prefix = "LD_"), 
#'                              str_remove(sensors_tmp$`Références (cf protocole exp)`, pattern = "LD_"), 
#'                              sensors_tmp$`Références (cf protocole exp)`)
#'   sensors_tmp$name <- tolower(sensors_tmp$name)
#'   # Set the reference geometry
#'   ref_point_coords <- data.frame(latitude = sensors_tmp$latitude[which(sensors_tmp$emplacement == "mât")[1]], 
#'                                  longitude = sensors_tmp$longitude[which(sensors_tmp$emplacement == "mât")[1]])
#'   # Project geometries relatively to the reference one
#'   sensors_tmp$lat <- NA
#'   sensors_tmp$lat[which(sensors_tmp$emplacement == "mât")] <- 0
#'   sensors_tmp$lat[which(sensors_tmp$emplacement != "mât")] <- sensors_tmp$latitude[which(sensors_tmp$emplacement != "mât")] 
#'                                                               - ref_point_coords$latitude
#'   sensors_tmp$latitude <- sensors_tmp$lat
#'   sensors_tmp$lon <- NA
#'   sensors_tmp$lon[which(sensors_tmp$emplacement == "mât")] <- 0
#'   sensors_tmp$lon[which(sensors_tmp$emplacement != "mât")] <- sensors_tmp$longitude[which(sensors_tmp$emplacement != "mât")] 
#'                                                               - ref_point_coords$longitude
#'   sensors_tmp$longitude <- sensors_tmp$lon
#'   sensors_tmp <- subset(sensors_tmp, select = -c(latdeg, longdeg, `Coordonnées GPS_2 (HL)`, Notes))
#'   sensors <- st_as_sf(x = sensors_tmp,
#'                       coords = c("longitude", "latitude"), 
#'                       crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", 
#'                       na.fail = FALSE)
#'   sensors <- sensors[!sf::st_is_empty(sensors$geometry), ]  # Remove rows with empty geometry
#'   
#'   return(list(sensors = sensors, ref_point_coords = ref_point_coords))
#' }
#' 
#' @section functions_mapping: Geographical data mapping utilities
#' 
#' @title Get Centre Point from GeoDataFrame
#' @description This function calculates the centre point from a GeoDataFrame by creating 
#' a concave hull around the combined points.
#' @param geodf The GeoDataFrame containing spatial data.
#' @return A dataframe representing the coordinates of the centre point.
#' @details This function takes a GeoDataFrame, combines the points, and constructs a 
#' concave hull around them. It then calculates the centre point of the concave hull 
#' and returns the coordinates as a dataframe.
#' @examples
#' \dontrun{
#' # Example usage
#' geodf <- ...  # Replace with your GeoDataFrame
#' centre_point <- get_geodf_centre_point(geodf)
#' }
#' @export
get_geodf_centre_point <- function(geodf){
  points_hull <- geodf %>% 
    st_combine() %>% st_cast("POINT") %>% st_sf() %>% unique()
  concave_hull <- points_hull %>% concaveman()
  centre_point <- data.frame(st_centroid(concave_hull) %>% st_coordinates())
  return(centre_point)
}
#' 
#' @title Get Wind Turbine Relative Coordinates
#' @description This function creates a dataset of wind turbines with relative 
#' coordinates calculated with respect to a reference point.
#' @param ref_point_coords A dataframe with latitude and longitude of the reference point.
#' @return A Simple Features (sf) object representing wind turbines with relative coordinates.
#' @details This function creates a dataset of wind turbines with fixed coordinates. It 
#' then calculates the relative coordinates of each wind turbine with respect to a reference 
#' point specified by `ref_point_coords` and returns the result as an sf object.
#' @examples
#' \dontrun{
#' # Example usage
#' ref_point_coords <- data.frame(latitude = ..., longitude = ...)  # Replace with your reference point coordinates
#' wind_turbines <- get_wind_turbine_relative_coords(ref_point_coords)
#' }
#' @export
get_wind_turbine_relative_coords <- function(ref_point_coords){
  # Wind turbines
  wind_turbines_tmp <- data.frame(name = 1, latitude = 48.2293394, longitude = 1.5193507) %>% 
    add_row(name = 2, latitude = 48.2269236, longitude = 1.5242859) %>% 
    add_row(name = 3, latitude = 48.2246078, longitude = 1.5287062) %>% 
    add_row(name = 4, latitude = 48.2221774, longitude = 1.5330350) %>% 
    add_row(name = 5, latitude = 48.2209701, longitude = 1.5374960) %>% 
    add_row(name = 6, latitude = 48.2190466, longitude = 1.5426537) %>% 
    add_row(name = 7, latitude = 48.2169310, longitude = 1.5497321) %>% 
    add_row(name = 8, latitude = 48.2149968, longitude = 1.5547343)
  # Project geometries relatively to the reference one
  wind_turbines_tmp$lat <- wind_turbines_tmp$latitude - ref_point_coords$latitude
  wind_turbines_tmp$latitude <- wind_turbines_tmp$lat
  wind_turbines_tmp$lon <- wind_turbines_tmp$longitude - ref_point_coords$longitude
  wind_turbines_tmp$longitude <- wind_turbines_tmp$lon
  wind_turbines <- st_as_sf(x = wind_turbines_tmp,
                            coords = c("longitude", "latitude"), 
                            crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", 
                            na.fail = FALSE)
  return(wind_turbines = wind_turbines)
}
#'Define map icons
# gray_pin <- makeIcon(iconUrl = "www/img/pin_icon_gray.svg",
#                      iconWidth = 20, iconHeight = 30)
# black_pin <- makeIcon(iconUrl = "www/img/pin_icon_black.svg",
#                       iconWidth = 20, iconHeight = 30)
# green_pin <- makeIcon(iconUrl = "www/img/pin_icon_green.svg",
#                       iconWidth = 20, iconHeight = 30)

# north_arrow_pin <- makeIcon(iconUrl = "https://upload.wikimedia.org/wikipedia/commons/8/84/North_Pointer.svg", 
#                             iconWidth = 20, iconHeight = 30)
# acoust_pin <- makeIcon(iconUrl = "www/img/microphone_black_pin.png",
#                        iconWidth = 25, iconHeight = 25)
# mast_pin <- makeIcon(iconUrl = "anemometer_black_pin.png",
#                      iconWidth = 25, iconHeight = 25)
# lidar_pin <- makeIcon(iconUrl = "www/img/lidar_black_pin.png",
#                       iconWidth = 25, iconHeight = 25)
# 
# # Icons according to unique(sensors$emplacement)
# sensors_icons <- iconList('green_pin' = green_pin, 
#                      'black_pin' = black_pin, 
#                      'wind_turbine_pin' = wind_turbine_pin)
# # sensors_icons <- iconList("AcoustB" = acoust_pin, 
# #                      "mât" = mast_pin, 
# #                      "LIDAR" = lidar_pin)
wind_turbine_pin <- makeIcon(iconUrl = paste(dirname(getwd()), "www", "img", "wind_turbine.svg", sep = .Platform$file.sep),
                             iconWidth = 20, iconHeight = 30)
north_arrow_pin <- makeIcon(iconUrl = "https://upload.wikimedia.org/wikipedia/commons/8/84/North_Pointer.svg",
                            iconWidth = 20, iconHeight = 30)
#' 
#' @title Initialize Sensors Color Icons
#' @description This function initializes color-related columns for sensors and assigns 
#' corresponding icons based on sensor locations.
#' @param sensors_df A dataframe containing sensor information.
#' @return A modified dataframe with initialized color-related columns and assigned icons.
#' @details This function initializes the color-related columns for sensors, setting the 
#' initial color to black. It then assigns icons based on the sensor locations and updates 
#' the "color_icon" column accordingly.
#' @examples
#' \dontrun{
#' # Example usage
#' sensors_data <- ...  # Replace with your sensor dataframe
#' initialized_sensors <- init_sensors_color_icons(sensors_data)
#' }
#' @export
init_sensors_color_icons <- function(sensors_df){
  sensors_df$color_selected <- "black"
  sensors_df <- sensors_df %>% 
    mutate(color_icon = case_when(field == "acoustic" & system =="A1" ~ "acoustic_black_pin", 
                                  field == "acoustic" & system =="A2" ~ "acoustic_black_pin", 
                                  field == "acoustic" & system =="A3" ~ "acoustic_black_pin", 
                                  field == "acoustic" & system =="A4" ~ "acoustic_black_pin", 
                                  field == "acoustic" & system =="A5" ~ "acoustic_black_pin", 
                                  field == "meteo" & system == "mast" ~ "mast_black_pin", 
                                  field == "meteo" & system == "S1" ~ "sonic_black_pin", 
                                  field == "meteo" & system == "S2" ~ "sonic_black_pin", 
                                  field == "meteo" & system == "S3" ~ "sonic_black_pin", 
                                  field == "meteo" & system == "LIDAR" ~ "lidar_black_pin"))
  return(sensors_df)
}
#' 
#' @title Set Sensors Color and Icon
#' @description This function sets the color and icon for selected and non-selected sensors.
#' @param sensors_df A dataframe containing sensor information.
#' @param selected_sensors A character vector specifying the selected sensor names.
#' @return A modified dataframe with updated color and icon information for selected 
#' and non-selected sensors.
#' @details This function sets the color of selected sensors to green and non-selected sensors 
#' to red. It also updates the "color_icon" column based on the sensor locations and selected colors.
#' @examples
#' \dontrun{
#' # Example usage
#' sensors_data <- ...  # Replace with your sensor dataframe
#' selected_sensor_names <- c("sensor1", "sensor2")  # Replace with your selected sensor names
#' updated_sensors <- set_sensors_color_icon(sensors_data, selected_sensors = selected_sensor_names)
#' }
#' @export
set_sensors_color_icon <- function(sensors_df, selected_sensors){
  selected_sensors_list <- which(str_split_i(string = sensors_df$system, pattern = '_', i = 1) %in% str_to_upper(selected_sensors))
  sensors_df[selected_sensors_list, "color_selected"] <- "green"
  unselected_sensors_list <- which(!str_split_i(string = sensors_df$system, pattern = '_', i = 1) %in% str_to_upper(selected_sensors))
  sensors_df[unselected_sensors_list, "color_selected"] <- "red"
  sensors_df <- sensors_df %>% 
    mutate(color_icon = case_when(field == "acoustic" & color_selected == "green" ~ 'acoustic_green_pin',
                                  field == "acoustic" & color_selected == "red" ~ 'acoustic_red_pin',
                                  field == "meteo" & system == "mast" & color_selected == "green" ~ 'mast_green_pin',
                                  field == "meteo" & system == "mast" & color_selected == "red" ~ 'mast_red_pin',
                                  field == "meteo" & system == "S1" & color_selected == "green" ~ 'sonic_green_pin',
                                  field == "meteo" & system == "S1" & color_selected == "red" ~ 'sonic_red_pin',
                                  field == "meteo" & system == "S2" & color_selected == "green" ~ 'sonic_green_pin',
                                  field == "meteo" & system == "S2" & color_selected == "red" ~ 'sonic_red_pin',
                                  field == "meteo" & system == "S3" & color_selected == "green" ~ 'sonic_green_pin',
                                  field == "meteo" & system == "S3" & color_selected == "red" ~ 'sonic_red_pin',
                                  field == "meteo" & system == "LIDAR" & color_selected == "green" ~ 'lidar_green_pin',
                                  field == "meteo" & system == "LIDAR" & color_selected == "red" ~ 'lidar_red_pin'))
  return(sensors_df)
}
#' 
#' @title Sensors Icons Pins
#' @description This function generates a list of Awesome Icons for different sensor types and colors.
#' @return A list of Awesome Icons for acoustic, mast, and LIDAR sensors with black, green, and red colors.
#' @details This function uses the `awesomeIconList` function from the `leaflet` package to create icons 
#' for different sensor types (acoustic, mast, LIDAR) with variations in color (black, green, red).
#' @examples
#' \dontrun{
#' # Example usage
#' icons_list <- sensors_icons_pins()
#' }
#' @export
sensors_icons_pins <- function(){
  awesomeIconList("acoustic_black_pin" = makeAwesomeIcon(icon = "microphone",
                                                         library = "fa",
                                                         markerColor = "white",
                                                         iconColor = "black"),
                  "acoustic_green_pin" = makeAwesomeIcon(icon = "microphone",
                                                         library = "fa",
                                                         markerColor = "green",
                                                         iconColor = "black"),
                  "acoustic_red_pin" = makeAwesomeIcon(icon = "microphone",
                                                       library = "fa",
                                                       markerColor = "red",
                                                       iconColor = "black"),
                  "mast_black_pin" = makeAwesomeIcon(icon = "yen",
                                                     library = "fa",
                                                     iconRotate = 180,
                                                     markerColor = "white",
                                                     iconColor = "black"),
                  "mast_green_pin" = makeAwesomeIcon(icon = "yen",
                                                     library = "fa",
                                                     iconRotate = 180,
                                                     markerColor = "green",
                                                     iconColor = "black"),
                  "mast_red_pin" = makeAwesomeIcon(icon = "yen",
                                                   library = "fa",
                                                   iconRotate = 180,
                                                   markerColor = "red",
                                                   iconColor = "black"),
                  "lidar_black_pin" = makeAwesomeIcon(icon = "yen",
                                                      library = "fa",
                                                      iconRotate = 180,
                                                      markerColor = "white",
                                                      iconColor = "black"),
                  "lidar_green_pin" = makeAwesomeIcon(icon = "yen",
                                                      library = "fa",
                                                      iconRotate = 180,
                                                      markerColor = "green",
                                                      iconColor = "black"),
                  "lidar_red_pin" = makeAwesomeIcon(icon = "yen",
                                                    library = "fa",
                                                    iconRotate = 180,
                                                    markerColor = "red",
                                                    iconColor = "black"), 
                  "sonic_black_pin" = makeAwesomeIcon(icon = "yen",
                                                 library = "fa",
                                                 iconRotate = 180,
                                                 markerColor = "white",
                                                 iconColor = "black"), 
                  "sonic_green_pin" = makeAwesomeIcon(icon = "yen",
                                                      library = "fa",
                                                      iconRotate = 180,
                                                      markerColor = "green",
                                                      iconColor = "black"), 
                  "sonic_red_pin" = makeAwesomeIcon(icon = "yen",
                                                      library = "fa",
                                                      iconRotate = 180,
                                                      markerColor = "red",
                                                      iconColor = "black"))
}
#' 
#' @title Set Sensors URL and Image
#' @description This function sets the URL and image columns in the sensor DataFrame 
#' based on the sensor location.
#' @param sensors_df The sensor DataFrame.
#' @return The sensor DataFrame with added URL and image columns.
#' @details This function uses the `case_when` function from the `dplyr` package to set 
#' the URL and image columns (`url` and `img`) based on the sensor measurement field (`field`).
#' @examples
#' \dontrun{
#' # Example usage
#' sensors_data <- read.csv("sensors_data.csv")
#' sensors_data <- set_sensors_url_img(sensors_data)
#' }
#' @export
set_sensors_url_img <- function(sensors_df){
  sensors_df <- sensors_df %>% 
    mutate(
      url = case_when(field == "acoustic" & system =="A1" ~ "https://www.01db.com/fr/nos-solutions/nos-produits/station-de-mesure-du-bruit/", 
                      field == "acoustic" & system =="A2" ~ "https://www.01db.com/fr/nos-solutions/nos-produits/station-de-mesure-du-bruit/", 
                      field == "acoustic" & system =="A3" ~ "https://www.01db.com/fr/nos-solutions/nos-produits/station-de-mesure-du-bruit/", 
                      field == "acoustic" & system =="A4" ~ "https://www.01db.com/fr/nos-solutions/nos-produits/station-de-mesure-du-bruit/", 
                      field == "acoustic" & system =="A5" ~ "https://www.01db.com/fr/nos-solutions/nos-produits/station-de-mesure-du-bruit/", 
                      field == "meteo" & system == "mast" ~ "https://www.youngusa.com/product/ultrasonic-anemometer/", 
                      field == "meteo" & system == "S1" ~ "https://www.youngusa.com/product/ultrasonic-anemometer/", 
                      field == "meteo" & system == "S2" ~ "https://www.youngusa.com/product/ultrasonic-anemometer/", 
                      field == "meteo" & system == "S3" ~ "https://www.youngusa.com/product/ultrasonic-anemometer/", 
                      field == "meteo" & system == "LIDAR" ~ "https://www.zxlidars.com/wind-lidars/zx-300/"), 
      img = case_when(field == "acoustic" & system =="A1" ~ "https://storage.googleapis.com/am-corporate.appspot.com/1/2021/04/1BB-duo.png", 
                      field == "acoustic" & system =="A2" ~ "https://storage.googleapis.com/am-corporate.appspot.com/1/2021/04/1BB-duo.png", 
                      field == "acoustic" & system =="A3" ~ "https://storage.googleapis.com/am-corporate.appspot.com/1/2021/04/1BB-duo.png", 
                      field == "acoustic" & system =="A4" ~ "https://storage.googleapis.com/am-corporate.appspot.com/1/2021/04/1BB-duo.png", 
                      field == "acoustic" & system =="A5" ~ "https://storage.googleapis.com/am-corporate.appspot.com/1/2021/04/1BB-duo.png", 
                      field == "meteo" & system == "mast" ~ "https://www.youngusa.com/wp-content/uploads/2008/01/81000-White-Background-600x908.jpg", 
                      field == "meteo" & system == "S1" ~ "https://www.youngusa.com/wp-content/uploads/2008/01/81000-White-Background-600x908.jpg", 
                      field == "meteo" & system == "S2" ~ "https://www.youngusa.com/wp-content/uploads/2008/01/81000-White-Background-600x908.jpg", 
                      field == "meteo" & system == "S3" ~ "https://www.youngusa.com/wp-content/uploads/2008/01/81000-White-Background-600x908.jpg", 
                      field == "meteo" & system == "LIDAR" ~ "https://www.nrgsystems.com/assets/images-products/ZX-1__FitWzEyMDAsODAwXQ.jpg"))
  return(sensors_df)
}
#' 
#' @title Collapse Sensors Information
#' @description
#' This function takes a data frame of sensor information and collapses it based on the system.
#' It groups the information by the system, and for each group, it creates a summary including measurements, variables, heights, types, models, coordinates, colors, URLs, images, and longitudes.
#' @param sensors A data frame containing information about sensors.
#' @return
#' A collapsed data frame summarizing sensor information by system.
#' @examples
#' # Example:
#' sensors_data <- read.csv("path/to/sensors_info.csv")
#' collapsed_data <- collapse_sensors_info(sensors_data)
#' head(collapsed_data)
#' @importFrom dplyr group_by summarise ungroup
#' @importFrom stringr toString
# Collapse column values for each unique sensor
collapse_sensors_info <- function(sensors){
  collapsed_sensors <- sensors %>%
    group_by(system) %>%
    summarise(measurement = toString(paste0("<li>", unique(measurement), collapse = "</li>")),
              `variable(s)` = toString(unique(`variable(s)`)),
              `height (m)` = toString(unique(`height (m)`)),
              type = toString(unique(type)),
              model = toString(unique(model)),
              lon = as.numeric(unique(lon)),
              lat = as.numeric(unique(lat)),
              color_selected = toString(unique(color_selected)),
              color_icon = toString(unique(color_icon)),
              url = toString(unique(url)),
              img = toString(unique(img))) %>%
    ungroup()
  return(collapsed_sensors)
}
#' 
#' @title Initialize Map of Sensors
#' @description This function initializes a leaflet map centered around the geographical coordinates of the sensors.
#' @param sensors The sensor geospatial DataFrame.
#' @return An initialized leaflet map.
#' @details This function uses the `leaflet` package to create a leaflet map centered around the geographical coordinates 
#' of the sensors. The center and zoom level are determined based on the calculated centre point of the sensor locations.
#' @seealso \code{\link{get_geodf_centre_point}}
#' @examples
#' \dontrun{
#' # Example usage
#' sensors_data <- read.csv("sensors_geospatial_data.csv")
#' my_map <- init_map_of_sensors(sensors_data)
#' }
#' @export
init_map_of_sensors <- function(sensors, wind_turbines){
  sensors <- collapse_sensors_info(sensors)
  combined_sensors_wind_turbines <- rbind(sensors[, "geometry"], wind_turbines[, "geometry"])
  centre_point <- get_geodf_centre_point(geodf = combined_sensors_wind_turbines)
  my_map <- leaflet(data = combined_sensors_wind_turbines) %>% 
    setView(lng = centre_point$X, lat = centre_point$Y, zoom = 14)
  return(my_map)
}
#' 
#' @title Add Sensors to Map
#' @description This function adds sensors to an existing leaflet map.
#' @param my_map An existing leaflet map.
#' @param sensors The sensor DataFrame with geospatial information.
#' @return The updated leaflet map with added sensor markers.
#' @details This function uses the `addAwesomeMarkers` function from the `leaflet` 
#' package to add markers for each sensor to an existing leaflet map. The markers 
#' include information such as the sensor location, icon color, label, and popup 
#' content with detailed information about the sensor.
#' @seealso \code{\link{sensors_icons_pins}}
#' @examples
#' \dontrun{
#' # Example usage
#' sensors_data <- read.csv("sensors_geospatial_data.csv")
#' my_map <- init_map_of_sensors(sensors_data)
#' my_map <- add_sensors_to_map(my_map, sensors_data)
#' }
#' @export
add_sensors_to_map <- function(my_map, sensors){
  sensors <- collapse_sensors_info(sensors)
  sensors <- sensors %>% 
    mutate(rank_selection = case_when(str_equal(x = sensors$color_selected, y = "green") ~ 1, 
                                      str_equal(x = sensors$color_selected, y = "red") ~ 0.5))
  marker_options <- markerOptions(zIndexOffset = as.integer(sensors$rank_selection) * 100)  # opacity = as.integer(sensors$rank_selection)
  my_map <- my_map %>% 
    addAwesomeMarkers(data = sensors, lng = ~lon, lat = ~lat, 
                      icon = ~ sensors_icons_pins()[color_icon], 
                      label = ~ system, 
                      group = "Sensors", 
                      options = marker_options,
                      popup = ~paste0('<head> ',
                                      '  <title>Right Image</title> ',
                                      '    <style> ',
                                      '      body {margin:1px;} ',
                                      '      p    {text-align:justify;} ',
                                      '      h2   {text-align:left;} ',
                                      '      img  {float:right; margin-left:1px; margin-right:1px; width="2px"}',
                                      '    </style> ',
                                      '  </head> ',
                                      '    <body> ',
                                      '      <h2>Sensor: ', system, '</h2> ',
                                      '      <p> ',
                                      '        <img src="', img, '" alt="Picture of ', '" width="30px"> ',
                                      '        <ul>',
                                      '          <li><b>Model: </b>', `type`, '(', model, ')</li>',
                                      '          <li><b>Height: </b>', `height (m)`, 'm</li>',
                                      '          <li><b>Observable(s): </b>',
                                      '            <ul>', measurement, '</ul>',
                                      '          </li>',
                                      '        </ul>',
                                      '      <p> ',
                                      '      <a href="', url, '" target="_blank">', 'Click here to get sensor information</a>',
                                      '      </p>',
                                      '    </body>'))
}
#' 
#' @title Add Wind Turbines to Map
#' @description This function adds wind turbines to an existing leaflet map.
#' @param my_map An existing leaflet map.
#' @param wind_turbines The wind turbines DataFrame with geospatial information.
#' @return The updated leaflet map with added wind turbine markers.
#' @details This function uses the `addMarkers` function from the `leaflet` package 
#' to add markers for each wind turbine to an existing leaflet map. The markers 
#' include information such as the wind turbine location, label, and a default icon.
#' @seealso \code{\link{wind_turbine_pin}}
#' @examples
#' \dontrun{
#' # Example usage
#' turbines_data <- read.csv("wind_turbines_geospatial_data.csv")
#' my_map <- init_map_of_sensors(sensors_data)
#' my_map <- add_wind_turbines_to_map(my_map, turbines_data)
#' }
#' @export
add_wind_turbines_to_map <- function(my_map, wind_turbines){
  my_map <- my_map %>% addMarkers(lng = wind_turbines$lon, 
                                  lat = wind_turbines$lat, 
                                  label = paste('Wind turbine', wind_turbines$name, sep = ' '), 
                                  icon = wind_turbine_pin, group = "Wind turbines")
  return(my_map)
}
#' 
#' @title Add Scale Bar to Map
#' @description This function adds a scale bar to an existing leaflet map.
#' @param my_map An existing leaflet map.
#' @return The updated leaflet map with added scale bar.
#' @details This function uses the `addScaleBar` function from the `leaflet` package 
#' to add a scale bar to the bottom right corner of an existing leaflet map. The 
#' scale bar provides information about the distance on the map in both metric and 
#' imperial units.
#' @examples
#' \dontrun{
#' # Example usage
#' my_map <- init_map_of_sensors(sensors_data)
#' my_map <- add_scale_bar_to_map(my_map)
#' }
#' @export
add_scale_bar_to_map <- function(my_map){
  my_map <- my_map %>% addScaleBar(position = "bottomright", 
                                   options = scaleBarOptions(maxWidth = 100, 
                                                             metric = TRUE, 
                                                             imperial = FALSE, 
                                                             updateWhenIdle = TRUE))
  return(my_map)
}
#' 
#' @title Add Grid to Map
#' @description This function adds a grid (graticule) to an existing leaflet map.
#' @param my_map An existing leaflet map.
#' @return The updated leaflet map with added grid.
#' @details This function uses the `clearGroup` and `addSimpleGraticule` functions 
#' from the `leaflet` package to add a grid to the map. The grid is defined by the 
#' specified interval, and it includes latitude and longitude lines. The grid is 
#' added to the "Grid" group on the map.
#' @examples
#' \dontrun{
#' # Example usage
#' my_map <- init_map_of_sensors(sensors_data)
#' my_map <- add_grid_to_map(my_map)
#' }
#' @export
add_grid_to_map <- function(my_map){
  my_map <- my_map %>% 
    clearGroup(group = "Grid") %>% 
    addSimpleGraticule(interval = 0.01, showOriginLabel = FALSE, redraw = "move", 
                       hidden = FALSE, group = "Grid")
  return(my_map)
}
#' 
#' @title Add Grid to Map Without Labels
#' @description This function adds a grid (graticule) to an existing leaflet map 
#' without displaying labels.
#' @param my_map An existing leaflet map.
#' @return The updated leaflet map with added grid without labels.
#' @details This function uses the `clearGroup` and `addSimpleGraticule` functions 
#' from the `leaflet` package to add a grid to the map. The grid is defined by the 
#' specified interval and includes latitude and longitude lines. Labels for the 
#' grid lines are not displayed. The grid is added to the "Grid" group on the map.
#' @examples
#' \dontrun{
#' # Example usage
#' my_map <- init_map_of_sensors(sensors_data)
#' my_map <- add_grid_to_map_without_labels(my_map)
#' }
#'
#' @export
add_grid_to_map_without_labels <- function(my_map){
  my_map <- my_map %>% 
    clearGroup(group = "Grid") %>% 
    addSimpleGraticule(interval = 0.01, showOriginLabel = FALSE, redraw = "move", 
                       hidden = FALSE, group = "Grid")
  return(my_map)
}
#' 
#' @title Add North Arrow to Map
#' @description This function adds a north arrow marker to an existing leaflet map.
#' @param my_map An existing leaflet map.
#' @return The updated leaflet map with added north arrow.
#' @details This function uses the `addMarkers` function from the `leaflet` package 
#' to add a marker representing the north direction to the map. The marker is placed 
#' at the maximum latitude and longitude coordinates of the map. The icon used for 
#' the north arrow is specified in the `north_arrow_pin` variable.
#' @examples
#' \dontrun{
#' # Example usage
#' my_map <- init_map_of_sensors(sensors_data)
#' my_map <- add_north_arrow_to_map(my_map)
#' }
#' @export
add_north_arrow_to_map <- function(my_map){
  my_map <- my_map %>% addMarkers(lat = max(my_map$x$limits$lat), 
                                  lng = max(my_map$x$limits$lng), 
                                  label = "North direction", 
                                  icon = north_arrow_pin)
  return(my_map)
}
#' 
#' @section functions_html: HTML documents utilities
#' 
#' @title Create Downloading Page
#' @description This function generates an HTML page with information about the 
#' downloading process, including the specified date range, acoustic indicators, 
#' acoustic sensors, and frequencies.
#' @param input A list or data frame containing input parameters such as 
#' start_date_download, end_date_download, start_time_download, end_time_download, 
#' acoustic_indicators, acoustic_sensors, and acoustic_frequencies.
#' @return An HTML page with information about the downloading process.
#' @details This function creates an HTML page with a header displaying the ANR-PIBE 
#' logo and a loading animation. It includes details about the specified date range, 
#' acoustic indicators, acoustic sensors, and frequencies. The information is 
#' formatted and displayed in a structured way.
#' @examples
#' \dontrun{
#' # Example usage
#' input_data <- list(
#'   start_date_download = "2023-01-01",
#'   end_date_download = "2023-01-31",
#'   start_time_download = "00:00:00",
#'   end_time_download = "23:59:59",
#'   acoustic_indicators = c("indicator1", "indicator2"),
#'   acoustic_sensors = c("sensor1", "sensor2"),
#'   acoustic_frequencies = c("frequency1", "frequency2")
#' )
#' html_page <- downloading_page(input_data)
#' }
#' @export
downloading_page <- function(input){
  the_start_date <- as.Date(x = input$start_date_download, format = "%Y-%m-%d")
  the_start_time <- update(as.POSIXct(x = input$start_time_download , 
                                      format = "%Y-%m-%d %H:%M:%S CEST"), 
                           year = year(the_start_date), 
                           month = month(the_start_date), 
                           mday = day(the_start_date))
  the_end_date <- as.Date(x = input$end_date_download, format = "%Y-%m-%d")
  the_end_time <- update(as.POSIXct(x = input$end_time_download , 
                                    format = "%Y-%m-%d %H:%M:%S CEST"), 
                         year = year(the_end_date), 
                         month = month(the_end_date), 
                         mday = day(the_end_date))
  pibe_logo <- paste("..", "img", "anr-pibe-logo.gif", sep = .Platform$file.sep)
  loading_dog <- paste("..", "img", "loading-dog.gif", sep = .Platform$file.sep)
  # pibe_logo <- paste(dirname(getwd()), "www", "img", "anr-pibe-logo.gif", sep = .Platform$file.sep)
  # loading_dog <- paste(dirname(getwd()), "www", "img", "loading-dog.gif", sep = .Platform$file.sep)
  html_page <- sprintf({
    '<p><img style="width:201px;height:45px;display: block; margin-left: auto; margin-right: auto;" 
    src="%s">
    </p><h3 style="text-align: center;">Downloading data ...</p>
    <p><img style="width:201px;display: block; margin-left: auto; margin-right: auto;" 
    src="%s">
    </p><h5 style="text-align: left;"><ul>
    <li>dates: %s to %s</li>
    <li>acoustic indicators: %s</li>
    <li>acoustic_sensors: %s</li>
    <li>frequencies: %s</li>
    </ul></h5><hr /> </p>'}, 
    PIBE_LOGO = pibe_logo, 
    LOADING = loading_dog, 
    START_TIME = the_start_time, 
    END_TIME = the_end_time, 
    ACOUSTIC_INDICATORS = paste(input$acoustic_indicators, collapse = ", "), 
    ACOUSTIC_SENSORS = paste(input$acoustic_sensors, collapse = ", "), 
    ACOUSTIC_FREQUENCIES = paste(input$acoustic_frequencies, collapse = ", "))
  return(html_page)
}










#' 
#' @title Shiny Modal Options UI Function
#' @description This function creates a Shiny modal options UI element.
#' @param id The ID of the modal options.
#' @return A Shiny UI element containing an action button to show the modal.
#' @examples
#' # Example usage:
#' ui <- modal.options.UI("my_modal")
#' @export
modal.options.UI<-function(id){
  ns <- NS(id)
  tagList(actionButton(ns("showModal"),"Show"))
}
#' 
#' @title Shiny Modal Options Function
#' @description This function creates a Shiny modal with options and handles user interactions.
#' @param input The input object from the Shiny app.
#' @param output The output object from the Shiny app.
#' @param session The Shiny session object.
#' @param options A list containing the modal options.
#' @return A modified list of options based on user interactions.
#' @examples
#' # Example usage:
#' options <- modal.options(input, output, session, list(check1 = FALSE))
#' @export
modal.options<-function(input,output,session,options){
  modal <- function(ns, options){
    modalDialog(
      size = "s",
      title = "Upload Data",
      easyClose = TRUE,
      fade = TRUE,
      checkboxInput(ns("check1"), "Option 1", value = options$check1),
      footer = tagList(actionButton(ns("submit"), "Submit", width = "100%"))
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
#' 
#' @section global_options: Global options
#' 
#' Page refresh button
jscode <- "shinyjs.refresh = function() { history.go(0); }"
#' 
#' Global options change to show milliseconds
my_options <- options(digits.secs = 3, quote = TRUE, 
                      shiny.maxRequestSize = 10 * 1024^2)
#' 
#' @section server_config: Remote server configuration
#' Remote server configuration
#' 
#' ElasticSearch parameters
config_file <- here::here(paste(dirname(getwd()), "www", "config.yml", sep = .Platform$file.sep))
#' 
#' ElasticSearch default parameters
default_params <- list(
  host = "localhost",
  port = 9200,
  transport_schema = "http",
  user = "",
  pwd = "",
  path = "",
  cainfo = ""
)
#'
#' @title Try to Load Configuration Parameters
#' @param config_file The path to the configuration file.
#' @param default_params The default parameters to be used if loading fails.
#' @return A list of configuration parameters.
#' @examples
#' # Example usage:
#' es_params <- try_load_config(file = "path/to/config.yaml", default_params = list(param1 = 1, param2 = "default"))
#' @export
es_params <- tryCatch(
  config::get(file = config_file),
  error = function(e) default_params
)
#' 
#' @section ui_init: User interface initialization
#' 
#' Selected data query parameters initialization
the_start_date <- "2020/07/21"
the_start_time <- "10:00"
the_end_date <- "2020/07/24"
the_end_time <- "01:00"
elastic_conn <- connect(host = es_params$host, port = es_params$port, user = es_params$user, pwd = es_params$pwd, 
                        path=es_params$path, transport_schema = es_params$transport_schema, cainfo = es_params$cainfo)
indexes_info <- get_indexes_info(elastic_conn)

scientific_domains <- c(sort(unique(indexes_info$field)), "both")
time_samplings <- sort(unique(indexes_info$timesampling))

acoustic_sensors <- get_sensors_for_field(indexes_info = indexes_info, wanted_field = 'acoustic', avoided_sensors = 'extraneous_noise')
meteo_sensors <- get_sensors_for_field(indexes_info = indexes_info, wanted_field = 'meteo')

acoustic_indexes <- get_indexes_names(indexes_info=indexes_info, field='acoustic')
meteo_indexes <- get_indexes_names(indexes_info=indexes_info, field='meteo')

# Mappings of acoustic indexes
acoustic_mappings <- get_indexes_mappings(conn = elastic_conn, indexes = acoustic_indexes)
# Acoustic variables names
acoustic_variables <- get_common_fields_variables(mappings_list = acoustic_mappings)
# Mappings of meteo indexes
meteo_mappings <- get_indexes_mappings(conn = elastic_conn, indexes = meteo_indexes)
# Meteo variables names
meteo_variables <- get_meteo_variables(mappings_list = meteo_mappings)
init <- list(selection = list(downloadFolder = paste(dirname(getwd()), "www", "data", sep = .Platform$file.sep),
                              start_date = the_start_date,
                              start_time = the_start_time,
                              start_date_time =  format_datetime_for_ui(the_start_date, the_start_time),
                              end_date = the_end_date,
                              end_time = the_end_time,
                              end_date_time =  format_datetime_for_ui(the_end_date, the_end_time),
                              scientific_domain = c("acoustic"),
                              time_sampling = "10min",
                              acoustic_sensors = acoustic_sensors[1],
                              acoustic_indicators = acoustic_variables$indicators[1],
                              acoustic_frequencies = acoustic_variables$frequencies[1:2],
                              meteo_sensors = NULL,
                              meteo_variables = NULL),
             all_choices = list(scientific_domains = scientific_domains,
                                time_samplings = time_samplings,
                                acoustic_sensors = acoustic_sensors,
                                acoustic_indicators = acoustic_variables$indicators,
                                acoustic_frequencies = acoustic_variables$frequencies,
                                facet_row = c(None='.', c("sensor", "indicators")),
                                meteo_sensors = meteo_sensors,
                                meteo_variables = meteo_variables,
                                scientific_domain = c('acoustic', 'meteo', 'both')))
