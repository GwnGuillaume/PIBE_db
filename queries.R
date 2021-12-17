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
  # # print("indexes:")
  # # print(indexes)
  # for (idt in seq(1, length(tmp_data))) {
  #   print(idt)
  #   tmp_val = as.numeric(tmp_data[[idt]]$fields[indicators][[1]])
  #   if(identical(tmp_val, numeric(0))) {
  #     tmp_val = NA
  #   }
  #   tmp_timestamp = as.POSIXct(tmp_data[[idt]]$`_source`$`@timestamp`)
  #   es.init.index = tmp_data[[idt]]$'_index'
  #   es.init.sensor <- str_split(es.init.index, pattern = "_", n = 3)[[1]][2]
  #   df <- df %>% add_row(timestamp = tmp_timestamp,
  #                        sensor = es.init.sensor,
  #                        indicators = indicators,
  #                        value = tmp_val)
  # }
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



