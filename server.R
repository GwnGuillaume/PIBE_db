library(ggplot2)
library(scales)

server <- function(input, output) {
  
  # Pibe project presentation (tabName = "pibe_project")
  pibe_logo = "https://www.anr-pibe.com/sites/pibe/files/anr-pibe-logo.gif"
  output$pibe_logo<-renderText({c('<img src="', pibe_logo, '">')})
  
  # Visualization
  output$plot1 <- renderPlot({
    set.seed(122)
    histdata <- rnorm(500)
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  # Download data from PIBE server
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(getwd(), "/data/", "data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # <img style="display: block; margin-left: auto; margin-right: auto;" 
      # src="https://www.anr-pibe.com/sites/pibe/files/anr-pibe-logo.gif?w=101px&h=45px&mode=crop&scale=both" />
      code_txt <- sprintf('<p><img style="width:201px;height:45px;display: block; margin-left: auto; margin-right: auto;" src="https://www.anr-pibe.com/sites/pibe/files/anr-pibe-logo.gif">
                          </p><h3 style="text-align: center;">Downloading data ...</p>
                          <p><img style="width:201px;display: block; margin-left: auto; margin-right: auto;" src="http://northerntechmap.com/assets/img/loading-dog-final.gif">
                          </p><h5 style="text-align: left;"><ul>
                          <li>dates: %s to %s</li>
                          <li>indicators: %s</li>
                          <li>sensors: %s</li>
                          <li>frequencies: %s</li>
                          </ul></h5><hr /> </p>', input$start_time, input$end_time, paste(input$indicators, collapse = ", "), paste(input$sensors, collapse = ", "), paste(input$frequencies, collapse = ", "))
      html_fp = paste(getwd(), "html", "download.html", sep="/")
      write.table(code_txt, file=html_fp, quote = FALSE, col.names = FALSE, row.names = FALSE)
      showModal(modalDialog(includeHTML(html_fp), footer="Cancel", size = "m", easyClose = TRUE))
      # showModal(modalDialog("Downloading data...",
      #                       footer="Cancel", size = "s", easyClose = TRUE))
      on.exit(removeModal())
      data <- download_data(conn, input)
      write.csv(data, file = file, row.names = FALSE)
    }
  )
  
  # Visualization
  # Avoid chain reaction
  reactdelay <- 1
  
  change_time_slider <- reactive({
    update(as.POSIXct(x = input$start_time, format = "%Y-%m-%d %H:%M:%S"), year = year(as.Date(x = input$start_date, format = "%Y-%m-%d")), month = month(as.Date(x = input$start_date, format = "%Y-%m-%d")), mday = day(as.Date(x = input$start_date, format = "%Y-%m-%d")))
  })
  
  observeEvent(input$time_slider, {
    the_start_date = as.Date(x = input$start_date, format = "%Y-%m-%d")
    the_start_date_time = update(as.POSIXct(x = input$start_time, format = "%Y-%m-%d %H:%M:%S"), year = year(the_start_date), month = month(the_start_date), mday = day(the_start_date))
    if (difftime(the_start_date_time, change_time_slider()) > reactdelay) {
      change_daterange(the_start_date_time)
      updateDateRangeInput(session,
                           "daterange",
                           start = input$time_slider[[1]],
                           end = input$time_slider[[2]])
    }
  })
  
  values <- reactiveValues(default = 0)
  observeEvent(input$refresh,{
    values$default <- input$refresh
  })
  
  output$plot <- renderPlot({
    print("downloadData")
    print(input$downloadData)
    data_path = paste(getwd(), "data", sep = "/")
    if (values$default == 0) {
      file_path = paste(data_path, "sample.csv", sep = "/")
    } else if (values$default == 1) {
      data_paths = list.files(path = data_path, full.names=TRUE)
      file_path = data_paths[which(file.info(data_paths)$ctime==max(file.info(data_paths)$ctime))]
    }
    print(paste("\n Loading file", file_path, sep = " "))
    dataset <- read_csv(file = file_path, col_names = TRUE)
    p <- ggplot(dataset, aes_string(x='timestamp', y='value')) +
      geom_line() +
      scale_x_datetime(labels = date_format(format = "%y/%m/%d-%H:%M"),
                       breaks=as.POSIXct(seq(from = as.POSIXct(x = input$time_slider[[1]], format = "%Y-%m-%d %H:%M:%S"), 
                                             to = as.POSIXct(x = input$time_slider[[2]], format = "%Y-%m-%d %H:%M:%S"), 
                                             by = (24*3600))),
                       limits = c(as.POSIXct(x = input$time_slider[[1]], format = "%Y-%m-%d %H:%M:%S"), 
                                  as.POSIXct(x = input$time_slider[[2]], format = "%Y-%m-%d %H:%M:%S"))) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      xlab(label = "Time") + ylab(label = "Sound level [dB(A)]")
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    if (input$jitter)
      p <- p + geom_jitter()
    if (input$smooth)
      p <- p + geom_smooth()
    
    print(p)
    
  }, height=700)
}

# library(shiny)
# library(ggplot2)
# library(scales)
# 
# function(input, output, session) {
#   
#   ## Avoid chain reaction
#   reactdelay <- 1
#   
#   change_slider <- reactive({
#     update(as.POSIXct(x = input$start_time, format = "%Y-%m-%d %H:%M:%S"), year = year(as.Date(x = input$start_date, format = "%Y-%m-%d")), month = month(as.Date(x = input$start_date, format = "%Y-%m-%d")), mday = day(as.Date(x = input$start_date, format = "%Y-%m-%d")))
#   })
# 
#   observeEvent(input$slider, {
#     the_start_date = as.Date(x = input$start_date, format = "%Y-%m-%d")
#     the_start_date_time = update(as.POSIXct(x = input$start_time, format = "%Y-%m-%d %H:%M:%S"), year = year(the_start_date), month = month(the_start_date), mday = day(the_start_date))
#     if (difftime(the_start_date_time, change_slider()) > reactdelay) {
#       change_daterange(the_start_date_time)
#       updateDateRangeInput(session,
#                            "daterange",
#                            start = input$slider[[1]],
#                            end = input$slider[[2]])
#     }
#   })
#   
#   dataset <- reactive({
#     the_start_date = as.Date(x = input$start_date, format = "%Y-%m-%d")
#     the_start_date_time = update(as.POSIXct(x = input$start_time, format = "%Y-%m-%d %H:%M:%S"), year = year(the_start_date), month = month(the_start_date), mday = day(the_start_date))
#     the_end_date = as.Date(x = input$end_date, format = "%Y-%m-%d")
#     the_end_date_time = update(as.POSIXct(x = input$end_time, format = "%Y-%m-%d %H:%M:%S"), year = year(the_end_date), month = month(the_end_date), mday = day(the_end_date))
#     print("\n Reloading data")
#     data <- get_data(conn = conn, 
#              start_date = format(as.POSIXct(x = input$start_date, format = "%Y-%m-%d"), "%Y/%m/%d"), 
#              start_time = format(as.POSIXct(x = the_start_date_time, format = "%Y-%m-%d %H:%M:%S"), "%H:%M"),  # input$start_time, 
#              end_date = format(as.POSIXct(x = input$end_date, format = "%Y-%m-%d"), "%Y/%m/%d"), 
#              end_time = format(as.POSIXct(x = the_end_date_time, format = "%Y-%m-%d %H:%M:%S"), "%H:%M"),  # input$end_time
#              fields = input$fields, 
#              timesampling = input$timesampling, 
#              sensors = input$sensors, 
#              indicators = input$indicators, 
#              frequencies = input$frequencies)
#     print(data)
#   })
#   
#   
#   output$plot <- renderPlot({
#     
#     print("\n Output:")
#     print(input$start_date)
#     print(input$start_time)
#     print(input$end_date)
#     print(input$end_time)
#     print(input$timesampling)
#     print(dataset())
#     
#     p <- ggplot(dataset(), aes_string(x='timestamp', y='value', group = 'indicators', colour = 'indicators')) +
#       geom_line() + 
#       scale_x_datetime(labels = date_format(format = "%y/%m/%d-%H:%M"), 
#                        breaks=as.POSIXct(seq(from = as.POSIXct(x = input$slider[[1]], format = "%Y-%m-%d %H:%M:%S"), to = as.POSIXct(x = input$slider[[2]], format = "%Y-%m-%d %H:%M:%S"), by = (24*3600))),
#                        limits = c(as.POSIXct(x = input$slider[[1]], format = "%Y-%m-%d %H:%M:%S"), as.POSIXct(x = input$slider[[2]], format = "%Y-%m-%d %H:%M:%S"))) + 
#       theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#       xlab(label = "Time") + ylab(label = "Sound level [dB(A)]")
# 
#     if (input$color != 'None')
#       p <- p + aes_string(color=input$color)
#     
#     facets <- paste(input$facet_row, '~', input$facet_col)
#     if (facets != '. ~ .')
#       p <- p + facet_grid(facets)
#     
#     if (input$jitter)
#       p <- p + geom_jitter()
#     if (input$smooth)
#       p <- p + geom_smooth()
#     
#     print(p)
#     
#   }, height=700)
#   
# }
