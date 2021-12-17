library(ggplot2)
library(scales)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  # Pibe project presentation (tabName = "pibe_project")
  pibe_logo = "https://www.anr-pibe.com/sites/pibe/files/anr-pibe-logo.gif"
  output$pibe_logo<-renderText({c('<img src="', pibe_logo, '">')})
  
  # Visualization
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
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

  dataset <- reactive({
    the_start_date = as.Date(x = input$start_date, format = "%Y-%m-%d")
    the_start_date_time = update(as.POSIXct(x = input$start_time, format = "%Y-%m-%d %H:%M:%S"), year = year(the_start_date), month = month(the_start_date), mday = day(the_start_date))
    the_end_date = as.Date(x = input$end_date, format = "%Y-%m-%d")
    the_end_date_time = update(as.POSIXct(x = input$end_time, format = "%Y-%m-%d %H:%M:%S"), year = year(the_end_date), month = month(the_end_date), mday = day(the_end_date))
    print("\n Reloading data")
    data <- get_data(conn = conn,
             start_date = format(as.POSIXct(x = input$start_date, format = "%Y-%m-%d"), "%Y/%m/%d"),
             start_time = format(as.POSIXct(x = the_start_date_time, format = "%Y-%m-%d %H:%M:%S"), "%H:%M"),  # input$start_time,
             end_date = format(as.POSIXct(x = input$end_date, format = "%Y-%m-%d"), "%Y/%m/%d"),
             end_time = format(as.POSIXct(x = the_end_date_time, format = "%Y-%m-%d %H:%M:%S"), "%H:%M"),  # input$end_time
             fields = input$fields,
             timesampling = input$timesampling,
             sensors = input$sensors,
             indicators = input$indicators,
             frequencies = input$frequencies)
  })


  output$plot <- renderPlot({

    p <- ggplot(dataset(), aes_string(x='timestamp', y='value', group = 'indicators', colour = 'indicators')) +
      geom_line() +
      scale_x_datetime(labels = date_format(format = "%y/%m/%d-%H:%M"),
                       breaks=as.POSIXct(seq(from = as.POSIXct(x = input$time_slider[[1]], format = "%Y-%m-%d %H:%M:%S"), to = as.POSIXct(x = input$time_slider[[2]], format = "%Y-%m-%d %H:%M:%S"), by = (24*3600))),
                       limits = c(as.POSIXct(x = input$time_slider[[1]], format = "%Y-%m-%d %H:%M:%S"), as.POSIXct(x = input$time_slider[[2]], format = "%Y-%m-%d %H:%M:%S"))) +
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
