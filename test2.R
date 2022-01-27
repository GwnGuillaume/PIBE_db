library(shiny)
library(shinydashboard)

# ui ---------------------------------------------------------------------------

ui <- dashboardPage(
  
  # title ----
  dashboardHeader(title = "Test Application"),
  
  # sidebar ----
  dashboardSidebar(
    sidebarMenu(id = "sidebarid",
                menuItem("Page 1", tabName = "page1"),
                menuItem("Page 2", tabName = "page2"),
                conditionalPanel(
                  'input.sidebarid == "page2"',
                  sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
                  selectInput("title", "Select plot title:", choices = c("Hist of x", "Histogram of x"))
                )
    )
  ),
  
  # body ----
  dashboardBody(
    tabItems(
      # page 1 ----
      tabItem(tabName = "page1", "Page 1 content. This page doesn't have any sidebar menu items."),
      # page 2 ----
      tabItem(tabName = "page2", 
              "Page 2 content. This page has sidebar meny items that are used in the plot below.",
              br(), br(),
              plotOutput("distPlot"))
    )
  )
)

# server -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "darkgray", border = "white", main = input$title)
  })
  
}

shinyApp(ui, server)