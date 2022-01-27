library(shiny)
library(shinydashboard)
library(data.table)
library(DT)

ui <- dashboardPage(title = "Dashboard Title",
                    dashboardHeader(title = "My Dashboard"),
                    dashboardSidebar(
                      sidebarMenu(id = "menu",
                                  menuItem(text = "Data", icon = icon("database"), 
                                           fileInput(inputId = "file", label = "Choose CSV File",
                                                     multiple = TRUE,
                                                     accept = ".csv")
                                  ),
                                  menuItem(text = "My Items",  icon = icon("book"),
                                           menuSubItem(text = "Full Table", tabName = "inputData"),
                                           menuSubItem(text = "Item 1", tabName = "item01"),
                                           menuSubItem(text = "Item 2", tabName = "item02"),
                                           menuSubItem(text = "Item 3", tabName = "item03")
                                  )
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "inputData", class = "active",
                                h1(textOutput("heading")),
                                dataTableOutput("loaded.data")),
                        tabItem(tabName = "items", h1(textOutput("heading0")), class = "active",
                                tabItem(tabName = "item01", class = "active", h1(textOutput("heading1")), dataTableOutput("table1")),
                                tabItem(tabName = "item02", class = "active", h1(textOutput("heading2")), dataTableOutput("table2")),
                                tabItem(tabName = "item03", class = "active", h1(textOutput("heading3")), dataTableOutput("table3"))
                        )
                      )
                    )
)

server <- function(input, output) {
  # Load the data and assign it to a reactive object
  df <- reactive({
    inFile <- input$file
    if(is.null(inFile)) {
      return(NULL)
    } else {
      tbl <- fread(input$file$datapath, sep = ",", quote = '"', stringsAsFactors = TRUE)
      return(tbl)
    }
  })
  
  output$heading <- renderText({
    if(is.null(df())) {
      return(NULL)
    } else {
      return("Data loaded")
    }
  })
  
  output$loaded.data <- renderDT({
    if(is.null(df())) {
      return(NULL)
    } else {
      df()
    }
  })
  
  output$heading0 <- renderText({
    if(is.null(df())) {
      return(NULL)
    } else {
      return("In the sub-menus below you will find the tables")
    }
  })
  
  output$heading1 <- renderText({
    if(is.null(df())) {
      return(NULL)
    } else {
      return("Heading item 1")
    }
  })
  
  output$table1 <- renderDT({
    if(is.null(df())) {
      return(NULL)
    } else {
      return(df()[ , c("VAR1", "VAR2")])
    }
  })
  
  output$heading2 <- renderText({
    if(is.null(df())) {
      return(NULL)
    } else {
      return("Heading item 2")
    }
  })
  
  output$table2 <- renderDT({
    if(is.null(df())) {
      return(NULL)
    } else {
      return(df()[ , c("VAR2", "VAR3")])
    }
  })
  
  output$heading3 <- renderText({
    if(is.null(df())) {
      return(NULL)
    } else {
      return("Heading item 3")
    }
  })
  
  output$table3 <- renderDT({
    if(is.null(df())) {
      return(NULL)
    } else {
      return(df()[ , c("VAR2", "VAR3")])
    }
  })
  
}

shinyApp(ui, server)


# library(shiny)
# library(shinydashboard)
# library(shinyjs)
# 
# ui <- dashboardPage(
#   dashboardHeader(), 
#   dashboardSidebar(
#     sidebarMenu(
#       id = "sidebarID",
#       menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
#       menuItem("Widgets", icon = icon("th"), tabName = "widgets"),
#       menuItem("Charts", id = "chartsID", tabName = "charts", icon = icon("bar-chart-o"), expandedName = "CHARTS",
#                menuSubItem("Sub-item 1", tabName = "subitem1"),
#                menuSubItem("Sub-item 2", tabName = "subitem2")
#       ),
#       hidden(menuItem("hiddenCharts", tabName = "hiddenCharts"))
#     )
#   ),
#   dashboardBody(
#     useShinyjs(),
#     tabItems(
#       tabItem("dashboard", "Dashboard tab content"),
#       tabItem("widgets", "Widgets tab content"),
#       tabItem("hiddenCharts", "Charts Tab"),
#       tabItem("subitem1", "Sub-item 1 tab content"),
#       tabItem("subitem2", "Sub-item 2 tab content") 
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   observeEvent(input$sidebarItemExpanded, {
#     if(input$sidebarItemExpanded == "CHARTS"){
#       updateTabItems(session, "sidebarID", selected = "hiddenCharts")
#     }
#   })
# }
# 
# shinyApp(ui, server)
# 
# result
# 
# 
# # library(shiny)
# # library(shinydashboard)
# # #submodules
# # submodule_ui <- function(id,tabName){
# #   ns <- NS(id)
# #   
# #   tabItem(
# #     tabName = tabName,
# #     box(
# #       title = "some title",
# #       textOutput(ns("some_output"))
# #     )
# #   )
# #   
# # }
# # 
# # submodule_server <- function(id,see){
# #   moduleServer(
# #     id,
# #     function(input, output, session){
# #       output$some_output <- renderText({
# #         see
# #       })
# #     }
# #   )
# # }
# # 
# # 
# # #module
# # module_ui <- function(id,tabName1,tabName2){
# #   ns <- NS(id)
# #   ### tabsItems now produced in module, submodules separated by comma
# #   tabItems(
# #     submodule_ui(ns("sub1"),
# #                  tabName = tabName1),
# #     submodule_ui(ns("sub2"),
# #                  tabName = tabName2)
# #   )
# #   
# #   
# # }
# # 
# # module_server <- function(id){
# #   moduleServer(
# #     id,
# #     function(input, output, session){
# #       submodule_server("sub1","hello")
# #       submodule_server("sub2","world !")
# #     }
# #   )
# # }
# # 
# # 
# # #app
# # 
# # ui <- dashboardPage(
# #   header = dashboardHeader(
# #     title = "dummy app"
# #   ),
# #   sidebar = dashboardSidebar(
# #     sidebarMenu(
# #       menuItem(
# #         text = "submodule1",
# #         tabName = "subtab1"
# #       ),
# #       menuItem(
# #         text = "submodule2",
# #         tabName = "subtab2"
# #       )
# #     )
# #   ),
# #   body = dashboardBody(
# #     
# #     module_ui(
# #       id = "module",
# #       tabName1 = "subtab1",
# #       tabName2 = "subtab2"
# #       
# #     )
# #   )
# # )
# # 
# # server <- function(input, output){
# #   module_server("module")
# # }
# # 
# # shinyApp(ui,server)
# # 
# # 
# # # library(shiny)
# # # library(shinydashboard)
# # # 
# # # ui <- dashboardPage(
# # #   dashboardHeader(),
# # #   dashboardSidebar(
# # #     sidebarMenu(id = "sidebarmenu",
# # #                 menuItem("A", tabName = "a",  icon = icon("group", lib="font-awesome")),
# # #                 menuItem("B", tabName = "b", icon = icon("check-circle", lib = "font-awesome")),
# # #                 conditionalPanel("input.sidebarmenu === 'b'",
# # #                                  sliderInput("b", "Under sidebarMenu", 1, 100, 50)
# # #                 )
# # #     ),
# # #     sliderInput("x", "Outside of menu", 1, 100, 50)
# # #   ),
# # #   dashboardBody()
# # # )
# # # 
# # # server <- function(input, output) {}
# # # 
# # # shinyApp(ui, server)
# # # 
# # # 
# # # 
# # # # library(shiny)
# # # # 
# # # # 
# # # # modal.options.UI<-function(id){
# # # #   print("id")
# # # #   print(id)
# # # #   ns <- NS(id)
# # # #   print("NS")
# # # #   print(ns)
# # # #   tagList(
# # # #     actionButton(ns("showModal"), "Show")
# # # #   )
# # # #   
# # # # }
# # # # 
# # # # modal.options<-function(input,output,session,options){
# # # #   modal<-function(ns,options){
# # # #     modalDialog(
# # # #       size = "s",
# # # #       title = "Upload Data",
# # # #       easyClose = TRUE,
# # # #       fade = TRUE,
# # # #       checkboxInput(ns("check1"), "Option 1", value = options$check1),
# # # #       footer=tagList(
# # # #         actionButton(ns("submit"), "Submit", width = "100%")
# # # #       )
# # # #     )
# # # #   }
# # # #   
# # # #   observeEvent(input$showModal,{
# # # #     ns <- session$ns
# # # #     print("ns")
# # # #     print(ns)
# # # #     showModal(modal(ns=ns, options))
# # # #   })
# # # #   
# # # #   observeEvent(input$submit,{
# # # #     print("input$submit")
# # # #     print(input$submit)
# # # #     options$check1<-input$check1
# # # #     print("input$check1")
# # # #     print(input$check1)
# # # #     removeModal()
# # # #   })
# # # #   
# # # #   return(options)
# # # # }
# # # # 
# # # # 
# # # # 
# # # # ui <- fluidPage(
# # # #   tabsetPanel(
# # # #     tabPanel(
# # # #       "Tab1",
# # # #       modal.options.UI("options"),
# # # #       textOutput("out1")
# # # #     ),
# # # #     tabPanel(
# # # #       "Tab2",
# # # #       modal.options.UI("options"),
# # # #       textOutput("out2")
# # # #     )
# # # #   )
# # # # )
# # # # 
# # # # # New-style modules
# # # # myModuleServer <- function(id, prefix = "") {
# # # #   moduleServer(
# # # #     id,
# # # #     function(input, output, session) {
# # # #       output$out1<-output$out2<-renderText({options$check1})
# # # #     }
# # # #   )
# # # # }
# # # # 
# # # # server <- function(input, output) {
# # # #   
# # # #   options<-callModule(modal.options, "options1", options=if(exists("options")){options}else{reactiveValues(check1=FALSE)})
# # # #   options<-callModule(modal.options, "options2", options=if(exists("options")){options}else{reactiveValues(check1=FALSE)})
# # # #   print("output")
# # # #   print(output)
# # # #   myModuleServer("myModule1", prefix = "Show: ")
# # # #   # output$out1<-output$out2<-renderText({options$check1})
# # # #   
# # # # }
# # # # 
# # # # shinyApp(ui = ui, server = server)