library(shiny)
library(shinydashboard)

veg.chem <- readRDS("./Data/veg_chem.rds")

ui <- dashboardPage(
  
  dashboardHeader(title = "Restricted Chemicals and Toxicity Level",titleWidth = 400),
  
  dashboardSidebar(width = 200),
  
  dashboardBody(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          htmlOutput("type"),
          htmlOutput("active")
        ),
        mainPanel(tableOutput("table"))
      ),
      
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }") # inserted to suppress warning messages
    )
  )
)
server <- function(input, output){
  output$type <- renderUI({
    selectInput("type","Type:",veg.chem$Type)
  })
  
  output$active <- renderUI({
    data_available <- dplyr::filter(veg.chem, Type == input$type)$`Active Ingredient`
    selectInput("ac","Active Ingredient:",data_available)
  })
  
  output$table <- renderTable({
    dplyr::filter(veg.chem,Type == input$type, `Active Ingredient` == input$ac)
  })
}

shinyApp(ui,server)