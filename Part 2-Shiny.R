library(shiny)
library(shinydashboard)

veg.chem <- readRDS("./Data/veg_chem.rds")

ui <- dashboardPage(
  dashboardHeader(title = "Restricted Chemicals"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
        box(title = "Table", status = "primary",tableOutput("table")),width=12),
    fluidRow(
      column(width = 6,
        box(
        selectInput("type", "Type:", veg.chem$Type), 
        selectInput("ac","Active Ingredient",filter(veg.chem,Type == input$type)$`Active Ingrediant`))
    )
    )
  )
)


server <- function(input, output) {
  output$table <- renderTable({
    filter(veg.chem, Type == input$type, `Active Ingrediant` == input$ac)
  }, align = "l")
}

# Run the application 
shinyApp(ui = ui, server = server)

