library(shiny)

veg.chem <- readRDS("./Data/veg_chem.rds")

ui <- fluidPage(
   
   titlePanel("Restricted Chemicals and Toxicity Level"),
   
   sidebarLayout(
      sidebarPanel(
        htmlOutput("type"),
        htmlOutput("ac")
      ),
      
      mainPanel(
         tableOutput("table")
      )
   ),
   
   tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }") # inserted to suppress warning messages
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$type <-  renderUI({
    selectInput(inputId = "type", label = "Type:", choices = veg.chem$Type)
  })
  
  output$ac <-  renderUI({
    data_available <- filter(veg.chem, Type == input$type)$`Active Ingredient`
    
    selectInput(inputId = "ac", label = "Active Ingredient", choices = data_available)
  })
  
  output$table <- renderTable({
     filter(veg.chem, Type == input$type, `Active Ingredient` == input$ac)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

