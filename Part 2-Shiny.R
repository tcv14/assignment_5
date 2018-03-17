# load required packages
library(shiny)
library(shinydashboard)
library(tidyverse)

# load data for shiny app
veg.chem <- readRDS("./Data/veg_chem.rds")

# reorganize the table so it would be more informative 
veg.chem <- veg.chem %>% select(`EPA Pesticide Chemical Code`,`Active Ingredient`, 
                                "Type", "Commodity", `Toxicity Measurements (oral, for an experimental rat)`)

# define the UI of the app
ui <- dashboardPage(
  
  dashboardHeader(title = "Restricted Chemicals and Toxicity Level",titleWidth = 400), # create the title of the app
  
  dashboardSidebar(width = 200), # define the width of the sidebar
  
  dashboardBody(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          htmlOutput("type"), # reactive input
          htmlOutput("active") # reactive input
        ),
        mainPanel(tableOutput("table"), # table output
                  textOutput("text")) # text output, for sources of toxicity information
      ),
      
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }") # inserted to suppress warning messages
    )
  )
)

# define server for reactive inputs and rendering table, text
server <- function(input, output){
  output$type <- renderUI({
    selectInput("type","Type:",veg.chem$Type)
  }) # reactive input for type
  
  output$active <- renderUI({
    data_available <- dplyr::filter(veg.chem, Type == input$type)$`Active Ingredient`
    selectInput("ac","Active Ingredient:",data_available)
  }) # reactive input for active ingredient
  
  output$table <- renderTable({
    dplyr::filter(veg.chem,Type == input$type, `Active Ingredient` == input$ac)
  }) # table output
  
  output$text <- renderPrint({
    cat("Information taken from","https://www.fao.org,","https://pubchem.ncbi.nlm.nih.gov,", "https://pmep.cce.cornell.edu,", "https://www.bartlett.com,", "https://sitem.herts.ac.uk,",
        sep = "\n")  # text ouput
  }) 
}

# run the app
shinyApp(ui,server)