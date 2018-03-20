# load required packages
library(shiny)
library(shinydashboard)
library(tidyverse)

# load data for shiny app
load("./Data/veg_chem.RData")

# reorganize the table so it would be more informative 
veg.chem.48 <- veg.chem.48 %>% select(`EPA Pesticide Chemical Code`,`Active Ingredient`, 
                                "Type", "Commodity", `Toxicity Measurements (oral, for an experimental rat)`)

# define the UI of the app
ui <- dashboardPage(skin="green",
  
  dashboardHeader(title = "Vegetables and Chemicals",titleWidth = 300), # create the title of the app
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Toxicity Level", tabName = "toxicity", icon = icon("tree-deciduous",lib = "glyphicon")),
      menuItem("Other Graphs", tabName = "other", icon = icon("picture", lib = "glyphicon"))
      ),
    width = 200), # define the width of the sidebar
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "toxicity",
              h2("Restricted Chemicals and Toxicity Level"),
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    htmlOutput("type"), # reactive input
                    htmlOutput("active") # reactive input
                  ),
                  mainPanel(tableOutput("table1"), # table output
                            htmlOutput("text1")) # text output, for sources of toxicity information
                )
              )
      ),
      
      tabItem(tabName = "other",
              h2("Other Helpful Graphs"),
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    selectInput("year", "Year:", unique(veg.tidy$Year)), # reactive input
                    selectInput("variable", "Variable:", c("Commodity", "Domain", "Type")) # reactive input
                  ),
                  mainPanel(htmlOutput("text2"), # text output, for explanations of graphs
                            plotOutput("graphs") # graph output ) 
                  ) 
                )
              ))
      
    ),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }" # inserted to suppress messages
    )
  )
)
  

# define server for reactive inputs and rendering table, text
server <- function(input, output){
  output$type <- renderUI({
    selectInput("type","Type:",veg.chem.48$Type)
  }) # reactive input for type
  
  output$active <- renderUI({
    data_available <- dplyr::filter(veg.chem.48, Type == input$type)$`Active Ingredient`
    selectInput("ac","Active Ingredient:",data_available)
  }) # reactive input for active ingredient
  
  output$table1 <- renderTable({
    dplyr::filter(veg.chem.48,Type == input$type, `Active Ingredient` == input$ac)
  }) # table output
  
  output$text1 <- renderText({
    paste('<B>Information taken from:</B>',"<p>https://www.fao.org</p>","<p>https://pubchem.ncbi.nlm.nih.gov</p>", "<p>https://pmep.cce.cornell.edu</p>", "<p>https://www.bartlett.com</p>", "<p>https://sitem.herts.ac.uk</p>")  # text ouput
  })
  
  output$text2 <- renderText({
    paste('<b>Instructions:</b>', "Select year and the name of one variable to see a histogram of the variable for that year.")
  })
  
  output$graphs <- renderPlot({
    
    newdata <- filter(veg.tidy, Year==input$year)
    if (input$variable=="Commodity") {
      ggplot(newdata, aes(Commodity)) + geom_bar(stat="count") + 
        theme(axis.text.x=element_text(angle=65, hjust=1))
    }
    else if (input$variable=="Domain") {
      ggplot(newdata, aes(Domain)) + geom_bar(stat="count") +
        theme(axis.text.x=element_text(angle=65, hjust=1))
    }
    else {
      ggplot(newdata, aes(Type)) + geom_bar(stat="count") +
        theme(axis.text.x=element_text(angle=65, hjust=1))
    }
  })
}

# run the app
shinyApp(ui,server)