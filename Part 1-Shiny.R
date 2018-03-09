# load required package
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("ATMP", tabName = "ATMP", icon = icon("dashboard")),
      menuItem("WTMP", tabName = "WTMP", icon = icon("th"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      # First tab content
      tabItem(tabName = "ATMP",
              h2("Time Series of ATMP"),
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Time Frame",
                  sliderInput("slider", "Number of observations:", min = 1985, max = 2017,
                              value = c(1995,2000))
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "WTMP",
              h2("Time Series of WTMP"),
              fluidRow(
                box(plotOutput("plot2", height = 250)),
                
                box(
                  title = "Time Frame",
                  sliderInput("slider", "Number of observations:", min = 1985, max = 2017,
                              value = c(1995,2000))
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  source("Part 1.R")
  
  output$plot1 <- renderPlot({
    year.selected <- tidy.shiny$Year[seq_len(input$slider)]
    ggplot(tidy.shiny,aes(year.selected, ATMP)) + geom_line() +
      ylab('Air Temperature') + scale_x_date(date_breaks = '1 year',date_labels = '%b %y') +
      theme(axis.text.x=element_text(angle=90, hjust=1))
  })
  
  output$plot1 <- renderPlot({
    year.selected <- tidy.shiny$Year[seq_len(input$slider)]
    ggplot(tidy,aes(year.selected, WTMP)) + geom_line() +
      ylab('Sea Temperature') + scale_x_date(date_breaks = '1 year',date_labels = '%b %y') +
      theme(axis.text.x=element_text(angle=90, hjust=1))
  })
}

shinyApp(ui, server)