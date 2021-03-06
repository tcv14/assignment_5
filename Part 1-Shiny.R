# load required packages
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "NOAA Bouy 46035"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Air Temperature", tabName = "ATMP", icon = icon("cloud",lib = "glyphicon")),
      menuItem("Sea Temperature", tabName = "WTMP", icon = icon("tint", lib = "glyphicon")),
      menuItem("Comparison", tabName = "ATMP_WTMP", icon = icon("equalizer", lib = "glyphicon"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row
    tabItems(
      # First tab content
      # Create a box for the plot
      tabItem(tabName = "ATMP",
              h2("Time Series Plot of Air Temperature"),
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                # Create a box for the slider
                box(
                  title = "Select Time Frame:",
                  sliderInput("slider1", "Year:", min = 1985, max = 2017,
                              value = c(1995,2000))
                )
              )
      ),
      
      # Second tab content
      # Create a box for the plot
      tabItem(tabName = "WTMP",
              h2("Time Series Plot of Sea Temperature"),
              fluidRow(
                box(plotOutput("plot2", height = 250)),
                
                # Create a box for the slider
                box(
                  title = "Select Time Frame:",
                  sliderInput("slider2", "Year:", min = 1985, max = 2017,
                              value = c(1995,2000))
                )
              )
      ),
      # Thrid tab content
      # Create a box for the plot
      tabItem(tabName = "ATMP_WTMP",
              h2("Time Series Comparison of Air and Sea Temperature"),
              fluidRow(
                box(plotOutput("plot3", height = 500)),
                
                # Create a box for the slider
                box(
                  title = "Select Time Frame:",
                  sliderInput("slider3", "Year:", min = 1985, max = 2017,
                              value = c(1995,2000))
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  source("Part 1.R",local=TRUE)
  
  # Plot time series of ATMP vs. Date
  output$plot1 <- renderPlot({
    tidy.ATMP <- filter(tidy.shiny, tidy.shiny$Year==c(min(input$slider1):max(input$slider1)))
    ggplot(tidy.ATMP,aes(Date, ATMP)) + geom_line() +
      ylab('Air Temperature') + scale_x_date(date_breaks = '1 year',date_labels = '%b %y') +
      theme(axis.text.x=element_text(angle=65, hjust=1))
  })
  
  # Plot time series of WTMP vs. Date
  output$plot2 <- renderPlot({
    tidy.WTMP <- filter(tidy.shiny, tidy.shiny$Year==c(min(input$slider2):max(input$slider2)))
    ggplot(tidy.WTMP,aes(Date, WTMP)) + geom_line() +
      ylab('Sea Temperature') + scale_x_date(date_breaks = '1 year',date_labels = '%b %y') +
      theme(axis.text.x=element_text(angle=65, hjust=1))
  })
  
  # Plot time series of both ATMP vs. Date and WTMP vs. Date
  output$plot3 <- renderPlot({
    tidy.ATMP <- filter(tidy.shiny, tidy.shiny$Year==c(min(input$slider3):max(input$slider3)))
    tidy.WTMP <- filter(tidy.shiny, tidy.shiny$Year==c(min(input$slider3):max(input$slider3)))
    # Create two plots stacked on top of each other
    gridExtra::grid.arrange(ggplot(tidy.ATMP,aes(Date, ATMP)) + geom_line(color = "rosybrown3") +
                              ylab('Air Temperature') + scale_x_date(date_breaks = '1 year',date_labels = '%b %y') +
                              theme(axis.text.x=element_text(angle=65, hjust=1)),
                            ggplot(tidy.WTMP,aes(Date, WTMP)) + geom_line(color = "cornflowerblue") +
                              ylab('Sea Temperature') + scale_x_date(date_breaks = '1 year',date_labels = '%b %y') +
                              theme(axis.text.x=element_text(angle=65, hjust=1)),
                            nrow=2
                            )
  })
}

shinyApp(ui, server)
