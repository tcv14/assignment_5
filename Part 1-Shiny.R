# load required packages
library(plyr)
library(tidyverse)
library(shiny)
library(shinydashboard)
source('Part 1.R')

ui <- dashboardPage(
  dashboardHeader(title = "NOAA Bouy 46035"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Air Temperature", tabName = "ATMP", icon = icon("dashboard")),
      menuItem("Sea Temperature", tabName = "WTMP", icon = icon("th"))
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
      )
    )
  )
)

server <- function(input, output) {
  # Plot time series of ATMP vs. Date
  output$plot1 <- renderPlot({
    tidy.ATMP <- dplyr::filter(tidy.shiny,Year %in% min(input$slider1):max(input$slider1))
    ggplot(tidy.ATMP,aes(Date, ATMP)) + geom_line(na.rm=TRUE) +
      ylab('Air Temperature') + scale_x_date(date_breaks = '1 year',date_labels = '%b %y') +
      theme(axis.text.x=element_text(angle=90, hjust=1))
  })
  # Plot time series of WTMP vs. Date
  output$plot2 <- renderPlot({
    tidy.WTMP <- dplyr::filter(tidy.shiny,Year %in% min(input$slider2):max(input$slider2))
    ggplot(tidy.WTMP,aes(Date, WTMP)) + geom_line(na.rm=TRUE) +
      ylab('Sea Temperature') + scale_x_date(date_breaks = '1 year',date_labels = '%b %y') +
      theme(axis.text.x=element_text(angle=90, hjust=1))
  })
}

shinyApp(ui, server)
