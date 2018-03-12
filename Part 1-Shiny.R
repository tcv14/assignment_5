# load required packages
library(plyr)
library(tidyverse)
library(shiny)
library(shinydashboard)
source('Part 1.R')

ui <- dashboardPage(
  dashboardHeader(title = "Bouy 46035"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Air Temperature", tabName = "ATMP", icon = icon("dashboard")),
      menuItem("Sea Temperature", tabName = "WTMP", icon = icon("th"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      # First tab content
      tabItem(tabName = "ATMP",
              h2("Time Series Plot of Air Temperature"),
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Select Time Frame",
                  sliderInput("slider1", "Year:", min = 1985, max = 2017,
                              value = c(1995,2000))
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "WTMP",
              h2("Time Series Plot of Sea Temperature"),
              fluidRow(
                box(plotOutput("plot2", height = 250)),
                
                box(
                  title = "Select Time Frame",
                  sliderInput("slider2", "Year:", min = 1985, max = 2017,
                              value = c(1995,2000))
                )
              )
      )
    )
  )
)

server <- function(input, output) {
<<<<<<< HEAD
  # Plot time series of ATMP vs. Date
  output$plot1 <- renderPlot({
<<<<<<< HEAD
    tidy.ATMP <- dplyr::filter(tidy.shiny,Year %in% min(input$slider1):max(input$slider1))
    ggplot(tidy.ATMP,aes(Date, ATMP)) + geom_line(na.rm=TRUE) +
=======
  source("Part 1.R",local=TRUE)
  
  output$plot1 <- renderPlot({
    selected1 <- tidy.shiny$Year==min(input$slider1):max(input$slider1)
=======
    selected1 <- suppressWarnings(tidy.shiny$Year==min(input$slider1):max(input$slider1))
>>>>>>> parent of 00c16e2... removed suppress messages
    year.selected1 <- tidy.shiny$Year[which(selected1)]
    ATMP.selected <- tidy.shiny$ATMP[which(selected1)]
    tidy.ATMP <- dplyr::bind_cols(data.frame(year.selected1),data.frame(ATMP.selected),
                                  data.frame(tidy.shiny$Date[which(selected1)])) %>%
      dplyr::rename(Date=`tidy.shiny.Date.which.selected1..`)
    ggplot(tidy.ATMP,aes(Date, ATMP.selected)) + geom_line() +
>>>>>>> parent of eeeac39... added comments and suppressed warnings
      ylab('Air Temperature') + scale_x_date(date_breaks = '1 year',date_labels = '%b %y') +
      theme(axis.text.x=element_text(angle=90, hjust=1))
  })
  
  output$plot2 <- renderPlot({
<<<<<<< HEAD
<<<<<<< HEAD
    tidy.WTMP <- dplyr::filter(tidy.shiny,Year %in% min(input$slider2):max(input$slider2))
    ggplot(tidy.WTMP,aes(Date, WTMP)) + geom_line(na.rm=TRUE) +
=======
    selected2 <- tidy.shiny$Year==min(input$slider2):max(input$slider2)
=======
    selected2 <- suppressWarnings(tidy.shiny$Year==min(input$slider2):max(input$slider2))
>>>>>>> parent of 00c16e2... removed suppress messages
    year.selected2 <- tidy.shiny$Year[which(selected2)]
    WTMP.selected <- tidy.shiny$WTMP[which(selected2)]
    tidy.WTMP <- dplyr::bind_cols(data.frame(year.selected2),data.frame(WTMP.selected),
                                  data.frame(tidy.shiny$Date[which(selected2)])) %>%
      dplyr::rename(Date=`tidy.shiny.Date.which.selected2..`)
    ggplot(tidy.WTMP,aes(Date, WTMP.selected)) + geom_line() +
>>>>>>> parent of eeeac39... added comments and suppressed warnings
      ylab('Sea Temperature') + scale_x_date(date_breaks = '1 year',date_labels = '%b %y') +
      theme(axis.text.x=element_text(angle=90, hjust=1))
  })
}

shinyApp(ui, server)
