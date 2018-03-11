# load required package
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "NOAA Bouy 46035"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Air Temperature", tabName = "ATMP", icon = icon("cloud",lib = "glyphicon")),
      menuItem("Sea Temperature", tabName = "WTMP", icon = icon("tint",lib = "glyphicon"))
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
  source("Part 1.R",local=TRUE)
  # Plot time series of ATMP vs. Date
  output$plot1 <- renderPlot({
    selected1 <- suppressWarnings(tidy.shiny$Year==min(input$slider1):max(input$slider1))
    year.selected1 <- tidy.shiny$Year[which(selected1)]
    ATMP.selected <- tidy.shiny$ATMP[which(selected1)]
    tidy.ATMP <- dplyr::bind_cols(data.frame(year.selected1),data.frame(ATMP.selected),
                                  data.frame(tidy.shiny$Date[which(selected1)])) %>%
      dplyr::rename(Date=`tidy.shiny.Date.which.selected1..`)
    ggplot(tidy.ATMP,aes(Date, ATMP.selected)) + geom_line(na.rm=TRUE) +
      ylab('Air Temperature') + scale_x_date(date_breaks = '1 year',date_labels = '%b %y') +
      theme(axis.text.x=element_text(angle=65, hjust=1))
  })
  # Plot time series of WTMP vs. Date
  output$plot2 <- renderPlot({
    selected2 <- suppressWarnings(tidy.shiny$Year==min(input$slider2):max(input$slider2))
    year.selected2 <- tidy.shiny$Year[which(selected2)]
    WTMP.selected <- tidy.shiny$WTMP[which(selected2)]
    tidy.WTMP <- dplyr::bind_cols(data.frame(year.selected2),data.frame(WTMP.selected),
                                  data.frame(tidy.shiny$Date[which(selected2)])) %>%
      dplyr::rename(Date=`tidy.shiny.Date.which.selected2..`)
    ggplot(tidy.WTMP,aes(Date, WTMP.selected)) + geom_line(na.rm=TRUE) +
      ylab('Sea Temperature') + scale_x_date(date_breaks = '1 year',date_labels = '%b %y') +
      theme(axis.text.x=element_text(angle=65, hjust=1))
  })
}

shinyApp(ui, server)
