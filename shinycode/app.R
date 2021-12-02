library(shiny)
library(here)
library(tidyverse)
library(hrbrthemes)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Strava distance histograms"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 10,
                  value = 3)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data <- read_csv(here("Data", "test_data.csv"))
  
  output$distPlot <- renderPlot({

    ggplot(data = data, aes(x = distance/1000)) +
      geom_histogram(color = "#e9ecef", alpha = 0.6, position = 'identity', binwidth = input$bins) +
      theme_ipsum() +
      ylab("Density") +
      xlab("Distance (in km)") +
      labs(fill = "")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)