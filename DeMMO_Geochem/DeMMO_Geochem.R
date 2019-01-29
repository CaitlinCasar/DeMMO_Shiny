library(shiny)
library(shinythemes)
#library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)

geochem_data <- read.csv("geochem_averages_long.csv", header = TRUE)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme=shinytheme("cosmo"),
   
   # Application title
   titlePanel("DeMMO Geochemistry"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "parameter", h3("Select parameter"),
                    #label = "Y-axis:",
                    choices = unique(geochem_data$parameter),
                    selected = "temp_C")
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("geochemPlot")
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
 # output$geochemPlot <- renderPlot({
 #   ggplot(geochem_data, aes(Site, input$parameter, color=Site)) +
 #     geom_point() +
 #     geom_line(size=1.5) 
 # })


  output$geochemPlot <- renderPlot({
  data <- geochem_data %>%
    filter(parameter == input$parameter)
  geochem_plot <- ggplot(data, aes(measured, Site, group=parameter, color=parameter)) +
    geom_point() +
    geom_path() 
  geochem_plot
  })
}

# Run the application 
app <- shinyApp(ui = ui, server = server)
runApp(app)

