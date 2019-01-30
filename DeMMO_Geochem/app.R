library(shiny)
library(shinythemes)
#library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(grid)
library(rapportools)


geochem_data <- read.csv("geochem_averages_long.csv", header = TRUE)
site_coords <- read.csv("site_coords.csv", header = TRUE)
imgage <- jpeg::readJPEG("mine_levels.jpg")


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme=shinytheme("slate"),
   
   # Application title
   titlePanel("DeMMO Geochemistry"),
  fluidRow(
    column(width = 7, plotOutput("DeMMO_sites", click="plot_click", height="auto")),
    column(width = 5,plotOutput("geochemPlot"))),
  
  #verbatimTextOutput("info"),
  
  fluidRow(
    column(width = 7, h5("Click on the dots on the map to view more information about each DeMMO site. Use the 
  menu below the plot area to view average fracture fluid geochemistry at DeMMO.", br(), h6("Published geochemical data from Osburn
    et al. 2014."))),
    column(width = 5,selectInput(inputId = "parameter", #h5("Select parameter"),
              label = "",
              choices = unique(geochem_data$parameter),
              selected = "Choose a parameter"))
   
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
 # output$geochemPlot <- renderPlot({
 #   ggplot(geochem_data, aes(Site, input$parameter, color=Site)) +
 #     geom_point() +
 #     geom_line(size=1.5) 
 # })
  selected_site <- reactiveVal()
  selected_site_info <- reactiveVal()
  #observeEvent(input$plot_click,{
    #selected_site(as.character(nearPoints(site_coords, input$plot_click)[[1]]))
  #})
  
  #observeEvent(input$plot_click,{
    #selected_site_info(as.character(nearPoints(site_coords, input$plot_click)[[4]]))
  #})
  
  output$DeMMO_sites <- renderPlot({
  ggplot(site_coords, aes(x, y, asp=1)) +
    annotation_custom(rasterGrob(imgage, 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc"))) +
    geom_point(size=5, color="#AFD36C") +
    scale_x_continuous(limits = c(0, 539.667)) +
    scale_y_continuous(limits = c(0, 227.026)) +
    theme(legend.position="none") +
      theme(line = element_blank(),
            text = element_blank(),
            title = element_blank(),
            plot.background = element_rect(
              fill = "#272b30",
              color = "#272b30")) +
      theme(plot.margin=grid::unit(c(0,0,0,0), "line"))
    #theme_nothing() + labs(x = NULL, y = NULL) 
    #plot_grid(geochem_plot, scale=1.1)
      
  },
  height = function() {
    session$clientData$DeMMO_sites_width
  })
  observeEvent(input$plot_click, ignoreInit=T,ignoreNULL = T,{
    selected_site(as.character(nearPoints(site_coords, input$plot_click)[[1]]))
    selected_site_info(as.character(nearPoints(site_coords, input$plot_click)[[4]]))
    if(!is.empty(selected_site())){
    showModal(modalDialog(
      title = paste0(selected_site()),
      
      div(img(
        #src = base64enc::dataURI(file = "mine.jpg", mime = "image/jpeg"),
        src = base64enc::dataURI(file = paste0(selected_site(), ".JPG"), mime = "image/jpeg"),
        alt = "mine",
        width="300",
        align = "center"),
      footer = modalButton("Close")
    ),
    easyClose = TRUE,
    h5(selected_site_info()),
    style="text-align: center;")
    )
    }
  })
  
  output$geochemPlot <- renderPlot({
  data <- geochem_data %>%
    filter(parameter == input$parameter)
  ggplot(data, aes(measured, reorder(Site, desc(Site)), group=parameter)) +
    geom_path(color="black", linetype="dotted") +
    geom_point(size = 5, color="#AFD36C") +
    xlab(input$parameter) +
    theme_grey() +
    theme(legend.position="none") +
    theme(axis.title.y=element_blank()) 
  })
  #output$info <- renderPrint({
    #selected_site_info
    # With base graphics, need to tell it what the x and y variables are.
    #as.character(nearPoints(site_coords, input$plot_click)[[1]])
    # nearPoints() also works with hover and dblclick events
  #})
}

# Run the application 
shinyApp(ui = ui, server = server)
