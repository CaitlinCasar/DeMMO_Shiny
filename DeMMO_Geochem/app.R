library(shiny)
library(shinythemes)
#library(readr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(DT)
library(tools)
library(grid)
library(rapportools)
library(plotly)


geochem_data <- read.csv("geochemistry.csv", header = TRUE) %>%
  dplyr::filter(!notes %in% c("after drilling", "after packer", "packer day") & !annual_day %in% 138:140) %>%
  dplyr::mutate_at(dplyr::vars(date), lubridate::mdy) %>% 
  dplyr::group_by(Site, date) %>% 
  dplyr::select(field_trip, depth, Temp_C:Zn, DOC_mg_per_L, mM_DIC, CO2_nM:H2_nM) %>%
  tidyr::gather(parameter, measured, depth:H2_nM) %>%
  dplyr::bind_rows(tibble(Site = NA, date=NA, field_trip=NA,
                          parameter="Please choose a parameter.", 
                          measured=NA)) 

site_coords <- read.csv("site_coords.csv", header = TRUE, stringsAsFactors = FALSE)
imgage <- jpeg::readJPEG("mine_levels.jpg")


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(includeScript("google-analytics.js")),
  tags$meta(property="og:title", content="Shiny"),
  tags$meta(property="og:type", content="website"),
  tags$meta(property="og:url", content="https://deepminemicrobialobservatory.com/shiny/sample-apps/rmd/"),
  tags$meta(property="og:image", content="DeMMO_fb_og.jpg"),
  
  theme=shinytheme("slate"),
  
  # Application title
  titlePanel("DeMMO Geochemistry"),
  fluidRow(
    column(width = 7, plotlyOutput("DeMMO_sites"),
           verbatimTextOutput("click")),
    column(width = 5,plotlyOutput("geochemPlot"))),
  
  #verbatimTextOutput("info"),
  
  fluidRow(
    column(width = 7, h5("Click on the dots on the map to view more information about each DeMMO site. Use the 
                         menu below the plot area to view average fracture fluid geochemistry at DeMMO.", br(), h6("Published geochemical data from Osburn
                                                                                                                   et al. 2019."))),
    column(width = 5,
           selectInput(inputId = "parameter", #h5("Select parameter"),
                       label = "",
                       choices = unique(geochem_data$parameter),
                       selected = "Please choose a parameter."),
           sliderInput("slider", "Time", min = min(geochem_data$date),max =max(geochem_data$date),value=c(min(geochem_data$date),max =max(geochem_data$date)),timeFormat="%b %Y"),
           verbatimTextOutput("range")
    )
    
    )
    )


# Define server logic required to draw plots
server <- function(input, output) {
  output$DeMMO_sites <- renderPlotly({
    site_coords%>%
      plot_ly(x = ~x,
              y = ~y,
              type = "scatter",
              mode = "markers",
              text=~Site,
              hoverinfo="text",
              marker = list(
                opacity = 0.7, sizemode = "diameter",
                color = "#AFD36C",
                size = 20
              ),
              source = "A") %>%
      layout(
        paper_bgcolor="#272b30",
        plot_bgcolor="#272b30",
        xaxis = list(title = FALSE,
                     zeroline = FALSE,
                     showline = FALSE,
                     showticklabels = FALSE,
                     showgrid = FALSE, 
                     range = c(0, 539.667)),
        yaxis = list(title = FALSE,
                     zeroline = FALSE,
                     showline = FALSE,
                     showticklabels = FALSE,
                     showgrid = FALSE, 
                     showgrid = F, range = c(0,227.026)),
        images = list(
          list(
            source = "https://raw.githubusercontent.com/DeepSubsurfer/DeMMO_Shiny/master/DeMMO_Geochem/mine_levels.jpg",
            xanchor = "left",
            xref = "x",
            yref = "y",
            x = 0 ,
            y = 227.026,
            sizex = 539.667,
            sizey =227.026,
            #opacity = 0.4,
            layer = "below",
            sizing = "stretch"
          )
        ), #title = "DeMMO",
        showlegend = FALSE)%>%
      config(displayModeBar = F) %>%
      animation_opts(
        1000, easing = "elastic", redraw = FALSE
      )
  })
  observeEvent(event_data("plotly_click", source="A"), ignoreInit=T,ignoreNULL = T,{
    selected_coords <- event_data("plotly_click", source="A")
    selected_site <- site_coords %>% dplyr::filter(x %in% (selected_coords$x-5):(selected_coords$x+5) & y %in% (selected_coords$y-5):(selected_coords$y+5))
    showModal(modalDialog(
      title = paste0(selected_site[1]),
      
      div(img(
        src = base64enc::dataURI(file = paste0(selected_site[1], ".JPG"), mime = "image/jpeg"),
        alt = "mine",
        width="300",
        align = "center",
        margin(0, 'auto')),
        footer = modalButton("Close")
      ),
      easyClose = TRUE,
      h6(selected_site[4]),
      style="text-align: justify;")
    )
    #}
  })
  
  # output$click <- renderPrint({
  #   message('clicked')
  #   d <- event_data("plotly_click")
  #   if (!is.null(d)) d
  # })
  
  output$geochemPlot <- renderPlotly({
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    
    palette = gg_color_hue(length(unique(na.omit(geochem_data$field_trip))))
    names(palette) <- unique(na.omit(geochem_data$field_trip))
    
    geochem_data$parameter <- as.factor(geochem_data$parameter)
    na.omit(geochem_data) %>% 
      filter(date>=as.Date(input$slider[1]) & date<=as.Date(input$slider[2]) & parameter == input$parameter) %>%
      plot_ly(x = ~measured,
              y = ~Site,
              #group = ~parameter,
              color= ~field_trip,
              colors = palette,
              type = "scatter",
              mode = "markers",
              text= ~paste('field trip:',field_trip,
                           '<br> measured: ', measured),
              #frame=~field_trip,
              hoverinfo="text",
              marker = list(
                opacity = 0.7, sizemode = "diameter",
                size = 20),
              source='B') %>%
      layout(
        xaxis=list(
          title = paste(stringr::str_replace(input$parameter, '_', ' '))
        ),
        yaxis = list(autorange = "reversed",
                     title = FALSE), 
        showlegend = FALSE
      )
    # data <- geochem_data %>%
    #   filter(date>=as.Date(input$slider[1]) & date<=as.Date(input$slider[2]) & parameter == input$parameter)
    # ggplot2::ggplot(na.omit(data), aes(measured, reorder(Site, desc(Site)), group=parameter, color=field_trip)) +
    #   geom_path(color="black", linetype="dotted") +
    #   geom_point(size = 5) +
    #   xlab(input$parameter) +
    #   theme_grey() +
    #   theme(legend.position="none") +
    #   theme(axis.title.y=element_blank()) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
