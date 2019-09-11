library(plotly)

df <-read.csv("https://github.com/angelaRout/test/blob/master/km_join.csv?raw=true2")

site_coords <- read.csv("site_coords.csv", header = TRUE)
image <- jpeg::readJPEG("mine_levels.jpg")

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

test_plotly <- site_coords%>%
  plot_ly(x = ~x,
          y = ~y,
          type = "scatter",
          mode = "markers",
          #color=~Site,
          #size=~FREQ,
          text=~Site,
          #frame=~MONTH,
          hoverinfo="text",
          marker = list(
            opacity = 0.7, sizemode = "diameter",
            color = "#AFD36C",
            size = 20
          )) %>%
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
                 #titlefont = list(
                   #family = "Arial, sans-serif",
                   #size = 0,
                   #color = "lightgrey"
                 #),
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
  ) #%>%
  #animation_button(
    #x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  #) #%>%
  #animation_slider(
    #currentvalue = list(prefix = "MONTH ", font = list(color="grey"))
  #)

test_plotly



ggplot2::ggplot(na.omit(data), aes(measured, reorder(Site, desc(Site)), group=parameter, color=field_trip)) +
  geom_path(color="black", linetype="dotted") +
  geom_point(size = 5) +
  xlab(input$parameter) +
  theme_grey() +
  theme(legend.position="none") +
  theme(axis.title.y=element_blank()) 

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

palette = gg_color_hue(length(unique(na.omit(geochem_data$field_trip))))
names(palette) <- unique(na.omit(geochem_data$field_trip))

plot <- na.omit(geochem_data) %>% 
  filter(date>=as.Date(input$slider[1]) & date<=as.Date(input$slider[2]) & parameter == input$parameter) %>%
  plot_ly(x = ~measured,
          y = ~Site,
          #group = ~parameter,
          color= ~field_trip,
          colors = palette,
          type = "scatter",
          mode = "markers",
          text=~field_trip,
          #frame=~field_trip,
          hoverinfo="text",
          marker = list(
            opacity = 0.7, sizemode = "diameter",
            size = 20)) %>%
  layout(
    yaxis = list(autorange = "reversed"), 
     showlegend = FALSE
  )

plot





df <-read.csv("https://raw.githubusercontent.com/angelaRout/test/master/km_join.csv")

test_plotly <- df%>%
  plot_ly(x = ~lon_cluster,
          y = ~lat_cluster,
          type = "scatter",
          mode = "markers",
          color=~TIME.OF.DAY,
          size=~FREQ,
          text=~TIME.OF.DAY,
          frame=~MONTH,
          hoverinfo="text",
          marker = list(
            opacity = 0.7, sizemode = "diameter"
          )) %>%
  layout(
    xaxis = list(range = c(-9700, -8680)),
    yaxis = list(range = c(5659820,5660570)),
    images = list(
      list(
        source = "https://raw.githubusercontent.com/angelaRout/test/master/university2.png",
        xanchor = "left",
        xref = "x",
        yref = "y",
        x = -9700 ,
        y = 5660570,
        sizex = 1020,
        sizey = 750,
        opacity = 0.4,
        layer = "below",
        sizing = "stretch"
      )
    ), title = "Point Clusters",
    showlegend = TRUE)%>%
  config(displayModeBar = F) %>%
  animation_opts(
    1000, easing = "elastic", redraw = FALSE
  ) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "MONTH ", font = list(color="grey"))
  )

test_plotly