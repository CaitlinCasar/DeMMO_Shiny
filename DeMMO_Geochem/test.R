library(shiny)
library(shinythemes)
#library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(tidyr)
library(pacman)
library(grid)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(jpeg, png, ggplot2, grid, neuropsychology)

imgage <- jpeg::readJPEG("mine_levels.jpg")

geochem_data <- read.csv("MasterGeoChemHmStk.csv", header = TRUE)
geochem_averages <- aggregate(geochem_data[, 5:59], list(geochem_data$Site), mean)
geochem_averages <- geochem_averages[, colSums(is.na(geochem_averages)) != nrow(geochem_averages)]
geochem_averages$Site <- c(1,2,5,6)
geochem_averages_long <- gather(geochem_averages, parameter, measured, temp_C:S2._ugL, factor_key=TRUE)
#colnames(geochem_averages_long)[1] <- "Site"
geochem_averages_long <- geochem_averages_long %>% drop_na()
geochem_averages_long$measured <- as.numeric(geochem_averages_long$measured)

site_coords <- read.csv("site_coords.csv", header = TRUE)

geochem_plot <- ggplot(site_coords, aes(x, y, asp=1)) +
  annotation_custom(rasterGrob(imgage, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc"))) +
  geom_point(size=5, color="#AFD36C") +
  scale_x_continuous(limits = c(0, 539.667)) +
  scale_y_continuous(limits = c(0, 227.026)) +
  theme(legend.position="none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

