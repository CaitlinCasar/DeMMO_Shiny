library(shiny)
library(shinythemes)
#library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(tidyr)

geochem_data <- read.csv("MasterGeoChemHmStk.csv", header = TRUE)
geochem_averages <- aggregate(geochem_data[, 5:59], list(geochem_data$Site), mean)
geochem_averages <- geochem_averages[, colSums(is.na(geochem_averages)) != nrow(geochem_averages)]
geochem_averages_long <- gather(geochem_averages, parameter, measured, temp_C:S2._ugL, factor_key=TRUE)
colnames(geochem_averages_long)[1] <- "Site"

geochem_plot <- ggplot(geochem_averages_long, aes(log10(measured), Site, group=parameter, color=parameter)) +
  geom_point() +
  geom_path() 
