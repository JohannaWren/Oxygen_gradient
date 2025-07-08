# Date: July 7, 2025
# Author: Johanna LK Wren & Emma Scott-Wellman
# email: johanna.wren@noaa.gov emma.scott-wellman@noaa.gov
# Description: RScript for Figures used in Symposium Presentation 

library(ggpmisc)
library(dplyr)
library(readxl)
library(here)
library(ggplot2)
library(lubridate)
library(tidyr)
library(data.table)
library(paletteer)

# Set working directory
myDir <- paste(here(), sep='/') # Emma's file path
# myDir <- paste(here(), 'CTD', sep='/')  # Johanna's File path
setwd(myDir)

# Read in files 
ctdAll <- read.csv('CTD_data_forAnalysis.csv')
stnInfo <- read.csv('CTD_metadata_forAnalysis.csv')

# Make a table with labels you want and the index that we can use for plotting
id.labs <- stnInfo$Station2
names(id.labs) <- stnInfo$Cast

# Read in nutrient data and clean out the '<', 'm', and 'NA's'
nut <- read.csv('SE2204_nutrient_metadata_USE_THIS.csv')
# Adding Cast column that is the same as the CTD cast numbers. 
nut <- nut %>% 
  left_join(stnInfo[,c('Station2', 'Cast')], by=c('Station'='Station2'))
head(nut)

nut$Depth2 <- as.numeric(substr(nut$Depth, 1, nchar(nut$Depth)-1))
head(nut)
# Remove the m from nutrient file depths and create a new column with numeric depth only

nut$Ammonia <- ifelse(nut$Ammonia == "<0.02", 0.01, as.numeric(nut$Ammonia))
nut$Phosphate <- ifelse(nut$Phosphate == "<0.008", 0.007, as.numeric(nut$Phosphate))
nut$Silicate <- as.numeric(nut$Silicate)
nut$Date <- as.Date(nut$Date, '%m/%d/%y') 


#------------------------------ Final Figures ---------------------------------------
plot_ocng_section_nocont <- function(data, ocng_var, Res1, Res2, title_label, Units, Color) {
  clean_data <- data %>%
    select(newLat, Depth, !!sym(ocng_var)) %>%
    rename(Depth = Depth, OCNVar = !!sym(ocng_var)) %>%
    filter(!is.na(Depth), !is.na(OCNVar))
  
  sample_points <- clean_data
  
  # Interpolation with MBA
  interp <- mba.surf(clean_data, no.X = Res1, no.Y = Res2, extend = FALSE)
  dimnames(interp$xyz.est$z) <- list(interp$xyz.est$x, interp$xyz.est$y)
  
  # Convert to dataframe
  interp_df <- melt(interp$xyz.est$z, varnames = c("newLat", "Depth"), value.name = "OCNVar") %>%
    mutate(OCNVAr = round(OCNVar, 1))
  
  # Plot
  ggplot(data = interp_df, aes(x = newLat, y = Depth)) +
    geom_raster(aes(fill = OCNVar)) +
    scale_fill_viridis_c( option = Color) +
    scale_y_reverse() +
    guides(size = "none", 
           fill = guide_colourbar(title.position = "right"), 
           title.theme = element_text(angle = 270, hjust = 0.5, vjust = 0.5)) +
    labs(
      # y = "Depth [m]",
      # x = "Latitude",
      x = NULL, 
      y = NULL, 
      fill = paste0(title_label, Units),
      # title = paste("SE2204", title_label, "Section Plot"),
      # subtitle = "Interpolated over depth and space"
    ) +
    coord_cartesian(expand = 0) +
    theme(legend.title = element_text(angle = 90, hjust=0.5), 
          legend.direction = "vertical",
          legend.key.height = unit(1, 'null'), 
          legend.key.width = unit(0.5, 'cm'), 
          legend.margin = margin(0,0,0,0))
}

OSPlot <- plot_ocng_section_nocont(data = ctdAll, ocng_var = "Oxygen", Res1 = 400, Res2 = 400 , title_label = "Oxygen", Units = " [μmol/kg]", Color = "inferno")
OSPlot




# Panel Plot of Temp, Salinity, Oxygen, Nitrate, Fluorescence 

plot_ocng_section <- function(data, ocng_var, Res1, Res2, title_label, Units) {
  clean_data <- data %>%
    select(newLat, Depth, !!sym(ocng_var)) %>%
    rename(Depth = Depth, OCNVar = !!sym(ocng_var)) %>%
    filter(!is.na(Depth), !is.na(OCNVar))
  
  sample_points <- clean_data
  
  # Interpolation with MBA
  interp <- mba.surf(clean_data, no.X = Res1, no.Y = Res2, extend = FALSE)
  dimnames(interp$xyz.est$z) <- list(interp$xyz.est$x, interp$xyz.est$y)
  
  # Convert to dataframe
  interp_df <- melt(interp$xyz.est$z, varnames = c("newLat", "Depth"), value.name = "OCNVar") %>%
    mutate(OCNVAr = round(OCNVar, 1))
  
  # Plot
  ggplot(data = interp_df, aes(x = newLat, y = Depth)) +
    geom_raster(aes(fill = OCNVar)) +
    scale_fill_viridis_c(option = "turbo") +
    scale_y_reverse() +
    geom_contour(aes(z = OCNVar), binwidth = 1, colour = "black", alpha = 0.2) +
    guides(size = "none", 
           fill = guide_colourbar(title.position = "right"), 
           title.theme = element_text(angle = 270, hjust = 0.5, vjust = 0.5)) +
    labs(
      # y = "Depth [m]",
      # x = "Latitude",
      x = NULL, 
      y = NULL, 
      fill = paste0(title_label, Units),
      # title = paste("SE2204", title_label, "Section Plot"),
      # subtitle = "Interpolated over depth and space"
    ) +
    coord_cartesian(expand = 0) +
    theme(legend.title = element_text(angle = 90, hjust=0.5), 
          legend.direction = "vertical",
          legend.key.height = unit(1, 'null'), 
          legend.key.width = unit(0.5, 'cm'), 
          legend.margin = margin(0,0,0,0))
}
TempSPlot <- plot_ocng_section(data = ctdAll, ocng_var = "Temperature", Res1 = 100, Res2 = 100, title_label = "Temperature", Units = ' [°C]' )
TempSPlot


# SalinitySPlot
SalinitySPlot <- plot_ocng_section_nocont(data = ctdAll, ocng_var = "Salinity", Res1 = 1000, Res2 = 1000, title_label = "Salinity", Units = " [PSU]", Color = "viridis")
SalinitySPlot

# OSPlot
OSPlot <- plot_ocng_section_nocont(data = ctdAll, ocng_var = "Oxygen", Res1 = 400, Res2 = 400 , title_label = "Oxygen", Units = " [μmol/kg]", Color = "turbo")
OSPlot

# NSectionPlot
NSectionPlot

# Create the stitched plots 
library(patchwork)
combined_plot <- TempSPlot / SalinitySPlot / OSPlot / NSectionPlot
print(combined_plot)

library(cowplot)
combined_plotV <- plot_grid(TempSPlot / SalinitySPlot / OSPlot / NSectionPlot, ncol = 1, align = "v")
combined_plotV

final_plot <- ggdraw(combined_plotV) +
  draw_label("Latitude", x = 0.5, y = 0.01, vjust = 0, angle = 0, size = 12) +  
  draw_label("Depth [m]", x = 0.0009, y = 0.5, vjust = 1, angle = 90, size = 12) 

final_plot
# ggsave('SectionPlots_poster.png', width = 24, height = 36, units = "in") #for poster
# ggsave('SectionPlots_presentation.png', width = 10, height = 7.5, dpi = 300, units = "in") #for presentation
