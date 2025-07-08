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

# ---------------------------------- DATA --------------------------------------
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

# -------------------------------------------------------------------------------

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


# -------------------------------------------------------------------------------
# -------------------------------- PHYTO ---------------------------------------

bulk <- phyto %>% filter(Filter == "bulk")
bulk <- bulk %>% left_join(stnInfo, by = "Cast")



plot_section <- function(data, nutrient_col, title_label) {
  clean_data <- data %>%
    select(Lat, Depth.x, !!sym(nutrient_col)) %>%
    rename(Depth.x = Depth.x, NutVar = !!sym(nutrient_col)) %>%
    filter(!is.na(Lat), !is.na(Depth.x), !is.na(NutVar))
  
  sample_points <- clean_data
  
  # Interpolation with MBA
  interp <- mba.surf(clean_data, no.X = 500, no.Y = 500, extend = TRUE)
  dimnames(interp$xyz.est$z) <- list(interp$xyz.est$x, interp$xyz.est$y)
  
  # Convert to dataframe
  interp_df <- melt(interp$xyz.est$z, varnames = c("Lat", "Depth.x"), value.name = "NutVar") %>%
    mutate(NutVar = round(NutVar, 1))
  
  # Plot
  ggplot(data = interp_df, aes(x = Lat, y = Depth.x)) +
    geom_raster(aes(fill = NutVar)) +
    scale_fill_viridis_c(breaks = c(0.02, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)) +
    scale_y_reverse() +
    geom_contour(aes(z = NutVar), binwidth = 1, colour = "black", alpha = 0.2) +
    geom_point(data = sample_points, aes(x = Lat, y = Depth.x),
               colour = "black", size = 0.2, alpha = 0.4, shape = 8) +
    guides(size = "none", 
           fill = guide_colourbar(title.position = "right"), 
           title.theme = element_text(angle = 270, hjust = 0.5, vjust = 0.5)) +
    labs(
      # y = "Depth [m]",
      # x = "Latitude",
      x = NULL, 
      y = NULL, 
      fill = "Bulk Chlorophyll [µg/L]"
      # title = paste("SE2204", title_label, "Section Plot"),
      # subtitle = "Interpolated over depth and space; \nblack dots show actual sampling locations."
    ) +
    coord_cartesian(expand = 0) +
    theme(legend.title = element_text(angle = 90, hjust=0.5), 
          legend.direction = "vertical",
          legend.key.height = unit(1, 'null'), 
          legend.key.width = unit(0.5, 'cm'), 
          legend.margin = margin(0,0,0,0))
}


plot_section(data = bulk, nutrient_col = "Chlorophyll", title_label = "Bulk")


plot_ocng_section <- function(data, ocng_var, Res1, Res2, title_label, Units) {
  clean_data <- data %>%
    select(Lat, Depth.x, !!sym(ocng_var)) %>%
    rename(Depth.x = Depth.x, OCNVar = !!sym(ocng_var)) %>%
    filter(!is.na(Depth.x), !is.na(OCNVar))
  
  sample_points <- clean_data
  
  # Interpolation with MBA
  interp <- mba.surf(clean_data, no.X = Res1, no.Y = Res2, extend = FALSE)
  dimnames(interp$xyz.est$z) <- list(interp$xyz.est$x, interp$xyz.est$y)
  
  # Convert to dataframe
  interp_df <- melt(interp$xyz.est$z, varnames = c("Lat", "Depth.x"), value.name = "OCNVar") %>%
    mutate(OCNVAr = round(OCNVar, 1))
  
  # Plot
  ggplot(data = interp_df, aes(x = Lat, y = Depth.x)) +
    geom_raster(aes(fill = OCNVar)) +
    scale_fill_viridis_c() +
    scale_y_reverse() +
    geom_contour(aes(z = OCNVar), binwidth = 1, colour = "black", alpha = 0.2) +
    geom_point(data = sample_points, aes(x = Lat, y = Depth.x),
               colour = "black", size = 0.2, alpha = 0.4, shape = 8) +
    guides(size = "none", 
           fill = guide_colourbar(title.position = "right"), 
           title.theme = element_text(angle = 270, hjust = 0.5, vjust = 0.5)) +
    labs(
      y = "Depth [m]",
      x = "Latitude",
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

plot_ocng_section(data = bulk, ocng_var = "Chlorophyll", Res1 = 250, Res2 = 250, title_label = "Bulk Chlorophyll", Units = " [µg/L]")
# ggsave('BulkSectionPlots_poster.png', width = 24, height = 36, units = "in") #for poster
# ggsave('BulkSectionPlots_presentation.png', width = 10, height = 5, dpi = 300, units = "in") #for presentation


#  -------------------------------- pie chart --------------------------------
# Create new data set with original phyto data 
phytoSizeStn <- phyto %>% 
  filter(Filter!= 'bulk') %>%
  group_by(Cast, Size) %>% 
  summarise(ChlAllDepth = sum(Chlorophyll, na.rm = T))

phytoSizeStn <- phytoSizeStn %>% group_by(Cast) %>% summarise(ChlTotalStn=sum(ChlAllDepth)) %>% left_join(phytoSizeStn)
head(phytoSizeStn)

# Adding a percent collumn to the dataset 
phytoSizeStn <- phytoSizeStn %>% mutate(Percent = ChlAllDepth / ChlTotalStn * 100)
head(phytoSizeStn)

phytoSizeStn_labeled <- phytoSizeStn %>%
  group_by(Cast) %>%
  arrange(desc(Size)) %>%
  mutate(
    cumulative = cumsum(Percent),
    midpoint = cumulative - Percent / 2,
    label = paste0(round(Percent), "%")
  )

ggplot(phytoSizeStn, aes(x = "", y = Percent, fill = factor(Size))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ Cast, labeller = labeller(Cast = id.labs)) +
  scale_fill_manual(values = c("#495d86", "#d9b021", "#d26424")) +
  labs(title = "Phytoplankton Size Composition", fill = "Size [µm]") +
  theme_void()
# ggsave('ChlPieChart.2_AllStns.png', width=10, height = 5.625, dpi = 300, units = 'in')
# A Night: 0.2 = 79.6% ; 2 = 15.2% ; 20 = 5.12%
# Station E Night: 0.2 = 74.2%; 2 = 20.3% ; 20 = 4.58%

selected_casts <- c(8,9,10,17,18,19,26,27,28,35,36,37,38)
phytoSizeStn13 <- phytoSizeStn %>% filter(Cast %in% selected_casts)

ggplot(phytoSizeStn13, aes(x = "", y = Percent, fill = factor(Size))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ Cast, labeller = labeller(Cast = id.labs)) +
  scale_fill_manual(values = c("#495d86", "#d9b021", "#d26424")) +
  labs(title = "Phytoplankton Size Composition", fill = "Size [µm]") +
  theme_void()
# ggsave('PhytoDayPie13_presentation.png', width = 10, height = 4, dpi = 300, units = "in") #for presentation



# Day
selected_casts_day <- c(8,10,17,19,26,28,35,37)
reversed_casts_day <- rev(selected_casts_day)

phyto_subset_day <- phytoSizeStn %>%
  filter(Cast %in% selected_casts_day) %>%
  mutate(Cast = factor(Cast, levels = reversed_casts_day))

ggplot(phyto_subset_day, aes(x = "", y = Percent, fill = factor(Size))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ Cast, labeller = labeller(Cast = id.labs), nrow=1) +
  scale_fill_manual(values = c("#495d86", "#d9b021", "#d26424")) +
  labs(title = "Phytoplankton Size Composition for Day Stations", fill = "Size [µm]") +
  theme_void()
# ggsave('PhytoDayPie_poster.png', width = 24, height = 36, units = "in") #for poster
# ggsave('PhytoDayPie_presentation.png', width = 10, height = 4, dpi = 300, units = "in") #for presentation

# Night
selected_casts_night <- c(9, 18, 27, 36, 38)
phytoSizeStn13 <- phytoSizeStn %>% filter(Cast %in% selected_casts_night)

ggplot(phytoSizeStn13, aes(x = "", y = Percent, fill = factor(Size))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ Cast, labeller = labeller(Cast = id.labs), nrow = 1) +
  scale_fill_manual(values = c("#495d86", "#d9b021", "#d26424")) +
  labs(title = "Phytoplankton Size Composition for Night Stations", fill = "Size [µm]") +
  theme_void()




# -------------------------------------------------------------------------------

# -------------------------------- ZOOPS ---------------------------------------

zoops <- read.csv(paste(here(), 'Biomass filter weights_USE_THIS.csv', sep='/')) # Emma's
# zoops <- read_xlsx(paste(here(), 'Data/Biomass filter weights_USE_THIS .xlsx', sep='/'), sheet = 1)  # Johanna's
head(zoops)

# Clean up zooplankton data
zoops <- zoops %>% 
  filter(net_cast_number <= 13) %>% 
  select(net_cast_number, size_fraction, net_dry_weight) %>% 
  group_by(net_cast_number, size_fraction) %>% 
  summarise(net_dry_weight=sum(net_dry_weight))

zoopTrend <- lm(size_fraction ~ net_dry_weight, data=zoops)
head(zoops)

#Create a dataframe using the edges of the bins to calculate bin width 
bin_edges <- data.frame(
  size_fraction = c(200, 500, 1000, 2000, 5000),
  lower_bound = c(200, 500, 1000, 2000, 5000),
  upper_bound = c(500, 1000, 2000, 5000, 10000)
) %>%
  mutate(bin_width = upper_bound - lower_bound)

# Merge with zoops data and calculate normalized biomass
# Create columns with  calculated normalized biomass and then add log biomass and  log size columns 
normalized_biomass <- zoops %>%
  left_join(bin_edges, by = "size_fraction") %>%
  mutate(
    normalized_biomass = net_dry_weight / bin_width,
    log_normalized_biomass = log2(normalized_biomass),
    log_normalized_size = log2(size_fraction))

library(ggpmisc)
ggplot(normalized_biomass, aes(x = log_normalized_size, y = log_normalized_biomass)) +
  geom_point(color = "#8ab69c") +
  geom_smooth(method = "lm", se = FALSE, color = "#49755b", linewidth = 0.6) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 3,
    label.x = "right",
    label.y = "top"
  ) +
  facet_wrap(~ net_cast_number, ncol = 4) +
  labs(
    x = expression(log[2]~"Size Fraction [µm]"),
    y = expression(log[2]~"Nomalized Biomass [g]"),
    title = "Zooplankton Biomass Spectrum by Station"
  ) +
  theme_bw(base_size = 12)
# ggsave('ZoopSizeAbun_linearR_Normlog2.png', width=10, height = 5.625, dpi = 300, units = 'in')


# Day Stations Zooplankton Biomass Spectrum
# Filter data for only day stations 
norm_biomass_day <- normalized_biomass %>%
  filter(net_cast_number %in% c(1, 3, 4, 6, 7, 9, 10, 12))

ggplot(norm_biomass_day, aes(x = log2(size_fraction), y = log2(normalized_biomass))) +
  geom_point(color = "#8ab69c") +
  geom_smooth(method = "lm", se = FALSE, color = "#49755b", linewidth = 0.6) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 3,
    label.x = "right",
    label.y = "top"
  ) +
  facet_wrap(~ net_cast_number, ncol = 4) +
  labs(
    x = expression(log[2]~"Size Fraction [µm]"),
    y = expression(log[2]~"Nomalized Biomass [g]"),
    title = "Zooplankton Biomass Spectrum by Day Stations"
  ) +
  theme_bw(base_size = 12)
# ggsave('DAYZoopSizeAbun_linearR_Normlog2.png', width=10, height = 5.625, dpi = 300, units = 'in')

# Night Stations Zooplankton Biomass Spectrucm
norm_biomass_night <- normalized_biomass %>%
  filter(net_cast_number %in% c(2, 5, 8, 11, 13))

ggplot(norm_biomass_night, aes(x = log2(size_fraction), y = log2(normalized_biomass))) +
  geom_point(color = "#8ab69c") +
  geom_smooth(method = "lm", se = FALSE, color = "#49755b", linewidth = 0.6) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 3,
    label.x = "right",
    label.y = "top"
  ) +
  facet_wrap(~ net_cast_number, ncol = 4) +
  labs(
    x = expression(log[2]~"Size Fraction [µm]"),
    y = expression(log[2]~"Nomalized Biomass [g]"),
    title = "Zooplankton Biomass Spectrum by Night Stations"
  ) +
  theme_bw(base_size = 12)
# ggsave('NIGHTZoopSizeAbun_linearR_Normlog2.png', width=10, height = 5.625, dpi = 300, units = 'in')











