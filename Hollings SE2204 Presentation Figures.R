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
library(MBA)
library(reshape2)

# ---------------------------------- DATA --------------------------------------
# Set working directory
myDir <- paste(here(), sep='/') # Emma's file path
# myDir <- paste(here(), 'Data', sep='/')  # Johanna's File path
setwd(myDir)

# Read in files 
ctdAll <- read.csv('CTD_data_forAnalysis.csv')
stnInfo <- read.csv('CTD_metadata_forAnalysis.csv')
ctdMeta <- read.csv('CTD_log.csv')
ctdCNV <- read.csv('SE2204_CTDdata_fromCNVfile.csv')
ctdCNV <- ctdCNV %>%  
  mutate(newLat = latitude, Depth=depth) # change column names to work with function for plotting section plots

# Make a table with labels you want and the index that we can use for plotting
id.labs <- stnInfo$Station2
names(id.labs) <- stnInfo$Cast

# Read in nutrient data and clean out the '<', 'm', and 'NA's'
nut <- read.csv('SE2204_nutrient_metadata_USE_THIS.csv')
# Adding Cast column that is the same as the CTD cast numbers. 
nut <- nut %>% 
  left_join(stnInfo[,c('Station2', 'Cast')], by=c('Station'='Station2')) %>% 
  mutate(newLat = Latitude)
head(nut)
# Remove the m from nutrient file depths and create a new column with numeric depth only
nut$Depth <- as.numeric(substr(nut$Depth, 1, nchar(nut$Depth)-1))
head(nut)
# Remove the contaminated data point
nut[49,8:11] <- NA

nut$Ammonia <- ifelse(nut$Ammonia == "<0.02", 0.01, as.numeric(nut$Ammonia))
nut$Phosphate <- ifelse(nut$Phosphate == "<0.008", 0.007, as.numeric(nut$Phosphate))
nut$Silicate <- as.numeric(nut$Silicate)
nut$Date <- as.Date(nut$Date, '%m/%d/%y') 


# Phytoplankton data
#phyto <- read.csv(paste(here(), 'fluorometry_SE2204.csv', sep='/'))  # Emma's
phyto <- read.csv('fluorometry_SE2204.csv')  # Johanna's
head(phyto)

# Clean up the phytoplankton data and make sure and turn filter into sizes
phyto <- phyto %>% 
  #filter(Filter != 'bulk') %>% 
  mutate(Size=as.numeric(Filter))
# Turning a character (bulk) into a number produces an NA. To put the size in there instead we run the below line. Bulk filters are 0.7um pore size
phyto$Size[is.na(phyto$Size)] <-  0.7
head(phyto)
# Make file for nice station plotting
id.labs <- phyto$Station
names(id.labs) <- phyto$Cast
head(id.labs)

#Link Cast labels to the Station ID
cast_labels <- phyto %>%
  distinct(Cast, Station) %>%
  arrange(Cast)
label_vector <- setNames(cast_labels$Station, cast_labels$Cast)

# Add flow cytometry data
cyto <- read.csv('cytometry_summary.csv')
cytoAllDepth <- cyto %>% 
  group_by(Station, Longitude, Latitude, Date) %>% 
  summarise(PRO=sum(PRO_per_mL, na.rm=T), SYN=sum(SYN_per_mL, na.rm=T), PEUK=sum(PEUK_per_mL, na.rm=T), HBACT=sum(HBACT_per_mL, na.rm=T)) 
cytoAllDepth$TotalCounts <- rowSums(cytoAllDepth[,5:8])
# Add casts so can merge with other data
cytoAllDepth$Cast <- c(8,35,36,37,38,9,10,17,18,19,26,27,28,5,2,14,11,23,20,32,29,39,43)
# make wide for easier plotting in ggplot
cytoAllDepth <- cytoAllDepth %>% 
  pivot_longer(PRO:HBACT, names_to = 'Phytos', values_to = 'Count') %>% 
  select(Station, Cast, Date, Longitude, Latitude, Phytos, Count, TotalCounts) %>% 
  mutate(Percent=Count/TotalCounts*100)
head(cytoAllDepth)


# Zooplankton data
# zoops <- read.csv(paste(here(), 'Biomass filter weights_USE_THIS.csv', sep='/')) # Emma's
zoops <- read.csv('Data/Biomass filter weights_USE_THIS.csv')  # Johanna's
head(zoops)



# -------------------------------------------------------------------------------

#------------------------------ Final Figures ---------------------------------------

# -------------------------------------------------------------------------------

# Track line map
# Read in files 
ctdAll <- read.csv('CTD_data_forAnalysis.csv')
stnInfo <- read.csv('CTD_metadata_forAnalysis.csv')
ctdMeta <- read.csv('CTD_log.csv')

# Make a table with labels you want and the index that we can use for plotting
id.labs <- stnInfo$Station2
@@ -46,6 +47,36 @@ nut$Date <- as.Date(nut$Date, '%m/%d/%y')
# -------------------------------------------------------------------------------

#------------------------------ Final Figures ---------------------------------------
# Track line map
# Read in waypoints
all_wpts <- ctdMeta
all_wpts_short <- all_wpts %>% 
  select(LonDecimalDegree, LatDecimalDegree, StnType) %>% 
  rename(lon=LonDecimalDegree, lat=LatDecimalDegree)

# Make acctual track line plot
ggplot() +
  borders("world", 
          xlim = c(-160, -145), 
          ylim = c(18, 25),
          fill="gray",colour="grey40", size=0.25) +
  coord_map() +
  geom_path(data=all_wpts_short, aes(lon, lat), color='gray30') +
  geom_point(data=all_wpts, aes(LonDecimalDegree, LatDecimalDegree, color=StnType)) + 
  scale_color_manual(values = c('base'='orange', 'extended'='steelblue'), name='Station') +
  geom_text(data=all_wpts[which(all_wpts$StnType == 'extended')[c(2,8,14,20,26)],], aes(LonDecimalDegree, LatDecimalDegree, label=LETTERS[1:5]), hjust = 0, nudge_x = 0.85, size=3) +
  geom_text(data=all_wpts[which(all_wpts$StnType == 'base'),], aes(LonDecimalDegree, LatDecimalDegree, label=1:13), hjust = 0, nudge_x = -1.25, size=3) +
  #geom_hline(yintercept = 17.6, linetype='dashed', color='gray', linewidth=0.5) + 
  scale_x_continuous(breaks=seq(-160,-145,by=5), labels=seq(160,145,by=-5), limits=c(-160,-145)) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab('Longitude (°W)') +
  ylab('Latitude (°N)') +
  ylim(c(8,32)) 
# ggsave('Map_line.png', width = 24, height = 36, units = "in") #for poster
# ggsave('Map_line_presentation.png', width = 10, height = 7.5, dpi = 300, units = "in") #for presentation

# ggsave('Map.png', width = 24, height = 36, units = "in") #for poster
# ggsave('Map_presentation.png', width = 10, height = 7.5, dpi = 300, units = "in") #for presentation


# -------------------------------------------------------------------------------
# ----------------------------- OCEANOGRAPHY ------------------------------------
# Panel Plot of Temp, Salinity, Oxygen, Nitrate, Fluorescence 
plot_ocng_section <- function(data, ocng_var, Res1, Res2, title_label, Units, Color='turbo', ContourLine=F) {
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
  p <- ggplot(data = interp_df, aes(x = newLat, y = Depth)) +
    geom_raster(aes(fill = OCNVar)) +
    scale_fill_viridis_c(option = Color) +
    scale_y_reverse() +
    scale_x_reverse() +
    guides(size = "none", 
           fill = guide_colourbar(title.position = "right"), 
           title.theme = element_text(angle = 270, hjust = 0.5, vjust = 0.5)) +
    labs(x = NULL,  y = NULL, fill = paste0(title_label, Units)) +
    coord_cartesian(expand = 0) +
    theme(legend.title = element_text(angle = 90, hjust=0.5), 
          legend.direction = "vertical",
          legend.key.height = unit(1, 'null'), 
          legend.key.width = unit(0.5, 'cm'), 
          legend.margin = margin(0,0,0,0))
    
  # Add contour line if toggle is set to true. Default is no contour line
    if (ContourLine == T) {
      if (ocng_var == 'oxygen') {
        p <- p + geom_contour(aes(z=OCNVar), colour='black', alpha=0.5, breaks=c(45,89))
      } else {
          p <- p + geom_contour(aes(z = OCNVar), colour = "black", alpha = 0.2)
      }
      }
  return(p)
}


# Temperature
TempSPlot <- plot_ocng_section(data = ctdCNV, ocng_var = "temperature", Res1 = 300, Res2 = 300, title_label = "Temperature", Units = ' [°C]', ContourLine = T)
TempSPlot
# Salinity
SalinitySPlot <- plot_ocng_section(data = ctdCNV, ocng_var = "salinity", Res1 = 1000, Res2 = 1000, title_label = "Salinity", Units = " [PSU]", Color = "viridis", ContourLine = T)
SalinitySPlot
# Oxygen
OSPlot <- plot_ocng_section(data = ctdCNV, ocng_var = "oxygen", Res1 = 400, Res2 = 400 , title_label = "Oxygen", Units = " [μmol/kg]", Color = "turbo", ContourLine = T)
OSPlot
# Nutrients
NSectionPlot <- plot_ocng_section(data = nut, ocng_var = "Nitrate..Nitrite", Res1 = 300, Res2 = 300 , title_label = "Nitrate + Nitrite", Units = " [μmol/L]", Color = "viridis", ContourLine = T)
# Add sample points to the nutrient plot
NSectionPlot <- NSectionPlot + geom_point(data = nut, aes(x = Latitude, y = Depth),
           colour = "black", size = 0.2, alpha = 0.4, shape = 8)
NSectionPlot

# Stitch images together into one plot
 library(cowplot)
 combined_plotV <- plot_grid(TempSPlot , SalinitySPlot , OSPlot , NSectionPlot, ncol = 1, align = "v")
 combined_plotV

# MAke final plot and add labels
final_plot <- ggdraw(combined_plotV) +
  draw_label("Latitude", x = 0.5, y = 0.000009, vjust = 0, angle = 0, size = 12) +  
  draw_label("Depth [m]", x = 0.000005, y = 0.5, vjust = 1, angle = 90, size = 12) 

final_plot

combined_plotV <- combined_plotV +
  theme(plot.margin = margin(t = 10, r = 10, b = 30, l = 40))  # bottom and left margins increased

final_plot <- ggdraw(combined_plotV) +
  draw_label("Latitude", x = 0.5, y = 0.05, vjust = 1, angle = 0, size = 12) +  
  draw_label("Depth [m]", x = 0.03, y = 0.5, vjust = 1, angle = 90, size = 12)
final_plot

# ggsave('SectionPlots_presentation.png', width = 13, height = 8, dpi = 300, units = "in") #for presentation
# ggsave('SectionPlots_poster.png', width = 24, height = 36, units = "in") #for poster
# ggsave('SectionPlots_presentation_square.png', width = 10, height = 7.5, dpi = 300, units = "in") #for presentation

# -------------------------------------------------------------------------------
# -------------------------------- PHYTO ---------------------------------------
head(phyto)
bulk <- phyto %>% filter(Filter == "bulk")
head(bulk)
bulk <- bulk %>% left_join(stnInfo, by = "Cast")



# plot_section <- function(data, nutrient_col, title_label) {
#   clean_data <- data %>%
#     select(Lat, Depth.x, !!sym(nutrient_col)) %>%
#     rename(Depth.x = Depth.x, NutVar = !!sym(nutrient_col)) %>%
#     filter(!is.na(Lat), !is.na(Depth.x), !is.na(NutVar))
#   
#   sample_points <- clean_data
#   
#   # Interpolation with MBA
#   interp <- mba.surf(clean_data, no.X = 500, no.Y = 500, extend = TRUE)
#   dimnames(interp$xyz.est$z) <- list(interp$xyz.est$x, interp$xyz.est$y)
#   
#   # Convert to dataframe
#   interp_df <- melt(interp$xyz.est$z, varnames = c("Lat", "Depth.x"), value.name = "NutVar") %>%
#     mutate(NutVar = round(NutVar, 1))
#   
#   # Plot
#   ggplot(data = interp_df, aes(x = Lat, y = Depth.x)) +
#     geom_raster(aes(fill = NutVar)) +
#     scale_fill_viridis_c(breaks = c(0.02, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)) +
#     scale_y_reverse() +
#     geom_contour(aes(z = NutVar), binwidth = 1, colour = "black", alpha = 0.2) +
#     geom_point(data = sample_points, aes(x = Lat, y = Depth.x),
#                colour = "black", size = 0.2, alpha = 0.4, shape = 8) +
#     guides(size = "none", 
#            fill = guide_colourbar(title.position = "right"), 
#            title.theme = element_text(angle = 270, hjust = 0.5, vjust = 0.5)) +
#     labs(
#       # y = "Depth [m]",
#       # x = "Latitude",
#       x = NULL, 
#       y = NULL, 
#       fill = "Bulk Chlorophyll [µg/L]"
#       # title = paste("SE2204", title_label, "Section Plot"),
#       # subtitle = "Interpolated over depth and space; \nblack dots show actual sampling locations."
#     ) +
#     coord_cartesian(expand = 0) +
#     theme(legend.title = element_text(angle = 90, hjust=0.5), 
#           legend.direction = "vertical",
#           legend.key.height = unit(1, 'null'), 
#           legend.key.width = unit(0.5, 'cm'), 
#           legend.margin = margin(0,0,0,0))
# }
# plot_section(data = bulk, nutrient_col = "Chlorophyll", title_label = "Bulk")


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
    scale_x_reverse() +
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
  labs(fill = "Size [µm]") +
  theme_void()
# ggsave('PhytoDayPie13_presentation.png', width = 10, height = 4, dpi = 300, units = "in") #for presentation



# Day
selected_casts_day <- c(8,10,17,19,26,28,35,37)
# reversed_casts_day <- rev(selected_casts_day)

phyto_subset_day <- phytoSizeStn %>%
  filter(Cast %in% selected_casts_day) %>%
  mutate(Cast = factor(Cast, levels = selected_casts_day))

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


# Pythoplankton composition and chlorophyll
# Ryan requested figure
phytoSizeStn %>% 
  mutate(NS=if_else(Cast > 28, 'South', 'North')) %>% 
  filter(Size > 0.2) %>% 
  ggplot(aes(x = ChlAllDepth, y = Percent)) + 
  geom_smooth(method='lm', se = T, color='darkgray', alpha=0.2) +
    geom_point(aes(color=NS), size=2) + 
    scale_color_manual(values = c("North" = "#3288bd", "South" = "#d53e4f"), name='') +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    xlab('Total chlorophyll-a [µg/L]') + 
    ylab('Percent of total chlorophyll-a')
  
# ggsave('Percent_TotalChl_poster.png', width = 24, height = 36, units = "in") #for poster
# ggsave('Percent_TotalChl__presentation.png', width = 10, height = 4, dpi = 300, units = "in") #for presentation


# -------------------------------------------------------------------------------

# -------------------------------- ZOOPS ---------------------------------------

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

zoops <- read.csv(paste(here(), 'Biomass filter weights_USE_THIS.csv', sep='/')) # Emma's
# zoops <- read.csv(paste(here(), 'Data/Biomass filter weights_USE_THIS.csv', sep='/'))  # Johanna's
head(zoops)

ggplot(zoops, aes(x = net_cast_number, y = standard_haul_factor)) +
  geom_col(fill = '#414487FF')+ 
  theme_bw()
# ggsave('SHF_Stn6.png', width=10, height = 5.625, dpi = 300, units = 'in')

# Plot for VWS
ggplot(zoops, aes(x = net_cast_number, y = volume_water_strained)) +
  geom_col(fill = '#440154FF') + 
  theme_bw()
# ggsave('VWS_Stn6.png', width=10, height = 5.625, dpi = 300, units = 'in')


# SHF and VWS plotted side by side 
zoops_long <- zoops %>%
  pivot_longer(cols = c(standard_haul_factor, volume_water_strained),
               names_to = "variable", values_to = "value")

# Group by Region
zoops$Region <- ifelse(zoops$net_cast_number %in% 1:8, "North", "South")
zoops$time_of_day <- ifelse(zoops$net_cast_number %in% c(2, 5, 8, 11, 13), "Night", "Day")
zoops <- zoops %>% 
  select(4,5,10,11,18,19,22,23,24,36,37)


# Calculate scale factor to standardize the new y-axis to the pre-existing y-axis 
scale_factor <- max(zoops$standardized_SHF, na.rm = TRUE) / 
  max(zoops$standardized_plankton_volume * 1000, na.rm = TRUE)

# Omit na's in the net_cast_number added by the excel sheet, no data lost
zoops <- zoops[!is.na(zoops$net_cast_number), ]

zoops$alpha <- ifelse(zoops$time_of_day == 'Day', 0.7, 1)

# SHF and Standardized Biomass Plot with two Y-Axis 
ggplot(zoops, aes(x = factor(net_cast_number))) +
  geom_col(aes(y = standardized_SHF, fill = Region)) +
  geom_point(aes(y = standardized_plankton_volume * 1000 * scale_factor, shape=time_of_day), color = "black", size = 2.5) +
  theme_bw() +
  labs(
    x = "Station",
    y = "Standard Haul Factor",
    fill = "Region", shape='') +
  scale_y_continuous(
    name = "Standard Haul Factor",
    sec.axis = sec_axis(~ . / (scale_factor*1000), 
                        name = expression("Standardized Plankton Biomass [g/m"^3*"]"))) +
  ggtitle("Standardized Zooplankton Biomass") +
  scale_fill_manual(values = c("North" = "#3288bd", "South" = "#d53e4f")) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank())
# ggsave('SHFZoop_doubleyaxis.png', width=10, height = 5.625, dpi = 300, units = 'in')


# SHF Plot with one Y-Axis (Red and Blue different for Day/Night)
zoops$RegionTime <- paste(zoops$Region, zoops$time_of_day, sep = "-")
zoops$RegionTime <- factor(zoops$RegionTime, levels = c("North-Day", "North-Night", "South-Day", "South-Night"))

ggplot(zoops, aes(x = factor(net_cast_number), y = standardized_SHF, fill = RegionTime)) +
  geom_col() +
  theme_bw() +
  labs(
    x = "Station",
    y = "Standard Haul Factor",
    fill = " ") +
  scale_y_continuous(name = "Standard Haul Factor") +
  ggtitle("Standardized Zooplankton Biomass") +
    scale_fill_manual(values = c(
      "North-Day" = "#07abcc",
      "North-Night" = "#2166ac",
      "South-Day" = "#df1317",
      "South-Night" = "#8A0407"
    )) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        legend.position = "bottom")
# ggsave('SHFZoop_RedBlue.png', width=10, height = 5.625, dpi = 300, units = 'in')


ggplot(zoops, aes(x = factor(net_cast_number), y = standardized_SHF, fill = RegionTime)) +
  geom_col() +
  theme_bw() +
  labs(
    x = "Station",
    y = "Standard Haul Factor",
    fill = "Region-Time"
  ) +
  ggtitle("Standardized Zooplankton Biomass") +
  scale_y_continuous(name = "Standard Haul Factor") +
  scale_fill_manual(values = c(
    "North-Day" = "#66c2a5",
    "North-Night" = "#3288bd",
    "South-Day" = "#fc8d62",
    "South-Night" = "#d53e4f"
  )) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# -----------------------------------------------------------------------------

# -------------------------------- FlowCytometry ------------------------------
library(tidyverse)

# Assuming your dataset is called `phyto_data`
# Here's how you could reshape and plot it:

# Step 1: Pivot longer to get Phytos in one column
phyto_long <- cyto %>%
  pivot_longer(
    cols = c(PRO_per_mL, SYN_per_mL, PEUK_per_mL),
    names_to = "Phytos",
    values_to = "Abundance_per_mL"
  ) %>%
  mutate(
    Abundance_10e3 = Abundance_per_mL / 1000,  # Convert to x10³ ml⁻¹
    Phytos = recode(Phytos,
                    "PRO" = "Prochlorococcus",
                    "SYN" = "Synechococcus",
                    "PEUK" = "Photosynthetic Eukaryotes")
  )

# Step 2: Plot
ggplot(phyto_long, aes(x = Abundance_10e3, y = Depth)) +
  geom_point() +
  scale_y_reverse() +  # Depth increases downward
  facet_wrap(~ Station, scales = "free_x") +
  labs(
    title = "Phytoplankton abundance (×10³ ml⁻¹) vs Depth",
    x = "Abundance (×10³ ml⁻¹)",
    y = "Depth (m)"
  ) +
  theme_minimal()

library(tidyverse)

# Step 1: Reshape to long format
phyto_long <- cyto %>%
  pivot_longer(
    cols = c(PRO_per_mL, SYN_per_mL, PEUK_per_mL),
    names_to = "Phytos",
    values_to = "Abundance_per_mL"
  ) %>%
  mutate(
    Abundance_10e3 = Abundance_per_mL / 1000,
    Phytos = recode(Phytos,
                    "PRO_per_mL" = "Prochlorococcus",
                    "SYN_per_mL" = "Synechococcus",
                    "PEUK_per_mL" = "Photosynthetic Eukaryotes")
  )

# Step 2: Plot — facet by Station, shape by Phytos
ggplot(phyto_long, aes(x = Abundance_10e3, y = Depth, shape = Phytos)) +
  geom_point(size = 3) +
  geom_line(aes(group = Phytos), linetype = "dotted") +
  scale_y_reverse() +
  facet_wrap(~ Station) +
  labs(
    title = "Phytoplankton abundance (×10³ ml−1) vs Depth by Station",
    x = "Abundance (×10³ml³)",
    y = "Depth (m)",
    shape = "Phytoplankton"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))


ggplot(cytoAllDepth, aes(x = "", y = Percent, fill = factor(Phytos))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ Cast, labeller = labeller(Cast = id.labs)) +
  scale_fill_manual(values = c("#495d86", "#d9b021", "#d26424", "#414487FF")) +
  labs(title = "Phytoplankton Size Composition", fill = "") +
  theme_void()



PS <- cyto %>% group_by(Station, Longitude, Latitude, Date) %>% 
  summarise(SYN=sum(SYN_per_mL, na.rm=T), PEUK=sum(PEUK_per_mL, na.rm=T)) 

PS$TotalCounts <- rowSums(PS[,5:6])
# Add casts so can merge with other data
PS$Cast <- c(8,35,36,37,38,9,10,17,18,19,26,27,28,5,2,14,11,23,20,32,29,39,43)
PS <- PS %>% 
  pivot_longer(SYN:PEUK, names_to = 'Phytos', values_to = 'Count') %>% 
  select(Station, Cast, Date, Longitude, Latitude, Phytos, Count, TotalCounts) %>% 
  mutate(Percent=Count/TotalCounts*100)
head(PS)


ggplot(PS, aes(x = "", y = Percent, fill = factor(Phytos))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ Cast, labeller = labeller(Cast = id.labs)) +
  scale_fill_manual(values = c("#d9b021", "#414487FF")) +
  labs(title = "Phytoplankton Size Composition", fill = "") +
  theme_void()
# ggsave('PeukSyn_Pie.png', width=10, height = 5.625, dpi = 300, units = 'in')



# Not helpful
ProPS <- cyto %>% group_by(Station, Longitude, Latitude, Date) %>% 
  summarise(PRO=sum(PRO_per_mL, na.rm=T), SYN=sum(SYN_per_mL, na.rm=T), PEUK=sum(PEUK_per_mL, na.rm=T)) 

ProPS$TotalCounts <- rowSums(ProPS[,5:7])
# Add casts so can merge with other data
ProPS$Cast <- c(8,35,36,37,38,9,10,17,18,19,26,27,28,5,2,14,11,23,20,32,29,39,43)
ProPS <- ProPS %>% 
  pivot_longer(PRO:PEUK, names_to = 'Phytos', values_to = 'Count') %>% 
  select(Station, Cast, Date, Longitude, Latitude, Phytos, Count, TotalCounts) %>% 
  mutate(Percent=Count/TotalCounts*100)
head(ProPS)


ggplot(ProPS, aes(x = "", y = Percent, fill = factor(Phytos))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ Cast, labeller = labeller(Cast = id.labs)) +
  scale_fill_manual(values = c("#d9b021", "#414487FF", "#d26424")) +
  labs(title = "Phytoplankton Size Composition", fill = "Size [µm]") +
  theme_void()


PS <- cyto %>% group_by(Station, Longitude, Latitude, Date) %>% 
  summarise(SYN=sum(SYN_per_mL, na.rm=T), PEUK=sum(PEUK_per_mL, na.rm=T)) 

PS$TotalCounts <- rowSums(PS[,5:6])
# Add casts so can merge with other data
PS$Cast <- c(8,35,36,37,38,9,10,17,18,19,26,27,28,5,2,14,11,23,20,32,29,39,43)
PS <- PS %>% 
  pivot_longer(SYN:PEUK, names_to = 'Phytos', values_to = 'Count') %>% 
  select(Station, Cast, Date, Longitude, Latitude, Phytos, Count, TotalCounts) %>% 
  mutate(Percent=Count/TotalCounts*100)
head(PS)


ggplot(PS, aes(x = "", y = Percent, fill = factor(Phytos))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ Cast, labeller = labeller(Cast = id.labs)) +
  scale_fill_manual(values = c("#d9b021", "#414487FF")) +
  labs(title = "Phytoplankton Size Composition", fill = "") +
  theme_void()
# ggsave('PeukSyn_Pie.png', width=10, height = 5.625, dpi = 300, units = 'in')