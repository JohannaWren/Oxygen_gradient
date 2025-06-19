# Date: June 16, 2025
# Author: Johanna LK Wren & Emma Scott-Wellman
# email: johanna.wren@noaa.gov emma.scott-wellman@noaa.gov
# Description: Cleaned Script to take an exploratory look at CTD data from SE-22-04 Hot Spot/Oxygen gradient cruise


# Load libraries
library(dplyr)
library(ggplot2)
library(here)
library(lubridate)
library(tidyr)
library(data.table)
library(akima) 
library(MBA)
library(reshape2)

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


# --------------------- Depth Profile Function ---------------------------------

# Create Depth Profile Function 
depthProfile <- function(CTDdata, PlotVar, VarName, figTitle) {
  # Calculate depth of oxygen minimum
  o2_min <- CTDdata %>% 
    group_by(Cast) %>% 
    slice(which.min(get(PlotVar))) %>% 
    select(Cast, Depth)
  # Make depth profile with line showing oxygen minimum
  p <- CTDdata %>%
    ggplot(aes(x=get(PlotVar), y=Depth)) +
    geom_path() +
    scale_y_reverse() +
    facet_wrap(.~Cast, labeller=labeller(Cast=id.labs)) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    {if (PlotVar %in% c('Oxygen', 'FlECO.AFL')) geom_hline(data = o2_min, aes(yintercept = Depth), linetype= 'dashed', color='red')} +
    xlab(VarName) + ylab('Depth [m]') +
    ggtitle(figTitle)
  return(p)
}


# Make oxygen profile with oxygen min marked with line
oxyProfile <- depthProfile(ctdAll, 'Oxygen', 'Oxygen [umol/kg]', 'Oxygen depth profile for SE2204')
oxyProfile
#ggsave(plot=oxyProfile, filename='O2DepthProfiles_AllStns_min.pdf', width=11, height = 8, dpi = 300, units = 'in')
#ggsave(plot=oxyProfile, filename='O2DepthProfiles_AllStns_min.png', width=10, height = 5.625, dpi = 300)

#Thermocline Profiles 
tempProfile <- depthProfile(ctdAll, "Temperature", 'Temperature [°C]', 'Temerature depth profile for SE2204')
tempProfile
# ggsave('TempDepthProfiles_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')
# ggsave('TempDepthProfiles_AllStns.png', width=10, height = 5.625, dpi = 300)

# Halocline Profiles
salProfile <- depthProfile(ctdAll, "Salinity", 'Salinity [PSU]', 'Salinity depth profile for SE2204')
salProfile
# ggsave('SalinityDepthProfiles_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')
# ggsave('SalinityDepthProfiles_AllStns.png', width=10, height = 5.625, dpi = 300)

# Pycnocline Profiles 
DensProfile <- depthProfile(ctdAll, "Density", labs(x = expression('σ'[θ])), 'Density depth profile for SE2204')
DensProfile
# ggsave('DensityDepthProfiles_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')
# ggsave('DensityDepthProfiles_AllStns.png', width=10, height = 5.625, dpi = 300)

# Fluorescence Profiles 
FluorProfile <- depthProfile(ctdAll, "Florescence", 'Fluorescence [ug/L]', 'Temerature depth profile for SE2204')
FluorProfile
# ggsave('FluorDepthProfiles_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')
# ggsave('FluorDepthProfiles_AllStns.png', width=10, height = 5.625, dpi = 300)





# --------------------- Interpolated Depth Profile Function -------------------
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

# -----------------------------------------------------------------------------
# Plot from NutriVis
nutriDN <- nut %>%
  select(Latitude, Depth2, Nitrate..Nitrite) %>%
  rename(NutVar = Nitrate..Nitrite) %>%
  filter(!is.na(Latitude), !is.na(Depth2), !is.na(NutVar))

# Interpolation with MBA
ctd_mba <- mba.surf(nutriDN, no.X = 300, no.Y = 300, extend = TRUE)
dimnames(ctd_mba$xyz.est$z) <- list(ctd_mba$xyz.est$x, ctd_mba$xyz.est$y)

# Convert to dataframe
mba_df <- melt(ctd_mba$xyz.est$z, varnames = c("Latitude", "Depth"), value.name = "NutVar") %>%
  mutate(NutVar = round(NutVar, 1))

# Plot
ggplot(data = mba_df, aes(x = Latitude, y = Depth)) +
  geom_raster(aes(fill = NutVar)) +
  scale_fill_viridis_c() +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_contour(aes(z = NutVar), binwidth = 1, colour = "black", alpha = 0.2) +
  geom_point(data = nutriDN, aes(x = Latitude, y = Depth2),
             colour = "black", size = 0.2, alpha = 0.4, shape = 8) +
  labs(
    y = "Depth (m)",
    x = "Latitude",
    fill = "Nitrate + Nitrite\n(µmol/L)",
    title = "SE2204 All Stations",
    subtitle = "Interpolated over depth and space; \nBlack dots show actual sampling locations.\nUsed lat instead of station numbers"
  ) +
  coord_cartesian(expand = 0) +
  theme_minimal()

# ---------------------------------------------------------------------------
interpolate_nutrient_plot <- function(data, lat_col, depth_col, nutrient_col, VarName, figTitle) {
  lat_sym <- sym(lat_col)
  depth_sym <- sym(depth_col)
  nut_sym <- sym(nutrient_col)
  
  # Convert and clean data
  nut_data <- data %>%
    mutate(
      Latitude = as.numeric(!!lat_sym),
      Depth2 = as.numeric(!!depth_sym),
      Nutrient = as.numeric(!!nut_sym)
    ) %>%
    filter(!is.na(Latitude), !is.na(Depth2), !is.na(Nutrient))
  
  # Interpolation
  interp_result <- with(nut_data,
                        interp(x = Latitude, y = Depth2, z = Nutrient,
                               duplicate = "mean", linear = TRUE))
  
  # Create dataframe from interp result
  interp_df <- expand.grid(Latitude = interp_result$x, Depth2 = interp_result$y)
  interp_df$Nutrient <- as.vector(interp_result$z)
  
  # Plot
  plot_title <- paste("Interpolated", nutrient_col, "Concentration")
  
  int_plot <- ggplot(interp_df, aes(x = Latitude, y = Depth2, z = Nutrient)) +
    geom_contour_filled() +
    scale_y_reverse() +
    scale_fill_viridis_d() +
    labs(
      title = plot_title,
      x = "Latitude",
      y = "Depth (m)",
      fill = nutrient_col
    ) +
    theme_minimal() +
    xlab(VarName) + ylab('Depth [m]') +
    ggtitle(figTitle)
  return(int_plot)
}

# Section Plot for Silicate 
interpolate_nutrient_plot(data = nut, lat_col = 'Latitude', depth_col = 'Depth2', nutrient_col = 'Silicate', VarName = 'Silicate [umol/kg]', figTitle = 'SE2204 Interpolated Silicate Concentration' )

# Section Plot for Phosphate 
interpolate_nutrient_plot(data = nut, lat_col = 'Latitude', depth_col = 'Depth2', nutrient_col = 'Phosphate', VarName = 'Phosphate [umol/kg]', figTitle = 'SE2204 Interpolated Phosphate Concentration' )

# Section Plot for Nitrate+Nitrite 
interpolate_nutrient_plot(data = nut, lat_col = 'Latitude', depth_col = 'Depth2', nutrient_col = 'Nitrate..Nitrite', VarName = 'Silicate [umol/kg]', figTitle = 'SE2204 Interpolated Nitrate+Nitrite Concentration' )

# Section Plot for Ammonia
interpolate_nutrient_plot(data = nut, lat_col = 'Latitude', depth_col = 'Depth2', nutrient_col = 'Ammonia', VarName = 'Silicate [umol/kg]', figTitle = 'SE2204 Interpolated Ammonia Concentration' )


# --------------------- CTD, GLORYS, and WOA comparisons -----------------------

# Read in CLIMATOLOGY GLORYS Data 
# clim <- read.csv(paste0(here(), '/CTD/GLORYScomp/GLORYS_Climatology_JunJul_SE2204.csv'))
clim <- read.csv('GLORYS_Climatology_JunJul_SE2204.csv') #Emma's 
head(clim)

# Plot of CTD vs Climatology GLORYS Data 
ggplot() + 
  geom_path(data=ctdAll, aes(y=Depth, x=Oxygen, color='CTD')) +
  geom_path(data=clim, aes(y=Depth, x=Oxygen, color='GLORYS')) +
  scale_y_reverse() +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), scales = 'free_x') +
  scale_color_manual(
    name = "Data Source",  # Legend title
    values = c("CTD" = "blue", "GLORYS" = "red")) +
  labs(color = "Data Source") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle('Climatology Oxygen: GLORYS Model and SE2204 CTD Observations')
# ggsave('O2Climatology_GLORYS.png', width=10, height = 5.625, dpi = 300)

# Anomaly 
# First we need the depths to be the same for both datasets
oxyAnom <- function(CTDdata, ModelData, CastNr, Variable="Oxygen") {
  cst2ctd <- CTDdata %>% 
    filter(Cast == CastNr) %>% 
    select(Depth, get(Variable)) %>% 
    data.frame()
  # Then a matching one with GLORYS data
  cst2glo <- ModelData %>% 
    filter(Cast == CastNr) %>% 
    select(Depth, get(Variable))
  # Interpolate glorys data over the depths in the CTD data
  modelProfile <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$Depth)
  modProfile <- data.frame(Depth=modelProfile$x, Oxygen=modelProfile$y)
  anomCTDglo <- cst2ctd$Oxygen-modProfile$Oxygen %>% 
    bind_cols(cst2ctd$Depth) %>% 
    rename(OxygenAnom=...1, Depth=...2)
  return(anomXTDglo)
}




anomaly =ctdAll$Oxygen - clim$Oxygen
threshold <- 2 * sd(anomaly)
df.am <- data.frame( Depth = ctdAll$Depth, Anomaly = anomaly, Outlier = abs(anomaly > threshold)) 
Outlier <- abs(df.am$Anomaly) > threshold


#Read in MONTHLY GLORYS Data
oxymonth <- read.csv("../GLORYS_Monthly_JunJul_SE2204.csv")
oxymonth <- read.csv("GLORYS_Monthly_JunJul_SE2204.csv") #Emma's


#Plot of CTD vs Monthly GLORYS Data 
ggplot() + 
  geom_path(data=ctdAll, aes(y=Depth, x=Oxygen, color='CTD')) +
  geom_path(data=oxymonth, aes(y=Depth, x=Oxygen, color='GLORYS')) +
  scale_y_reverse() +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), scales = 'free_x') +
  scale_color_manual(
    name = "Data Source",  # Legend title
    values = c("CTD" = "blue", "GLORYS" = "red")) +
  labs(color = "Data Source") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle('Monthly Oxygen: GLORYS Model and SE2204 CTD Observations')
# ggsave('O2Monthly_GLORYS.png', width=10, height = 5.625, dpi = 300)


# Read in DAILY GLORYS Data 
oxy <- read.csv('../GLORYS_oxygen_SE2204.csv')
oxy <- read.csv('GLORYS_oxygen_SE2204.csv') #Emma's 
head(oxy)

# Plot of CTD vs Daily GLORYS Data 
ggplot() + 
  geom_path(data=ctdAll, aes(y=Depth, x=Oxygen, color='CTD')) +
  geom_path(data=oxy, aes(y=Depth, x=Oxygen, color='GLORYS')) +
  scale_y_reverse() +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), scales = 'free_x') +
  scale_color_manual(
    name = "Data Source",  # Legend title
    values = c("CTD" = "blue", "GLORYS" = "red")) +
  labs(color = "Data Source") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle('Daily Oxygen: GLORYS Model and SE2204 CTD Observations')
# ggsave('O2GLORYSDaily_CTD.png', width=10, height = 5.625, dpi = 300)




