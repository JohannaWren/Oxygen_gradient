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


# -----------------------------AOU--------------------------------------------
ctdAll$TK <- NA
ctdAll$TK <- ctdAll$Temperature + 273.15

AOU <- function(CTDdata, cast_id, TK, Variable1 = 'Oxygen', Variable2 = ctdAll$Salinity) {
  cst2ctd <- CTDdata %>% 
    filter(ctdAll$Cast == cast_id) %>% 
    select(Depth, all_of(Variable1)) %>% 
    data.frame()
  saturation <- -173.9894 + 
    255.5907 * (100 / TK) +
    146.4813 * log(TK / 100) -
    22.2040 * (TK / 100) +
    Variable2 * (-0.037362 + 0.016504 * (TK / 100) - 0.0020564 * (TK / 100)^2)
  output <- saturation - O2
  return(output)
}

ctdAll$AOU <- NA
casts <- unique(ctdAll$Cast)
for (cast_id in casts) {
  cast_data <- ctdAll[ctdAll$Cast == cast_id, ]
  aou_result <- AOU(CTDdata = cast_data, TK = cast_data$TK)
  ctdAll$AOU[ctdAll$Cast == cast_id] <- aou_result
}
ctdAll$TK <- ctdAll$Temperature + 273.15


# Define AOU function
AOU <- function(CTDdata, cast_id, TK, Variable1 = 'Oxygen', Variable2 = 'Salinity') {
  cast_data <- CTDdata %>% filter(Cast == cast_id)
  O2 <- cast_data[[Variable1]]
  Salinity <- cast_data[[Variable2]]
  
  # Calculate saturation (Murray and Riley (1969))
  saturation <- -173.9894 + 
    255.5907 * (100 / TK) +
    146.4813 * log(TK / 100) -
    22.2040 * (TK / 100) +
    Salinity * (-0.037362 + 0.016504 * (TK / 100) - 0.0020564 * (TK / 100)^2)
  
  # Calculate AOU
  output <- saturation - O2
  return(output)
}

ctdAll$AOU <- NA
casts <- unique(ctdAll$Cast)

for (cast_id in casts) {
  cast_data <- ctdAll[ctdAll$Cast == cast_id, ]
  aou_result <- AOU(CTDdata = cast_data, cast_id = cast_id, TK = cast_data$TK)
  ctdAll$AOU[ctdAll$Cast == cast_id] <- aou_result
}


interp_data <- ctdAll[!is.na(ctdAll$AOU) & !is.na(ctdAll$newLat) & !is.na(ctdAll$Depth), ]
interp_result <- with(interp_data, interp(x = newLat, y = Depth, z = AOU,
                                          duplicate = "mean",
                                          linear = TRUE,
                                          extrap = FALSE))
interp_df <- data.frame(
  expand.grid(newLat = interp_result$x, Depth = interp_result$y),
  AOU = as.vector(interp_result$z)
)

ggplot(interp_df, aes(x = newLat, y = Depth, fill = AOU)) +
  geom_raster(interpolate = TRUE) + 
  scale_y_reverse() +                  
  scale_fill_viridis_c(option = 'turbo') +
  labs(
    title = "Interpolated AOU by Latitude and Depth",
    x = "Latitude [°]",
    y = "Depth [m]",
    fill = "AOU"
  ) +
  theme_minimal()

# AOU Plot 2
aou_data <- ctdAll[!is.na(ctdAll$AOU) & !is.na(ctdAll$newLat) & !is.na(ctdAll$Depth), ]
aou_interp <- with(aou_data, interp(
  x = newLat,
  y = Depth,
  z = AOU,
  duplicate = "mean",
  linear = TRUE,
  extrap = FALSE
))

aou_df <- data.frame(
  expand.grid(newLat = aou_interp$x, Depth = aou_interp$y),
  AOU = as.vector(aou_interp$z)
)

AOUVis <- ggplot(data = aou_df, aes(x = newLat, y = Depth)) +
  geom_raster(aes(fill = AOU)) +
  scale_y_reverse() +
  scale_fill_viridis_c(option = "turbo") +
  geom_contour(aes(z = AOU), binwidth = 10, colour = "black", alpha = 0.3) +
  guides(size = "none", 
         fill = guide_colourbar(title.position = "right"), 
         title.theme = element_text(angle = 270, hjust = 0.5, vjust = 0.5)) +
  labs(
    # y = "Depth [m]",
    # x = "Latitude",
    y = NULL, 
    x = NULL,
    fill = "AOU [µmol/kg]",
    # title = "Apparent Oxygen Utilization (AOU)",
    # subtitle = "Interpolated across Latitude and Depth"
  ) +
  coord_cartesian(expand = 0) +
  theme(legend.title = element_text(angle = 90, hjust=0.5), 
        legend.direction = "vertical",
        legend.key.height = unit(1, 'null'), 
        legend.key.width = unit(0.5, 'cm'), 
        legend.margin = margin(0,0,0,0))



AOUVis

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
# ggsave(plot=oxyProfile, filename='O2DepthProfiles_AllStns_min.png', width=10, height = 5.625, dpi = 300)

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
FluorProfile <- depthProfile(ctdAll, "Flourescence", 'Fluorescence [ug/L]', 'Temerature depth profile for SE2204')
FluorProfile
# ggsave('FluorDepthProfiles_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')
# ggsave('FluorDepthProfiles_AllStns.png', width=10, height = 5.625, dpi = 300)

# -------------------------------- NUTRIENTS ----------------------------------

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
plot_nutrient_section <- function(data, nutrient_col, title_label) {
  clean_data <- data %>%
    select(Latitude, Depth2, !!sym(nutrient_col)) %>%
    rename(Depth = Depth2, NutVar = !!sym(nutrient_col)) %>%
    filter(!is.na(Latitude), !is.na(Depth), !is.na(NutVar))
  
  sample_points <- clean_data
  
  # Interpolation with MBA
  interp <- mba.surf(clean_data, no.X = 300, no.Y = 300, extend = TRUE)
  dimnames(interp$xyz.est$z) <- list(interp$xyz.est$x, interp$xyz.est$y)
  
  # Convert to dataframe
  interp_df <- melt(interp$xyz.est$z, varnames = c("Latitude", "Depth"), value.name = "NutVar") %>%
    mutate(NutVar = round(NutVar, 1))
  
  # Plot
  ggplot(data = interp_df, aes(x = Latitude, y = Depth)) +
    geom_raster(aes(fill = NutVar)) +
    scale_fill_viridis_c() +
    scale_y_reverse() +
    geom_contour(aes(z = NutVar), binwidth = 1, colour = "black", alpha = 0.2) +
    geom_point(data = sample_points, aes(x = Latitude, y = Depth),
               colour = "black", size = 0.2, alpha = 0.4, shape = 8) +
    guides(size = "none", 
           fill = guide_colourbar(title.position = "right"), 
           title.theme = element_text(angle = 270, hjust = 0.5, vjust = 0.5)) +
    labs(
      # y = "Depth [m]",
      # x = "Latitude",
      x = NULL, 
      y = NULL, 
      fill = paste0(title_label, " [µmol/L]"),
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

# For Nitrate + Nitrite
NSectionPlot <- plot_nutrient_section(nut, "Nitrate..Nitrite", "Nitrate + Nitrite")
NSectionPlot
# ggsave('NSectionPlot_interp.png', width=10, height = 5.625, dpi = 300)

# For Silicate
plot_nutrient_section(nut, "Silicate", "Silicate")
# ggsave('SiliSectionPlot_interp.png', width=10, height = 5.625, dpi = 300)

# For Phosphate
plot_nutrient_section(nut, "Phosphate", "Phosphate")
# ggsave('PhosSectionPlot_interp.png', width=10, height = 5.625, dpi = 300)

# For Ammonia
plot_nutrient_section(nut, "Ammonia", "Ammonia")
# ggsave('AmmonSectionPlot_interp.png', width=10, height = 5.625, dpi = 300)





# ----------------------   Test Fluorescence Plots  --------------------------

plot_nutrient_section <- function(data, nutrient_col, title_label) {
  clean_data <- data %>%
    select(newLat, Depth, !!sym(nutrient_col)) %>%
    rename(Depth = Depth, NutVar = !!sym(nutrient_col)) %>%
    filter(!is.na(newLat), !is.na(Depth), !is.na(NutVar))
  
  sample_points <- clean_data
  
  # Interpolation with MBA
  interp <- mba.surf(clean_data, no.X = 300, no.Y = 300, extend = TRUE)
  dimnames(interp$xyz.est$z) <- list(interp$xyz.est$x, interp$xyz.est$y)
  
  # Convert to dataframe
  interp_df <- melt(interp$xyz.est$z, varnames = c("newLat", "Depth"), value.name = "NutVar") %>%
    mutate(NutVar = round(NutVar, 1))
  
  # Plot
  ggplot(data = interp_df, aes(x = newLat, y = Depth)) +
    geom_raster(aes(fill = NutVar)) +
    scale_fill_viridis_c() +
    scale_y_reverse() +
    geom_contour(aes(z = NutVar), binwidth = 1, colour = "black", alpha = 0.2) +
    geom_point(data = sample_points, aes(x = newLat, y = Depth),
               colour = "black", size = 0.2, alpha = 0.4, shape = 8) +
    labs(
      y = "Depth (m)",
      x = "Latitude",
      fill = paste0(title_label, "\n(µmol/L)"),
      title = paste("SE2204", title_label, "Section Plot"),
      subtitle = "Interpolated over depth and space; \nblack dots show actual sampling locations."
    ) +
    coord_cartesian(expand = 0) 
}


ctd200 <- ctdAll %>% filter(!Depth > 300)
plot_nutrient_section(ctd200, "Flourescence", "Fluorescence")


ctd200 <- ctdAll %>% filter(!Depth > 400)
plot_ocng_section(data = ctd200, ocng_var = "Flourescence", title_label = "Fluorescence")



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
# daily <- read.csv(paste0(here(), '/CTD/GLORYScomp/GLORYS_oxygen_SE2204.csv'))
# monthly <- read.csv(paste0(here(), '/CTD/GLORYScomp/GLORYS_Monthly_JunJul_se2204.csv'))
# woa <- read.csv(paste0(here(), '/CTD/GLORYScomp/WOA_Climatology_JunJul_SE2204.csv'))

clim <- read.csv('GLORYS_Climatology_JunJul_SE2204.csv') #Emma's 
daily <- read.csv('GLORYS_oxygen_SE2204.csv')
monthly <- read.csv('GLORYS_Monthly_JunJul_se2204.csv')
woa <- read.csv('WOA_Climatology_JunJul_SE2204.csv')
head(clim)

ggplot() + 
  geom_path(data=ctdAll, aes(y=Depth, x=Oxygen, color='CTD')) +
  geom_path(data=daily, aes(y=Depth, x=Oxygen, color='GLORYS')) +
  scale_y_reverse() +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), scales = 'free_x', nrow=1) +
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
  ggtitle('Oxygen Concentration: Daily GLORYS and SE2204 CTD Observations')
# ggsave('O2Daily_GLORYS_nrow=1.png', width=10, height = 1.8, dpi = 300)

ggplot() + 
  geom_path(data=ctdAll, aes(y=Depth, x=Oxygen, color='CTD')) +
  geom_path(data=daily, aes(y=Depth, x=Oxygen, color='GLORYS')) +
  scale_y_reverse() +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), scales = 'free_x', nrow=1) +
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
  ggtitle('Oxygen Concentration: Monthly GLORYS and SE2204 CTD Observations')
# ggsave('O2Monthly_GLORYS_nrow1.png', width=10, height = 1.8, dpi = 300)

# Plot of CTD vs Climatology GLORYS Data 
ggplot() + 
  geom_path(data=ctdAll, aes(y=Depth, x=Oxygen, color='CTD')) +
  geom_path(data=clim, aes(y=Depth, x=Oxygen, color='GLORYS')) +
  scale_y_reverse() +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), scales = 'free_x', nrow=1) +
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
  ggtitle('Oxygen Concentration: Climatology GLORYS and SE2204 CTD Observations')
# ggsave('O2Clim_GLORYS_nrow1.png', width=10, height = 1.8, dpi = 300)

ggplot() + 
  geom_path(data=ctdAll, aes(y=Depth, x=Oxygen, color='CTD')) +
  geom_path(data=woa, aes(y=Depth, x=Oxygen, color='WOA')) +
  scale_y_reverse() +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), scales = 'free_x', nrow=1) +
  scale_color_manual(
    name = "Data Source",  # Legend title
    values = c("CTD" = "blue", "WOA" = "red")) +
  labs(color = "Data Source") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle('Oxygen Concentration: WOA and SE2204 CTD Observations')
# ggsave('O2WOA_pan.png', width=10, height = 5.6, dpi = 300)
# ggsave('O2WOA_nrow1.png', width=10, height = 5.6, dpi = 300)

#------------------------------ Anomaly ---------------------------------------
# First we need the depths to be the same for both datasets
oxyAnom <- function(CTDdata, ModelData, CastNr, Variable='Oxygen') {
  cst2ctd <- CTDdata %>% 
    filter(Cast == CastNr) %>% 
    select(Depth, all_of(Variable)) %>% 
    data.frame()
  # Then a matching one with GLORYS data
  cst2glo <- ModelData %>%
    filter(Cast == CastNr) %>%
    select(Depth, all_of(Variable))
  # Interpolate glorys data over the depths in the CTD data
  modelProfile <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$Depth)
  modProfile <- data.frame(Depth=modelProfile$x, Oxygen=modelProfile$y)
  # Calcuate the anomaly
  anomCTDglo <- modProfile$Oxygen-cst2ctd$Oxygen
  anomCTDglo <- data.frame(OxygenAnom=anomCTDglo, Depth=cst2ctd$Depth, Cast=CastNr)

  return(anomCTDglo)
}

oxyAnomAll <- data.frame()
for (i in stnInfo$Cast) {
  oxyAnomAll <- rbind(oxyAnomAll, oxyAnom(daily, clim, i))
}

ggplot(oxyAnomAll, aes(OxygenAnom, Depth)) + 
  geom_path() + 
  geom_vline(xintercept = 0, linetype='dashed', color='tan1')+ 
  scale_y_reverse() +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle('GLORYS Daily vs Climatology Oxygen Anomalies')
# ggsave('Anom_GLORYSDM_AllStns(panel).png', width=10, height = 5.625, dpi = 300)


# Anomaly for WOA and CTD Data
oxyAnomAll <- data.frame()
for (i in stnInfo$Cast) {
  oxyAnomAll <- rbind(oxyAnomAll, cbind(oxyAnom(CTDdata = ctdAll, ModelData = woa, CastNr = i), Cast=i))
}

ggplot(oxyAnomAll, aes(OxygenAnom, Depth)) + 
  geom_path() + 
  geom_vline(xintercept = 0, linetype='dashed', color='tan1')+ 
  scale_y_reverse() +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), nrow=1) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle('WOA vs SE2204 CTD Oxygen Anomalies')
# ggsave('Anom_WOACTD_AllStns(panel).png', width=10, height = 5.625, dpi = 300)
# ggsave('Anom_WOACTD_AllStns(nrow1).png', width=10, height = 5.625, dpi = 300)

# Anomaly for GLORYS daily vs climatology
oxyAnomAll <- data.frame()
for (i in stnInfo$Cast) {
  oxyAnomAll <- rbind(oxyAnomAll, cbind(oxyAnom(daily, clim, i), Cast=i))
}

ggplot(oxyAnomAll, aes(OxygenAnom, Depth)) + 
  geom_path() + 
  geom_vline(xintercept = 0, linetype='dashed', color='tan1')+ 
  scale_y_reverse() +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle('GLORYS Daily vs Climatology Oxygen Anomalies')
# ggsave('Anom_GLORYSDM_AllStns(panel).png', width=10, height = 5.625, dpi = 300)


# Anomaly for CTD and Monthly
oxyAnomAll <- data.frame()
for (i in stnInfo$Cast) {
  oxyAnomAll <- rbind(oxyAnomAll, cbind(oxyAnom(CTDdata = ctdAll, ModelData = monthly, CastNr = i), Cast=i))
}

ggplot(oxyAnomAll, aes(OxygenAnom, Depth)) + 
  geom_path() + 
  geom_vline(xintercept = 0, linetype='dashed', color='tan1')+ 
  scale_y_reverse() +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle('Monthly GLORYS vs SE2204 CTD Oxygen Anomalies')
# ggsave('Anom_GLorMCTD_AllStns(panel).png', width=10, height = 5.625, dpi = 300)
# ggsave('Anom_GLorMCTD_AllStns(nrow1).png', width=10, height = 5.625, dpi = 300)


# Anomaly for CTD and Monthly
oxyAnomAll <- data.frame()
for (i in stnInfo$Cast) {
  oxyAnomAll <- rbind(oxyAnomAll, cbind(oxyAnom(CTDdata = ctdAll, ModelData = clim, CastNr = i), Cast=i))
}

ggplot(oxyAnomAll, aes(OxygenAnom, Depth)) + 
  geom_path() + 
  geom_vline(xintercept = 0, linetype='dashed', color='tan1')+ 
  scale_y_reverse() +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), nrow=1) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle('Climatology GLORYS vs SE2204 CTD Oxygen Anomalies')
# ggsave('Anom_GLorClimCTD_AllStns(panel).png', width=10, height = 5.625, dpi = 300)
# ggsave('Anom_GLorClimCTD_AllStns(nrow1).png', width=10, height = 5.625, dpi = 300)






#------------------------------ Final Figures ---------------------------------------
#---------------------------- OCNG Depth Profiles -----------------------------
# --------------------- Interpolated Depth Profile Function -------------------

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
  
  ####-----JOHANNA TESTING NEW INTERPOLATION-----####
  temp.interp = akima::interp(x = clean_data$newLat,
                              y = clean_data$Depth,
                              z = clean_data$Oxygen_Raw,
                              duplicate = "mean", nx = 500, ny = 500)
  temp.interp = akima::interp(x = ctd.tb.all$latitude,
                              y = ctd.tb.all$pressure,
                              z = unlist(ctd.tb.all[,'oxygen']),
                              duplicate = "mean", nx = 500, ny = 300)
  # Look at the interpolation
  image(temp.interp)
  # Put it in a ggplot friendly format and limit it to the top 1,000 meters
  # Choose which variable you want to plot
  interp_df = akima::interp2xyz(temp.interp) %>%
    as.tibble() %>%
    rename(newLat = x, Depth = y, OCNVar = z) %>%
    na.omit() %>%
    filter(Depth <=1300 & Depth > 4)
  ####-----JOHANNA TESTING NEW INTERPOLATION-----####
  
  
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

# AOU
AOUVis


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

# wrap_elements(panel = combined_plot) +
#   labs(tag = "Latitude") +
#   theme(
#     plot.tag = element_text(size = rel(1)),
#     plot.tag.position = "bottom"
#   )
#   
# wrap_elements(panel = combined_plot) +
#   labs(tag = "Depth [m]") +
#   theme(
#     plot.tag = element_text(size = rel(1), angle = 90),
#     plot.tag.position = "left"
#   )



combined_plotV <- plot_grid(OSPlot / AOUVis / NSectionPlot, ncol = 1, align = "v")
combined_plotV

final_plot <- ggdraw(combined_plotV) +
  draw_label("Latitude", x = 0.5, y = 0.01, vjust = 0, angle = 0, size = 12) +  
  draw_label("Depth [m]", x = 0.0009, y = 0.5, vjust = 1, angle = 90, size = 12) 

final_plot
# ggsave('SectionPlotAOU_poster.png', width = 24, height = 36, units = "in") #for poster
# ggsave('SectionPlotAOU_presentation.png', width = 10, height = 7.5, dpi = 300, units = "in") #for presentation
