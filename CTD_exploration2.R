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
tempProfile <- depthProfile(ctdAll, "T090C", 'Temperature [°C]', 'Temerature depth profile for SE2204')
tempProfile
# ggsave('TempDepthProfiles_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')
# ggsave('TempDepthProfiles_AllStns.png', width=10, height = 5.625, dpi = 300)

# Halocline Profiles
salProfile <- depthProfile(ctdAll, "Sal00", 'Salinity [PSU]', 'Salinity depth profile for SE2204')
salProfile
# ggsave('SalinityDepthProfiles_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')
# ggsave('SalinityDepthProfiles_AllStns.png', width=10, height = 5.625, dpi = 300)

# Pycnocline Profiles 
DensProfile <- depthProfile(ctdAll, "T090C", labs(x = expression('σ'[θ])), 'Density depth profile for SE2204')
DensProfile
# ggsave('DensityDepthProfiles_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')
# ggsave('DensityDepthProfiles_AllStns.png', width=10, height = 5.625, dpi = 300)

# Fluorescence Profiles 
FluorProfile <- depthProfile(ctdAll, "FlECO.AFL", 'Fluorescence [ug/L]', 'Temerature depth profile for SE2204')
FluorProfile
# ggsave('FluorDepthProfiles_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')
# ggsave('FluorDepthProfiles_AllStns.png', width=10, height = 5.625, dpi = 300)


# --------------------- Interpolated Depth Profile Function -------------------




# --------------------- CTD, GLORYS, and WOA comparisons -------------------
# Read in CLIMATOLOGY GLORYS Data 
clim <- read.csv(paste0(here(), '/CTD/GLORYScomp/GLORYS_Climatology_JunJul_SE2204.csv'))
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
cst2ctd <- ctdAll %>% 
  filter(Cast == 2) %>% 
  select(Depth, Oxygen) %>% 
  data.frame()
# Then a matching one with GLORYS data
cst2glo <- clim %>% 
  filter(Cast == 2) %>% 
  select(Depth, Oxygen)
# Interpolate glorys data over the depths in the CTD data
modelProfile <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$Depth)
modProfile <- data.frame(Depth=modelProfile$x, Oxygen=modelProfile$y)
anomCTDglo <- cst2ctd$Oxygen-modProfile$Oxygen %>% 
  bind_cols(cst2ctd$Depth) %>% 
  rename(OxygenAnom=...1, Depth=...2)

anomaly =ctdAll$Oxygen - clim$Oxygen
threshold <- 2 * sd(anomaly)
df.am <- data.frame( Depth = ctdAll$Depth, Anomaly = anomaly, Outlier = abs(anomaly > threshold)) 
Outlier <- abs(df.am$Anomaly) > threshold


#Read in MONTHLY GLORYS Data
oxymonth <- read.csv("../GLORYS_Monthly_JunJul_SE2204.csv")

#Plot of CTD vs Monthly GLORYS Data 
ggplot() + 
  geom_path(data=ctdAll, aes(y=DepSM, x=Oxygen, color='CTD')) +
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




