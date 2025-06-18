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
#myDir <- paste(here(),'CTD_processed_headers', sep='/') # Emma's file path
myDir <- paste(here(), 'CTD', sep='/')  # Johanna's File path
setwd(myDir)

# Read in files 
ctdAll <- read.csv('CTD_data_forAnalysis.csv')
stnInfo <- read.csv('CTD_metadata_forAnalysis.csv')


files = dir(path = ".", pattern = ".asc", full.names = TRUE)
ctd_list <- list()
# read in files in loop
for (i in seq_along(files)) {
  ctd_list[[i]] <- read.csv(files[i])
}
# Turn list into data.frame
ctdAll <- rbindlist(ctd_list, use.names = T, idcol = T) %>% 
  rename(FileNum=.id, Depth=DepSM)

# Make Datetime column
ctdAll$DateTime <- as.POSIXct(paste(ctdAll$mm.dd.yyyy, ctdAll$hh.mm.ss), format='%m/%d/%Y %H:%M:%S')
# remove missing oxygen data
ctdAll$Oxygen <- ctdAll$Sbox0Mm.Kg
ctdAll$Oxygen[which(ctdAll$Oxygen < 0)] <- NA

# read in station list from file
stns <- read.csv('../SE2204_CTDlocations.csv')  # Change this to fit the directory your file is in
stns$Cast <- 1:nrow(stns)
head(stns)

p <- vector()
for (i in seq_along(files)) {
  p[i] <- grep(paste0('dSE-22-04_', stns$Station[i]), files)
}

# Add the file names and index number to the stns data.frame
stnInfo <- data.frame(stns, FileName=files[p], FileNum=p)
head(stnInfo)
# Remove all casts that have eDNA sampling only
stnInfo <-  stnInfo[which(stnInfo$Sampling == stnInfo$Sampling[2]),]

ctdAll <- ctdAll %>% 
  filter(FileNum %in% stnInfo$FileNum)

# Add new index row for station ID
for (x in unique(stnInfo$FileNum)) { 
  ctdAll[which(ctdAll$FileNum == x), 'Cast'] <- stnInfo$Cast[which(stnInfo$FileNum == x)] 
}

# Make a little table with labels you want and the index that we can use for plotting
id.labs <- stnInfo$Station2
names(id.labs) <- stnInfo$Cast

# Replace the missing value codes for Latitude with average latitude for that cast
ctdAll$newLat <- ctdAll$Latitude
for (l in unique(ctdAll$Cast)) {
  idl <- which(ctdAll$Cast == l)
  idc <- which(ctdAll$Cast == l & ctdAll$Latitude < 0) 
  #print(summary(ctdAll[idc,c('Latitude', 'Cast')]))
  ctdAll$newLat[idc] <- mean(ctdAll$Latitude[idl], na.rm = T)
}
head(ctdAll)

# check to see if there are still NA's in the file. Zero means no NAs
length(which(is.na(ctdAll$newLat)))


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




