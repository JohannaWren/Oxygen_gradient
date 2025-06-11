# Date: June 3, 2025
# Author: Johanna LK Wren & Emma Scott-Wellman
# email: johanna.wren@noaa.gov emma.scott-wellman@noaa.gov
# Description: Script to take an exploratory look at CTD data from SE-22-04 Hot Spot/Oxygen gradient cruise


# Load libraries
library(dplyr)
library(ggplot2)
library(here)
library(lubridate)
library(tidyr)
library(data.table)

# Set working directory
#myDir <- paste(here(),'CTD_processed_headers', sep='/') # Emma's file path
myDir <- paste(here(), 'CTD', 'CTD_processed', sep='/')  # Johanna's File path
setwd(myDir)

# Read in files in a loop
files = dir(path = ".", pattern = ".asc", full.names = TRUE)
ctd_list <- list()
# read in files in loop
for (i in seq_along(files)) {
  ctd_list[[i]] <- read.csv(files[i])
}

# check data
head(ctd_list[[10]])
str(ctd_list[[10]])

# Turn list into data.frame
ctdAll <- rbindlist(ctd_list, use.names = T, idcol = T) %>% 
  rename(FileNum=.id)

# Make Datetime column
ctdAll$DateTime <- as.POSIXct(paste(ctdAll$mm.dd.yyyy, ctdAll$hh.mm.ss), format='%m/%d/%Y %H:%M:%S')
# remove missing oxygen data
ctdAll$Oxygen_cleaned <- ctdAll$Sbox0Mm.Kg
ctdAll$Oxygen_cleaned[which(ctdAll$Oxygen_cleaned < 0)] <- NA

# read in station list from file
stns <- read.csv('../SE2204_CTDlocations.csv')  # Change this to fit the directory your file is in
stns$Cast <- 1:nrow(stns)
head(stns)
# Match the station code in stns$Station with the file names
# I'm doing this in a loop because it's easier for me to think that way 
p <- vector()
for (i in seq_along(files)) {
  p[i] <- grep(paste0('dSE-22-04_', stns$Station[i]), files)
}

# Add the file names and index number to the stns data.frame
stnInfo <- data.frame(stns, FileName=files[p], FileNum=p)
head(stnInfo)
# Remove all casts that have eDNA sampling only
stnInfo <-  stnInfo[which(stnInfo$Sampling == stnInfo$Sampling[2]),]
# # Then you can sort out the eDNA casts (this replaces the code that we wrote before)
# # I chose to use the type of sampling done at the station instead of depth since that is a better descriptor of the sampling
# Filter out all eDNA and opportunistic stations from ctdAll
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
  print(summary(ctdAll[idc,c('Latitude', 'Cast')]))
  ctdAll$newLat[idc] <- mean(ctdAll$Latitude[idl], na.rm = T)
}
head(ctdAll)

# check to see if there are still NA's in the file. Zero means no NAs
length(which(is.na(ctdAll$newLat)))

# make a depth profile
# this plots all of the profiles on one plot
ggplot(ctdAll, aes(x=Oxygen_cleaned, y=DepSM)) + 
  geom_path() +
  scale_y_reverse()

# # make a multi-panel plot with multiple variables from one station
# # put data into "long" format
# ctd_long <- ctd %>% 
#   select(DateTime, DepSM, T090C, FlECO.AFL, Sigma.E00, Sal00, Oxygen_cleaned) %>% 
#   gather(varName, value, T090C:Oxygen_cleaned)
# 
# ggplot(ctd_long, aes(value, DepSM, color=varName)) + 
#   geom_path() +
#   facet_wrap(.~varName, scales = 'free_x') + 
#   scale_y_reverse()


# Plot Oxycline
o2_min <- ctdAll %>% 
  group_by(Cast) %>% 
  slice(which.min(Oxygen_cleaned)) %>% select(Cast, DepSM)

ctdAll %>% 
  ggplot(aes(x=Oxygen_cleaned, y=DepSM)) + 
    geom_path() +
    scale_y_reverse() + 
    facet_wrap(.~Cast, labeller=labeller(Cast=id.labs)) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
    geom_hline(data = o2_min, aes(yintercept = DepSM), linetype= 'dashed', color='red') +
    #geom_hline(yintercept = 500, linetype = "dashed", color = "gray")
    xlab('Oxygen [umol/kg]') + ylab('Depth [m]') +
    ggtitle('Oxygen Depth Profiles SE2204')

ggsave('O2DepthProfiles_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')


#Thermocline Profiles 
ctdAll %>% 
  ggplot(aes(x=T090C, y=DepSM)) + 
  geom_path() +
  scale_y_reverse() + 
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), scales = 'free_x') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  xlab('Temperature [°C]')+ ylab('Depth [m]') +
  ggtitle('Temperature Depth Profiles SE2204')

ggsave('TempDepthProfiles_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')

# Halocline Profiles
ctdAll %>% 
  ggplot(aes(x=Sal00, y=DepSM)) + 
  geom_path() +
  scale_y_reverse() + 
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), scales = 'free_x') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  xlab('Salinity [PSU]')+ ylab('Depth [m]') +
  ggtitle('Salinity Depth Profiles SE2204')

ggsave('SalinityDepthProfiles_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')

# Pycnocline Profiles 
ctdAll %>% 
  ggplot(aes(x=Sigma.E00, y=DepSM)) + 
  geom_path() +
  scale_y_reverse() + 
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), scales = 'free_x') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(x = expression('σ'[θ])) + 
  ylab('Depth [m]') +
  ggtitle('Density Depth Profiles SE2204')

ggsave('DensityDepthProfiles_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')

# Fluorescence Profiles 
ctdAll %>% 
  ggplot(aes(x=FlECO.AFL, y=DepSM)) + 
  geom_path() +
  scale_y_reverse() + 
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  xlab('Fluorescence [ug/L]') + 
  ylab('Depth [m]') +
  ggtitle('Fluorescence Depth Profiles SE2204')
  

ggsave('FluorDepthProfiles_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')







# NUTRIENTS

# Merging nutrient files and cleaning the starting rows (check SE2204_Nutrient-Vis.R)
#library(readxl)
#nutMeta <- read.csv('../SE2204_nutrient_metadata (1).csv')
#nutDat <- read_xls('../SE2204_Nutrient_Data.xls', skip=12)
#nutDat <- data.frame(nutDat[-1,])

# Read in Nutrient Data
# 10 samples at different depths per station except for NUT_028 Station 1 (9 samples total)
nut <- read.csv('../SE2204_nutrient_metadata.csv')
# Adding Cast column that is the same as the CTD cast numbers. 
nut <- nut %>% 
  left_join(stnInfo[,c('Station2', 'Cast')], by=c('Station'='Station2'))
head(nut)

# Remove the m from nutrient file depths and create a new column with numeric depth only
nut$Depth2 <- as.numeric(substr(nut$Depth, 1, nchar(nut$Depth)-1))
head(nut)

# Plotting
# Silicate 
ggplot(nut, aes(x=Silicate, y=Depth2)) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), scales= 'free_x') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  xlab('Silicate [umol/L]') + 
  ylab('Depth [m]') +
  ggtitle('Silicate Depth Profiles SE2204')


ggsave('SilicateDepthProfiles_AllStns.pdf', width=11, height = 8) 

# Phosphate
ggplot(nut, aes(x=Phosphate, y=Depth2)) +
  geom_point() +
  scale_y_reverse() +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  xlab('Phosphate [umol/L]') + 
  ylab('Depth [m]') +
  ggtitle('Phosphate Depth Profiles SE2204')


ggsave('PhosphateDepthProfiles_AllStns.pdf', width=11, height = 8)

# Phosphate Pt. 2
ggplot(nut, aes(x=Phosphate, y=Depth2)) +
  geom_point() +
  scale_y_reverse() +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), scales = 'free_x') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  xlab('Phosphate [umol/L]') + 
  ylab('Depth [m]') +
  ggtitle('Phosphate Depth Profiles SE2204')


ggsave('PhosphateXDepthProfiles_AllStns.pdf', width=11, height = 8)

#Nitrite/Nitarte 
ggplot(nut, aes(x= Nitrate...Nitrite, y=Depth2)) +
  geom_point() +
  scale_y_reverse() +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), scales = 'free_x') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  xlab('Nitrate/Nitrite [umol/L]') + 
  ylab('Depth [m]') +
  ggtitle('Nitrate/Nitrite Depth Profiles SE2204')


ggsave('NitrogenDepthProfiles_AllStns.pdf', width=11, height = 8)


# Ammonia 
ggplot(nut, aes(x= Ammonia, y=Depth2)) +
  geom_point() +
  scale_y_reverse() +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), scales = 'free_x') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  xlab('Ammonia [umol/L]') + 
  ylab('Depth [m]') +
  ggtitle('Ammonia Depth Profiles SE2204')


ggsave('AmmoniaDepthProfiles_AllStns.pdf', width=11, height = 8)


# Oxygen Vs. Temperature
ctdAll %>% 
  ggplot(aes(x=newLat, y=Oxygen_cleaned, color = Cast)) + 
  geom_point(alpha = 1/10) +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  scale_color_viridis_c() +
  theme_minimal() +
  xlab('Latitude [°N]') + 
  ylab('Oxygen [umol/kg]') +
  ggtitle('Oxygen vs. Temperature Profiles SE2204')

ggsave('O2vsTemp_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')


# ---------- Code to plot multiple profiles in one plot -------------
oxy <- read.csv('GLORYS_oxygen_SE2204.csv')

ggplot() + 
  geom_path(data=ctdAll, aes(y=DepSM, x=Oxygen_cleaned), color='blue') +
  geom_path(data=oxy, aes(y=Depth.y, x=Oxygen), color='red') +
  scale_y_reverse() + 
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), scales = 'free_x') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


# Correlations between Glorys and CTD data by station
# List all the cast numbers
unique(ctdAll$Cast)
# make a variable for the station you want to compare with CTD data
cst2ctd <- ctdAll %>% 
  filter(Cast == 43) %>% 
  select(DepSM, Oxygen_cleaned) %>% 
  data.frame()
# Then a matching one with glorys data
cst2glo <- oxy %>% 
  filter(Cast == 43) %>% 
  select(Depth.y, Oxygen) 
# Interpolate glorys data over the depths in the CTD data
test <- approx(cst2glo$Depth.y, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# Calculate the correlation between glorys and CTD (output is r, not R^2)
oxyCor <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
oxyCor




### Changing date time and working with GLORYS oxygen data ####
library(ncdf4)
nc <- nc_open('~/Downloads/cmems_mod_glo_bgc-bio_anfc_0.25deg_P1D-m_1749590619002.nc')
depth <- ncvar_get(nc, varid='depth')
depth <- data.frame(DepthNr=1:37, Depth=depth)

# combine CTD metadata with oxygen data
stnInfo$DateTime <- as.Date(stnInfo$Date, '%m/%d/%y %H:%M')
oxyLong$DateTime <- as.Date(substr(oxyLong$Date, 2, 11), '%Y.%m.%d')
test <- oxyLong %>% 
  left_join(stnInfo[,c(1,8, 10, 13)], by=c('Station', 'DateTime'))
test <- na.omit(test)

# Add depths to the oxygen dataframe
test <- test %>% 
  left_join(depth, by=c('Depth'='DepthNr'))

