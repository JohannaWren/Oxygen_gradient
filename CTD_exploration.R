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
myDir <- paste(here(),'CTD_processed_headers', sep='/') # Emma's file path
#myDir <- paste(here(), 'CTD', 'CTD_processed', sep='/')  # Johanna's File path
setwd(myDir)

# Read is the data
# Single file
#ctd <- read.csv('dSE-22-04_E_01.asc')
# Read in files in a loop
files = dir(path = ".", pattern = ".asc", full.names = TRUE)
ctd_list <- list()
# read in files in loop
for (i in seq_along(files)) {
  ctd_list[[i]] <- read.csv(files[i])
  # # Make Datetime column
  # ctd_list[[i]]$DateTime <- as.POSIXct(paste(ctd_list[[i]]$mm.dd.yyyy, ctd_list[[i]]$hh.mm.ss), format='%m/%d/%Y %H:%M:%S') 
  # # remove missing oxygen data 
  # ctd_list[[i]]$Oxygen_cleaned <- ctd_list[[i]]$Sbox0Mm.Kg
  # ctd_list[[i]]$Oxygen_cleaned[which(ctd_list[[i]]$Oxygen_cleaned < 0)] <- NA
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
# Then you can sort out the eDNA casts (this replaces the code that we wrote before)
# I chose to use the type of sampling done at the station instead of depth since that is a better descriptor of the sampling
idx <- stnInfo$Cast[which(stnInfo$Sampling != stnInfo$Sampling[2])]

# Add new index row for station ID
for (x in 1:46) { 
  ctdAll[which(ctdAll$FileNum == x), 'Cast'] <- stnInfo$Cast[which(stnInfo$FileNum == x)] 
}

# Make a little table with labels you want and the index that we can use for plotting
id.labs <- stnInfo$Station2
names(id.labs) <- stnInfo$Cast

# make index of eDNA casts to exclude
#idx <- c(14, 16, 17, 20, 23, 26, 29, 32, 35, 38, 41, 42, 44, 46)
# # make index of eDNA casts to exclude
# test <- ctdAll %>% group_by(.id) %>% slice(which.max(DepSM)) %>% select(.id, DepSM) %>% filter(DepSM<1000)
# idx <- test$.id

# make a depth profile
# this plots all of the profiles on one plot
ggplot(ctdAll, aes(x=Oxygen_cleaned, y=DepSM)) + 
  geom_path() +
  scale_y_reverse()

# Find the depth of the oxygen minimum
#idy <- which(ctdAll$Oxygen_cleaned == min(ctd$Oxygen_cleaned, na.rm = T))
#O2min <- ctdAll[idy,'DepSM']
#O2min

# make a multipanel plot with multiple variables from one station
# put data into "long" format
ctd_long <- ctd %>% 
  select(DateTime, DepSM, T090C, FlECO.AFL, Sigma.E00, Sal00, Oxygen_cleaned) %>% 
  gather(varName, value, T090C:Oxygen_cleaned)

ggplot(ctd_long, aes(value, DepSM, color=varName)) + 
  geom_path() +
  facet_wrap(.~varName, scales = 'free_x') + 
  scale_y_reverse()

o2_min <- ctdAll %>% 
  filter(! Cast %in% idx) %>% 
  group_by(Cast) %>% 
  slice(which.min(Oxygen_cleaned)) %>% select(Cast, DepSM)

#y_min <- ctdAll[which(ctdAll$Oxygen_cleaned == min(ctdAll$Oxygen_cleaned, na.rm=T)), 'DepSM']
# make a multi-panel plot with one variable from all stations
ctdAll %>% 
  filter(! Cast %in% idx) %>% 
  ggplot(aes(x=Oxygen_cleaned, y=DepSM)) + 
    geom_path() +
    scale_y_reverse() + 
    facet_wrap(.~Cast, labeller=labeller(Cast=id.labs)) +
    #theme_bw() +
    #theme(panel.grid.major = element_blank()) +
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

# # Pull Id's for station matching
# test2 <- ctdAll %>% group_by(.id) %>% slice(which.min(DateTime)) %>% select(DateTime) %>% data.frame()
# test2$DateTime <- test2$DateTime - (60*60*10)
# test2

#Thermocline Profiles 
ctdAll %>% 
  filter(! Cast %in% idx) %>% 
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
  filter(! Cast %in% idx) %>% 
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
  filter(! Cast %in% idx) %>% 
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
  ggtitle('Salinity Depth Profiles SE2204')

ggsave('DensityDepthProfiles_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')

# Fluorescence Profiles 
ctdAll %>% 
  filter(! Cast %in% idx) %>% 
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
  xlab('[ug/L]') + 
  ylab('Depth [m]') +
  ggtitle('Fluorescence Depth Profiles SE2204')
  

ggsave('FluorDepthProfiles_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')


# nutrients
library(readxl)
nutMeta <- read.csv('../SE2204_nutrient_metadata (1).csv')
nutDat <- read_xls('../SE2204_Nutrient_Data.xls', skip=12)
nutDat <- data.frame(nutDat[-1,])
# Read in Nutrient Data
# 10 samples at different depths per station except for NUT_028 Station 1 (9 samples total)
nut <- read.csv('../SE2204_nutrient_metadata.csv')
nut$Cast2 <- 1:nrow(nut)
stnInfo2 <- data.frame(nut)
head(stnInfo2)

nut$Depth2 <- as.numeric(substr(nut$Depth, 1, nchar(nut$Depth)-1))
head(nut)

#nut_group <- ceiling(seq_along(nut[,1]) / 10)
ggplot(nut, aes(x=Silicate, y=Depth2)) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(.~Station)


