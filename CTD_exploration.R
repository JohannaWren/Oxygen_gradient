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
#myDir <- paste(here(), 'CTD', 'CTD_processed', sep='/') # Emma's file path
myDir <- paste(here(), 'CTD', 'CTD_processed', sep='/')  # Johanna's File path
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
# make index of eDNA casts to exclude
idx <- c(14, 17)

# check data
head(ctd_list[[10]])
str(ctd_list[[10]])

# Turn list into data.frame
ctdAll <- rbindlist(ctd_list, use.names = T, idcol = T)
# Make Datetime column
ctdAll$DateTime <- as.POSIXct(paste(ctdAll$mm.dd.yyyy, ctdAll$hh.mm.ss), format='%m/%d/%Y %H:%M:%S')
# remove missing oxygen data
ctdAll$Oxygen_cleaned <- ctdAll$Sbox0Mm.Kg
ctdAll$Oxygen_cleaned[which(ctdAll$Oxygen_cleaned < 0)] <- NA

# make a depth profile
# this plots all of the profiles on one plot
ggplot(ctdAll, aes(x=Oxygen_cleaned, y=DepSM)) + 
  geom_path() +
  scale_y_reverse()

# Find the depth of the oxygen minimum
idx <- which(ctd$Oxygen_cleaned == min(ctd$Oxygen_cleaned, na.rm = T))
O2min <- ctd[idx,'DepSM']
O2min

# make a multipanel plot with multiple variables from one station
# put data into "long" format
ctd_long <- ctd %>% 
  select(DateTime, DepSM, T090C, FlECO.AFL, Sigma.E00, Sal00, Oxygen_cleaned) %>% 
  gather(varName, value, T090C:Oxygen_cleaned)

ggplot(ctd_long, aes(value, DepSM, color=varName)) + 
  geom_path() +
  facet_wrap(.~varName, scales = 'free_x') + 
  scale_y_reverse()

# make a multi-panel plot with one variable from all stations
ctdAll %>% 
  filter(! .id %in% idx) %>% 
  ggplot(aes(x=Oxygen_cleaned, y=DepSM, color=as.factor(.id))) + 
    geom_path() +
    scale_y_reverse() + facet_wrap(.~.id)



