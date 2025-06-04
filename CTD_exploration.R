# Date: June 3, 2025
# Author: Johanna LK Wren & Emma Scott-Wellman
# email: johanna.wren@noaa.gov emma.scott-wellman@noaa.gov
# Description: Script to take an exploratory look at CTD data from SE-22-04 Hot Spot/Oxygen gradient cruise


# Load libraries
library(dplyr)
library(ggplot2)
library(here)
library(lubridate)

# Set working directory
myDir <- paste(here(), 'CTD', 'CTD_processed', sep='/')
setwd(myDir)

# Read is the data
#ctd <- read.table('dSE-22-04_02_01.asc', sep = ',', header = F)
ctd <- read.csv('dSE-22-04_02_01.asc', skip = 1, header = F)
# check data
head(ctd)
str(ctd)
# Add header (hard coded in for now because special character is giving us issues)
hdrstr <- c('Scan', 'Date', 'Time', 'Pressure', 'Conductivity', 'Oxygen_raw', 'Temperature', 'Flourescence', 'Lat', 'Lon', 'SigmaTheta', 'Depth', 'Oxygen_umKg', 'Salinity', 'Flag')
names(ctd) <- hdrstr
# check data
head(ctd)
str(ctd)

# Convert date to date format and charaters to numeric
ctd$DateTime <- as.POSIXct(paste(ctd$Date, ctd$Time), format='%m/%d/%Y %H:%M:%S') 
# remove missing oxygen data 
ctd$Oxygen_cleaned <- ctd$Oxygen_umKg
ctd$Oxygen_cleaned[which(ctd$Oxygen_cleaned < 0)] <- NA

# make a depth profile
ggplot(ctd, aes(x=Oxygen_cleaned, y=Depth)) + 
  geom_path() +
  scale_y_reverse()

# Find the depth of the oxygen minimum
idx <- which(ctd$Oxygen_cleaned == min(ctd$Oxygen_cleaned, na.rm = T))
O2min <- ctd[idx,'Depth']

