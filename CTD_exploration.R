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
ctd <- read.table('dSE-22-04_02_01.asc', sep = ',')
# check data
head(ctd)
str(ctd)
# Move first row to column names
names(ctd) <- ctd[1,]
ctd <- ctd[-1,]
# check data
head(ctd)
str(ctd)
#ctd[,4:ncol(ctd)] <- as.numeric(ctd)

# Convert date to date format and charaters to numeric
ctd %>% mutate(Date=as.Date)
  mutate_if(is.character, as.numeric)
