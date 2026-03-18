# Manuscript crips
# Easily accessible script with everything I did for the manuscript. 

# Clear workspace
rm(list=ls())

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
library(stringr)

# Set working directory
myDir <- here()  # Johanna's File path
setwd(myDir)

# Read in CTD files 
ctdAll <- read.csv('CTD/CTD_data_forAnalysis.csv')
stnInfo <- read.csv('CTD/CTD_metadata_forAnalysis.csv')

# Make a table with labels you want and the index that we can use for plotting
id.labs <- stnInfo$Station2
names(id.labs) <- stnInfo$Cast

# SST and MLD
# Calculate MLD and other variables. These calculations are from Fiedler 2010 and I am defining the MLD independently 
# of the thermocline as the depth at which T=SST-0.8°C (Kara et al. 2000,2003) and the thermocline using the 
# **Variable representative isotherm method** as it best estimates the thermocline depth in subtropical waters where 
# thermocline strength tends to be a bit weaker. 
ctdMLD <- ctdAll %>% 
  group_by(Cast) %>% 
  filter(Depth <=20) %>% 
  summarise(SST=mean(Temperature)) %>%
  mutate(T_MLD=SST-0.8)

# Temp at 400 meters and thermocline temp
ctdMLD <- ctdAll %>% 
  group_by(Cast) %>% 
  filter(near(Depth, 400, 0.1)) %>% 
  summarise(T_400=mean(Temperature)) %>%
  full_join(ctdMLD) %>%
  select(Cast, SST, T_MLD, T_400) %>%
  mutate(TT=T_MLD-(0.25*(T_MLD-T_400)), T_TB=T_MLD-(0.5*(T_MLD-T_400)))

ML <- data.frame(Cast=NA, D_MLD=NA, D_TT=NA, D_TB=NA, TS=NA, intercept=NA)
cast <- stnInfo$Cast
for (i in cast) {
  MLtemp <- ctdMLD$T_MLD[which(ctdMLD$Cast == i)]
  myDat <- ctdAll %>% 
    filter(Cast == i) %>% 
    mutate(Difference = abs(Temperature-MLtemp)) %>% 
    arrange(Difference) %>% 
    slice(1) %>% 
    select(Depth) %>% 
    rename(D_MLD=Depth)
  
  Ttemp <- ctdMLD$TT[which(ctdMLD$Cast == i)]
  myTemp <- ctdAll %>% 
    filter(Cast == i) %>% 
    mutate(Difference = abs(Temperature-Ttemp)) %>% 
    arrange(Difference) %>% 
    slice(1) %>% 
    select(Depth) %>% 
    rename(D_TT=Depth)
  
  TBtemp <- ctdMLD$T_TB[which(ctdMLD$Cast == i)]
  myTemp2 <- ctdAll %>% 
    filter(Cast == i) %>% 
    mutate(Difference = abs(Temperature-TBtemp)) %>% 
    arrange(Difference) %>% 
    slice(1) %>% 
    select(Depth) %>% 
    rename(D_TB=Depth)
  
  TSdat <- ctdAll %>%
    filter(Cast == i, Depth > as.numeric(myDat) & Depth <= as.numeric(myTemp2))
  
  LM <- lm(Depth ~ Temperature, data=TSdat)
  TS <- LM$coeff[[2]]
  int <- LM$coeff[[1]]
  
  ML <- rbind(ML, data.frame(Cast=i, myDat, myTemp, myTemp2, TS=TS, intercept=int))
}
ctdMLD <- inner_join(ctdMLD, ML) %>%
  mutate(TW=D_TB-D_MLD)

## Oxygen minimum (depth and oxygen at the lowest concentration)
ctdMLD <- ctdAll %>% 
  select(Cast, Depth, Oxygen) %>% 
  group_by(Cast) %>% 
  slice_min(Oxygen) %>% 
  rename(OMZdepth=Depth, OxygenMin=Oxygen) %>% 
  left_join(ctdMLD)

## Deep chlorophyll max
ctdMLD <- ctdAll %>% 
  select(Cast, Depth, Flourescence) %>% 
  group_by(Cast) %>% 
  slice_max(Flourescence) %>% 
  rename(DCMdepth=Depth, ChlMax=Flourescence) %>% 
  left_join(ctdMLD)

# Add metadata to the data frame
ctdMLD <- left_join(stnInfo[,c(4,5,8,9)],ctdMLD, by='Cast')

## Nutricline depth
# Defined as the 1um nitrate level
nutmeta <- read.csv('Data/SE2204_nutrient_metadata_USE_THIS.csv') 
nutmeta$Depth <- as.numeric(str_sub(nutmeta$Depth, start=1, end=-2))
nutmeta$SampleNo <- 1:nrow(nutmeta)
nutmeta <- nutmeta[-28,]
nut <- nutmeta #full_join(nutrients, nutmeta)
head(nut)

# Turn the less than values into low numerics so we can make all data numeric instead of character
nut$Ammonia <- ifelse(nut$Ammonia == "<0.02", 0.01, as.numeric(nut$Ammonia))
nut$Phosphate <- ifelse(nut$Phosphate == "<0.008", 0.007, as.numeric(nut$Phosphate))
nut$Silicate <- as.numeric(nut$Silicate)
nut$`Nitrate + Nitrite` <- as.numeric(nut$`Nitrate + Nitrite`)
nut$Date <- as.Date(nut$Date, '%m/%d/%y') 
head(nut)
## nutricline depth 
meanNutricline <- nut %>% 
  select(Station, Depth, Nitrate..Nitrite) %>% 
  group_by(Station) %>% 
  filter(Nitrate..Nitrite > 1) %>% 
  summarise(min(Depth)) %>% 
  data.frame()

## Print out the values of various variables to put in the text
# omz at station A (30N)
mean(mean(ctdMLD$OMZdepth[1:2]))
mean(mean(ctdMLD$OxygenMin[1:2]))
mean(mean(ctdMLD$SST[1:2]))
mean(mean(ctdMLD$D_TT[1:2]))
mean(mean(ctdMLD$DCMdepth[1:2]))
# omz at station E (10.7N)
mean(mean(ctdMLD$OMZdepth[22:23]))
mean(mean(ctdMLD$OxygenMin[22:23]))
mean(mean(ctdMLD$SST[22:23]))
mean(mean(ctdMLD$D_TT[22:23]))
mean(mean(ctdMLD$DCMdepth[22:23]))



# Depth integrated chl-a
chlInt <- (((chl0+chl10)/2) * (depth10-depth0))

i=39
ctdAll %>% filter(Cast == i) %>% 
  ggplot() +
    geom_path(aes(Temperature, Depth)) +
  scale_y_reverse() +
  scale_x_continuous(position='top') +
  theme_light() + 
  geom_hline(data=ctdMLD[which(ctdMLD$Cast == i),], aes(yintercept = D_MLD), color='darkgray', linetype='dashed') +
  geom_point(data=ctdMLD[which(ctdMLD$Cast == i),], aes(T_MLD, D_MLD), color='green', size=2) +
  geom_point(data=ctdMLD[which(ctdMLD$Cast == i),], aes(TT, D_TT), color='red', size=2) +
  geom_point(data=ctdMLD[which(ctdMLD$Cast == i),], aes(T_TB, D_TB), size=2) +
  #geom_abline(data=ctdMLD[which(ctdMLD$Cast == i),], aes(slope = TS*-1, intercept = intercept*-1), color='blue', linewidth=0.25) +
  annotate('text', x=20, y=750, label=paste('Station', ctdMLD$Station2[which(ctdMLD$Cast == i)]))
  








### Section plots ###




# Nutrients
# Make a new dataset for nutrient analysis with better names
nutriDN <- nut %>% 
  dplyr::select(Latitude, Depth, Nitrate..Nitrite) %>% 
  rename(NutVar=Nitrate..Nitrite)
# Interpolate to fill section plot
ctd_mba <- mba.surf(na.omit(nutriDN), no.X = 300, no.Y = 300, extend = T)
dimnames(ctd_mba$xyz.est$z) <- list(ctd_mba$xyz.est$x, ctd_mba$xyz.est$y)
ctd_mba <- melt(ctd_mba$xyz.est$z, varnames = c('Latitude', 'Depth'), value.name = 'NutVar') %>% 
  mutate(NutVar = round(NutVar, 1))



ggplot(data = ctd_mba, aes(x = Latitude, y = Depth)) +
  geom_raster(aes(fill = NutVar)) +
  scale_fill_viridis_c(option = 'mako') +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_contour(aes(z = NutVar), breaks=1, colour = "white", alpha = 0.3) + 
  geom_contour(aes(z = NutVar), breaks=c(5,10), colour = "white", alpha = 0.2, linewidth=0.25) + 
  geom_contour(aes(z = NutVar), breaks=c(15,20,25,30), colour = "black", alpha = 0.3, linewidth=0.25) +  ### Activate to see which pixels are real and not interpolated
  geom_point(data = nutriDN, aes(x = Latitude, y = Depth), colour = 'gray40', size = 0.2, alpha = 0.4, shape = 8) +
  labs(y = "Depth (m)", x = 'Latitude', fill = "Phosphate\n(µmol/L)") +
  coord_cartesian(expand = 0)
