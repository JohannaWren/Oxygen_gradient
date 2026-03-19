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
library(viridis)

# Set working directory
myDir <- here()  # Johanna's File path
setwd(myDir)

# Read in CTD files 
ctdAll <- read.csv('CTD/CTD_data_forAnalysis_fromCNV.csv')
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
  summarise(SST=mean(Temperature, na.rm=T)) %>%
  mutate(T_MLD=SST-0.8)

# Temp at 400 meters and thermocline temp
ctdMLD <- ctdAll %>% 
  group_by(Cast) %>% 
  filter(near(Depth, 400, 0.1)) %>% 
  summarise(T_400=mean(Temperature, na.rm=T)) %>%
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
  select(Cast, Depth, Fluorescence) %>% 
  group_by(Cast) %>% 
  slice_max(Fluorescence) %>% 
  rename(DCMdepth=Depth, ChlMax=Fluorescence) %>% 
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
nut$Nitrate..Nitrite <- as.numeric(nut$Nitrate..Nitrite)
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







### Section plots ###
# Make function for the plot
# Modified to fit from Emma's code in CTD_exploration2.R
plot_ocng_section_nocont <- function(data, ocng_var, Res1, Res2, title_label, Color, contours, contourCols, XLab, scalar=1) {
  clean_data <- na.omit(data) %>%
    select(Latitude, Depth, !!sym(ocng_var)) %>%
    rename(OCNVar = !!sym(ocng_var)) %>% 
    filter(Depth <= 1000*scalar)
  
  # Interpolation with MBA
  interp <- mba.surf(clean_data, no.X = Res1, no.Y = Res2, extend = T)
  dimnames(interp$xyz.est$z) <- list(interp$xyz.est$x, interp$xyz.est$y)
  
  # Convert to dataframe
  interp_df <- melt(interp$xyz.est$z, varnames = c("Latitude", "Depth"), value.name = "OCNVar") %>%
    mutate(OCNVAr = round(OCNVar, 1))
  
  # Plot
  ggplot(data = interp_df, aes(x = Latitude, y = Depth)) +
    geom_raster(aes(fill = OCNVar)) +
    geom_contour(aes(z=OCNVar), breaks=contours, colour=contourCols, alpha=0.3) + 
    geom_segment(data=data.frame(lat=unique(round(clean_data$Latitude, 1))), aes(x = lat, y = 5, xend = lat, yend = 25*scalar)) +
    scale_fill_gradientn(colors = Color, na.value = 'Transparent') +
    scale_y_reverse(expand = F, breaks=seq(0,1000,200)*scalar, labels=seq(0,1000,200)*scalar) +
    scale_x_continuous(expand = F, breaks=seq(10,30,2)) +
    guides(size = "none", 
           fill = guide_colourbar(title.position = "right"), 
           title.theme = element_text(angle = 270, hjust = 0.5, vjust = 0.5)) +
    labs(
      y = "Depth [m]",
      x = XLab, 
      fill = title_label,
    ) +
    theme(legend.title = element_text(angle = 90, hjust=0.5), 
          legend.direction = "vertical",
          legend.key.height = unit(1, 'null'), 
          legend.key.width = unit(0.5, 'cm'), 
          legend.margin = margin(0,0,0,0))
}

# Make plot for each variable of interest
OxyPlot <- plot_ocng_section_nocont(data = ctdAll, ocng_var = "Oxygen", Res1 = 300, Res2 = 300 , 
                                   title_label = "Oxygen [µmol/kg]", Color = c('#F7FBFF', '#08519C'), 
                                   contours = c(90, 45), contourCols = 'black', 
                                   XLab=NULL)

TempPlot <- plot_ocng_section_nocont(data = ctdAll, ocng_var = "Temperature", Res1 = 300, Res2 = 300 , 
                                   title_label = "Temperature [°C]", Color = oceColorsTurbo(9), 
                                   contours = c(0), contourCols = 'darkgray', 
                                   XLab=NULL)

SaltPlot <- plot_ocng_section_nocont(data = ctdAll, ocng_var = "Salinity", Res1 = 300, Res2 = 300 , 
                                     title_label = "Salinity", Color = oceColorsSalinity(9), 
                                     contours = c(0), contourCols = 'darkgray', 
                                     XLab=NULL)

N2Plot <- plot_ocng_section_nocont(data = nut, ocng_var = "Nitrate..Nitrite", Res1 = 300, Res2 = 300 , 
                         title_label = "Nitrate [µmol/L]", Color =viridis(10, option='mako')[2:10], #also like 'rocket'
                         contours = c(1,10,20,30), contourCols = 'white', 
                         XLab=NULL, 
                         scalar=0.2)

ChlPlot <- plot_ocng_section_nocont(data = ctdAll, ocng_var = "Fluorescence", Res1 = 300, Res2 = 300 , 
                                    title_label = expression("Total Chlorophyll [mg/m"^{3}*"]"), Color = oceColorsChlorophyll(9), #also like 'rocket'
                                    contours = c(1,10,20,30), contourCols = 'white', 
                                    XLab="Latitude [°N]",
                                    scalar=0.2)

cowplot::plot_grid(TempPlot, SaltPlot, OxyPlot, N2Plot, ChlPlot, ncol=1, rel_heights = c(1,1,1,1,1.15), align = 'v')
ggsave('CTD/Section_testPlots_forMS.png', width=10, height=12, units='in')


####-----JOHANNA TESTING NEW INTERPOLATION-----####
# temp.interp = akima::interp(x = clean_data$Latitude,
#                             y = clean_data$Depth,
#                             z = clean_data$Oxygen_Raw,
#                             duplicate = "mean", nx = 500, ny = 500)
# temp.interp = akima::interp(x = ctd.tb.all$latitude,
#                             y = ctd.tb.all$pressure,
#                             z = unlist(ctd.tb.all[,'oxygen']),
#                             duplicate = "mean", nx = 500, ny = 300)
# # Look at the interpolation
# image(temp.interp)
# # Put it in a ggplot friendly format and limit it to the top 1,000 meters
# # Choose which variable you want to plot
# interp_df = akima::interp2xyz(temp.interp) %>%
#   as.tibble() %>%
#   rename(Latitude = x, Depth = y, OCNVar = z) %>%
#   na.omit() %>%
#   filter(Depth <=1300 & Depth > 4)
####-----JOHANNA TESTING NEW INTERPOLATION END-----####

# # temp-depth profile plot for thermocline determination
# i=39
# ctdAll %>% filter(Cast == i) %>% 
#   ggplot() +
#   geom_path(aes(Temperature, Depth)) +
#   scale_y_reverse() +
#   scale_x_continuous(position='top') +
#   theme_light() + 
#   geom_hline(data=ctdMLD[which(ctdMLD$Cast == i),], aes(yintercept = D_MLD), color='darkgray', linetype='dashed') +
#   geom_point(data=ctdMLD[which(ctdMLD$Cast == i),], aes(T_MLD, D_MLD), color='green', size=2) +
#   geom_point(data=ctdMLD[which(ctdMLD$Cast == i),], aes(TT, D_TT), color='red', size=2) +
#   geom_point(data=ctdMLD[which(ctdMLD$Cast == i),], aes(T_TB, D_TB), size=2) +
#   #geom_abline(data=ctdMLD[which(ctdMLD$Cast == i),], aes(slope = TS*-1, intercept = intercept*-1), color='blue', linewidth=0.25) +
#   annotate('text', x=20, y=750, label=paste('Station', ctdMLD$Station2[which(ctdMLD$Cast == i)]))



