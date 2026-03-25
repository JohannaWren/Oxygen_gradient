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
library(readxl)
library(cowplot)

# Set working directory
myDir <- here()  # Johanna's File path
setwd(myDir)

# Read in CTD files 
ctdAll <- read.csv('CTD/CTD_data_forAnalysis_fromCNV.csv')
stnInfo <- read.csv('CTD/CTD_metadata_forAnalysis.csv')
DayNight <- c('D','N','D', 'N', 'D', 'N', 'D', 'D', 'N', 'D','N','D','D','N','D','N','D','D','N','D','N','D','N')
stnInfo$DayNight <- DayNight
stnInfo$bins2 <- cut(x=round(stnInfo$Lat,2), breaks = seq(30,10,-4))

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
ctdMLD <- left_join(stnInfo[,c(4,5,8,9,15,16,17)],ctdMLD, by='Cast')

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
# Figure for binned data
ggplot(ctdMLD, aes(bins2, OMZdepth)) + 
  geom_boxplot() + 
  theme_classic() + 
  scale_y_reverse() + 
  xlab(label = NULL) +
  ylab(label='Depth (m)') + 
  annotate(geom='text', y=-Inf, x=Inf, label='Depth of Oxygen minimum', vjust=1, hjust=1)

phyto %>% 
  filter(Filter == 'bulk', Depth==0) %>% ggplot(aes(bins2, Chlorophyll)) + geom_boxplot(aes(fill=DayNight)) + scale_fill_discrete(palette=c('gray90', 'gray60')) + 
  theme_classic() + 
  xlab(label = NULL) +
  ylab(label='Chlorophyll a (mg/m3)') + 
  annotate(geom='text', y=Inf, x=Inf, label='Surface Chlorophyll', vjust=1, hjust=1)

## Chlorophyll data
phyto <- read.csv(paste(here(), 'Data/fluorometry_SE2204.csv', sep='/'))
head(phyto)

# Clean up the phytoplankton data and make sure and turn filter into sizes
phyto <- phyto %>% 
  mutate(Size=as.numeric(Filter))

# Make file for nice station plotting
id.labs <- phyto$Station
names(id.labs) <- phyto$Cast
head(id.labs)

#Link Cast labels to the Station ID
cast_labels <- phyto %>%
  distinct(Cast, Station) %>%
  arrange(Cast)
label_vector <- setNames(cast_labels$Station, cast_labels$Cast)

# Add day/night to data from metadata file
phyto <-phyto %>% 
  left_join(stnInfo[,c('Cast','DayNight', 'bins2')])

# Get bulk surface Chlorophyll
ExtStns <- c(2,5,11,14,20,23,29,32,39,43)
ExtStnsLetter <- c('A','A','B','B','C','C','D','D','E','E')
phyto %>% 
  filter(Filter == 'bulk', Depth==0) %>% 
  filter(Cast %in% ExtStns) %>% 
  mutate(StnLetter = ExtStnsLetter) %>% 
  ggplot() +
    geom_bar(aes(StnLetter, Chlorophyll, fill=DayNight), stat='identity', position='dodge') + 
    theme_classic() +
    scale_fill_discrete(palette = c('lightgray', 'darkgray')) +
    coord_cartesian(expand=F)



# Depth integrated chl-a
# Function to depth integrate one filter size
chlDepthInt <- function(phytoDat) {
  chlTotVec <- vector()
  for (i in 1:9) {
    chlLayer <- (phytoDat$Chlorophyll[i]+phytoDat$Chlorophyll[i+1])/2
    chlDepth <- abs(phytoDat$Depth[i]-phytoDat$Depth[i+1])
    chlTotVec <- cbind(chlTotVec, chlLayer*chlDepth)
  }
  return(sum(as.vector(chlTotVec)))
}
# Loop through all stations and all filter sizes and calculate the depth integrated chl for all
depthIntChl <- data.frame(Cast=NA, Filter=NA, Chl=NA)
for (i in unique(phyto$Cast)) {
  for (j in unique(phyto$Filter)) {
    chl <- phyto %>% 
      filter(Cast == i, Filter == j) %>% 
      chlDepthInt()
    
    depthIntChl <- depthIntChl %>% 
      add_row(Cast=i, Filter=j, Chl=chl)
    
  }
}
# Add metadata
depthIntChl <- stnInfo %>% 
  select(Station2, Cast, Lon, Lat) %>% 
  left_join(depthIntChl, by='Cast')

# Make quick plot
# Make label names
phytogroups <- c('0.2'='pico', '2'='nano', '20'='micro')

depthIntChl %>% filter(Filter!='bulk') %>% 
  ggplot(aes(as.factor(round(Lat,1)), Chl)) + 
    geom_bar(stat='identity') + 
    facet_wrap(.~Filter, scales='free_y', labeller=as_labeller(phytogroups)) +
    theme_bw() + 
    theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90))

# Phytoplankton biomass
# Use size fractions only
phytoSize <- phyto %>% 
  #filter(Filter != 'bulk') %>%  
  mutate(Filter = as.numeric(Filter))
# Define groups
bin_edges2 <- data.frame(
  Filter = c(0.2, 2, 20),
  plower_bound = c(0.2, 2, 20),
  pupper_bound = c(2, 20, 200 )) %>%
  mutate(pbin_width = pupper_bound - plower_bound)
# join datasets
pnorm <- phytoSize %>%
  left_join(bin_edges2, by = "Filter") %>%
  mutate(p_normalized_biomass = Chlorophyll / pbin_width)

## Zooplankton
zoopsAll <- read_xlsx(paste(here(), 'Data/Bongo data.xlsx', sep='/'), sheet = 2) %>% 
  filter(!is.na(net_cast_number)) # remove filters that have weights but weren't used
zoopsMeta <- read_xlsx('Data/Bongo data.xlsx', sheet=1)
head(zoopsAll)

zoops <- zoopsAll %>% 
  select(net_cast_number, size_fraction, net_dry_weight, QC_net_weight) %>% 
  left_join(zoopsMeta[,c('station_id',"net_opening_m2",'flow_counts Net#1','actual_depth_max_m')], by=c('net_cast_number'='station_id'))


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



