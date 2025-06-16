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
# myDir <- paste(here(), 'CTD', 'CTD_processed', sep='/')  # Johanna's File path
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
  #print(summary(ctdAll[idc,c('Latitude', 'Cast')]))
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

depthProfile <- function(CTDdata, PlotVar, VarName, figTitle) {
  # Calculate depth of oxygen minimum
  o2_min <- CTDdata %>% 
    group_by(Cast) %>% 
    slice(which.min(get(PlotVar))) %>% 
    select(Cast, DepSM)
  # Make depth profile with line showing oxygen minimum
  p <- CTDdata %>%
    ggplot(aes(x=get(PlotVar), y=DepSM)) +
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
    xlab(VarName) + ylab('Depth [m]') +
    ggtitle(figTitle)
  return(p)
}

# Make oxygen profile with oxygen min marked with line
oxyProfile <- depthProfile(ctdAll, 'Oxygen_cleaned', 'Oxygen [umol/kg]', 'Oxygen depth profile for SE2204')
ggsave(plot=oxyProfile, filename='O2DepthProfiles_AllStns_min.pdf', width=11, height = 8, dpi = 300, units = 'in')
ggsave(plot=oxyProfile, filename='O2DepthProfiles_AllStns_min.png', width=10, height = 5.625, dpi = 300)

# Make temperature profile with oxygen min marked with line
oxyProfile <- depthProfile(ctdAll, "T090C", 'Oxygen [umol/kg]', 'Oxygen depth profile for SE2204')
ggsave(plot=oxyProfile, filename='O2DepthProfiles_AllStns_min.pdf', width=11, height = 8, dpi = 300, units = 'in')
ggsave(plot=oxyProfile, filename='O2DepthProfiles_AllStns_min.png', width=10, height = 5.625, dpi = 300)


# # Plot Oxycline
# o2_min <- ctdAll %>% 
#   group_by(Cast) %>% 
#   slice(which.min(Oxygen_cleaned)) %>% select(Cast, DepSM)
# 
# ctdAll %>% 
#   ggplot(aes(x=Oxygen_cleaned, y=DepSM)) + 
#     geom_path() +
#     scale_y_reverse() + 
#     facet_wrap(.~Cast, labeller=labeller(Cast=id.labs)) +
#     theme_bw() +
#     theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank()) +
#     geom_hline(data = o2_min, aes(yintercept = DepSM), linetype= 'dashed', color='red') +
#     #geom_hline(yintercept = 500, linetype = "dashed", color = "gray")
#     xlab('Oxygen [umol/kg]') + ylab('Depth [m]') +
#     ggtitle('Oxygen Depth Profiles SE2204')

ggsave('O2DepthProfiles_AllStns_min.pdf', width=11, height = 8, dpi = 300, units = 'in')
ggsave('O2DepthProfiles_AllStns_min.png', width=10, height = 5.625, dpi = 300)


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
ggsave('TempDepthProfiles_AllStns.png', width=10, height = 5.625, dpi = 300)

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
ggsave('SalinityDepthProfiles_AllStns.png', width=10, height = 5.625, dpi = 300)

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
ggsave('DensityDepthProfiles_AllStns.png', width=10, height = 5.625, dpi = 300)

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
ggsave('FluorDepthProfiles_AllStns.png', width=10, height = 5.625, dpi = 300)




#----------------------------- NUTRIENTS ---------------------------------------

# Merging nutrient files and cleaning the starting rows (check SE2204_Nutrient-Vis.R)
#library(readxl)
#nutMeta <- read.csv('../SE2204_nutrient_metadata (1).csv')
#nutDat <- read_xls('../SE2204_Nutrient_Data.xls', skip=12)
#nutDat <- data.frame(nutDat[-1,])
library(akima)
library(viridis)
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

# Interpolations for Nutrients along Latitude 
nut_data <- nut %>%
  mutate(
    Depth2 = as.numeric(Depth2),
    Latitude = as.numeric(Latitude),
    Phosphate = as.numeric(Phosphate)
  ) %>%
  filter(!is.na(Phosphate), !is.na(Latitude), !is.na(Depth2))

interp_result <- with(nut_data,
                interp(x = Latitude, y = Depth2, z = Phosphate,
                duplicate = "mean", linear = TRUE))

interp_df <- expand.grid(Latitude = interp_result$x,Depth2 = interp_result$y)
interp_df$Phosphate <- as.vector(interp_result$z)

int.phos = 
  ggplot(interp_df, aes(x = Latitude, y = Depth2, z = Phosphate)) +
  geom_contour_filled() +
  scale_y_reverse() +
  scale_fill_viridis_d() +
  labs(
    title = "Interpolated Phosphate Concentration",
    x = "Latitude",
    y = "Depth (m)",
    fill = "Phosphate"
  ) +
  theme_minimal()
int.phos
ggsave('PhosInterp.png', width=10, height = 5.625, dpi = 300)

nut_sili <- nut %>%
  mutate(
    Depth2 = as.numeric(Depth2),
    Latitude = as.numeric(Latitude),
    Silicate = as.numeric(Silicate)
  ) %>%
  filter(!is.na(Silicate), !is.na(Latitude), !is.na(Depth2))

interp_result <- with(nut_sili,
                      interp(x = Latitude, y = Depth2, z = Silicate,
                             duplicate = "mean", linear = TRUE))  
interp_df <- expand.grid(Latitude = interp_result$x,Depth2 = interp_result$y)
interp_df$Silicate <- as.vector(interp_result$z)

int.sili = 
  ggplot(interp_df, aes(x = Latitude, y = Depth2, z = Silicate)) +
  geom_contour_filled() +
  scale_y_reverse() +
  scale_fill_viridis_d() +
  labs(
    title = "Interpolated Silicate Concentration",
    x = "Latitude",
    y = "Depth (m)",
    fill = "Silicate"
  ) +
  theme_minimal()
int.sili
ggsave('SiliInterp.png', width=10, height = 5.625, dpi = 300)

nut_nit <- nut %>%
  mutate(
    Depth2 = as.numeric(Depth2),
    Latitude = as.numeric(Latitude),
    Nitrate...Nitrite = as.numeric(Nitrate...Nitrite)
  ) %>%
  filter(!is.na(Nitrate...Nitrite), !is.na(Latitude), !is.na(Depth2))

interp_result <- with(nut_nit,
                      interp(x = Latitude, y = Depth2, z = Nitrate...Nitrite,
                             duplicate = "mean", linear = TRUE))  
interp_df <- expand.grid(Latitude = interp_result$x,Depth2 = interp_result$y)
interp_df$Nitrate...Nitrite <- as.vector(interp_result$z)

int.nit = 
  ggplot(interp_df, aes(x = Latitude, y = Depth2, z = Nitrate...Nitrite)) +
  geom_contour_filled() +
  scale_y_reverse() +
  scale_fill_viridis_d() +
  labs(
    title = "Interpolated Nitrate..Nitrite Concentration",
    x = "Latitude",
    y = "Depth (m)",
    fill = "Nitrate..Nitrite"
  ) +
  theme_minimal()
int.nit
ggsave('NInterp.png', width=10, height = 5.625, dpi = 300)


nut_am <- nut %>%
  mutate(
    Depth2 = as.numeric(Depth2),
    Latitude = as.numeric(Latitude),
    Ammonia = as.numeric(Ammonia)
  ) %>%
  filter(!is.na(Ammonia), !is.na(Latitude), !is.na(Depth2))

interp_result <- with(nut_am,
                      interp(x = Latitude, y = Depth2, z = Ammonia,
                             duplicate = "mean", linear = TRUE))  
interp_df <- expand.grid(Latitude = interp_result$x,Depth2 = interp_result$y)
interp_df$Ammonia <- as.vector(interp_result$z)

int.am =
  ggplot(interp_df, aes(x = Latitude, y = Depth2, z = Ammonia)) +
  geom_contour_filled() +
  scale_y_reverse() +
  scale_fill_viridis_d() +
  labs(
    title = "Interpolated Ammonia Concentration",
    x = "Latitude",
    y = "Depth (m)",
    fill = "Ammonia" ) +
  theme_minimal()
int.am
ggsave('AmmoniaInterp.png', width=10, height = 5.625, dpi = 300)

cowplot::plot_grid(int.phos, int.sili, int.nit, int.am, nrow = 4)


# Plotting
# Silicate 
ggplot(nut, aes(x = Latitude, y = Depth2, color = Phosphate)) +
  geom_point(size = 3) +
  scale_y_reverse() + 
  scale_color_viridis_d() + 
  labs(
    title = "Phosphate Concentration Along Latitude",
    x = "Latitude",
    y = "Depth (m)",
    color = "Phosphate" ) +
  theme_minimal() +
  theme(legend.key.size = unit(0.05, 'cm'))
ggsave('PhosphateLat_bubble.png', width=10, height = 5.625, dpi = 300)

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
ggsave('SilicateDepthProfiles_line.png', width=10, height = 5.625, dpi = 300)

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
ggsave('PhosphateDepthProfile_scatter.png', width=10, height = 5.625, dpi = 300)

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
ggsave('PhosphateDepthProfile_scatterfreex.png', width=10, height = 5.625, dpi = 300)

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
ggsave('NDepthProfile_scatter.png', width=10, height = 5.625, dpi = 300)

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
ggsave('AmmoniaDepthProfile_scatter.png', width=10, height = 5.625, dpi = 300)

# Oxygen Vs. Temperature
# install.packages("paletteer")
library(paletteer)

ctdAll %>% 
  ggplot(aes(x=newLat, y=Oxygen_cleaned, color = Cast)) + 
  geom_point(alpha = 1/10) +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  # scale_color_viridis_c() +
  scale_colour_paletteer_c(`"grDevices::Harmonic"`) +
  theme_minimal() +
  xlab('Latitude [°N]') + 
  ylab('Oxygen [umol/kg]') +
  ggtitle('Oxygen vs. Temperature Profiles SE2204')

ggsave('O2vsTemp_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')
ggsave('O2vsT_linearR.png', width=10, height = 5.625, dpi = 300)









# ----------------------------- GLORYS ---------------------------------------
# Read in CLIMATOLOGY GLORYS Data 
clim <- read.csv('../GLORYS_Climatology_JunJul_SE2204.csv')
head(clim)

# Plot of CTD vs Climatology GLORYS Data 
ggplot() + 
  geom_path(data=ctdAll, aes(y=DepSM, x=Oxygen_cleaned, color='CTD')) +
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

ggsave('O2Climatology_GLORYS.png', width=10, height = 5.625, dpi = 300)



#Read in MONTHLY GLORYS Data
oxymonth <- read.csv("../GLORYS_Monthly_JunJul_SE2204.csv")

#Plot of CTD vs Monthly GLORYS Data 
ggplot() + 
  geom_path(data=ctdAll, aes(y=DepSM, x=Oxygen_cleaned, color='CTD')) +
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

ggsave('O2Monthly_GLORYS.png', width=10, height = 5.625, dpi = 300)


# Read in DAILY GLORYS Data 
oxy <- read.csv('../GLORYS_oxygen_SE2204.csv')
head(oxy)

# Plot of CTD vs Daily GLORYS Data 
ggplot() + 
  geom_path(data=ctdAll, aes(y=DepSM, x=Oxygen_cleaned, color='CTD')) +
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

ggsave('O2GLORYSDaily_CTD.png', width=10, height = 5.625, dpi = 300)



# Correlations between GLORYS and CTD data by station
# List all the cast numbers
unique(ctdAll$Cast)

# Function to calculate correlations
# Defined as PearsonCorr for Pearson Correlation where r and R^2 are calculated in a correlation significance test
PearsonCorr <- function(CTDdata, Gdata, CastNr) {
  # make a variable for the station you want to compare with CTD data
  cst2ctd <- CTDdata %>% 
    filter(Cast == CastNr) %>% 
    select(DepSM, Oxygen_cleaned) %>% 
    data.frame()
  
  # Then a matching one with GLORYS data
  cst2glo <- Gdata %>% 
    filter(Cast == CastNr) %>% 
    select(Depth, Oxygen)
  # Interpolate glorys data over the depths in the CTD data
  test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
  # Calculate the correlation between glorys and CTD (output is r, not R^2)
  oxyCor <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
  return(oxyCor)
}

#Defining lists for Daily, Monthly, and Climatology GLORYS data
oxyCorList <- list()
castIdx <- unique(ctdAll$Cast)
for (i in seq_along(castIdx)) {
  oxyCorList[[i]] <- PearsonCorr(ctdAll, oxy, castIdx[i])
}

climCorList <- list()
castIdx <- unique(ctdAll$Cast)
for (i in seq_along(castIdx)) {
  climCorList[[i]] <- PearsonCorr(ctdAll, clim, castIdx[i])
}

monthCorList <- list()
castIdx <- unique(ctdAll$Cast)
for (i in seq_along(castIdx)) {
monthCorList[[i]] <- PearsonCorr(ctdAll, oxymonth, castIdx[i])
}

#Making a table for correlations between CTD and GLORYS
corTable <- matrix(nrow = length(castIdx), ncol = 4)
for (j in 1:length(oxyCorList)) {
  x <- oxyCorList[[j]]$estimate^2
  y <- climCorList[[j]]$estimate^2
  z <- monthCorList[[j]]$estimate^2
  corTable[j,1] <- castIdx[j]
  corTable[j,2] <- x
  corTable[j,3] <- y
  corTable[j,4] <- z
}
corTable <- as.data.frame(corTable)
names(corTable) <- c('Cast', 'Daily_Rsq', 'Clim_Rsq', 'Monthly_Rsq')
corTable


# # make a variable for the station you want to compare with CTD data
#   cst2ctd <- ctdAll %>%
#   filter(Cast == 8) %>%
#   select(DepSM, Oxygen_cleaned) %>%
#   data.frame()
# # Then a matching one with glorys data
# cst2glo <- oxy %>%
#   filter(Cast == 8) %>%
#   select(Depth, Oxygen)
# 
# # Interpolate glorys data over the depths in the CTD data
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# # Calculate the correlation between glorys and CTD (output is r, not R^2)
# oxyCor8 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor8
# 
# # make a variable for the station you want to compare with CTD data
# cst2ctd <- ctdAll %>%
#   filter(Cast == 9) %>%
#   select(DepSM, Oxygen_cleaned) %>%
#   data.frame()
# # Then a matching one with glorys data
# cst2glo <- oxy %>%
#   filter(Cast == 9) %>%
#   select(Depth, Oxygen)
# # Interpolate glorys data over the depths in the CTD data
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# # Calculate the correlation between glorys and CTD (output is r, not R^2)
# oxyCor9 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor9
# 
# # make a variable for the station you want to compare with CTD data
# cst2ctd <- ctdAll %>%
#   filter(Cast == 10) %>%
#   select(DepSM, Oxygen_cleaned) %>%
#   data.frame()
# # Then a matching one with glorys data
# cst2glo <- oxy %>%
#   filter(Cast == 10) %>%
#   select(Depth, Oxygen)
# # Interpolate glorys data over the depths in the CTD data
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# # Calculate the correlation between glorys and CTD (output is r, not R^2)
# oxyCor10 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor10
# 
# # make a variable for the station you want to compare with CTD data
# cst2ctd <- ctdAll %>%
#   filter(Cast == 17) %>%
#   select(DepSM, Oxygen_cleaned) %>%
#   data.frame()
# # Then a matching one with glorys data
# cst2glo <- oxy %>%
#   filter(Cast == 17) %>%
#   select(Depth, Oxygen)
# # Interpolate glorys data over the depths in the CTD data
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# # Calculate the correlation between glorys and CTD (output is r, not R^2)
# oxyCor17 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor17
# 
# # make a variable for the station you want to compare with CTD data
# cst2ctd <- ctdAll %>%
#   filter(Cast == 18) %>%
#   select(DepSM, Oxygen_cleaned) %>%
#   data.frame()
# # Then a matching one with glorys data
# cst2glo <- oxy %>%
#   filter(Cast == 18) %>%
#   select(Depth, Oxygen)
# # Interpolate glorys data over the depths in the CTD data
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# # Calculate the correlation between glorys and CTD (output is r, not R^2)
# oxyCor18 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor18
# 
# # make a variable for the station you want to compare with CTD data
# cst2ctd <- ctdAll %>%
#   filter(Cast == 19) %>%
#   select(DepSM, Oxygen_cleaned) %>%
#   data.frame()
# # Then a matching one with glorys data
# cst2glo <- oxy %>%
#   filter(Cast == 19) %>%
#   select(Depth, Oxygen)
# # Interpolate glorys data over the depths in the CTD data
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# # Calculate the correlation between glorys and CTD (output is r, not R^2)
# oxyCor19 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor19
# 
# cst2ctd <- ctdAll %>%
#   filter(Cast == 26) %>%
#   select(DepSM, Oxygen_cleaned) %>%
#   data.frame()
# cst2glo <- oxy %>%
#   filter(Cast == 26) %>%
#   select(Depth, Oxygen)
# 
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# oxyCor26 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor26
# 
# cst2ctd <- ctdAll %>%
#   filter(Cast == 27) %>%
#   select(DepSM, Oxygen_cleaned) %>%
#   data.frame()
# cst2glo <- oxy %>%
#   filter(Cast == 27) %>%
#   select(Depth, Oxygen)
# 
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# oxyCor27 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor27
# 
# cst2ctd <- ctdAll %>%
#   filter(Cast == 28) %>%
#   select(DepSM, Oxygen_cleaned) %>%
#   data.frame()
# cst2glo <- oxy %>%
#   filter(Cast == 28) %>%
#   select(Depth, Oxygen)
# 
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# oxyCor28 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor28
# 
# cst2ctd <- ctdAll %>%
#   filter(Cast == 35) %>%
#   select(DepSM, Oxygen_cleaned) %>%
#   data.frame()
# cst2glo <- oxy %>%
#   filter(Cast == 35) %>%
#   select(Depth, Oxygen)
# 
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# oxyCor35 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor35
# 
# cst2ctd <- ctdAll %>%
#   filter(Cast == 36) %>%
#   select(DepSM, Oxygen_cleaned) %>%
#   data.frame()
# cst2glo <- oxy %>%
#   filter(Cast == 36) %>%
#   select(Depth, Oxygen)
# 
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# oxyCor36 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor36
# 
# cst2ctd <- ctdAll %>%
#   filter(Cast == 37) %>%
#   select(DepSM, Oxygen_cleaned) %>%
#   data.frame()
# cst2glo <- oxy %>%
#   filter(Cast == 37) %>%
#   select(Depth, Oxygen)
# 
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# oxyCor37 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor37
# 
# cst2ctd <- ctdAll %>%
#   filter(Cast == 38) %>%
#   select(DepSM, Oxygen_cleaned) %>%
#   data.frame()
# cst2glo <- oxy %>%
#   filter(Cast == 38) %>%
#   select(Depth, Oxygen)
# 
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# oxyCor38 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor38
# 
# cst2ctd <- ctdAll %>%
#   filter(Cast == 2) %>%
#   select(DepSM, Oxygen_cleaned) %>%
#   data.frame()
# cst2glo <- oxy %>%
#   filter(Cast == 2) %>%
#   select(Depth, Oxygen)
# 
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# oxyCor2 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor2
# 
# cst2ctd <- ctdAll %>%
#   filter(Cast == 5) %>%
#   select(DepSM, Oxygen_cleaned) %>%
#   data.frame()
# cst2glo <- oxy %>%
#   filter(Cast == 5) %>%
#   select(Depth, Oxygen)
# 
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# oxyCor5 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor5
# 
# cst2ctd <- ctdAll %>% 
#   filter(Cast == 11) %>% 
#   select(DepSM, Oxygen_cleaned) %>% 
#   data.frame()
# cst2glo <- oxy %>% 
#   filter(Cast == 11) %>% 
#   select(Depth, Oxygen) 
# 
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# oxyCor11 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor11
# 
# cst2ctd <- ctdAll %>% 
#   filter(Cast == 14) %>% 
#   select(DepSM, Oxygen_cleaned) %>% 
#   data.frame()
# cst2glo <- oxy %>% 
#   filter(Cast == 14) %>% 
#   select(Depth, Oxygen) 
# 
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# oxyCor14 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor14
# 
# cst2ctd <- ctdAll %>% 
#   filter(Cast == 20) %>% 
#   select(DepSM, Oxygen_cleaned) %>% 
#   data.frame()
# cst2glo <- oxy %>% 
#   filter(Cast == 20) %>% 
#   select(Depth, Oxygen) 
# 
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# oxyCor20 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor20
# 
# cst2ctd <- ctdAll %>% 
#   filter(Cast == 23) %>% 
#   select(DepSM, Oxygen_cleaned) %>% 
#   data.frame()
# cst2glo <- oxy %>% 
#   filter(Cast == 23) %>% 
#   select(Depth, Oxygen) 
# 
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# oxyCor23 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor23
# 
# cst2ctd <- ctdAll %>% 
#   filter(Cast == 29) %>% 
#   select(DepSM, Oxygen_cleaned) %>% 
#   data.frame()
# cst2glo <- oxy %>% 
#   filter(Cast == 29) %>% 
#   select(Depth, Oxygen) 
# 
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# oxyCor29 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor29
# 
# cst2ctd <- ctdAll %>% 
#   filter(Cast == 32) %>% 
#   select(DepSM, Oxygen_cleaned) %>% 
#   data.frame()
# cst2glo <- oxy %>% 
#   filter(Cast == 32) %>% 
#   select(Depth, Oxygen) 
# 
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# oxyCor32 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor32
# 
# cst2ctd <- ctdAll %>% 
#   filter(Cast == 39) %>% 
#   select(DepSM, Oxygen_cleaned) %>% 
#   data.frame()
# cst2glo <- oxy %>% 
#   filter(Cast == 39) %>% 
#   select(Depth, Oxygen) 
# 
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# oxyCor39 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor39
# 
# cst2ctd <- ctdAll %>% 
#   filter(Cast == 43) %>% 
#   select(DepSM, Oxygen_cleaned) %>% 
#   data.frame()
# cst2glo <- oxy %>% 
#   filter(Cast == 43) %>% 
#   select(Depth, Oxygen) 
# 
# test <- approx(cst2glo$Depth, cst2glo$Oxygen, xout=cst2ctd$DepSM)
# oxyCor43 <- cor.test(x=test$y, y=cst2ctd$Oxygen_cleaned)
# oxyCor43
# 
# # corrT = matrix(c(10., oxyCor10$estimate, 11.,  oxyCor11$estimate), ncol=2, byrow=TRUE)
# # colnames(corrT) = c('Cast Number', 'r')
# # rownames(corrT) <- c('10', '14')
# # finalT = as.table(corrT)
# # finalT
# 
# # idx <- ls() %>% grep(pattern='oxyCor')
# # ls()[idx]
# # ls()[idx][-c(1:2,26)]
# # idxCor <- ls()[idx][-c(1:2,26)]
# # idxCor







# ------------ For Reference-----------------------------------------------#
###### Working with GLORYS .nc files to match with CTD depth profiles ######
#--------------------------------------------------------------------------#
#### DAILY GLORYS DATA COMPARISON WITH CTD DATA
# Get the depths from the GLORYS .nc file and replace them with the depth index when reading in the data
library(ncdf4)
nc <- nc_open('~/Downloads/cmems_mod_glo_bgc_my_0.25deg_P1D-m_1749767284346.nc')
depth <- ncvar_get(nc, varid='depth')
nc_close(nc)

# read in the GLORYS .nc file
library(raster)
# remove the file names for the files we don't need. We can use the stnInfo file to only use the filenames of the stations we want to load in the data
files <- stnInfo$FileName
# Preallocate list
o2 <- list()
# Turn GLORYS nc into long format data frame
# Read in GLORYS data for all dates in a loop over depth
for (i in seq_along(depth)) {
  oxy <- brick('~/Downloads/cmems_mod_glo_bgc_my_0.25deg_P1D-m_1749767284346.nc', varname='o2', level=i)
  # extract the glorys data from the cruise locations and add station number and locations to the data and save in a list
  o2[[i]] <- cbind(cbind(Depth=depth[i], stnInfo[,c(1,4,5)]), as.data.frame(raster::extract(oxy, stnInfo[,c('Lon', 'Lat')])))
}
# Turn the list into a dataframe and rename the id column to Depth
oxy <- rbindlist(o2, use.names = T, idcol = F)
head(oxy)
tail(oxy)

# Turn the dataframe from wide to long format so it's consistent with the CTD data
oxyLong <- gather(oxy, key='Date', value = 'Oxygen', -Depth:-Lat)
# Make a DateTime column to match with CTD data by removing the X in front of the date in the Date columne here (former colunm neading)
oxyLong$DateTime <- as.Date(substr(oxyLong$Date, 2, 11), '%Y.%m.%d')
head(oxyLong)
# combine CTD metadata with oxygen data
time <- as.Date(stnInfo$Date, '%m/%d/%y %H:%M')
oxyLong$DateTime <- as.Date(substr(oxyLong$Date, 2, 11), '%Y.%m.%d')
test <- oxyLong %>% 
  left_join(stnInfo[,c(1,8, 10, 13)], by=c('Station', 'DateTime'))
test <- na.omit(test)
head(test)
oxyLong <- test
write.csv(test, 'SE2204_CTD_processed_down_cnv/GLORYS_oxygen_SE2204.csv', quote = F, row.names = F)

### CLIMATOLOGICAL GLORYS COMPARISON WITH CTD DATA
library(terra)
# Read in the .nc file as a SpatRast object
oxyT <- rast('~/Downloads/cmems_mod_glo_bgc_my_0.25deg_P1M-m_1749767610042.nc')
# get all unique depths
d <- unique(depth(oxyT))
# Make empty lists to store data
oxyClim <- list()
oxyCastClim <- list()
for (i in seq_along(d)) {
  # Subset data to one depth only
  oxyD <- terra::subset(oxyT, depth(oxyT) == d[i])
  # Calculate the mean (climatolory) by month for all years for the one depth
  oxyClim[[i]] <- tapp(oxyD, 'month', 'mean')
  oxyCastClim[[i]] <- terra::extract(oxyClim[[i]], stnInfo[,4:5], xy=T) %>% 
    select(x,y, m_6, m_7) %>% 
    bind_cols(stnInfo[,c(1,8,10,13)], Depth=d[i])  
}
oxyClim67 <- rbindlist(oxyCastClim, use.names = T, idcol = F) %>% 
  select(7,5,6,1,2,8,9,3,4) %>% 
  rename(Lon=x, Lat=y, June=m_6, July=m_7)
head(oxyClim67)

# Put into long format and remove GLORYS data for months when there was no sampling
oxyClimLong <- gather(oxyClim67, key='MonthB', value='Oxygen', -Cast:-Depth) %>%  # put in long format
  mutate(Month=month(DateTime)) %>%  # add a month column
  filter((MonthB == 'June' & Month == 6) | (MonthB == 'July' & Month == 7))  # Remove GLORYS data for months that don't coincide with sampling times
head(oxyClimLong)

write.csv(oxyClimLong, 'SE2204_CTD_processed_down_cnv/GLORYS_Climatology_JunJul_SE2204.csv', quote = F, row.names = F)

### MONTHLY GLORYS DATA COMPARISON WITH CTD DATA
# Subset data to June and July 2022 only for all depths (couldn't figure out how to do this in one go so doing one month at a time)
oxyM1 <- terra::subset(oxyT, time(oxyT) == as.Date('2022-06-01'))
oxyM2 <- terra::subset(oxyT, time(oxyT) == as.Date('2022-07-01'))
# combine into one SpatRaster
oxyM <- c(oxyM1,oxyM2)
# Extract values for each cast location for all depths and times
oxyCastM <- terra::extract(oxyM, stnInfo[,4:5], xy=T) %>% 
  gather(key='DepthIdx', value='Oxygen', 2:101) %>% 
  mutate(DateIdx = str_sub(DepthIdx, -3, -1), 
         Depth=as.numeric(str_sub(DepthIdx, 10,-5)), 
         Month=if_else(DateIdx == '349', 6, 7)) %>% 
  rename(LonG=x, LatG=y)
head(oxyCastM)
# Merge the monthly GLORYS data with the cast metadata. Had to do this by ID (1-23)
oxyMonthLong <- stnInfo %>% 
  mutate(ID=1:n(), Month=month(DateTime)) %>% 
  select(c(1,8,10,13,4,5,14,15)) %>% 
  right_join(oxyCastM, by=c('ID', 'Month')) %>% 
  na.omit() %>% 
  select(3,1,2,5,6,4,8,14,12)
head(oxyMonthLong)
# Save file
write.csv(oxyClimLong, 'SE2204_CTD_processed_down_cnv/GLORYS_Monthly_JunJul_SE2204.csv', quote = F, row.names = F)







