# Script to start looking at oceanographic data during the cruise

# Libraries
library(dplyr)
library(readxl)
library(stringr)

# 
setwd('~/Documents/Cruises/SE2204_BETOceanography/SE2204 - Hot Spot/')
##-------------------------------------------------------------
## Read in the data
##-------------------------------------------------------------
# Nutrients
nutrients <- readxl::read_xls('SE2204_Nutrient_data.xls', skip=12)
nutrients <- nutrients[-1,]
colnames(nutrients)[1] <- 'index'
nutrients$index <- as.numeric(nutrients$index)
str(nutrients)
nutmeta <- read.csv('SE2204_nutrient_metadata.csv')
nutmeta$Depth <- as.numeric(str_sub(nutmeta$Depth, start=1, end=-2))
nutmeta$index <- 1:nrow(nutmeta)
nutmeta <- nutmeta[-28,]
nut <- full_join(nutrients, nutmeta)
nut

nut$Ammonia <- ifelse(nut$Ammonia == "<0.02", 0.01, as.numeric(nut$Ammonia))
nut$Phosphate <- ifelse(nut$Phosphate == "<0.008", 0.007, as.numeric(nut$Phosphate))
nut$Silicate <- as.numeric(nut$Silicate)
nut$`Nitrate + Nitrite` <- as.numeric(nut$`Nitrate + Nitrite`)
nut$Date <- as.Date(nut$Date, '%m/%d/%y') 
nut

# # CTD
# CTDlog <- read.csv('CTD/CTD_log.csv') %>% 
#   mutate(Date=as.Date(Date, format='%m/%d/%y')) %>% 
#   filter(Sampling == 'chlorophyll and nutrients')
# 
# # merge ctdlog and nutrients
# # add station ID to the ctd log
# idx <- paste(rep(2:16, each=2), rep(1:2), sep='.')
# idx[1] <- '2.3'
# idx[2] <- '2.4'
# idx <- idx[-21]
# CTDlog$StationID <- idx
# nutri <- full_join(CTDlog[,c(4:6,8:10)], nut)
# nutri

# Plot
# nutriDN <- nutri %>% filter(Day.Night == 'Day', Location == 'Cyclonic 1')
# 
# estimate_temp_by_date <- function(taget_lat, target_depth) {
#   data_for_date <- nutriDN %>% 
#     filter(Lat == taget_lat) %>%
#     arrange(Depth)
#   
#   # approx() is one way to do a linear interpolation
#   approx(data_for_date$Depth, data_for_date$`N+N`, xout = target_depth)$y
# }
# 
# temp_interp_depth <- crossing(
#   # the same dates as sonde_tbl_1993
#   tibble(Lat = unique(nutriDN$Lat)),
#   # depths can now be any value
#   tibble(Depth = seq(from=0, to=200, by=5))
# ) %>%
#   group_by(Lat) %>%
#   mutate(NN = estimate_temp_by_date(Lat[1], Depth))
# 
# # create a function that will, given a depth, estimate the temp on any given day
# estimate_temp_by_depth <- function(target_depth, target_lat) {
#   data_for_depth <- temp_interp_depth %>% 
#     filter(Depth == target_depth) %>%
#     arrange(Lat)
#   approx(data_for_depth$Lat, data_for_depth$NN, xout = target_lat)$y
# }
# 
# temp_raster <- crossing(
#   # dates can now be any value
#   tibble(Lat = seq(max(nutriDN$Lat), min(nutriDN$Lat), by = -0.1)),
#   # depths must be the same as in temp_interp_depth
#   tibble(Depth = unique(temp_interp_depth$Depth))
# ) %>%
#   group_by(Depth) %>%
#   mutate(NN = estimate_temp_by_depth(Depth[1], Lat))
# 
# ggplot(temp_raster, aes(Lat, Depth, fill = NN)) +
#   geom_raster() +
#   scale_y_reverse() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = FALSE)
#-----------------------

library(MBA)
library(reshape2)
library(ggplot2)
nutriDN <- nut %>% 
  dplyr::select(Latitude, Depth, Phosphate) %>% 
  rename(NutVar=Phosphate)

ctd_mba <- mba.surf(na.omit(nutriDN), no.X = 300, no.Y = 300, extend = T)
dimnames(ctd_mba$xyz.est$z) <- list(ctd_mba$xyz.est$x, ctd_mba$xyz.est$y)
ctd_mba <- melt(ctd_mba$xyz.est$z, varnames = c('Latitude', 'Depth'), value.name = 'NutVar') %>% 
  #filter(Depth < 0) %>% 
  mutate(NutVar = round(NutVar, 1))

ggplot(data = ctd_mba, aes(x = Latitude, y = Depth)) +
  geom_raster(aes(fill = NutVar)) +
  scale_fill_viridis_c(option = 'mako') +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_contour(aes(z = NutVar), binwidth = 1, colour = "black", alpha = 0.2) +
  #geom_contour(aes(z = NutVar), breaks = 20, colour = "black") +
  ### Activate to see which pixels are real and not interpolated
  geom_point(data = nutriDN, aes(x = Latitude, y = Depth),
             colour = 'black', size = 0.2, alpha = 0.4, shape = 8) +
  ###
  labs(y = "Depth (m)", x = 'Latitude', fill = "Phosphate\n(µmol/L)") +
  coord_cartesian(expand = 0) +
  ggtitle('SE2204 All stations', subtitle = 'Interpolated over depth and space just to give an idea of patterns.\nBlack dots show actual sampling locations.Used lat instead of station numbers')

ggsave('SE2204_Phosphate_wide.png', width=11, height=6)





# Flowcytometry data
cytoraw <- read_xlsx('SE2204_flowcytometry_data.xlsx', skip=19)
head(cytoraw)
cytometa <- read.csv('SE2204_flowcytometry_metadata.csv')
cytometa$Depth <- as.numeric(str_sub(cytometa$Depth, start=1, end=-2))
cytometa$SampleNo <- 1:nrow(cytometa)
cytometa <- cytometa[-28,]
meta <- left_join(cytometa, nutmeta[,c(2:6)], by=c('Station', 'Depth'))
cyto <- full_join(cytoraw, meta)

cytoDN <- cyto %>% 
  dplyr::select(Latitude, Depth, HBACT_per_mL) %>% 
  rename(NutVar=HBACT_per_mL)

cyto_mba <- mba.surf(na.omit(cytoDN), no.X = 300, no.Y = 300, extend = T)
dimnames(cyto_mba$xyz.est$z) <- list(cyto_mba$xyz.est$x, cyto_mba$xyz.est$y)
cyto_mba <- melt(cyto_mba$xyz.est$z, varnames = c('Latitude', 'Depth'), value.name = 'NutVar') %>% 
  #filter(Depth < 0) %>% 
  mutate(NutVar = round(NutVar, 1))


ggplot(data = cyto_mba, aes(x = Latitude, y = Depth)) +
  geom_raster(aes(fill = NutVar)) +
  scale_fill_viridis_c(option = 'viridis') +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_contour(aes(z = NutVar), binwidth = 1e5, colour = "black", alpha = 0.2) +
  #geom_contour(aes(z = NutVar), breaks = 20, colour = "black") +
  ### Activate to see which pixels are real and not interpolated
  geom_point(data =cytoDN, aes(x = Latitude, y = Depth),
             colour = 'black', size = 0.2, alpha = 0.4, shape = 8) +
  ###
  labs(y = "Depth (m)", x = 'Latitude', fill = "HBACT\n(no./mL)") +
  coord_cartesian(expand = 0) +
  ggtitle('SE2204 All stations', subtitle = 'Interpolated over depth and space just to give an idea of patterns.\nBlack dots show actual sampling locations.Used lat instead of station numbers')

ggsave('SE2204_flowcytometry_HBACT.png', width=8, height=8)
