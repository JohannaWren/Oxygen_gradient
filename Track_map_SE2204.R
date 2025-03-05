# Quick map with temp and track line for SE2004 cruise

# read libraries
library(raster)
library(rasterVis)
library(mapdata)
library(maptools)
library(cmocean)
library(dplyr)
library(RColorBrewer)
library(rerddap)
library(latticeExtra)
library(grid)

# set working directory
setwd('~/Documents/Cruises/BEToceanography/SE2204 - Hot Spot/')

# download land data and save as spatial polygon in 360 degrees
land <- map('world2', fill=TRUE, xlim=c(190,230), ylim=c(0,40), plot=FALSE)
ids <- sapply(strsplit(land$names, ":"), function(x) x[1])
bPols <- map2SpatialPolygons(land, IDs=ids, proj4string=CRS("+proj=longlat +datum=WGS84"))

# download land data and save as spatial polygon in -180/180 degrees
land2 <- map('world', fill=TRUE, xlim=c(-170,-130), ylim=c(0,40), plot=FALSE)
ids2 <- sapply(strsplit(land$names, ":"), function(x) x[1])
bPols2 <- map2SpatialPolygons(land2, IDs=ids2, proj4string=CRS("+proj=longlat +datum=WGS84"))

# define locations and track lines
track <- data.frame(Lon=c(202.5, 220, 220, 204, 202), Lat=c(21.3, 30, 10, 18.5, 21.3))
track180 <- track %>% mutate(Lon=Lon-360)
all_wpts <- read.csv('SE2204_wpt6_145.csv')
wpts <- all_wpts[,1:2]
wpts$lon <- wpts$lon+360
wpts_ext <- all_wpts %>% filter(stn_type == 'extended') %>% dplyr::select(lon, lat)
wpts_base <- all_wpts %>% filter(stn_type == 'base') %>% dplyr::select(lon, lat)
wpts_base360 <- mutate(wpts_base, lon=lon+360)
wpts_ext360 <- mutate(wpts_ext, lon=lon+360)

# define map theme (color scheme)
mapThemeTemp <- rasterTheme(region=rev(brewer.pal(11, 'Spectral')))
mapThemeSSH <- rasterTheme(region=rev(brewer.pal(11, 'RdBu')))
mapThemeChl <- rasterTheme(region=cmocean('algae')(50))
mapThemeOxy <- rasterTheme(region=cmocean('amp')(50))

# Define domain
lat1 <- 0
lat2 <- 40
lon1 <- 190
lon2 <- 230
time1 <- '2012-06-1'
time2 <- '2021-06-01'

#------------------------------------------------
## Sea Surface Temperature
sstInfo <- info(url = 'https://oceanwatch.pifsc.noaa.gov/erddap/', datasetid = 'CRW_sst_v3_1_monthly')
sstDat <- griddap(sstInfo, time = c(time1,time2), latitude = c(lat1, lat2), longitude = c(lon1, lon2), 
                  fields = sstInfo$variables$variable_name,
                  stride = c(12,1,1), 
                  store=disk('BET_test'))
sstAll <- brick('BET_SST/BET_SST.nc')
# average SST
sst <- mean(sstAll)

# Make map
sstPlot <- levelplot(sst, margin=FALSE, pretty=T, par.setting=mapThemeTemp, at=seq(11,30,0.25), main='Mean SST June 2011-2021', 
                     colorkey=list(title='SST (deg C)', title.gpar=list(cex=0))) + 
                layer(sp.polygons(bPols, fill='gray50', col='gray30')) +
                layer(panel.lines(track, col='gray', lwd=2)) +
                layer(panel.lines(wpts, col='black', lwd=2)) +
                layer(panel.points(wpts_ext360, col='#56B4E9', lwd=2)) +
                layer(panel.points(wpts_base360, col='#E69f00', cex=1)) +
                contourplot(sst, pretty=T, margin=F, at=seq(24,28,2))

#------------------------------------------------
## Temperature at Depth - WOA surface
tempInfo <- info(url = 'http://apdrc.soest.hawaii.edu/erddap/', datasetid = 'hawaii_soest_2350_b70f_4040')
tempDat <- griddap(tempInfo, time=c('0000-06-15', '0000-06-15'), LEV=c(0, 0), latitude = c(lat1, lat2), longitude = c(lon1-360, lon2-360), 
                  fields = tempInfo$variables$variable_name[1],
                  store=disk('BET_Temp0'))
temp<- raster('BET_Temp0/83e4f02e5e958f21a06e889eed5b198b.nc')
# Make map
tempPlot0 <- levelplot(temp, pretty=T, margin=FALSE, at=seq(17,30,0.15), par.setting=mapThemeTemp, main='Mean Temperature at surface June WOA18', 
                     colorkey=list(title='Temp at 0m (degC)', title.gpar=list(cex=0))) + 
  layer(sp.polygons(bPols2, fill='gray50', col='gray30')) +
  layer(panel.lines(track180, col='gray', lwd=2)) +
  layer(panel.lines(all_wpts[,1:2], col='black', lwd=2)) +
  layer(panel.points(wpts_ext, col='#56B4E9', cex=1)) +
  layer(panel.points(wpts_base, col='#E69f00', cex=1)) 
  
#------------------------------------------------
## Temperature at Depth - WOA 100m
tempInfo <- info(url = 'http://apdrc.soest.hawaii.edu/erddap/', datasetid = 'hawaii_soest_2350_b70f_4040')
tempDat <- griddap(tempInfo, time=c('0000-06-15', '0000-06-15'), LEV=c(100, 100), latitude = c(lat1, lat2), longitude = c(lon1-360, lon2-360), 
                   fields = tempInfo$variables$variable_name[1],
                   store=disk('BET_Temp10'))
temp<- raster('BET_Temp10/7a0b21905afa7bfb37487b3516ded587.nc')
# Make map
tempPlot100 <- levelplot(temp, pretty=T, margin=FALSE, at=seq(10,27.5,0.25), par.setting=mapThemeTemp, main='Mean Temperature at 100m June WOA18', 
                      colorkey=list(title='Temp at 100m (degC)', title.gpar=list(cex=0))) + 
  layer(sp.polygons(bPols2, fill='gray50', col='gray30')) +
  layer(panel.lines(track180, col='gray', lwd=2)) +
  layer(panel.lines(all_wpts[,1:2], col='black', lwd=2)) +
  layer(panel.points(wpts_ext, col='#56B4E9', cex=1)) +
  layer(panel.points(wpts_base, col='#E69f00', cex=1)) 
  
  
#------------------------------------------------
## Temperature at Depth - WOA 250m
tempInfo <- info(url = 'http://apdrc.soest.hawaii.edu/erddap/', datasetid = 'hawaii_soest_2350_b70f_4040')
tempDat <- griddap(tempInfo, time=c('0000-06-15', '0000-06-15'), LEV=c(250, 250), latitude = c(lat1, lat2), longitude = c(lon1-360, lon2-360), 
                   fields = tempInfo$variables$variable_name[1],
                   store=disk('BET_Temp25'))
temp <- raster('BET_Temp25/5d7105be2f00ff83998adb0575b4a594.nc')
# Make map
tempPlot250 <- levelplot(temp, pretty=T, margin=FALSE, at=seq(7.3,15.5,0.15), par.setting=mapThemeTemp, main='Mean Temperature at 250m June WOA18', 
                      colorkey=list(title='Temp at 250m (degC)', title.gpar=list(cex=0))) + 
  layer(sp.polygons(bPols2, fill='gray50', col='gray30')) +
  layer(panel.lines(track180, col='gray', lwd=2)) +
  layer(panel.lines(all_wpts[,1:2], col='black', lwd=2)) +
  layer(panel.points(wpts_ext, col='#56B4E9', cex=1)) +
  layer(panel.points(wpts_base, col='#E69f00', cex=1))


#------------------------------------------------
## Temperature at Depth - WOA 400m
tempInfo <- info(url = 'http://apdrc.soest.hawaii.edu/erddap/', datasetid = 'hawaii_soest_2350_b70f_4040')
tempDat <- griddap(tempInfo, time=c('0000-06-15', '0000-06-15'), LEV=c(400, 400), latitude = c(lat1, lat2), longitude = c(lon1-360, lon2-360), 
                   fields = tempInfo$variables$variable_name[1],
                   store=disk('BET_Temp40'))
temp <- raster('BET_Temp40/f5cc8b1892aaaebc63664fcdc63e90ba.nc')
# Make map
tempPlot400 <- levelplot(temp, pretty=T, margin=FALSE, at=seq(5.8,11,0.075), par.setting=mapThemeTemp, main='Mean Temperature at 400m June WOA18', 
                      colorkey=list(title='Temp at 450m (degC)', title.gpar=list(cex=0))) + 
  layer(sp.polygons(bPols2, fill='gray50', col='gray30')) +
  layer(panel.lines(track180, col='gray', lwd=2)) +
  layer(panel.lines(all_wpts[,1:2], col='black', lwd=2)) +
  layer(panel.points(wpts_ext, col='#56B4E9', cex=1)) +
  layer(panel.points(wpts_base, col='#E69f00', cex=1))

#------------------------------------------------
## Chlorophyll 
chlInfo <- info(url = 'https://oceanwatch.pifsc.noaa.gov/erddap/', datasetid = 'noaa_snpp_chla_monthly')
chlDat <- griddap(chlInfo, time = c(time1,time2), latitude = c(lat1, lat2), longitude = c(lon1, lon2), 
                  #fields = chlInfo$variables$variable_name,
                  stride = c(12,1,1), 
                  store=disk('BET_Chl'))
chlAll <- brick('BET_Chl/09e9d8d25b54045c978631a74bc116c5.nc')
# average Chl
chl <- mean(chlAll)  #stackApply(brick('BET_Chl.nc'), 3, mean)

# Make map
chlPlot <- levelplot(chl, zscaleLog=T, pretty=T, margin=FALSE, par.setting=mapThemeChl, main='Mean Chl June 2012-2021', 
                     colorkey=list(title='Chl a (mg m-3)', title.gpar=list(cex=0))) + 
                layer(sp.polygons(bPols, fill='gray50', col='gray30')) +
                layer(panel.lines(track, col='gray', lwd=2)) +
                layer(panel.lines(wpts, col='black', lwd=2)) +
                layer(panel.points(wpts_ext360, col='#56B4E9', cex=1)) +
                layer(panel.points(wpts_base360, col='#E69f00', cex=1))

#------------------------------------------------
## Oxygen
oxyInfo <- info(url = 'http://apdrc.soest.hawaii.edu/erddap/', datasetid = 'hawaii_soest_d1b1_3f6d_8024')
oxyDat <- griddap(oxyInfo, time=c('0000-06-15', '0000-06-15'), LEV=c(350, 350), latitude = c(lat1, lat2), longitude = c(lon1-360, lon2-360), 
                  fields = oxyInfo$variables$variable_name[1],
                  store=disk('BET_Oxy'))
oxy <- raster('BET_Oxy/307c994d0311286c02c17951e47c25b7.nc')
oxyL <- (oxy*0.022391)*.98  # convert from micro mole/kg to mL/L
# Make map
oxyPlot <- levelplot(oxyL, pretty=T, margin=FALSE, at=seq(0,5,0.1), par.setting=mapThemeOxy, main='Mean Oxygen 350m June WOA18', 
                     colorkey=list(title='Dissolved O2 (mL/L)', title.gpar=list(cex=0))) + 
  layer(sp.polygons(bPols2, fill='gray50', col='gray30')) +
  layer(panel.lines(track180, col='gray', lwd=2)) +
  layer(panel.lines(all_wpts[,1:2], col='black', lwd=2)) +
  layer(panel.points(wpts_ext, col='#56B4E9', cex=1)) +
  layer(panel.points(wpts_base, col='#E69f00', cex=1)) +
  contourplot(oxyL, pretty=T, margin=F, at=0:3)

#-------------------------
## Oxygen along 145 West
oxyDat <- griddap(oxyInfo, time=c('0000-06-15', '0000-06-15'), LEV=c(0,1000), latitude = c(lat1, lat2), longitude = c(-145, -145), 
                  fields = oxyInfo$variables$variable_name[1],
                  store=disk('BET_Oxy'))
#oxy <- raster('BET_Oxy/fc180688e36497dc2b1fe4090ff15bf0.nc')
oxyDat$data$oxy <- (oxyDat$data$oan*0.022391)*.98  # convert from micro mole/kg to mL/L

oxy145 <- ggplot() + 
  geom_contour_filled(data=oxyDat$data, aes(x=lat, y=LEV, z=oxy), binwidth = 0.20) + 
  geom_contour(data=oxyDat$data, aes(x=lat, y=LEV, z=oxy), breaks = 1.5, color='white') +
  coord_cartesian(expand=F) + 
  scale_y_reverse(breaks=seq(0,1000,100)) + 
  xlim(c(10,30)) +
  xlab('Latitude') + 
  ylab('Depth (m)') + 
  ggtitle('Oxygen level at 145W') +
  theme_minimal()


oxyDat <- griddap(oxyInfo, time=c('0000-06-15', '0000-06-15'), LEV=c(0,1000), latitude = c(lat1, lat2), longitude = c(-140, -140), 
                  fields = oxyInfo$variables$variable_name[1],
                  store=disk('BET_Oxy'))
#oxy <- raster('BET_Oxy/fc180688e36497dc2b1fe4090ff15bf0.nc')
oxyDat$data$oxy <- (oxyDat$data$oan*0.022391)*.98  # convert from micro mole/kg to mL/L

oxy140 <- ggplot() + 
  geom_contour_filled(data=oxyDat$data, aes(x=lat, y=LEV, z=oxy), binwidth = 0.20) + 
  geom_contour(data=oxyDat$data, aes(x=lat, y=LEV, z=oxy), breaks = 1.5, color='white') +
  coord_cartesian(expand=F) + 
  scale_y_reverse(breaks=seq(0,1000,100)) + 
  xlim(c(10,30)) +
  xlab('Latitude') + 
  ylab('Depth (m)') + 
  ggtitle('Oxygen level at 140W') +
  theme_minimal()

  #------------------------------------------------
# Save plots
png('EnviroGraphsTemp_145.png', width=11, height=16, units ='in', res=300) 
#gridExtra::grid.arrange(sstPlot, tempPlot, chlPlot, oxyPlot, ncol=2) 
gridExtra::grid.arrange(tempPlot0, tempPlot100, tempPlot250, tempPlot400, chlPlot, oxyPlot, ncol=2) 
dev.off()

#------------------------------------------------
# Bathymetry
library(marmap)
bathy <- getNOAA.bathy(lon1-360, lon2-360, lat1, lat2, resolution=1, keep=TRUE)

# Turn the bathymetry into a raster for plotting
bathyRaster <- marmap::as.raster(bathy)
# Constrain the raster to ocean only
bathyRaster[bathyRaster > 0] <- NA
extent(bathyRaster) <- c(extent(bathyRaster)[1]+360, extent(bathyRaster)[2]+360, extent(bathyRaster)[3], extent(bathyRaster)[4])

# Make map
png('SE22004_NEtrack_Bathy.png', width=6, height=6, res=300, units='in')
levelplot(bathyRaster, margin=FALSE, par.setting=rasterTheme(region=rev(cmocean('deep')(11))),
  main="SE-22-04 trackline") +
  layer(sp.polygons(bPols, fill='gray50', col='gray30')) +
  layer(panel.lines(track, col='black', lwd=2)) 
trellis.focus("legend", side="right", clipp.off=TRUE, highlight=FALSE)
grid.text('Depth \n(m)', 0.3, -0.025, hjust=0.6, vjust=1, gp=gpar(fontsize=9))
trellis.unfocus()
dev.off()

# Track only
Lab <- cbind(rbind(cobb_pts, ring_pts), Station=letters[1:8])
blank <- crop(o2mean350, extent(200, 223,7,33))
values(blank) <- NA
png('SE2006_NEtrack.png', width=5, height=6, res=200, units='in')
levelplot(blank, labels=T, pretty=T, margin=FALSE, par.setting=mapThemeOxy,
          main="SE-20-06 trackline", colorkey=FALSE) +
  layer(sp.polygons(bPols, fill='gray50', col='gray30')) +
  layer(panel.lines(track, col='black')) +
  layer(panel.lines(track2, col='red')) +
  layer(panel.points(cobb_pts, col='black', pch=19, cex=1), label=letters[1:6]) +
  layer(panel.points(ring_pts, col='red', pch=19, cex=1))
dev.off()


