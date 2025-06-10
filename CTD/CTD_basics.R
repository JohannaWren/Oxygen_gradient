# Basic CTD analysis
# from: https://semba-blog.netlify.app/09/21/2018/vertical-structure-of-temperature-salinity-and-fluorescence-of-the-pemba-channel-from-ctd-measurements/
#       https://semba-blog.netlify.app/10/05/2018/ctd-data-in-r-with-oce-and-tidyverse-package/
# Script to go through the basics of CTD processing. 

# Clear workspace
rm(list=ls())

# Load packages
require(oce)
require(ocedata)
require(sf)
require(leaflet)
require(tidyverse)
library(here)

# Set working directory
mainDir <- paste(here(), 'CTD/CTD_processed/SE2204_CTD_processed_down_cnv/', sep='/')

#--------------------------------------------
# Extract ctd data from .cnv files
#--------------------------------------------
# preallocate the container to store the individual 
ctd = list()

## Loop the CNV file and create a list of casted CTD
setwd(mainDir)
files = dir(path = ".", pattern = "dSE.+cnv", full.names = TRUE )
# Remove files for eDNA stations
#files <- files[-(c(9,11:12,14,15,17,18,20,21,23,24,26:27)+1)]
files

# read in station list from file
stns <- read.csv('../../SE2204_CTDlocations.csv')  # Change this to fit the directory your file is in
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

# Now we can use the stnInfo file to only use the filenames of the stations we want to load in the data
filesStn <- stnInfo$FileName
filesStn
files <- filesStn

# Start loop
for (i in seq_along(filesStn)){
  ctd[[i]]  = read.ctd(filesStn[i]) #%>%
    #ctdTrim(method = "downcast")%>% # this doesn't seem to work very well on our data so I saved only the downcast in Seabird
    #ctdDecimate(p = 1) # align to the same standard pressure
}
# # Removing the file with missing data until we can figure out the file itself
# ctd[[8]] <- NULL

### Make a section of CTD cast from the List
# A section allows you to plot multiple stations along a lon/lat line
section = ctd %>% 
  as.section()

# Make two part figure with map on the right and depth-lat plot on the right
par(mfrow = c(1,2))
section %>% 
  plot(which = "map", showStations = TRUE, showStart = TRUE)
section %>% 
  plot(which = "oxygen", xtype = "latitude", ztype = "image", eos = "gsw")

# For a smoother plot, gridding it is the way to go. You can also subset the section here
###subset and grid section
section.gridded = section %>%
  #subset(latitude >= 10) %>%
  sectionGrid(p = seq(0,1300,1))

# # Remove some spurious values that are intruduced when gridded
# section.gridded@data$station[[6]]@data$salinity[1:6] <- NA
# section.gridded@data$station[[6]]@data$fluorescence[1:6] <- NA

## plot the gridded section
# This makes a four part plot. It doesn't scale well when enlarged, something happens to the color guide
par(mfrow = c(4,1))

section.gridded%>%
  plot(which = "temperature", xtype = "latitude", 
       ztype = "image",
       eos = "gsw", 
       xasx=F)

section.gridded%>%
  plot(which = "salinity", xtype = "latitude", 
       ztype = "image", 
       eos = "gsw")

section.gridded%>%
  plot(which = "oxygen", xtype = "latitude", 
       ztype = "image", 
       eos = "gsw")

section.gridded%>%
  plot(which = "fluorescence", xtype = "latitude", 
       ztype = "image", 
       eos = "gsw")

## Save all CTD casts as a new list
ctdAll <- ctd

#--------------------------------------------
## Draw profles of one CTD cast
ctd <- ctdAll[[1]]

# Make tibble
ctd.tb = ctd@data %>%
  as_tibble()

ctd.tb = ctd.tb %>% 
  mutate(datetime = ctd@metadata$time, 
         lon = ctd@metadata$longitude,
         lat = ctd@metadata$latitude) %>% 
  #separate(datetime, c("date", "time"), sep = " ")%>%
  select(lon,lat,pressure,temperature, salinity, oxygen, fluorescence)

# Single station profiles  
temp = ggplot(data = ctd.tb, aes(x = temperature, y = pressure))+
  geom_path(col = "red")+
  scale_y_reverse()+
  scale_x_continuous(position = "top", breaks = seq(5,30,5))+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.grid = element_blank(),
        panel.background = element_rect(colour = "black", fill = NULL))+
  labs(y="Pressure (dbar)",
       x=expression(Temperature~(~degree~C)))

salinity = ggplot(data = ctd.tb, aes(x = salinity, y = pressure))+
  geom_path(col = "red")+
  scale_y_reverse()+
  scale_x_continuous(position = "top", breaks = seq(34,35.4,0.5))+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.grid = element_blank(),
        panel.background = element_rect(colour = "black", fill = NULL))+
  labs(y="Pressure (dbar)",
       x= "Salinity")

oxygen = ggplot(data = ctd.tb, aes(x = oxygen, y = pressure))+
  geom_path(col = "red")+
  scale_y_reverse()+
  scale_x_continuous(position = "top", breaks = seq(0,200,50))+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.grid = element_blank(),
        panel.background = element_rect(colour = "black", fill = NULL))+
  labs(y="Pressure (dbar)",
       x=expression(Oxygen~(~mlL^-1)))

fluorescence = ggplot(data = ctd.tb, aes(x = fluorescence, y = pressure))+
  geom_path(col = "red")+
  scale_y_reverse()+
  scale_x_continuous(position = "top")+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.grid = element_blank(),
        panel.background = element_rect(colour = "black", fill = NULL))+
  labs(y="Pressure (dbar)",
       x= "Fluorsecence")

# Put all the plots together
cowplot::plot_grid(temp, salinity, oxygen, fluorescence, nrow = 1)


# Draw profiles at multiple stations 
dat <- data.frame()
for (i in seq_along(ctdAll)) {
  dat <- ctdAll[[i]]@data %>% 
    as_tibble() %>% 
    mutate(Station=substring(files[i],13,14)) %>% 
    add_row(dat)
}
#dat[which(dat$Station == '06' & dat$pressure <= 3),4:8] <- NA   # this removes spurious values from the top three meters of station 6
# I'm planning ot use ggplot for this so I'm converting from wide to long format
datN <- dat[,c(15, 9, 8, 11, 4, 7, 10, 12, 13)]   # remove unwanted data columns
datN <- gather(datN, key='Variable', value = 'value', -Station:-depth)

# This adds the latitude of each station as an alternative station name. I find it useful for plotting
idx <- data.frame()
for (i in seq_along(files)) { 
  lat <- round(mean(ctdAll[[i]]@data$latitude, na.rm=T),1)
  idx <- rbind(idx, cbind(substring(files[i],13,14), lat)) 
}
colnames(idx) <- c('Station', 'Latitude')
#idx <- idx[c(-10:-11, -13),]
idx$Latitude <- paste0(idx$Latitude,'°')
# Join the latitude column with the CTD dataset
dat2 <- left_join(datN, idx)

# Make a series of depth profiles that are organized from north to south along the transect
# The extended stations will have two profiles in the same figure since they are on roughly the same latitude
dat2 %>% 
  filter(Variable == 'oxygen') %>% 
  ggplot(aes(value, depth, group=Station)) + 
  geom_path() + 
  scale_x_continuous(breaks=c(0,100,200)) +
  scale_y_reverse() + 
  theme_bw() + 
  theme(panel.grid = element_blank(), strip.background = element_blank()) +
  facet_grid(.~Latitude) +
  labs(y="Depth (m)",
       x='Oxygen (ml/l)')
  

#--------------------------------------------
## Make color lat-depth plot like the ones above but with ggplot instead
# Make tibble
ctd.tb.all = dat%>% na.omit()

# Make color palette
# It's a slightly more muted version of 'jetcolors' from MATLAB, and a bit more vibrant than R's 'Spectral' 
myCol <- rev(c('#EE1B27', '#F54C20', '#F2E004', '#60BB49', '#2EB5E8', '#3851A4'))

vars <- c('temperature', 'salinity', 'oxygen', 'fluorescence')
varsUnit <- c('Temperature [C]', 'Salinity', 'Dissolved Oxygen [mL/L]', 'Total Chlorophyll [mg/m3]')
colVar <- list(myCol, myCol, c('#F7FBFF', '#08519C'))

plots <- list()
for (i in seq_along(vars)) {
  idx <- which(names(ctd.tb.all)==vars[i])
  # Interpolate data between station along the latitude gradient. This is essentially the same thing 'gradient.gridded' does further up in this script
  temp.interp = akima::interp(x = ctd.tb.all$latitude, 
                              y = ctd.tb.all$pressure, 
                              z = unlist(ctd.tb.all[,idx]),
                              duplicate = "mean", nx = 500, ny = 200)
  
  # Put it in a ggplot friendly format and limit it to the top 1,000 meters
  # Choose which variable you want to plot
  temp.interp = akima::interp2xyz(temp.interp) %>%
    as.tibble() %>%
    rename(latitude = x, pressure = y, variable = z) %>%
    na.omit() %>%
    filter(pressure <=1000 & pressure > 4)
  
  if(i < 4) {
    temp <- ggplot(data = temp.interp, aes(x = latitude, y = pressure)) +
      geom_raster(aes(fill = variable), interpolate = TRUE) +
      geom_segment(data=data.frame(lat=unique(round(ctd.tb.all$latitude, 1))), aes(x = lat, y = 5, xend = lat, yend = 25)) +
      scale_fill_gradientn(colors=colVar[[i]]) +
      scale_y_reverse(expand = c(0, 5), breaks=seq(0,1000,200), labels=seq(0,1000,200)) +
      scale_x_continuous(expand = c(0.001, 0.001), breaks=seq(10,30,2)) +
      theme_bw() +
      theme(legend.key.height = unit(1.7, "lines"),
            legend.title = element_text(size = 10, angle = 90),
            legend.title.align = 0.5,
            legend.direction = "vertical",
            legend.justification = c(0,0.5),
            axis.text = element_text(colour = 1, size = 10),
            axis.title = element_text(colour = 1, size = 11),
            panel.grid = element_blank(), 
            axis.title.x = element_blank(), 
            axis.text.x = element_blank()) +
      guides(fill=guide_colorbar(title.position = 'right', title = varsUnit[i])) +
      labs(x = "Latitude", y = "Pressure [dba]")
  } else {
    temp <- ggplot(data = temp.interp, aes(x = latitude, y = pressure)) +
      geom_raster(aes(fill = variable), interpolate = TRUE) +
      geom_segment(data=data.frame(lat=unique(round(ctd.tb.all$latitude, 1))), aes(x = lat, y = 5, xend = lat, yend = 9)) +
      scale_fill_distiller(palette='Greens', direction=1) +
      scale_y_reverse(expand = c(0, 0), breaks=seq(0,200,50), labels=seq(0,200,50), lim=c(200,0)) +
      scale_x_continuous(expand = c(0.001, 0.001), breaks=seq(10,30,2)) +
      theme_bw() +
      theme(legend.key.height = unit(1.6, "lines"),
            legend.title = element_text(size = 10, angle = 90),
            legend.title.align = 0.5,
            legend.direction = "vertical",
            legend.justification = c(0,0.5),
            axis.text = element_text(colour = 1, size = 10),
            axis.title = element_text(colour = 1, size = 11),
            panel.grid = element_blank()) +
      guides(fill=guide_colorbar(title.position = 'right', title = varsUnit[i])) +
      labs(x = "Latitude", y = "Pressure [dba]")
  }
  
  
  plots[[i]] <- temp
}

cowplot::plot_grid(plots[[1]], plots[[2]], plots[[3]], plots[[4]], ncol=1, rel_heights = c(1,1,1,1.15), align = 'v')
ggsave('testPlots.png', width=10, height=8, units='in')



# # Make the plot
# 
# 
# 
# ###########
# # Add data to image
# png('testPlot.png', width=10, height = 8, res=300, unit='in')
# plot(xlim=c(-0.01,0.04), ylim=c(-1200,0), ctd.tb$pressure*-1, type='n', main="", xlab="Temperature and Oxygen", ylab="Pressure", axes=F)
# axis(side=3)
# lim <- par()
# rasterImage(img, xleft=-5, xright=30, ybottom=-1200, ytop=0)
# grid()
# lines(y=ctd.tb$pressure*-1, x=ctd.tb$fluorescence, lwd=1, col='green')
# par(new=T)
# plot(y=ctd.tb$pressure*-1, x=ctd.tb$temperature, lwd=2, col='red', type='l', axes=F, xlab = "", ylab = "", xlim=c(0,26))
# lines(y=ctd.tb$pressure*-1, x=ctd.tb$oxygen, lwd=2, col='blue')
# axis(side = 1)      # Add second axis
# mtext("Fluorescence", side = 3, line = 3) 
# box()
# axis(side=2)
# dev.off()
# 
# png('testPlot.png', width=10, height = 8, res=300, unit='in')
# plot(xlim=c(0,26), ylim=c(-1200, 0), ctd.tb$pressure*-1, type='n', main="", xlab="Temperature & Oxygen", ylab="Depth")
# lim <- par()
# rasterImage(img, xleft=0, xright=26, ybottom=-1200, ytop=0)
# grid()
# lines(y=ctd.tb$pressure*-1, x=ctd.tb$temperature, lwd=2, col='red')
# lines(y=ctd.tb$pressure*-1, x=ctd.tb$oxygen, lwd=2, col='blue')
# dev.off()
# 
# 
# plot(xlim=c(0,26), ylim=c(-1200, 0), ctd.tb$pressure*-1, type='n', main="", xlab="Temperature & Oxygen", ylab="Depth")
# lim <- par()
# rasterImage(img, xleft=0, xright=26, ybottom=-1200, ytop=0)
# grid()
# lines(y=ctd.tb$pressure*-1, x=ctd.tb$temperature, lwd=2, col='red')
# lines(y=ctd.tb$pressure*-1, x=ctd.tb$oxygen, lwd=2, col='blue')
# 
# 
# ######
# #Check bottle depths
# files = dir(path = "bottles/", pattern = "bottlesFired.cnv", full.names = TRUE )
# y <- data.frame()
# for (i in 1:length(files)) {
#   ctd <- read.ctd(files[i]) %>% na.omit()
#   ctd.tb <- as.tibble(ctd@data) %>% mutate(Station=substring(files[i],19,22), DateTime=as.POSIXct('2000-01-01')+timeQ)
#   for (j in 1:12) { 
#     xmax <- ctd.tb %>% 
#       filter(bottlesFired == j) %>% 
#       select(timeQ, depth, latitude, longitude, temperature, salinity, oxygen) %>% 
#       filter(depth == max(depth)) %>% 
#       filter(timeQ == min(timeQ)) %>%
#       filter(row_number()==1) %>%
#       mutate(DateTimeGMT=as.POSIXct('2000-01-01', tz='GMT') + timeQ, DateTimeHST=with_tz(DateTimeGMT, 'HST'))
#     if (nrow(xmax) <1) { break}
#     xmax$station <- substring(files[i],19,22)
#     y=rbind(y, cbind(bottle=j, xmax)) 
#   }
# }
# y
# write.csv(y, 'bottlesFired_forOA.csv', quote=F, row.names = F)
# 
# ###
# # And ggplot
# library(grid)
# ggplot(data=ctd.tb, aes(temperature, pressure*-1)) + 
#   annotation_custom(rasterGrob(img, width = unit(1,"npc"), 
#                                height = unit(1,"npc")), 0, 26, -1200, 0) + 
#   geom_line()
