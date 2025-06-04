# Basic CTD analysis
# from: https://semba-blog.netlify.app/09/21/2018/vertical-structure-of-temperature-salinity-and-fluorescence-of-the-pemba-channel-from-ctd-measurements/
#       https://semba-blog.netlify.app/10/05/2018/ctd-data-in-r-with-oce-and-tidyverse-package/

require(oce)
require(ocedata)
require(sf)
require(leaflet)
require(tidyverse)

## preallocate the container to store the individual 
ctd = list()

### Loop the CNV file and create a list of casted CTD
for (i in 1:length(files)){
  
  ctd[[i]]  = read.ctd(files[i])%>%
    ctdTrim(method = "downcast")%>% # select downcast
    ctdDecimate(p = 1) # align to the same standard pressure
}

### Make a section of CTD cast from the List
section = ctd%>%as.section()

par(mfrow = c(1,2))
section%>%plot(which = "map", showStations = TRUE, showStart = TRUE)
section%>%plot(which = "temperature", xtype = "latitude", ztype = "image", eos = "gsw")

###subset and grid section
section.gridded = section%>%
  subset(latitude >= 25)%>%
  sectionGrid(p = seq(0,500,1))

## plot the gridded section

par(mfrow = c(2,2))

section.gridded%>%
  plot(which = "temperature", xtype = "latitude", 
       ztype = "image",
       eos = "gsw")

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


# Make tibble
ctd.tb = ctd@data%>%
  as_data_frame()

ctd.tb = ctd.tb %>% 
  mutate(datetime = ctd@metadata$time, 
         lon = ctd@metadata$longitude,
         lat = ctd@metadata$latitude) %>% 
  separate(datetime, c("date", "time"), sep = " ")%>%
  select(date, time, lon,lat,pressure,temperature, salinity, oxygen, fluorescence)


###########
# Add data to inmage
png('testPlot.png', width=10, height = 8, res=300, unit='in')
plot(xlim=c(-0.01,0.04), ylim=c(-1200,0), ctd.tb$pressure*-1, type='n', main="", xlab="Temperature and Oxygen", ylab="Pressure", axes=F)
axis(side=3)
lim <- par()
rasterImage(img, xleft=-5, xright=30, ybottom=-1200, ytop=0)
grid()
lines(y=ctd.tb$pressure*-1, x=ctd.tb$fluorescence, lwd=1, col='green')
par(new=T)
plot(y=ctd.tb$pressure*-1, x=ctd.tb$temperature, lwd=2, col='red', type='l', axes=F, xlab = "", ylab = "", xlim=c(0,26))
lines(y=ctd.tb$pressure*-1, x=ctd.tb$oxygen, lwd=2, col='blue')
axis(side = 1)      # Add second axis
mtext("Fluorescence", side = 3, line = 3) 
box()
axis(side=2)
dev.off()

png('testPlot.png', width=10, height = 8, res=300, unit='in')
plot(xlim=c(0,26), ylim=c(-1200, 0), ctd.tb$pressure*-1, type='n', main="", xlab="Temperature & Oxygen", ylab="Depth")
lim <- par()
rasterImage(img, xleft=0, xright=26, ybottom=-1200, ytop=0)
grid()
lines(y=ctd.tb$pressure*-1, x=ctd.tb$temperature, lwd=2, col='red')
lines(y=ctd.tb$pressure*-1, x=ctd.tb$oxygen, lwd=2, col='blue')
dev.off()


plot(xlim=c(0,26), ylim=c(-1200, 0), ctd.tb$pressure*-1, type='n', main="", xlab="Temperature & Oxygen", ylab="Depth")
lim <- par()
rasterImage(img, xleft=0, xright=26, ybottom=-1200, ytop=0)
grid()
lines(y=ctd.tb$pressure*-1, x=ctd.tb$temperature, lwd=2, col='red')
lines(y=ctd.tb$pressure*-1, x=ctd.tb$oxygen, lwd=2, col='blue')


###
# And ggplot
ggplot(data=ctd.tb, aes(temperature, pressure*-1)) + 
  annotation_custom(rasterGrob(img, width = unit(1,"npc"), 
                               height = unit(1,"npc")), 0, 26, -1200, 0) + 
  geom_line()
