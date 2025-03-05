# Quick map with temp and track line for SE2004 cruise

# read libraries
library(geosphere)
library(dplyr)
library(ggplot2)
library(lubridate)

# set working directory
setwd('~/Documents/Cruises/BEToceanography/SE2204 - Hot Spot/')

# define locations and track lines  21.317604, -157.967737
track <- data.frame(Lon=c(202.0323, 215, 215, 204, 202.0323), Lat=c(21.3176, 30, 10, 19, 21.3176))
track180 <- track %>% mutate(Lon=Lon-360)

# Adding ARGO float drop locations

argo <- data.frame(lon=c(-154,-151,-149.3124,-147,-145,-140), lat=c(23.4,25,25.93835,27,30,19))

## Calculate time and distance for different station scenarios
## Based on ship speed of 9 knots

# Distances to 10N
distsM10 <- distGeo(track)[1:4]
distsNM10 <- ceiling(distsM10/1852)
distDay10 <- (distsNM10/9)/24

# LEG 1 Lat and lon of each waypoint along track (sampling 10am and 10pm)
leg1_bearing <- bearing(track180[1,], track180[2,])
leg1_qtr <- destPoint(track180[1,], leg1_bearing, (distsM10[1]*c(0.25,0.50,0.75)))
leg1_wpts <- data.frame(lon=c(track180[1,1], argo[1:2,1], leg1_qtr[2,1], argo[4:5,1], track180[2,1]), 
                        lat=c(track180[1,2], argo[1:2,2], leg1_qtr[2,2], argo[4:5,1], track180[2,2]), 
                        samp_hour=c(NA,NA,NA, NA, NA, NA, NA), 
                        samp_dur=c(0, 1, 1, 24, 1, 1, 24), 
                        dist_to_here_NM=c(0, NA, NA, NA, NA, NA, NA),
                        dateS=c(as.POSIXct('2022-06-14 08:00'), NA, NA, NA, NA, NA, NA),
                        dateA=c(as.POSIXct('2022-06-14 08:00'), NA, NA, NA, NA, NA, NA),
                        dateD=c(as.POSIXct('2022-06-14 08:00'), NA, NA, NA, NA, NA, NA))
rownames(leg1_wpts) <- NULL
leg1_wpts <- leg1_wpts[c(1,4,7),]

for (i in 1:(nrow(leg1_wpts)-1)) {
  leg1_wpts$dist_to_here_NM[i+1] <- round(distGeo(leg1_wpts[i, 1:2], leg1_wpts[(i+1),1:2])/1852)
  da2 <- (leg1_wpts$dist_to_here_NM[i+1]/8.5*60*60) + leg1_wpts$dateD[i]
  leg1_wpts$dateS[i+1] <- da2
  leg1_wpts$dateA[i+1] <- ymd_h(ifelse(da2 <= ymd_h(paste0('2022-',month(da2), '-', day(da2), ' ', 8),tz = 'HST'), paste0('2022-',month(da2), '-', day(da2), ' ', 8), 
                                       ifelse(da2 > ymd_h(paste0('2022-',month(da2), '-', day(da2), ' ', 20),tz = 'HST'), paste0('2022-',month(da2), '-', day(da2)+1, ' ', 8), paste0('2022-',month(da2), '-', day(da2), ' ', 20))), tz='HST')
  leg1_wpts$dateD[i+1] <- ymd_h(ifelse(leg1_wpts$dateA[i+1] <= ymd_h(paste0('2022-',month(leg1_wpts$dateA[i+1]), '-', day(leg1_wpts$dateA[i+1]), ' ', 8),tz = 'HST'), paste0('2022-',month(leg1_wpts$dateA[i+1]), '-', day(leg1_wpts$dateA[i+1]), ' ', 8), paste0('2022-',month(leg1_wpts$dateA[i+1]), '-', day(leg1_wpts$dateA[i+1]), ' ', 20)), tz='HST') + (leg1_wpts$samp_dur[i+1]*60*60)
  leg1_wpts$samp_hour[i+1] <- hour(leg1_wpts$dateD[i+1] - (leg1_wpts$samp_dur[i+1]*60*60))
}
#leg1_wpts$dist_to_here_NM[2] <- round(distGeo(track180[1,], leg1_qtr[2,])/1852)
# leg1_wpts$dateA[2] <- (leg1_wpts$dist_to_here_NM[2]/9*60*60) + leg1_wpts$dateD[1]
# da2 <- (leg1_wpts$dist_to_here_NM[2]/9*60*60) + leg1_wpts$dateD[1]
# leg1_wpts$dateA[2] <- ymd_h(ifelse(da2 <= ymd_h(paste0('2022-',month(da2), '-', day(da2), ' ', 10),tz = 'HST'), paste0('2022-',month(da2), '-', day(da2), ' ', 10), 
#                                    ifelse(da2 > ymd_h(paste0('2022-',month(da2), '-', day(da2), ' ', 22),tz = 'HST'), paste0('2022-',month(da2), '-', day(da2)+1, ' ', 10), paste0('2022-',month(da2), '-', day(da2), ' ', 22))), tz='HST')
# leg1_wpts$dateD[2] <- ymd_h(ifelse(leg1_wpts$dateA[2] <= ymd_h(paste0('2022-',month(leg1_wpts$dateA[2]), '-', day(leg1_wpts$dateA[2]), ' ', 10),tz = 'HST'), paste0('2022-',month(leg1_wpts$dateA[2]), '-', day(leg1_wpts$dateA[2]), ' ', 10), paste0('2022-',month(leg1_wpts$dateA[2]), '-', day(leg1_wpts$dateA[2]), ' ', 22)), tz='HST') + (leg1_wpts$samp_dur[2]*60*60)
# leg1_wpts$samp_hour[2] <- hour(leg1_wpts$dateD[2] - (leg1_wpts$samp_dur[2]*60*60))
# 
# leg1_wpts$dist_to_here_NM[3] <- round(distGeo(leg1_qtr[2,], track180[2,])/1852)
# da3 <- (leg1_wpts$dist_to_here_NM[3]/9*60*60) + leg1_wpts$dateD[2]
# leg1_wpts$dateA[3] <- ymd_h(ifelse(da3 <= ymd_h(paste0('2022-',month(da3), '-', day(da3), ' ', 10),tz = 'HST'), paste0('2022-',month(da3), '-', day(da3), ' ', 10), 
#                                    ifelse(da3 > ymd_h(paste0('2022-',month(da3), '-', day(da3), ' ', 22),tz = 'HST'), paste0('2022-',month(da3), '-', day(da3)+1, ' ', 10), paste0('2022-',month(da3), '-', day(da3), ' ', 22))), tz='HST')
# leg1_wpts$dateD[3] <- ymd_h(ifelse(leg1_wpts$dateA[3] <= ymd_h(paste0('2022-',month(leg1_wpts$dateA[3]), '-', day(leg1_wpts$dateA[3]), ' ', 10),tz = 'HST'), paste0('2022-',month(leg1_wpts$dateA[3]), '-', day(leg1_wpts$dateA[3]), ' ', 10), paste0('2022-',month(leg1_wpts$dateA[3]), '-', day(leg1_wpts$dateA[3]), ' ', 22)), tz='HST') + (leg1_wpts$samp_dur[3]*60*60)
# leg1_wpts$samp_hour[3] <- hour(leg1_wpts$dateD[3] - (leg1_wpts$samp_dur[3]*60*60))
leg1_wpts$dateA[1] <- NA
leg1_wpts

# LEG 2 Lat and lon of each waypoint
n_pts <- 21
#dur <- c(21,3,3,3,3,21,3,3,3,3,21,3,3,3,3,21,3,3,3,3,21)
dur <- c(rep(c(24, rep(5, 4)),4),24)
#dur <- c(21,3,3,3,21,3,3,3,21,3,3,3,21,3,3,3,21,3,3,3,21)
#dur <- c(rep(c(29, rep(5, 7)),5),29)
leg2_wpts <- data.frame(lon=track180[2,1], lat=track180[2,2], samp_hour=20, samp_dur=dur[1], 
                        dist_to_here_NM=distGeo(leg1_wpts[14,1:2], track180[2,])/1852, dateS=leg1_wpts$dateA[nrow(leg1_wpts)],
                        dateA=leg1_wpts$dateA[nrow(leg1_wpts)], dateD=leg1_wpts$dateD[nrow(leg1_wpts)])
leg2_wpts
for (i in 1:(n_pts-1)) {
  yesterday <- leg2_wpts[i,1:2]
  smpl_dur_ystrd <- leg2_wpts$samp_dur[i]
  smpl_dur_today <- dur[i+1]
  
  today <- yesterday
  today$lat <- today$lat-1
  
  dist_ystrd_today <- 60
  
  dat <- leg2_wpts$dateD[i]+(round((dist_ystrd_today/9))*60*60)
  dateA_today <- ymd_h(ifelse(dat <= ymd_h(paste0('2022-',month(dat), '-', day(dat), ' ', 8),tz = 'HST'), paste0('2022-',month(dat), '-', day(dat), ' ', 8), 
               ifelse(dat > ymd_h(paste0('2022-',month(dat), '-', day(dat), ' ', 20),tz = 'HST'), paste0('2022-',month(dat), '-', day(dat)+1, ' ', 8), paste0('2022-',month(dat), '-', day(dat), ' ', 20))), tz='HST')
  dateD_today <- dateA_today+smpl_dur_today*60*60

  leg2_wpts <- rbind(leg2_wpts, data.frame(today, samp_hour=hour(dateA_today), samp_dur=smpl_dur_today, dist_to_here_NM=dist_ystrd_today, dateS=dat, dateA=dateA_today, dateD=dateD_today))
  rownames(leg2_wpts) <- c()
}
leg2_wpts

# LEG 3 Lat and lon of each waypoint along track (sampling 8am and 8pm)
leg3_bearing <- bearing(track180[3,], track180[4,])
leg3_qtr <- destPoint(track180[3,], leg3_bearing, ((distsM10[3]+distsM10[4])*seq(0,1,by=0.005)))[c(50,100),]
#leg3_qtr <- destPoint(track180[3,], leg3_bearing, (distsM10[1]*c(0.25,0.50,0.75)))
#leg3_wpts <- leg2_wpts[nrow(leg2_wpts),]
leg3_wpts <-  data.frame(lon=c(track180[3,1], leg3_qtr[2,1], track180[4,1], track180[5,1]), 
                         lat=c(track180[3,2], leg3_qtr[2,2], track180[4,2], track180[5,2]), 
                         samp_hour=c(leg2_wpts$samp_hour[nrow(leg2_wpts)],NA, NA, NA), 
                         samp_dur=c(leg2_wpts$samp_dur[nrow(leg2_wpts)], 22, 0, 0), 
                         dist_to_here_NM=c(leg2_wpts$dist_to_here_NM[nrow(leg2_wpts)], NA, NA, NA),
                         dateS=c(leg2_wpts$dateA[nrow(leg2_wpts)], NA, NA, NA),
                         dateA=c(leg2_wpts$dateA[nrow(leg2_wpts)], NA, NA, NA), 
                         dateD=c(leg2_wpts$dateD[nrow(leg2_wpts)], NA, NA, NA))

leg3_wpts$dist_to_here_NM[2] <- round(distGeo(leg3_wpts[1,1:2], leg3_wpts[2,1:2])/1852)
da4 <- (leg3_wpts$dist_to_here_NM[2]/9*60*60) + leg3_wpts$dateD[1]
leg3_wpts$dateS[2] <- da4
leg3_wpts$dateA[2] <- ymd_h(ifelse(da4 <= ymd_h(paste0('2022-',month(da4), '-', day(da4), ' ', 8),tz = 'HST'), paste0('2022-',month(da4), '-', day(da4), ' ', 8),
                                   ifelse(da4 > ymd_h(paste0('2022-',month(da4), '-', day(da4), ' ', 20),tz = 'HST'), paste0('2022-',month(da4), '-', day(da4)+1, ' ', 8), paste0('2022-',month(da4), '-', day(da4), ' ', 20))), tz='HST')
leg3_wpts$dateD[2] <- ymd_h(ifelse(leg3_wpts$dateA[2] <= ymd_h(paste0('2022-',month(leg3_wpts$dateA[2]), '-', day(leg3_wpts$dateA[2]), ' ', 8),tz = 'HST'), paste0('2022-',month(leg3_wpts$dateA[2]), '-', day(leg3_wpts$dateA[2]), ' ', 8), paste0('2022-',month(leg3_wpts$dateA[2]), '-', day(leg3_wpts$dateA[2]), ' ', 20)), tz='HST') + (leg3_wpts$samp_dur[2]*60*60)
leg3_wpts$samp_hour[2] <- hour(leg3_wpts$dateD[2] - (leg3_wpts$samp_dur[2]*60*60))

leg3_wpts$dist_to_here_NM[3] <- round(distGeo(leg3_wpts[2,1:2], leg3_wpts[3,1:2])/1852)
leg3_wpts$dateA[3] <- (leg3_wpts$dist_to_here_NM[3]/9*60*60) + leg3_wpts$dateD[2]
leg3_wpts$dateD[3] <- leg3_wpts$dateA[3] + (leg3_wpts$samp_dur[3]*60*60)
leg3_wpts$dateS[3] <- leg3_wpts$dateA[3]

leg3_wpts$dist_to_here_NM[4] <- round(distGeo(leg3_wpts[3,1:2], leg3_wpts[4,1:2])/1852)
leg3_wpts$dateA[4] <- (leg3_wpts$dist_to_here_NM[4]/9*60*60) + leg3_wpts$dateD[3]
leg3_wpts$dateS[4] <- leg3_wpts$dateA[4]
leg3_wpts

# ALL LEGS
#all_wpts <- rbind(leg1_wpts, leg2_wpts[-1,], leg3_wpts[-1,])
all_wpts <- rbind(leg1_wpts, leg2_wpts[-1,], leg3_wpts[-1,])
rownames(all_wpts) <- NULL
all_wpts$stn_type <- ifelse(all_wpts$samp_dur > 10, 'extended', ifelse(all_wpts$samp_dur < 5, 'none', 'base'))
all_wpts$stn <- ''
all_wpts$stn[which(all_wpts$stn_type == 'extended')] <- LETTERS[1:7]
all_wpts$stn_type[]

ggplot() +
  borders("world", 
          xlim = c(-160, -140), 
          ylim = c(18, 25),
          fill="gray",colour="grey40", size=0.25) +
  coord_map() +
  geom_path(data=all_wpts, aes(lon, lat)) +
  geom_point(data=all_wpts[which(all_wpts$stn_type == 'base'),], aes(lon, lat, shape=factor(samp_hour)), color='#E69f00', size=3) + 
  geom_point(data=all_wpts[which(all_wpts$stn_type == 'extended'),], aes(lon, lat, shape=factor(samp_hour)), color='#56B4E9', size=4) +
  #geom_point(data=argo, aes(lon, lat), shape='square', color='firebrick', size=2, alpha=0.75) +
  geom_text(data=all_wpts[which(all_wpts$stn_type == 'extended'),], aes(lon, lat, label=stn), hjust = 0, nudge_x = 0.75) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab('Longitude') +
  ylab('Latitude') +
  xlim(c(-160,-137)) +
  ylim(c(8,32))
#ggsave('SE2204_track_wpt5_Argo.png', width=5, height=6, dpi=200, units='in')


#write.csv(all_wpts, 'SE2204_waypoints5_ARGO.csv', row.names = F)

