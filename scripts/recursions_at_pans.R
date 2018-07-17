## elephant water hole analysis
# Khaudum NP 
# This script calculates recursions at centroids of potential natural water pans throughout Khaudum
# Millie Chapman & Will Oestreich 12 July 2018

library(recurse)
library(sp)
library(rgdal)
library(dplyr)
library(lubridate)
library(scales)
library(fields)
library(spam)
library(PBSmapping)
library(gdalUtils)
library(raster)
library(rgeos)
library(ggplot2)
library(gridExtra)
rm(list = ls())

# function to convert LL to UTM for recursion package
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

# function to convert  high recursion locations from utm to lat/lon 
UTMtoLongLat<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=utm +zone=33 +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=longlat +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

# natural pans 
pans<- readOGR("khaudum_data/khaudum_water/Khaudum_pans_pj.shp")

# convert shapefile for pans and calculate centroids
pans_ll <- spTransform(pans, CRS("+proj=longlat +datum=WGS84")) #change CRS
centroid1<- as.data.frame(gCentroid(pans_ll, byid=TRUE))
centroidUTM<-LongLatToUTM(centroid1$x, centroid1$y, 33)
centroidUTM_2<-centroidUTM[,c(2,3)]

# recursions at points (centroids of potential pan sites in UTM); wet season
ele_wet<-read.csv("outputs/ele_wet_mvmt.csv")
water_recurse<-getRecursionsAtLocations(ele_wet, centroidUTM_2, 200)
par(mfrow=c(1,1))
plot(water_recurse, centroidUTM_2)

# recursions at points (wet season), but for each year
ele_wet$Date<-as.Date(ele_wet$DateTime)
ele_wet_2013<-ele_wet[(as.numeric(ele_wet$Date)>as.numeric(as.Date("2012-10-01"))&as.numeric(ele_wet$Date)<as.numeric(as.Date("2013-03-01"))),]
water_recurse_2013<-getRecursionsAtLocations(ele_wet_2013[,c(1,2,3,4)], centroidUTM_2, 500)
ele_wet_2014<-ele_wet[(as.numeric(ele_wet$Date)>as.numeric(as.Date("2013-10-01"))&as.numeric(ele_wet$Date)<as.numeric(as.Date("2014-03-01"))),]
water_recurse_2014<-getRecursionsAtLocations(ele_wet_2014[,c(1,2,3,4)], centroidUTM_2, 500)
ele_wet_2015<-ele_wet[(as.numeric(ele_wet$Date)>as.numeric(as.Date("2014-10-01"))&as.numeric(ele_wet$Date)<as.numeric(as.Date("2015-03-01"))),]
water_recurse_2015<-getRecursionsAtLocations(ele_wet_2015[,c(1,2,3,4)], centroidUTM_2, 500)
par(mfrow=c(2,2))
plot(water_recurse_2013, centroidUTM, xlim=c(1040000, 1160000), ylim=c(-2220000, -2020000))
plot(water_recurse_2014, centroidUTM, xlim=c(1040000, 1160000), ylim=c(-2220000, -2020000))
plot(water_recurse_2015, centroidUTM, xlim=c(1040000, 1160000), ylim=c(-2220000, -2020000))

# merge number of revisits with pan centroid locations and sort descending
pan_revisit_2013 <- cbind(water_recurse_2013$revisits,centroidUTM_2)
pan_revisit_2014 <- cbind(water_recurse_2014$revisits,centroidUTM_2)
pan_revisit_2015 <- cbind(water_recurse_2015$revisits,centroidUTM_2)
pan_revisit_2013 <- pan_revisit_2013 %>% arrange(desc(`water_recurse_2013$revisits`))
pan_revisit_2014 <- pan_revisit_2014 %>% arrange(desc(`water_recurse_2014$revisits`))
pan_revisit_2015 <- pan_revisit_2015 %>% arrange(desc(`water_recurse_2015$revisits`))
# plot recursions only for centroids w/ > 0 recursions
pan_revisit_2013$`water_recurse_2013$revisits` <- ifelse(pan_revisit_2013$`water_recurse_2013$revisits` > 0, pan_revisit_2013$`water_recurse_2013$revisits`, NA)
pan_revisit_2014$`water_recurse_2014$revisits` <- ifelse(pan_revisit_2014$`water_recurse_2014$revisits` > 0, pan_revisit_2014$`water_recurse_2014$revisits`, NA)
pan_revisit_2015$`water_recurse_2015$revisits` <- ifelse(pan_revisit_2015$`water_recurse_2015$revisits` > 0, pan_revisit_2015$`water_recurse_2015$revisits`, NA)
par(mfrow=c(2,2))

p1<-ggplot(subset(pan_revisit_2013, !is.na(`water_recurse_2013$revisits`)), aes(x=X, y=Y, color=`water_recurse_2013$revisits`)) + geom_point(shape=19, size=2.5) + geom_point(data = centroidUTM_2, colour = "black",size=0.5) + scale_color_gradient(low="blue", high="red", limits=c(0, 40))
p1 + labs(x = "Easting", y = "Northing", colour = "Revisits") + annotate("text", x=1085000, y=-2045000, label= "2013", size=6)
p2<-ggplot(subset(pan_revisit_2014, !is.na(`water_recurse_2014$revisits`)), aes(x=X, y=Y, color=`water_recurse_2014$revisits`)) + geom_point(shape=19, size=2.5) + geom_point(data = centroidUTM_2, colour = "black",size=0.5) + scale_color_gradient(low="blue", high="red", limits=c(0, 40))
p2 + labs(x = "Easting", y = "Northing", colour = "Revisits") + annotate("text", x=1085000, y=-2045000, label= "2014", size=6)
p3<-ggplot(subset(pan_revisit_2015, !is.na(`water_recurse_2015$revisits`)), aes(x=X, y=Y, color=`water_recurse_2015$revisits`)) + geom_point(shape=19, size=2.5) + geom_point(data = centroidUTM_2, colour = "black",size=0.5) + scale_color_gradient(low="blue", high="red", limits=c(0, 40))
p3 + labs(x = "Easting", y = "Northing", colour = "Revisits") + annotate("text", x=1085000, y=-2045000, label= "2015", size=6)

# plot pan centroids along w/ high recursion points from elephant recursions
dry_loc<-read.csv("outputs/ele_dryRt.csv")
wet_loc<-read.csv("outputs/ele_wetRt.csv")
dry_ll<-UTMtoLongLat(dry_loc$X,dry_loc$Y,33)
wet_ll<-UTMtoLongLat(wet_loc$X,wet_loc$Y,33)
plot(pans_ll)
points(wet_ll$X, wet_ll$Y, col = "red", pch=20)

