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
centroidUTM<-centroidUTM[,c(2,3)]

# recursions at points (centroids of potential pan sites in UTM); wet season
ele_wet<-read.csv("outputs/ele_wet_mvmt.csv")
water_recurse<- getRecursionsAtLocations(ele_wet, centroidUTM, 200)
plot(water_recurse, centroidUTM)
par(mfrow=c(1,1))

# plot pan centroids along w/ high recursion points from elephant recursions
dry_loc<-read.csv("outputs/ele_dryRt.csv")
wet_loc<-read.csv("outputs/ele_wetRt.csv")
dry_ll<-UTMtoLongLat(dry_loc$X,dry_loc$Y,33)
wet_ll<-UTMtoLongLat(wet_loc$X,wet_loc$Y,33)
plot(pans_ll)
points(wet_ll$X, wet_ll$Y, col = "red", pch=20)

