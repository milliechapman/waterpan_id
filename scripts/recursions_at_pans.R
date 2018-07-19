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

# recursions at points (centroids of potential pan sites in UTM); dry season
ele_dry<-read.csv("outputs/ele_dry_mvmt.csv")
water_recurse_d<-getRecursionsAtLocations(ele_dry, centroidUTM_2, 200)
par(mfrow=c(1,1))
plot(water_recurse_d, centroidUTM_2)

# recursions at points (dry season), but for each year
ele_dry$Date<-as.Date(ele_dry$DateTime)
ele_dry_2013<-ele_dry[(as.numeric(ele_dry$Date)>as.numeric(as.Date("2013-05-01"))&as.numeric(ele_dry$Date)<as.numeric(as.Date("2013-10-01"))),]
water_recurse_2013_d<-getRecursionsAtLocations(ele_dry_2013[,c(1,2,3,4)], centroidUTM_2, 500)
ele_dry_2014<-ele_dry[(as.numeric(ele_dry$Date)>as.numeric(as.Date("2014-05-01"))&as.numeric(ele_dry$Date)<as.numeric(as.Date("2014-10-01"))),]
water_recurse_2014_d<-getRecursionsAtLocations(ele_dry_2014[,c(1,2,3,4)], centroidUTM_2, 500)
ele_dry_2015<-ele_dry[(as.numeric(ele_dry$Date)>as.numeric(as.Date("2015-05-01"))&as.numeric(ele_dry$Date)<as.numeric(as.Date("2015-10-01"))),]
water_recurse_2015_d<-getRecursionsAtLocations(ele_dry_2015[,c(1,2,3,4)], centroidUTM_2, 500)
par(mfrow=c(2,2))
plot(water_recurse_2013_d, centroidUTM, xlim=c(1040000, 1160000), ylim=c(-2220000, -2020000))
plot(water_recurse_2014_d, centroidUTM, xlim=c(1040000, 1160000), ylim=c(-2220000, -2020000))
plot(water_recurse_2015_d, centroidUTM, xlim=c(1040000, 1160000), ylim=c(-2220000, -2020000))

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


# natural pans 
pans_utm <- spTransform(pans, CRS("+proj=utm +zone=33 +datum=WGS84")) #change CRS

unique <- unique(pans_utm@data$PANS_ID)

unique_df<- as.data.frame(unique)

for (i in 1:length(unique)) {
  tmp <- pans_utm[pans_utm$PANS_ID == unique[i], ]
  tmp<-buffer(tmp, width = 1)
  #wet seasons
  a2013<-getRecursionsInPolygon(ele_wet_2013[,1:4], tmp, verbose = FALSE)
  unique_df$revisits2013w[i] =  a2013$revisits 
  a2014<-getRecursionsInPolygon(ele_wet_2014[,1:4], tmp, verbose = FALSE)
  unique_df$revisits2014w[i] =  a2014$revisits  
  a2015<-getRecursionsInPolygon(ele_wet_2015[,1:4], tmp, verbose = FALSE)
  unique_df$revisits2015w[i] =  a2015$revisits  
  # dry seasons
  b2013<-getRecursionsInPolygon(ele_dry_2013[,1:4], tmp, verbose = FALSE)
  unique_df$revisits2013d[i] =  b2013$revisits 
  b2014<-getRecursionsInPolygon(ele_dry_2014[,1:4], tmp, verbose = FALSE)
  unique_df$revisits2014d[i] =  b2014$revisits  
  b2015<-getRecursionsInPolygon(ele_dry_2015[,1:4], tmp, verbose = FALSE)
  unique_df$revisits2015d[i] =  b2015$revisits  
  #plot
  centroid<- gCentroid(tmp)
  unique_df$X[i]<-centroid@coords[1,1]
  unique_df$Y[i]<-centroid@coords[1,2]
}

unique_df$diff2013<-unique_df$revisits2013w-unique_df$revisits2013d
unique_df$diff2014<-unique_df$revisits2014w-unique_df$revisits2014d
unique_df$diff2015<-unique_df$revisits2015w-unique_df$revisits2015d

write.csv(unique_df, "outputs/pan_revisits_poly.csv")
