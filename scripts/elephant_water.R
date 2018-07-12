## elephant water hole analysis
# Khaudum NP 
# Millie chapman 8 July 2018
library(recurse)
library(sp)
library(rgdal)
library(dplyr)
library(lubridate)
library(scales)
library(fields)
library(spam)
library(PBSmapping)
rm(list = ls())

# read in cleaned elephant data
ele<- read.csv("khaudum_data/elephant/elephant_clean.csv")
#complete cases
ele<- ele[complete.cases(ele), ]

#Convert LL to UTM for recursion package
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}
utm<-LongLatToUTM(ele$Lon,ele$Lat,33)
ele<-cbind(ele, utm)

# add water hole points
water<- read.csv("khaudum_data/waterholes.csv")
utmw<-LongLatToUTM(water$Lon,water$Lat,33)
water<-cbind(water, utmw)
water<-water[,c(3:6)]

# Keep columns of relevance (X, Y, Date, ID)
ele_df<- ele[,c(8,9,4,1)]
ele_df$DateTime<-as.POSIXct(ele_df$DateTime)

#Split into list of data frames to check one individual recursion
ele_dfs<-split(ele_df, ele_df$ID)
lrecurse<- getRecursions(ele_dfs[[2]], 500)
plot(lrecurse, ele_dfs[[2]]) 

# Get month number
ele_df$Date<-as.Date(ele_df$DateTime)
ele_df$month<-as.numeric(format(ele_df$Date, "%m"))

#make wet and dry df
ele_wet<-ele_df[(ele_df$month>10|ele_df$month<2),]
ele_dry<-ele_df[(ele_df$month>5&ele_df$month<9),]

# dry season recurse
ele_dry<- ele_dry[,c(1,2,3,4)]
dry_recurse<- getRecursions(ele_dry, 500)

#wet season recurse
ele_wet<- ele_wet[,c(1,2,3,4)]
wet_recurse<- getRecursions(ele_wet, 500)

#plot recursion (fix labels...)
par(mfrow=c(1,2))
plot(wet_recurse,ele_wet, xlim=c(1040000, 1160000), ylim=c(-2200000, -2000000),
     legendPos = c(1100000, -2250000), main = "Wet Recursion Plot 200m")
plot(wet_recurse,ele_wet, xlim=c(1040000, 1160000), ylim=c(-2200000, -2000000),
     legendPos = c(1100000, -2250000), main = "Wet Recursion Plot 200m")
points(water$X, water$Y, col="blue", bg= "blue", pch=20)
plot(dry_recurse,ele_dry, xlim=c(1040000, 1160000), ylim=c(-2200000, -2000000),
     legendPos = c(1100000, -2250000), main = "Dry Recursion Plot 200m")
plot(dry_recurse,ele_dry, xlim=c(1040000, 1160000), ylim=c(-2200000, -2000000),
     legendPos = c(1100000, -2250000), main = "Dry Recursion Plot 200m")
points(water$X, water$Y, col= "blue", pch=20)

plot(dry_recurse,ele_dry, xlim=c(1040000, 1160000), ylim=c(-2200000, -2000000),
     legendPos = c(1100000, -2250000), main = "Dry Recursion Plot 200m")
plot(wet_recurse,ele_wet, xlim=c(1040000, 1160000), ylim=c(-2200000, -2000000),
     legendPos = c(1100000, -2250000), main = "Wet Recursion Plot 200m")

#plot just points with significant revisits (..may need to reconsider how this is defined..)
par(mfrow=c(1,2))
wet_r<-as.data.frame(wet_recurse$revisits)
ele_wetR<-cbind(ele_wet,wet_r)
ele_wetR$revisits<- ele_wetR$`wet_recurse$revisits`
visitThresholdw = quantile(ele_wetR$revisits, .95)
ele_wetRt<-ele_wetR[ele_wetR$revisits > 20,]
plot(wet_recurse, ele_wetRt, col = "red", xlim=c(1040000, 1160000), ylim=c(-2200000, -2000000), 
     main = "Wet Recursion Points 200m")
points(water$X, water$Y, col= "blue", pch=20)

dry_r<-as.data.frame(dry_recurse$revisits)
ele_dryR<-cbind(ele_dry,dry_r)
ele_dryR$revisits<- ele_dryR$`dry_recurse$revisits`
visitThresholdd = quantile(ele_dryR$revisits, .95)
ele_dryRt<-ele_dryR[ele_dryR$revisits > 20,]
plot(dry_recurse, ele_dryRt, col = "red", xlim=c(1040000, 1160000), ylim=c(-2200000, -2000000),
     main = "Dry Recursion Points 200m")
points(water$X, water$Y, col= "blue", pch=20)

#save high recursion locations as csv
write.table(ele_dryRt,"outputs/ele_dryRt.csv",col.names=T,row.names=F,sep=",")
write.table(ele_wetRt,"outputs/ele_wetRt.csv",col.names=T,row.names=F,sep=",")

#read in high recursion location csv's
dry_loc<-read.csv("outputs/ele_dryRt.csv")
wet_loc<-read.csv("outputs/ele_wetRt.csv")

#convert  high recursion locations from utm to lat/lon and save as csv for earth engine
UTMtoLongLat<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=utm +zone=33 +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=longlat +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}
dry_ll<-UTMtoLongLat(dry_loc$X,dry_loc$Y,33)
wet_ll<-UTMtoLongLat(wet_loc$X,wet_loc$Y,33)
dry_coords<-paste(dry_ll$Y, dry_ll$X, sep= " ")
write.table(dry_coords,"outputs/ele_dryRt_ll.csv",col.names=T,row.names=F,sep=",")
write.table(wet_ll,"outputs/ele_wetRt_ll.csv",col.names=T,row.names=F,sep=",")

# histogram of number of revisits by season 
par(mfrow=c(2,1))
hist(ele_dryR$revisits, breaks = 100, main = "Dry Season Revisits", xlab = "Revisits (radius = 500m)")
hist(ele_wetR$revisits, breaks = 100, main = "Wet Season Revisits", xlab = "Revisits (radius = 500m)")

## natural pans 
library(rgdal)
library(gdalUtils)
library(raster)

pans<- readOGR("khaudum_data/khaudum_water/Khaudum_pans_pj.shp")
ogrInfo("khaudum_data/khaudum_water/Khaudum_pans_pj.shp")
plot(pans)
plot(ele_wet$X, ele_wet$Y)
plot(pans)
centroid1<- as.data.frame(gCentroid(pans, byid=TRUE))

centroid <- SpatialPointsDataFrame(gCentroid(pans, byid=TRUE), 
                                      pans@data, match.ID=FALSE)

plot(centroid)

water_recurse<- getRecursionsAtLocations(ele_wet, centroid1, 200)
plot(water_recurse, centroid1, col = "red", xlim=c(1040000, 1160000), ylim=c(-2200000, -2000000),
     main = "Dry Recursion Points 200m")

xy <- wet_loc[,c(1,2)]

spdf <- SpatialPointsDataFrame(coords = xy, data = wet_loc,
                               proj4string = CRS("+proj=utm +zone=33 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

shapefile <- spTransform(pans, CRS("+proj=longlat +datum=WGS84")) #change CRS
centroid1<- as.data.frame(gCentroid(shapefile, byid=TRUE))
centroidUTM<-LongLatToUTM(centroid1$x, centroid1$y, 33)
centroidUTM<-centroidUTM[,c(2,3)]
water_recurse<- getRecursionsAtLocations(ele_wet, centroidUTM, 200)
plot(water_recurse, centroidUTM)
par(mfrow=c(1,1))

plot(shapefile)
points(wet_ll$X, wet_ll$Y, col = "red", pch=20)

## S4 method for signature 'ANY'
crs(ele_wet, asText= TRUE)
pan_crs<-crs(pans)
crs(ele_wet)<-pan_crs
projection(pans, asText=TRUE)
projection(x) <- value