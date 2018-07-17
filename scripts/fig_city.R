## elephant water hole analysis
# Khaudum NP 
# This script generates figures for the elephant movement, recursions, and water pan ID work
# Millie Chapman & Will Oestreich 16 July 2018

library(tidyr)
rm(list = ls())

#####
# Figure 1
# Map of Namibia + zoom-in on Khaudum NP. Includes locations at which elephants in the analysis were tagged.


#####
# Figure 2
# (a) Dry season elephant mvmts; (b) dry season high recursion points; (c) wet season elephant mvmts; 
# (d) wet season high recursion points.

# load data
ele_dry_mvmt <- read.csv("outputs/ele_dry_mvmt.csv")
ele_wet_mvmt <- read.csv("outputs/ele_wet_mvmt.csv")
ele_dryRt <- read.csv("outputs/ele_dryRt.csv")
ele_wetRt <- read.csv("outputs/ele_wetRt.csv")

# plots
par(mfrow=c(2,2))
# (a)
plot(ele_dry_mvmt$X,ele_dry_mvmt$Y)
# (b)
plot(ele_dryRt$X,ele_dryRt$Y, col = "red", xlim=c(1040000, 1160000), ylim=c(-2200000, -2000000), 
     main = "Dry season high recursion points (200m radius)")
# (c)
plot(ele_wet_mvmt$X,ele_wet_mvmt$Y)
# (d)
plot(ele_wetRt$X,ele_wetRt$Y, col = "red", xlim=c(1040000, 1160000), ylim=c(-2200000, -2000000), 
     main = "Wet season high recursion points (200m radius)")

#####
# Figure 3
# Difference in high recursion points between wet and dry seasons. 
# By removing wet season high recursion points that also show up
# during dry season, we should be left with only precipitation-driven 
# high recursion points for the dry season.


#####
# Figure 4
# Maps showing potential pan centroids w/ circles for wet season 
# recursions at these points overlaid. Reveals likely natural pan 
# locations for (a) 2013, (b) 2014, and (c) 2015.
# function to convert LL to UTM for recursion package
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}
# function to convert LL to UTM for recursion package
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}
# natural pans 
pans<- readOGR("khaudum_data/khaudum_water/Khaudum_pans_pj.shp")
# convert shapefile for pans and calculate centroids
pans_ll <- spTransform(pans, CRS("+proj=longlat +datum=WGS84")) #change CRS
centroid1<- as.data.frame(gCentroid(pans_ll, byid=TRUE))
centroidUTM<-LongLatToUTM(centroid1$x, centroid1$y, 33)
centroidUTM_2<-centroidUTM[,c(2,3)]
# all artifical water sources
art_water<-read.csv("khaudum_data/waterholes.csv")
art_water_utm<-LongLatToUTM(art_water$Lon,art_water$Lat,33)
#read in pan revisits and change low revisitation values to NA
pan_revisits<-read.csv("outputs/pan_revisits_poly.csv")
pan_revisits$revisits2013 <- ifelse(pan_revisits$revisits2013 > 1, pan_revisits$revisits2013, NA)
pan_revisits$revisits2014 <- ifelse(pan_revisits$revisits2014 > 1, pan_revisits$revisits2014, NA)
pan_revisits$revisits2015 <- ifelse(pan_revisits$revisits2015 > 1, pan_revisits$revisits2015, NA)
p1<-ggplot(subset(pan_revisits, !is.na(revisits2013)), aes(x=X, y=Y, color=revisits2013)) +
  geom_point(shape=19, size=2.5) + geom_point(data = centroidUTM_2, colour = "black",size=0.5) + 
  geom_point(data = art_water_utm, colour = "yellow",size=2.5,shape=15) + 
  scale_color_gradient(low="blue", high="red", limits=c(0, 130))
p1 + labs(x = "Easting", y = "Northing", colour = "Revisits") + 
  annotate("text", x=1085000, y=-2045000, label= "2013", size=6)
p2<-ggplot(subset(pan_revisits, !is.na(revisits2014)), aes(x=X, y=Y, color=revisits2014)) + 
  geom_point(shape=19, size=2.5) + geom_point(data = centroidUTM_2, colour = "black",size=0.5) + 
  geom_point(data = art_water_utm, colour = "yellow",size=2.5,shape=15) + 
  scale_color_gradient(low="blue", high="red", limits=c(0, 130))
p2 + labs(x = "Easting", y = "Northing", colour = "Revisits") + 
  annotate("text", x=1085000, y=-2045000, label= "2014", size=6)
p3<-ggplot(subset(pan_revisits, !is.na(revisits2015)), aes(x=X, y=Y, color=revisits2015)) + 
  geom_point(shape=19, size=2.5) + geom_point(data = centroidUTM_2, colour = "black",size=0.5) + 
  geom_point(data = art_water_utm, colour = "yellow",size=2.5,shape=15) + 
  scale_color_gradient(low="blue", high="red", limits=c(0, 130))
p3 + labs(x = "Easting", y = "Northing", colour = "Revisits") + 
  annotate("text", x=1085000, y=-2045000, label= "2015", size=6)




