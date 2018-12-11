## elephant water hole analysis
# Khaudum NP 
# This script generates figures for the elephant movement, recursions, and water pan ID work
# Millie Chapman & Will Oestreich 19 July 2018

library(tidyr)
library(ggplot2)
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
# function to convert LL to UTM for recursion package
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

#khaudum NP outline lons and lats
khaudum_np<-data.frame(lon=c(20.99761962890625, 20.621337890625, 20.59661865234375, 
                             20.55267333984375, 20.51422119140625, 20.41259765625, 
                             20.33843994140625, 20.390625, 20.99761962890625),
                       lat=c(-18.35452552912664, -18.3336694457713, -18.437924653474393, 
                             -18.47960905583197, -18.49523809543325, -18.682675052185548,
                             -19.02577027586865, -19.35001948171314, -19.427743935948918))
khaudum_np_utm<-LongLatToUTM(khaudum_np$lon, khaudum_np$lat, 33)
khaudum_np_utm<-khaudum_np_utm[,c(2,3)]

#####
# Figure 1
# Map of Namibia + zoom-in on Khaudum NP. Includes locations at which elephants in the analysis were tagged.
#afr<-readOGR("khaudum_data/Africa_SHP/Africa.shp")
#nam<-readOGR("khaudum_data/NAM_outline_SHP/NAM_outline.shp")
library(maps)  
library(mapdata)
library(ggplot2)

map1 <- fortify(map("worldHires", fill=TRUE, plot=FALSE))

gg <- ggplot()
gg <- gg + geom_map(data=map1, map=map1,
                    aes(x=long, y=lat, map_id=region),
                    fill="white", color="black")
gg <- gg + geom_map(data=data.frame(region="Namibia"), map=map1,
                    aes(map_id=region), fill="steelblue")
gg <- gg + xlim(0,40) + ylim(-40,20)
gg <- gg + coord_map()
gg <- gg + theme_bw()
gg


#####
# Figure 2
# (a) Dry season elephant mvmts; (b) dry season high recursion points; (c) wet season elephant mvmts; 
# (d) wet season high recursion points.

# load data
ele_dry_mvmt <- read.csv("outputs/ele_dry_mvmt.csv")
ele_wet_mvmt <- read.csv("outputs/ele_wet_mvmt.csv")
ele_dryRt <- read.csv("outputs/ele_dryRt.csv")
ele_wetRt <- read.csv("outputs/ele_wetRt.csv")

# all artifical water sources
art_water<-read.csv("khaudum_data/waterholes.csv")
art_water_utm<-LongLatToUTM(art_water$Lon,art_water$Lat,33)
art_water_met<-readOGR("khaudum_data/khaudum_water/All_Khaudum_waters_pj.shp")
art_water_met_utm<-LongLatToUTM(art_water_met$X_COORD,art_water_met$Y_COORD,33)

# plots
# (a)
p1<-ggplot(subset(ele_dry_mvmt, (X>1040000 & X<1160000 & Y>(-2165000) & Y<(-2000000))), aes(x=X, y=Y, colour=ID)) +
  geom_path(size=0.5)
p1 + labs(x = "Easting", y = "Northing") + 
  annotate("text", x=1058000, y=-2165000, label= "Dry season", size=6) +
  geom_polygon(data = khaudum_np_utm, aes(x=X,y=Y), alpha=0.1, colour="black") +
  ylim(-2170000,-2020000) + xlim(1040000,1160000)

# (b)
p2<-ggplot(ele_dryRt, aes(x=X,y=Y)) + geom_point(shape=1, size=2.5)
p2 + labs(x = "Easting", y = "Northing") + 
  annotate("text", x=1058000, y=-2165000, label= "Dry season", size=6) +
  geom_point(data = art_water_utm, colour = "yellow",size=2.5,shape=15) +
  geom_point(data = art_water_met_utm, colour = "green",size=2.5,shape=15) +
  geom_polygon(data = khaudum_np_utm, aes(x=X,y=Y), alpha=0.1, colour="black") +
  ylim(-2170000,-2020000) + xlim(1040000,1160000)

# (c)
p3<-ggplot(subset(ele_wet_mvmt, (X>1040000 & X<1160000 & Y>(-2165000) & Y<(-2000000))), aes(x=X, y=Y, colour=ID)) +
  geom_path(size=0.5)
p3 + labs(x = "Easting", y = "Northing") + 
  annotate("text", x=1058000, y=-2165000, label= "Wet season", size=6) +
  geom_polygon(data = khaudum_np_utm, aes(x=X,y=Y), alpha=0.1, colour="black") +
  ylim(-2170000,-2020000) + xlim(1040000,1160000)

# (d)
p4<-ggplot(ele_wetRt, aes(x=X,y=Y)) + geom_point(shape=1, size=2.5)
p4 + labs(x = "Easting", y = "Northing") +
  annotate("text", x=1058000, y=-2165000, label= "Wet season", size=6) +
  geom_point(data = art_water_utm, colour = "yellow",size=2.5,shape=15) +
  geom_point(data = art_water_met_utm, colour = "green",size=2.5,shape=15) +
  geom_polygon(data = khaudum_np_utm, aes(x=X,y=Y), alpha=0.1, colour="black") +
  ylim(-2170000,-2020000) + xlim(1040000,1160000)

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
pan_revisits$revisits2013w <- ifelse(pan_revisits$revisits2013w > 1, pan_revisits$revisits2013w, NA)
pan_revisits$revisits2014w <- ifelse(pan_revisits$revisits2014w > 1, pan_revisits$revisits2014w, NA)
pan_revisits$revisits2015w <- ifelse(pan_revisits$revisits2015w > 1, pan_revisits$revisits2015w, NA)
p1<-ggplot(subset(pan_revisits, !is.na(revisits2013w)), aes(x=X, y=Y, color=revisits2013w)) +
  geom_point(shape=19, size=2.5) + geom_point(data = centroidUTM_2, colour = "black",size=0.5) + 
  geom_point(data = art_water_utm, colour = "yellow",size=2.5,shape=15) + 
  scale_color_gradient(low="blue", high="red", limits=c(0, 130))
p1 + labs(x = "Easting", y = "Northing", colour = "Revisits") + 
  annotate("text", x=1085000, y=-2045000, label= "2013", size=6)
p2<-ggplot(subset(pan_revisits, !is.na(revisits2014w)), aes(x=X, y=Y, color=revisits2014w)) + 
  geom_point(shape=19, size=2.5) + geom_point(data = centroidUTM_2, colour = "black",size=0.5) + 
  geom_point(data = art_water_utm, colour = "yellow",size=2.5,shape=15) + 
  scale_color_gradient(low="blue", high="red", limits=c(0, 130))
p2 + labs(x = "Easting", y = "Northing", colour = "Revisits") + 
  annotate("text", x=1085000, y=-2045000, label= "2014", size=6)
p3<-ggplot(subset(pan_revisits, !is.na(revisits2015w)), aes(x=X, y=Y, color=revisits2015w)) + 
  geom_point(shape=19, size=2.5) + geom_point(data = centroidUTM_2, colour = "black",size=0.5) + 
  geom_point(data = art_water_utm, colour = "yellow",size=2.5,shape=15) + 
  scale_color_gradient(low="blue", high="red", limits=c(0, 130))
p3 + labs(x = "Easting", y = "Northing", colour = "Revisits") + 
  annotate("text", x=1085000, y=-2045000, label= "2015", size=6)

#####
# Figure 4 (alternative)
# Maps showing potential pan centroids w/ circles for difference in
# number of recursions between wet season and dry season at these 
# points overlaid. Reveals likely natural pan locations for (a) 2013, 
# (b) 2014, and (c) 2015.
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
art_water_met<-readOGR("khaudum_data/khaudum_water/All_Khaudum_waters_pj.shp")
art_water_met_utm<-LongLatToUTM(art_water_met$X_COORD,art_water_met$Y_COORD,33)
art_water_met_utm<-art_water_met_utm[art_water_met_utm$Y<0,]  #removes points with clearly incorrect location
#read in pan revisits and change low revisitation values to NA
pan_revisits<-read.csv("outputs/pan_revisits_poly.csv")
#changing negative values for differences to 0. This is because the distribution
#of difference values is not centered on zero (many more positive differences,
#as we might expect). Plus, we are ultimately most interested in those sites
#visited more during wet season than dry season (positive diff; natural pans).
#This also avoids issues with trying to center a positive-negative colorbar
#on zero (white) when presenting data that are not centered around zero.
pan_revisits$diff2013 <- ifelse(pan_revisits$diff2013 < 0, 0, pan_revisits$diff2013)
pan_revisits$diff2014 <- ifelse(pan_revisits$diff2014 < 0, 0, pan_revisits$diff2014)
pan_revisits$diff2015 <- ifelse(pan_revisits$diff2015 < 0, 0, pan_revisits$diff2015)

p1<-ggplot(subset(pan_revisits, !is.na(diff2013)), aes(x=X, y=Y, color=diff2013)) +
  geom_point(shape=19, size=2.5) + geom_point(data = centroidUTM_2, colour = "black",size=0.5) + 
  geom_point(data = art_water_utm, colour = "yellow",size=2.5,shape=15) + 
  geom_point(data = art_water_met_utm, colour = "green",size=2.5,shape=15) +
  scale_color_gradient(low="blue", high="red", limits=c(0, 130))
p1 + labs(x = "Easting", y = "Northing", colour = "Diff in revisits (wet - dry)") + 
  annotate("text", x=1085000, y=-2045000, label= "2013", size=6)
p2<-ggplot(subset(pan_revisits, !is.na(diff2014)), aes(x=X, y=Y, color=diff2014)) + 
  geom_point(shape=19, size=2.5) + geom_point(data = centroidUTM_2, colour = "black",size=0.5) + 
  geom_point(data = art_water_utm, colour = "yellow",size=2.5,shape=15) + 
  geom_point(data = art_water_met_utm, colour = "green",size=2.5,shape=15) +
  scale_color_gradient(low="blue", high="red", limits=c(0, 130))
p2 + labs(x = "Easting", y = "Northing", colour = "Diff in revisits (wet - dry)") + 
  annotate("text", x=1085000, y=-2045000, label= "2014", size=6)
p3<-ggplot(subset(pan_revisits, !is.na(diff2015)), aes(x=X, y=Y, color=diff2015)) + 
  geom_point(shape=19, size=2.5) + geom_point(data = centroidUTM_2, colour = "black",size=0.5) + 
  geom_point(data = art_water_utm, colour = "yellow",size=2.5,shape=15) + 
  geom_point(data = art_water_met_utm, colour = "green",size=2.5,shape=15) + 
  scale_color_gradient(low="blue", high="red", limits=c(0, 130))
p3 + labs(x = "Easting", y = "Northing", colour = "Diff in revisits (wet - dry)") + 
  annotate("text", x=1085000, y=-2045000, label= "2015", size=6)

##### 
# Alternative Fig 4 for Surf 'n' Turf presentation (Oct 2018)
pan_revisits$diff2014 <- ifelse(pan_revisits$diff2014 >50, 50, pan_revisits$diff2014)
p2<-ggplot(subset(pan_revisits, !is.na(diff2014)), aes(x=X, y=Y, color=diff2014)) + 
  geom_point(shape=19, size=4) + geom_point(data = centroidUTM_2, colour = "black",size=0.3) + 
  scale_color_gradient(low="white", high="red", limits=c(0, 50))
p2 + labs(x = "Easting", y = "Northing", colour = "Diff in revisits (wet - dry)") + 
  theme(text = element_text(size = 14)) +
  annotate("text", x=1070000, y=-2040000, label= "2014", size=6) +
  geom_polygon(data = khaudum_np_utm, aes(x=X,y=Y), alpha=0.1, colour="black") 

#####
# Figure 5
# (a) Bar plots of # of pans with x # of revisits for each year (wet season). 
# (b-d) For each year, # of pans (y) vs. # of recursions (x).
pan_revisits<-read.csv("outputs/pan_revisits_poly.csv")

# (a) wet season
# 1 year counts
count13<-sum(pan_revisits$revisits2013w>0)
count14<-sum(pan_revisits$revisits2014w>0)
count15<-sum(pan_revisits$revisits2015w>0)

# 2 and 3 year counts
count13_2yr<-0
count14_2yr<-0
count15_2yr<-0
count3yr<-0
for (i in 1:length(pan_revisits$revisits2013w)) {
  if (pan_revisits$revisits2013w[i]>0 & ((pan_revisits$revisits2014w[i]>0) | pan_revisits$revisits2015w[i]>0)) {
    count13_2yr<-count13_2yr+1
  }
  if (pan_revisits$revisits2014w[i]>0 & ((pan_revisits$revisits2013w[i]>0) | pan_revisits$revisits2015w[i]>0)) {
    count14_2yr<-count14_2yr+1
  }
  if (pan_revisits$revisits2015w[i]>0 & ((pan_revisits$revisits2013w[i]>0) | pan_revisits$revisits2014w[i]>0)) {
    count15_2yr<-count15_2yr+1
  }
  if (pan_revisits$revisits2013w[i]>0 & pan_revisits$revisits2014w[i]>0 & pan_revisits$revisits2015w[i]>0) {
    count3yr<-count3yr+1
  }
}

counts<-data.frame(Year=rep(c("2013","2014","2015"),3),
                   num_years=rep(c("1","2","3"),each=3),
                   num_pans<-c(count13,count14,count15,
                            count13_2yr, count14_2yr,count15_2yr,
                            count3yr,count3yr,count3yr))

ggplot(data=counts, aes(x=Year, y=num_pans, fill=num_years)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  annotate("text", x=1, y=157, label= "Wet season", size=6)

# (b)
dist2013<-as.data.frame(table(pan_revisits$revisits2013w))
ggplot(data=dist2013, aes(x=log10(as.numeric(levels(Var1))[Var1]), y=log10(Freq))) +
  geom_line()

# (c)
dist2014<-as.data.frame(table(pan_revisits$revisits2014w))
ggplot(data=dist2014, aes(x=as.numeric(levels(Var1))[Var1], y=Freq)) +
  geom_line() +
  xlim(c(1,55))


# (d)
dist2015<-as.data.frame(table(pan_revisits$revisits2015w))
ggplot(data=dist2015, aes(x=as.numeric(levels(Var1))[Var1], y=Freq)) +
  geom_line() +
  xlim(c(1,55))

