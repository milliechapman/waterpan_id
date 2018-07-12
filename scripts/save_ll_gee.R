## elephant water hole analysis
# Khaudum NP 
# This script reads in recursion points for elephants in wet & dry season above a certain threshold.
# Converts to lat/long and saves for use in Google Earth Engine
# Will Oestreich & Millie Chapman 12 July 2018
rm(list = ls())

#function to convert  high recursion locations from utm to lat/lon 
UTMtoLongLat<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=utm +zone=33 +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=longlat +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

#read in high recursion location csv's
dry_loc<-read.csv("outputs/ele_dryRt.csv")
wet_loc<-read.csv("outputs/ele_wetRt.csv")

dry_ll<-UTMtoLongLat(dry_loc$X,dry_loc$Y,33)
wet_ll<-UTMtoLongLat(wet_loc$X,wet_loc$Y,33)
dry_coords<-paste(dry_ll$Y, dry_ll$X, sep= " ")
wet_coords<-paste(wet_ll$Y, wet_ll$X, sep= " ")
write.table(dry_coords,"outputs/ele_dryRt_ll.csv",col.names=T,row.names=F,sep=",")
write.table(wet_coords,"outputs/ele_wetRt_ll.csv",col.names=T,row.names=F,sep=",")