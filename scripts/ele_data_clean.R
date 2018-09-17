#khaudum animal mvmt project
#data cleaning csv's from elephant satellite collars

rm(list = ls())

#packages
library(tidyr)
library(lubridate)

#data directory path 
data.path<-"khaudum_data/"

#elephant (raw data csv's are formatted a bit differently than the carnivores')
ele<-list.files(path = paste(data.path,"elephant/",sep = ""), pattern = "MET.K.*\\.csv", recursive = FALSE)
ele<-lapply(paste(data.path,"elephant/",ele, sep = ""), read.csv)
ele<-do.call(rbind,ele[1:length(ele)])
colnames(ele)[3]<-"Lat"
colnames(ele)[4]<-"Lon"
ele<-ele[ele$Lon< 22 & ele$Lon> 18,]
ele<-ele[ele$Lat< -17 & ele$Lat> -20,]
ele$DateTime<-as.POSIXct(ele$Date.Time, format="%m/%d/%y %H:%M", tz="GMT")
ele$Date.Time<-NULL
ele$Sex<-"F"
ele$Age<-"Adult"
ele<-unique(ele)
write.table(ele,paste(data.path,"elephant/clean/elephant_clean_all.csv",sep=""),col.names=T,row.names=F,sep=",")
