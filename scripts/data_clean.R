#khaudum animal mvmt project
#data cleaning csv's from satellite collars

#Will Oestreich 7/7/18
rm(list = ls())

#packages
library(tidyr)
library(lubridate)

#data directory path 
data.path<-"khaudum_data/"

#hyena
hyena<-list.files(path = paste(data.path,"/hyena/",sep = ""), pattern = "*SAT*", recursive = FALSE)
hyena<-lapply(paste(data.path,"hyena/",hyena, sep = ""), read.csv)
hyena<-do.call(rbind,hyena[1:length(hyena)])
hyena<-hyena[hyena$Lon< 22 & hyena$Lon> 18,]
hyena$datPOS<-as.POSIXlt(as.character(hyena$GMT.Date), format="%m/%d/%y", tz="GMT")
hyena$DateTime <- paste(hyena$datPOS, hyena$GMT.Time)
hyena$DateTime<-as.POSIXct(hyena$DateTime, tz="GMT")
hyena<-hyena[!duplicated(hyena$DateTime),]
hyena.md<-read.csv(paste(data.path,"hyena/hyena_metadata.csv",sep=""))
hyena<-merge(hyena, hyena.md, by = "ID", all.x = TRUE)
write.table(hyena,paste(data.path,"hyena/hyena_clean.csv",sep=""),col.names=T,row.names=F,sep=",")

#lion
lion<-list.files(path = paste(data.path,"lion/",sep = ""), pattern = "*SAT*", recursive = FALSE)
lion<-lapply(paste(data.path,"lion/",lion, sep = ""), read.csv)
lion<-do.call(rbind,lion[1:length(lion)])
lion<-lion[lion$Lon< 22 & lion$Lon> 18,]
lion$datPOS<-as.POSIXlt(as.character(lion$GMT.Date), format="%m/%d/%y", tz="GMT")
lion$DateTime <- paste(lion$datPOS, lion$GMT.Time)
lion$DateTime<-as.POSIXct(lion$DateTime, tz="GMT")
lion<-lion[!duplicated(lion$DateTime),]
lion.md<-read.csv(paste(data.path,"lion/lion_metadata.csv",sep=""))
lion<-merge(lion, lion.md, by = "ID", all.x = TRUE)
write.table(lion,paste(data.path,"lion/lion_clean.csv",sep=""),col.names=T,row.names=F,sep=",")

#wild dog
wd<-list.files(path = paste(data.path,"wilddog/",sep = ""), pattern = "*SAT*", recursive = FALSE)
wd<-lapply(paste(data.path,"wilddog/",wd, sep = ""), read.csv)
wd<-do.call(rbind,wd[1:length(wd)])
wd<-wd[wd$Lon< 22 & wd$Lon> 18,]
wd$datPOS<-as.POSIXlt(as.character(wd$GMT.Date), format="%m/%d/%y", tz="GMT")
wd$DateTime <- paste(wd$datPOS, wd$GMT.Time)
wd$DateTime<-as.POSIXct(wd$DateTime, tz="GMT")
wd<-wd[!duplicated(wd$DateTime),]
write.table(wd,paste(data.path,"wilddog/wilddog_clean.csv",sep=""),col.names=T,row.names=F,sep=",")

#elephant (raw data csv's are formatted a bit differently than the carnivores')
ele<-list.files(path = paste(data.path,"elephant/",sep = ""), pattern = "MET.K.*\\.csv", recursive = FALSE)
ele<-lapply(paste(data.path,"elephant/",ele, sep = ""), read.csv)
ele<-do.call(rbind,ele[1:length(ele)])
colnames(ele)[3]<-"Lat"
colnames(ele)[4]<-"Lon"
ele<-ele[ele$Lon< 22 & ele$Lon> 18,]
ele$DateTime<-as.POSIXct(ele$Date.Time, format="%m/%d/%y %H:%M", tz="GMT")
ele$Date.Time<-NULL
ele$Sex<-"F"
ele$Age<-"Adult"
write.table(ele,paste(data.path,"elephant/elephant_clean.csv",sep=""),col.names=T,row.names=F,sep=",")



