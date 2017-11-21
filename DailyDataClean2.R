RawData<-read.table("DailyDataRoughLess.txt",header = TRUE,sep="\t",as.is=T)
RawData[RawData==-9999]<-NA
RawData$Year<- substr(RawData$DATE,1,4)
RawData$Date<-substr(RawData$DATE,5,8)
RawData<-RawData[,!(names(RawData) %in% "DATE")]
numeric.cols<-c("STATION","STATION_NAME")
#RawData[,!(names(RawData) %in% numeric.cols)]<-sapply(RawData[,!(names(RawData) %in% numeric.cols)],as.double)
RawData<- RawData[substr(RawData$Year,1,2)=="19"&!is.na(RawData$Year),]
for (i in 1:nrow(RawData)){
  if (is.na(RawData[i,"TMAX"])) {
    RawData[i,"TMAX"]<-(RawData[i-1,"TMAX"]+RawData[i+1,"TMAX"])/2
  }
}


#Flight Days >10 C
FlightDays<-data.frame(0,0,0,0,0,0)
colnames(FlightDays)<-c("Station","Flight Days","Year","NumNA","Latitude","Longitude")
for (year in unique(RawData$Year)) {
  stations=unique(RawData$STATION)
  for (station in stations) {
    subset= RawData[RawData$Year==year & RawData$STATION==station,]
    flightdays= sum(subset$TMAX>10,na.rm=TRUE)
    if (flightdays!=0){
      na.days=sum(is.na(subset$TMAX))
      lat=subset[1,"LATITUDE"]
      long=subset[1,"LONGITUDE"]
      tempo=data.frame(station,flightdays,year,na.days,lat,long)
      colnames(tempo)<-c("Station","Flight Days","Year","NumNA","Latitude","Longitude")
      FlightDays<-rbind(FlightDays,tempo) 
    }}
  
}
FlightDays<-FlightDays[-1,]
FlightDays<-FlightDays[order(FlightDays$Year),]
FlightDays$Station<-substr(FlightDays$Station,7,30)


save(FlightDays,file="FlightDays.Rda")