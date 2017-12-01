RawData<-read.table("DailyDataRoughLess.txt",header = TRUE,sep="\t")
RawData[RawData==-9999]<-NA
RawData$Year<- substr(RawData$Date,1,4)
RawData$Date<-substr(RawData$Date,5,8)
for (i in 1:nrow(RawData)){
  if (is.na(RawData[i,"TMAX"])) {
  RawData[i,"TMAX"]<-(RawData[i-1,"TMAX"]+RawData[i+1,"TMAX"])/2
  }
}


#Flight Days >10 C
FlightDays<-data.frame(0,0,0,0)
colnames(FlightDays)<-c("Station","Flight Days","Year","NumNA")
for (year in unique(RawData$Year)) {
  stations=unique(RawData$Station)
  for (station in stations) {
    subset= RawData[RawData$Year==year & RawData$Station==station,]
    flightdays= sum(subset$TMAX>10,na.rm=TRUE)
    if (flightdays!=0){
    na.days=sum(is.na(subset$TMAX))
    tempo=data.frame(station,flightdays,year,na.days)
    colnames(tempo)<-c("Station","Flight Days","Year","NumNA")
    FlightDays<-rbind(FlightDays,tempo) 
  }}
    
}
FlightDays<-FlightDays[-1,]
FlightDays<-FlightDays[order(FlightDays$Year),]
FlightDays$Station<-substr(FlightDays$Station,7,30)


#Find lat long for stations
StationKey<-read.table("StationKeyClean.txt",as.is = TRUE)
for (station in FlightDays$Station) {
  FlightDays[FlightDays$Station==station,"Lat"]<-StationKey[StationKey$V1==station,2]
  FlightDays[FlightDays$Station==station,"Long"]<-StationKey[StationKey$V1==station,3]
}
save(FlightDays,file="FlightDays.Rda")