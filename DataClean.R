setwd("C:/Users/Anderson Lab/Desktop/Nick Z/Weather Scraper/WeatherWork")

if(file.exists("cleanData.Rda")){
  load("cleanData.Rda")
  
} else {

DataCleanZ<-function(file){
code2strZ <- function(tableZ) {
  #only works with state format files
  
  #State
  
  stateCode<-substring(tableZ[,1], 1,3)
  stateKey<- read.table("StateKey.txt",sep="-",colClasses=c("character","character"),strip.white=T,as.is=T)
  stateStr<-c()
  for (code in stateCode){
    index = match(code,stateKey[,1])
    stateStr <- c(stateStr,stateKey[index,2])
  }
  tableZ[,"State"]<-stateStr
  
  
  #Element Type (i.e. max, min...)
  
  eleCode<-substring(tableZ[,1], 5,6)
  eleKey<- read.table("ElementKey.txt",sep="=",colClasses=c("character","character"),strip.white=T,as.is=T)
  eleStr<-c()
  for (code in eleCode){
    index = match(code,eleKey[,1])
    eleStr <- c(eleStr,eleKey[index,2])
  }
  tableZ[,"Element"]<-eleStr
  
  #Year
  
  yearCode<-substring(tableZ[,1], 7,10)
  yearKey<- read.table("YearKey.txt",sep=",",colClasses=c("character","character"),strip.white=T,as.is=T)
  yearStr<-c()
  for (code in yearCode){
    index = match(code,yearKey[,1])
    yearStr <- c(yearStr,yearKey[index,2])
  }
  rm(yearStr) #comment out for st type files
  yearStr<-yearCode   #comment out for st type files
  tableZ[,"Year"]<-yearStr
  return<-tableZ
}


importZ <- function(file) {
  Data<-read.table(file,sep= "",colClasses="character")
  Data<-code2strZ(Data)
  Data[,1]<- NULL
  colnames(Data) <- c("January","February","March","April","May","June","July","August","September","October","November","December","State","Element","Year")
  return(Data)
}

return(importZ(file))

}
location="C:/Users/Anderson Lab/Desktop/Nick Z/Weather Scraper/WeatherWork/rawData"
files=list.files(location)
cleanData= c()
for (file in files){
  clean<-DataCleanZ(paste(location,file,sep = "/"))
  cleanData <- rbind(cleanData,clean)
}


save(cleanData,file="cleanData.Rda")

}
