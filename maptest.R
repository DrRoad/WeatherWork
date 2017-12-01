x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap")
 #install.packages(x) # warning: uncommenting this may take a number of minutes
lapply(x, library, character.only = TRUE) 
Arizona<-readOGR(dsn="C:/Users/Anderson Lab/Downloads/tl_2012_04_cousub",layer = "tl_2012_04_cousub")
plot(Arizona,col="lightgrey")
plot(Arizona["Tucson"==Arizona$NAME,],col="turquoise",add = TRUE)
lat<- coordinates(gCentroid(Arizona))[[1]]
lng<- coordinates(gCentroid(Arizona))[[2]]



Nation<-readOGR(dsn="C:/Users/Anderson Lab/Downloads/cb_2016_us_nation_20m",layer="cb_2016_us_nation_20m")
Airport<-readOGR(dsn="C:/Users/Anderson Lab/Desktop/Nick Z/Weather Scraper/WeatherWork",layer="airprtx010g.shp")
ggplot()+geom_polygon(data=State,aes(x=long, y= lat,group=group), fill="violet",color="red")+coord_fixed(1.3)+geom_point(data=Airports,aes(x=Longitude,y=Latitude),color="black",size=0.000001)
Airports<-read.table("Airport_Codes_mapped_to_Latitude_Longitude_in_the_United_States.tsv",header = TRUE,as.is=TRUE)
Airports<-Airports[,c(3,2,1)]
Airports$Longitude<-Airports$Longitude*-1

#cloud of points
point.cloud<-function(long,lat) {
pointsLat<-rnorm(75,mean=lat,sd=0.75)
pointsLong<-rnorm(75,mean=long,sd=0.75)
distance<-sqrt((pointsLat-lat)^2+(pointsLong-long)^2)
points<-data.frame(pointsLong,pointsLat,distance)
return(geom_point(data=points,aes(x=pointsLong,y=pointsLat),alpha=(1-distance/max(points$distance))^4))
}