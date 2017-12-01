load("cleanData.Rda")
library("data.tree")

#convert to factors
#cleanData$Element <- as.factor(cleanData$Element)
#cleanData$State <- as.factor(cleanData$State)
cleanData$Year <- as.numeric(as.character(cleanData$Year))

#create data structure
data<-Node$new("Data Tree")
for (ele in unique(cleanData$Element)) {
  assign(paste(ele),data$AddChild(ele))
}
for (ele in data$children){
  for (yea in unique(cleanData$Year)){
    assign(paste(yea),ele$AddChild(yea))
  }
}



for (ele in data$children) {
  for (yea in ele$children){
    eval(parse(text=paste("data$'",ele$name,"'$'",yea$name,"'",sep=""))) <- cleanData[cleanData$Element==ele$name & cleanData$Year==yea$name,]
  }
}


