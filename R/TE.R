if(F){
  if (!require(zoo)){ 
  install.packages("zoo",repos="http://cran.us.r-project.org")
  require(zoo, quietly = T) 
}
if (!require(ggplot2)){ 
  install.packages("ggplot2",repos="http://cran.us.r-project.org")
  require(ggplot2, quietly = T) 
} 
if (!require(countrycode)){ 
  install.packages("countrycode",repos="http://cran.us.r-project.org")
  require(countrycode, quietly = T) 
} 
if (!require(RJSONIO)){ 
  install.packages("RJSONIO",repos="http://cran.us.r-project.org")
  require(RJSONIO, quietly = T) 
}
if (!require(rworldmap)){ 
  install.packages("rworldmap",repos="http://cran.us.r-project.org")
  require(rworldmap, quietly = T) 
} 
if (!require(reshape)){ 
  install.packages("reshape",repos="http://cran.us.r-project.org")
  require(reshape, quietly = T) 
} 
if (!require(mapproj)){ 
  install.packages("mapproj",repos="http://cran.us.r-project.org")
  require(mapproj, quietly = T) 
} 
if (!require(plyr)){ 
  install.packages("plyr",repos="http://cran.us.r-project.org")
  require(plyr, quietly = T) 
} 
if (!require(RCurl)){ 
  install.packages("RCurl",repos="http://cran.us.r-project.org")
  require(RCurl, quietly = T) 
}
#library(RCurl)
#library(plyr)
require(scales)
library(ellipse)
library(RColorBrewer)
#library(treemap)
}
library(countrycode)
is.formula<-plyr::is.formula
rbind.fill<-plyr::rbind.fill
options(stringsAsFactors = FALSE)

#all <- map_data("world")
#all$region[!is.na(countrycode(all$region,"country.name","iso3c"))] <- countrycode(all$region,"country.name","iso3c")[!is.na(countrycode(all$region,"country.name","iso3c"))]
countrycode_data=countrycode_data;

comparisonproblems=c("Balance of Trade","Business Confidence","Consumer Confidence","Current Account","Exports","Imports","Consumer Price Index (CPI)","Consumer Spending","Core Consumer Prices","Currency","Exchange Rate","External Debt","Foreign Bond Investment","Foreign Direct Investment","Harmonised Consumer Prices","Household Spending","Housing Index","Stock Market")
#d=te.countries(c,"G200")
#nr<-countrycode(d,"country.name","iso3c")

#unique((all[c('region','nr')])[order(all$region),])
#as.data.frame(cbind(d,nr))
#EUROPE=c("Albania","Andorra","Austria","Belarus","Belgium","Bosnia and Herzegovina","Bulgaria","Channel Islands","Croatia","Cyprus","Czech Republic","Denmark","Estonia","Euro area","Faeroe Islands","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Isle of Man","Italy","Kosovo","Latvia","Liechtenstein","Lithuania","Luxembourg","Macedonia","Malta","Moldova","Monaco","Montenegro","Netherlands","Norway","Poland","Portugal","Romania","Russia","San Marino","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland","Turkey","Ukraine","United Kingdom")

#install.packages("RCurl")


#RCURLhttpheader= c(Authorization = "Client 9541a8a3c3ccb5b:3ce8e344216b372")
#RCURLopts = list(ssl.verifypeer = FALSE)
#getURL(url="http://api2.tradingeconomics.com/api/country",httpheader=RCURLhttpheader,.opts=RCURLopts)

###
RCURLgetURL=function(url,k=NULL,s=NULL){
  if(is.null(k)) k="9541a8a3c3ccb5b"
  if(is.null(s)) s="3ce8e344216b372"
  RCURLhttpheader= c(Authorization = paste("Client ",k,":",s,sep=""))
  RCURLopts = list(ssl.verifypeer = FALSE,timeout = 10000)
  getURL(url,httpheader=RCURLhttpheader,.opts=RCURLopts)
}
##### OLD API CALLS
if(F){
set.auth=function(u="guest",p="guest"){
    c(u,p)
 }
te.connect=function(c){
  a = 'http://54.83.43.149/data.aspx';
  paste(a,'?u=',c[1],'&p=',c[2],'&f=csv',sep='');
}
te.get.mat=function(c){
  url = paste(te.connect(c), '&q=matrix', sep=''); #print(url);
  read.csv(url)
}
te.get.hist=function(c,country,indicator,d1="2005"){
  head=FALSE
  if(tolower(d1)=="last"){d1="2009";head=TRUE}
  url = paste(te.connect(c), "&","d1=",d1,"&q=historical&c=",URLencode(country),"&i=",URLencode(indicator),sep=""); #print(url);
  df = read.csv(url)
  if(is.null(df$DateTime)){return (NULL)}
  df$DateTime=as.Date(df$DateTime,"%m/%d/%Y")
  if(head){df=df[order(df$DateTime,decreasing=TRUE),][1,]}
  df
}
te.get.hist.multi=function(c,reqArray,d1="2005"){
  dataFrame=data.frame()
  for(i in 1:length(reqArray)){
    dataFrame=rbind(dataFrame,te.get.hist(c,strsplit(reqArray[i],":")[[1]][1],strsplit(reqArray[i],":")[[1]][2],d1))
  }
  dataFrame
}
te.get.hist.multi.free=function(c,contArray,indArray,d1="2005"){
  dataFrame=data.frame()
  temp=data.frame()
  for(i in 1:length(contArray)){
    for(j in 1:length(indArray)){
      temp=te.get.hist(c,contArray[i],indArray[j],d1)
      if(is.null(temp)){next}
      dataFrame=rbind(dataFrame,temp)
    }
  }
  dataFrame
}
te.get.hist.multi.free.na=function(c,contArray,indArray,d1="2005"){
  options(stringsAsFactors = FALSE)
  dataFrame=data.frame()
  temp=data.frame()
  for(i in 1:length(contArray)){
    for(j in 1:length(indArray)){
      temp=te.get.hist(c,contArray[i],indArray[j],d1)
      if(is.null(temp)){temp = as.data.frame(t(c(contArray[i],indArray[j],NA,NA)));names(temp)=c("Country","Category","DateTime","Value")}
      dataFrame=rbind(dataFrame,temp)
    }
  }
  dataFrame
}
historicalToMatrix = function(c,countries,indicators){
  options(stringsAsFactors = FALSE)
  newdf=data.frame()
  df=data.frame()
  myTempDF=data.frame()
  df=te.get.hist.multi.free(c,countries,indicators,"last")
  for(i in 1:length(countries))
  {
    #print(i)
    myTempDF=cbind(data.frame(t(df[tolower(df$Country)==tolower(countries[i]),c('Category','Value')])),countries[i])
    if(length(myTempDF)<length(indicators)+1){next}
    names(myTempDF)=c(myTempDF['Category',1:length(indicators)],'Country')
    newdf=rbind(newdf,myTempDF['Value',])
  }
  #return(list(message=paste(names(newdf),collapse=" # ")))
  
  newdf[,1:length(indicators)]<-lapply(newdf[,1:length(indicators)],as.numeric)
  #return(list(message=paste(paste(df,collapse=" J "),paste(newdf,collapse=" H "),collapse=" ! ")))
  newdf
}
}
################
te.connect.new=function(){
  a = 'https://teapi.azurewebsites.net';
  #a = 'http://api2.tradingeconomics.com';
  a
}
te.get.mat.new=function(contArray,indArray){
  contArray = unique(contArray)
  indArray = unique(indArray)
  if(contArray[1]=="all"){
    country="all"
  }else{
    country="rAppsOCPU"
    for(i in 1:length(contArray)){
      country=paste(country,contArray[i],sep=",")
    }
  }
  if(indArray[1]=="all"){
    indicator="all"
  }else{
  indicator="rAppsOCPU"
    for(i in 1:length(indArray)){
      indicator=paste(indicator,indArray[i],sep=",")
    }
  }
  url = paste(te.connect.new(), "/country/",URLencode(country),"/",URLencode(indicator),"?f=csv",sep=""); #print(url);
  #print(nchar(url))
  if(nchar(url)>310){
    if(contArray[1]=="all"){
      country1="all"
      country2="all"
    }else if(length(contArray)==1){
      country1=contArray[1]
      country2=contArray[1]
    }else{
      country1="rAppsOCPU"
      country2="rAppsOCPU"
      for(i in 1:round(length(contArray)/2)){
        country1=paste(country1,contArray[i],sep=",")
      }
      for(i in (round(length(contArray)/2)+1):length(contArray)){
        country2=paste(country2,contArray[i],sep=",")
      }
    }
    if(indArray[1]=="all"){
      indicator1="all"
      indicator2="all"
    }else if(length(contArray)==1){
      indicator1=indArray[1]
      indicator2=indArray[1]
    }else{
      indicator1="rAppsOCPU"
      indicator2="rAppsOCPU"
      for(i in 1:round(length(indArray)/2)){
        indicator1=paste(indicator1,indArray[i],sep=",")
      }
      for(i in (round(length(indArray)/2)+1):length(indArray)){
        indicator2=paste(indicator2,indArray[i],sep=",")
      }
    }
    df1=te.get.hist.multi.free.new(strsplit(country1,",")[[1]],strsplit(indicator1,",")[[1]],d1)
    df2=te.get.hist.multi.free.new(strsplit(country2,",")[[1]],strsplit(indicator2,",")[[1]],d1)
    df3=te.get.hist.multi.free.new(strsplit(country1,",")[[1]],strsplit(indicator2,",")[[1]],d1)
    df4=te.get.hist.multi.free.new(strsplit(country2,",")[[1]],strsplit(indicator1,",")[[1]],d1)
    df = rbind(df1,df2,df3,df4)
    df
  }else{
  df = read.csv(textConnection(RCURLgetURL(url)), row.names=NULL)
  if(is.null(df$Country)){return (NULL)}
  names(df) <- c("Country","Category","Title","DateTime","Value","Source","Unit","URL","CategoryGroup")
  df$DateTime=as.Date(df$DateTime,"%m/%d/%Y")
  df
  }
}
te.get.mat.mat.new=function(contArray){
  if(contArray[1]=="all"){
    country=""
  }else{
    country="rAppsOCPU"
    for(i in 1:length(contArray)){
      country=paste(country,contArray[i],sep=",")
    }
  }
  url = paste(te.connect.new(), "/matrix/",URLencode(country),"?f=csv",sep=""); #print(url);
  df = read.csv(textConnection(RCURLgetURL(url)), row.names=NULL)
  if(is.null(df$Country)){return (NULL)}
  df
}
te.get.hist.new=function(country,indicator,d1="2005-01-01"){
  url = paste(te.connect.new(), "/historical/country/",URLencode(country),"/indicator/",URLencode(indicator),"/",d1,"?f=csv",sep=""); #print(url);
  df = read.csv(textConnection(RCURLgetURL(url)), row.names=NULL)
  if(is.null(df$DateTime)){return (NULL)}
  df$DateTime=as.Date(df$DateTime,"%m/%d/%Y")
  df
}
#reqArray = c("Country:indicator","country:indicator", ... )
te.get.hist.multi.new=function(reqArray,d1="2005-01-01"){
  dataFrame=data.frame()
  for(i in 1:length(reqArray)){
    dataFrame=rbind(dataFrame,te.get.hist.new(strsplit(reqArray[i],":")[[1]][1],strsplit(reqArray[i],":")[[1]][2],d1))
  }
  dataFrame
}
#contArray = c("Country","country", ... ) | indArray = c("indicator","indicator", ... )
te.get.hist.multi.free.new=function(contArray,indArray,d1="2005-01-01"){
  contArray = unique(contArray)
  indArray = unique(indArray)
  if(contArray[1]=="all"){
    country="all"
  }else{
    country="rAppsOCPU"
    for(i in 1:length(contArray)){
      country=paste(country,contArray[i],sep=",")
    }
  }
  if(indArray[1]=="all"){
    indicator="all"
  }else{
    indicator="rAppsOCPU"
    for(i in 1:length(indArray)){
      indicator=paste(indicator,indArray[i],sep=",")
    }
  }
url = paste(te.connect.new(), "/historical/country/",URLencode(country),"/indicator/",URLencode(indicator),"/",d1,"?f=csv",sep=""); #print(url);
#print(nchar(url))
if(nchar(url)>310){
  if(contArray[1]=="all"){
    country1="all"
    country2="all"
  }else if(length(contArray)==1){
    country1=contArray[1]
    country2=contArray[1]
  }else{
    country1="rAppsOCPU"
    country2="rAppsOCPU"
    for(i in 1:round(length(contArray)/2)){
      country1=paste(country1,contArray[i],sep=",")
    }
    for(i in (round(length(contArray)/2)+1):length(contArray)){
      country2=paste(country2,contArray[i],sep=",")
    }
  }
  if(indArray[1]=="all"){
    indicator1="all"
    indicator2="all"
  }else if(length(contArray)==1){
    indicator1=indArray[1]
    indicator2=indArray[1]
  }else{
    indicator1="rAppsOCPU"
    indicator2="rAppsOCPU"
    for(i in 1:round(length(indArray)/2)){
      indicator1=paste(indicator1,indArray[i],sep=",")
    }
    for(i in (round(length(indArray)/2)+1):length(indArray)){
      indicator2=paste(indicator2,indArray[i],sep=",")
    }
  }
  df1=te.get.hist.multi.free.new(strsplit(country1,",")[[1]],strsplit(indicator1,",")[[1]],d1)
  df2=te.get.hist.multi.free.new(strsplit(country2,",")[[1]],strsplit(indicator2,",")[[1]],d1)
  df3=te.get.hist.multi.free.new(strsplit(country1,",")[[1]],strsplit(indicator2,",")[[1]],d1)
  df4=te.get.hist.multi.free.new(strsplit(country2,",")[[1]],strsplit(indicator1,",")[[1]],d1)
  df = rbind(df1,df2,df3,df4)
  df
}else{
df = read.csv(textConnection(RCURLgetURL(url)), row.names=NULL)
if(is.null(df$DateTime)){return (NULL)}
df$DateTime=as.Date(df$DateTime,"%m/%d/%Y")
df
}
}
historicalToMatrix.new = function(c,countries,indicators){
  options(stringsAsFactors = FALSE)
  newdf=data.frame()
  df=data.frame()
  myTempDF=data.frame()
  df=te.get.mat.new(countries,indicators)
  for(i in 1:length(countries))
  {
    #print(i)
    myTempDF=cbind(data.frame(t(df[tolower(df$Country)==tolower(countries[i]),c('Category','Value')])),countries[i])
    if(length(myTempDF)<length(indicators)+1){next}
    names(myTempDF)=c(myTempDF['Category',1:length(indicators)],'Country')
    newdf=rbind(newdf,myTempDF['Value',])
  }
  #return(list(message=paste(names(newdf),collapse=" # ")))
  
  newdf[,1:length(indicators)]<-lapply(newdf[,1:length(indicators)],as.numeric)
  #return(list(message=paste(paste(df,collapse=" J "),paste(newdf,collapse=" H "),collapse=" ! ")))
  newdf
}
te.plot=function(c,country,indicator,d1="2005-01-01",opts=NULL){
  #dataFrame=te.get.hist(c,country,indicator,d1)
  dataFrame=te.get.hist.new(country,indicator,d1)
  if(is.null(dataFrame)){stop("Return to Sender: No Such Country - Indicator Pair.")}
  if(length(dataFrame)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
  if(length(dataFrame$Close)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
  
  labelsbreak=paste(round(as.numeric(dataFrame$DateTime[length(dataFrame$DateTime)]-dataFrame$DateTime[1], units = "days")/300),"month")
  
  #while(round((max(dataFrame$Value)-min(dataFrame$Value))/10,1)==0)
  #{    dataFrame$Value <- dataFrame$Value*100  }
  #ggplot(dataFrame, aes(x=DateTime, y=Value)) + 
  ggplot(dataFrame, aes(x=DateTime, y=Close)) + 
    geom_line(colour="#4863A0",size = 1, aes(group = 1)) + 
    #geom_point(size = 3) +
    #scale_colour_manual(values = c("7.4" = "red","#4863A0")) +
    #scale_x_date(breaks = labelsbreak,labels=date_format("%m/%y")) + 
    #scale_y_continuous(breaks = round(seq(min(dataFrame$Value), max(dataFrame$Value), by = round((max(dataFrame$Value)-min(dataFrame$Value))/6,1)),1)) +
    xlab("") + 
    #ylab(dataFrame$Category[1]) + 
    ylab("") + 
    ggtitle(paste( dataFrame$Country[1] ,"-", dataFrame$Category[1])) + 
    theme(
        #plot.title = element_text(face="bold"), 
        plot.title = element_blank(), 
        panel.border=element_blank(),
        axis.line=element_line(colour = "grey",size=.3),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey",size=.2),
        panel.grid.major = element_line(colour = "grey",size=.3))
  #+ geom_hline(yintercept  = mean(dataFrame$Value))
}
#reqArray=c("Portugal:Unemployment Rate","Greece:Unemployment Rate","Spain:Unemployment Rate")
te.plot.multi=function(c,country,indicator=NULL,d1="2005-01-01",opts=NULL){
options(stringsAsFactors = FALSE)
position=ifelse(!is.null(opts),ifelse(!opts,"none","right"),"right")
if(is.null(indicator))
{
  titl="Multiple Plot"
  axis=element_blank()
  #dataFrame=te.get.hist.multi(c,country,d1)
  dataFrame=te.get.hist.multi.new(country,d1)
  
  if(is.null(dataFrame)){stop("Return to Sender: No Such Country - Indicator Pair.")}
  if(length(dataFrame)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
  if(length(dataFrame$Close)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
  
  for(rows in 1:dim(dataFrame)[1])
    dataFrame$Category[rows] <- paste(substr(strsplit(dataFrame$Category[rows]," ")[[1]],1,5),collapse=" ")
  
  dataFrame$Country[!is.na(countrycode(dataFrame$Country,"country.name","iso3c"))] <- countrycode(dataFrame$Country,"country.name","iso3c")[!is.na(countrycode(dataFrame$Country,"country.name","iso3c"))]
  dataFrame$Country[tolower(dataFrame$Country)=="euro area"] <- "EA17"
  dataFrame$Indicator <- sapply(dataFrame, function(x) paste(substr(dataFrame$Country,1,4),dataFrame$Category,sep=" - "))[,1] 
  for(ca in unique(dataFrame$Indicator))
    #dataFrame[dataFrame$Indicator==ca,]$Value=as.numeric(scale(dataFrame[dataFrame$Indicator==ca,]$Value))
    dataFrame[dataFrame$Indicator==ca,]$Close=as.numeric(scale(dataFrame[dataFrame$Indicator==ca,]$Close))
}else if(length(country)==1){
  titl=paste(country)
  axis=element_blank()
  #dataFrame=te.get.hist.multi.free(c,country,indicator,d1)
  dataFrame=te.get.hist.multi.free.new(country,indicator,d1)
  
  if(is.null(dataFrame)){stop("Return to Sender: No Such Country - Indicator Pair.")}
  if(length(dataFrame)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
  if(length(dataFrame$Close)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
  
  for(rows in 1:dim(dataFrame)[1])
    dataFrame$Category[rows] <- paste(substr(strsplit(dataFrame$Category[rows]," ")[[1]],1,5),collapse=" ")
  
  dataFrame$Indicator <- sapply(dataFrame, function(x) dataFrame$Category)[,1] 
  for(ca in unique(dataFrame$Indicator))
    #dataFrame[dataFrame$Indicator==ca,]$Value=as.numeric(scale(dataFrame[dataFrame$Indicator==ca,]$Value))
    dataFrame[dataFrame$Indicator==ca,]$Close=as.numeric(scale(dataFrame[dataFrame$Indicator==ca,]$Close))
}else if(length(indicator)==1){
  titl=paste(indicator)
  axis=element_text()
  #dataFrame=te.get.hist.multi.free(c,country,indicator,d1)
  dataFrame=te.get.hist.multi.free.new(country,indicator,d1)
      
 if(is.null(dataFrame)){stop("Return to Sender: No Such Country - Indicator Pair.")}
 if(length(dataFrame)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
 if(length(dataFrame$Close)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
      
 #dataFrame$Country[!is.na(countrycode(dataFrame$Country,"country.name","iso3c"))] <- countrycode(dataFrame$Country,"country.name","iso3c")[!is.na(countrycode(dataFrame$Country,"country.name","iso3c"))]
 #dataFrame$Country[tolower(dataFrame$Country)=="euro area"] <- "EA17"
 #dataFrame$Indicator <- sapply(dataFrame, function(x) paste(substr(dataFrame$Country,1,4),dataFrame$Category,sep=" - "))[,1] 
 dataFrame$Indicator <- dataFrame$Country 
 
 if(indicator %in% comparisonproblems){
   axis=element_blank()
 for(ca in unique(dataFrame$Indicator))
     #dataFrame[dataFrame$Indicator==ca,]$Value=as.numeric(scale(dataFrame[dataFrame$Indicator==ca,]$Value))
     dataFrame[dataFrame$Indicator==ca,]$Close=as.numeric(scale(dataFrame[dataFrame$Indicator==ca,]$Close))
 }
}else{
  titl=paste("Cross-Indicators Analysis")
  axis=element_blank()
  #dataFrame=te.get.hist.multi.free(c,country,indicator,d1)
  dataFrame=te.get.hist.multi.free.new(country,indicator,d1)
  if(is.null(dataFrame)){stop("Return to Sender: No Such Country - Indicator Pair.")}
  if(length(dataFrame)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
  
  for(rows in 1:dim(dataFrame)[1])
    dataFrame$Category[rows] <- paste(substr(strsplit(dataFrame$Category[rows]," ")[[1]],1,5),collapse=" ")
  
  dataFrame$Country[!is.na(countrycode(dataFrame$Country,"country.name","iso3c"))] <- countrycode(dataFrame$Country,"country.name","iso3c")[!is.na(countrycode(dataFrame$Country,"country.name","iso3c"))]
  dataFrame$Country[tolower(dataFrame$Country)=="euro area"] <- "EA17"
  dataFrame$Indicator <- sapply(dataFrame, function(x) paste(substr(dataFrame$Country,1,4),dataFrame$Category,sep=" - "))[,1] 
  
  for(ca in unique(dataFrame$Indicator))
    #dataFrame[dataFrame$Indicator==ca,]$Value=as.numeric(scale(dataFrame[dataFrame$Indicator==ca,]$Value))
    dataFrame[dataFrame$Indicator==ca,]$Close=as.numeric(scale(dataFrame[dataFrame$Indicator==ca,]$Close))
}

  labelsbreak=paste(round(as.numeric(max(dataFrame$DateTime)-min(dataFrame$DateTime), units = "days")/300),"month")
  #ggplot(dataFrame,aes(x=DateTime, y=Value, colour=Indicator)) +
 ggplot(dataFrame,aes(x=DateTime, y=Close, colour=Indicator)) + 
    geom_line() + 
    #geom_point(size = 3) +
    #scale_colour_manual(values = c("7.4" = "red","#4863A0")) +
    #scale_x_date(breaks = labelsbreak,labels=date_format("%m/%y")) + 
    #scale_y_continuous(breaks = round(seq(min(dataFrame$Value), max(dataFrame$Value), by = round((max(dataFrame$Value)-min(dataFrame$Value))/6,1)),1)) +
    xlab("") + ylab("") +
    theme(axis.text.y = axis, 
          panel.border=element_blank(),
          axis.line=element_line(colour = "grey",size=.3),
          panel.background = element_blank(),
          panel.grid.minor = element_line(colour = "grey",size=.2),
          panel.grid.major = element_line(colour = "grey",size=.3)) +
  theme(legend.position=position)

    #+ ggtitle(titl) 
    #ylab(dataFrame$Category[1]) + 
    #ggtitle(paste( dataFrame$Country[1] ,"-", dataFrame$Category[1])) + 
    #theme(plot.title = element_text(face="bold"))
}
te.plot.compare.scale=function(c,country,indicator,d1=NULL,opts=NULL){
  if(length(country)>70){stop("Too many indicators to show. Please re-do selection.")}
  #d=te.get.hist.multi.free(c,country,indicator,"last")
  d=te.get.mat.new(country,indicator)
  if(is.null(d)){stop("Return to Sender: No Such Country - Indicator Pair.")}
  if(length(d)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
  titl = paste(indicator)
  d <- within(d,{Country <- reorder(Country,Value)})
  #d$Country <- countrycode(d$Country,"country.name","iso3c")
  qplot(Value, Country, data=d) +
    facet_grid(Category ~ ., scales = "free", space = "free") + 
    theme(axis.text.y=element_text(color="black"),
          strip.text.y = element_text(size=9),
          strip.background = element_rect(fill="#4775FF"),
           panel.background = element_rect(fill="#F7FAFC"),
         # panel.background = element_blank(),
         # panel.border=element_rect(fill=NA,colour="black"),
          panel.grid.major=element_line(colour="grey"),
          legend.position = "none") +
    geom_point(size = 7, shape=22, colour="black", aes(fill = Value)) + scale_fill_gradient(high = "red",low="black") +
    xlab("") + ylab("") 
  #+ ggtitle(titl)
}
te.plot.compare=function(c,country,indicator,d1="NULL",opts=NULL){
  #df=data.frame()
  #assign("df",historicalToMatrix(c,country,indicator), envir = environment())
  
  df=data.frame()
  assign("df",historicalToMatrix.new(c,country,indicator), envir = environment())
  
  if(is.null(df)){stop("Return to Sender: No Such Country - Indicator Pair.")}
  if(length(df)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
  df$Country[!is.na(countrycode(df$Country,"country.name","iso3c"))] <- countrycode(df$Country,"country.name","iso3c")[!is.na(countrycode(df$Country,"country.name","iso3c"))]
  df$Country[tolower(df$Country)=="euro area"] <- "EA17"
  titl=paste(indicator[1], indicator[2],sep=" ~ ")
  ggplot(df, aes(get(names(df)[1]), get(names(df)[2])),environment=environment()) + 
    geom_point(shape=22,fill="black") +
    # geom_point(aes(colour = get(names(df)[3]))) +
    # scale_colour_gradient(high = "red") +
    theme(panel.border=element_blank(),
          axis.line=element_line(colour = "grey",size=.3),
          panel.background = element_blank(),
          panel.grid.minor = element_line(colour = "grey",size=.2),
          panel.grid.major = element_line(colour = "grey",size=.3)) +
    geom_text(fontface=3, label=df$Country, hjust=0, vjust=0) +
    xlab(indicator[1]) + ylab(indicator[2]) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 1),se=F,colour="#08088A") +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2),se=F,colour="#08088A", size = .2) 
  #+ ggtitle(titl)
}
## ACCEPTS: G99 | Continent | EMERGING99 | PLUNGING99 ##
te.countries = function(c,group="G20"){
  m = te.get.mat.mat.new("all")
  if(substr(tolower(group),1,1)=="g" && !is.na(as.numeric(substr(tolower(group),2,nchar(group)))))
  {
    return (trim(head(m[order(m$GDP, decreasing = TRUE),],n=as.numeric(substr(tolower(group),2,nchar(group))))$Country))
  }
  if(substr(tolower(group),1,6)=="europe")
  {
    n=10
    if(!is.na(as.numeric(substr(group,7,nchar(group)))))
    {
      n=as.numeric(substr(group,7,nchar(group)))
    }
    return (trim(head((m[trim(m$Continent)=="Europe",])[order(m[trim(m$Continent)=="Europe",]$GDP, decreasing = TRUE),]$Country,n)))
  }
  if(substr(tolower(group),1,6)=="africa")
  {
    n=10
    if(!is.na(as.numeric(substr(group,7,nchar(group)))))
    {
      n=as.numeric(substr(group,7,nchar(group)))
    }
    return (trim(head((m[trim(m$Continent)=="Africa",])[order(m[trim(m$Continent)=="Africa",]$GDP, decreasing = TRUE),]$Country,n)))
  }
  if(substr(tolower(group),1,4)=="asia")
  {
    n=10
    if(!is.na(as.numeric(substr(group,5,nchar(group)))))
    {
      n=as.numeric(substr(group,5,nchar(group)))
    }
    return (trim(head((m[trim(m$Continent)=="Asia",])[order(m[trim(m$Continent)=="Asia",]$GDP, decreasing = TRUE),]$Country,n)))
  }
  if(substr(tolower(group),1,7)=="america")
  {
    n=10
    if(!is.na(as.numeric(substr(group,8,nchar(group)))))
    {
      n=as.numeric(substr(group,8,nchar(group)))
    }
    return (trim(head((m[trim(m$Continent)=="America",])[order(m[trim(m$Continent)=="America",]$GDP, decreasing = TRUE),]$Country,n)))
  }
  if(substr(tolower(group),1,9)=="australia")
  {
    n=10
    if(!is.na(as.numeric(substr(group,10,nchar(group)))))
    {
      n=as.numeric(substr(group,10,nchar(group)))
    }
    return (trim(head((m[trim(m$Continent)=="Australia",])[order(m[trim(m$Continent)=="Australia",]$GDP, decreasing = TRUE),]$Country,n)))
  }
  if(substr(tolower(group),1,8)=="emerging")
  {
    n=10
    if(!is.na(as.numeric(substr(group,9,nchar(group)))))
    {
      n=as.numeric(substr(group,9,nchar(group)))
    }
    return (trim(head(m[order(m$GDP_Growth_YoY, decreasing = TRUE),],n)$Country))
  }
  if(substr(tolower(group),1,8)=="plunging")
  {
    n=10
    if(!is.na(as.numeric(substr(group,9,nchar(group)))))
    {
      n=as.numeric(substr(group,9,nchar(group)))
    }
    return (trim(head(m[order(m$GDP_Growth_YoY, decreasing = FALSE),],n)$Country))
  }
}
te.indicators = function(c,group="labour"){
  #m = te.get.mat(c)
c("GDP Annual Growth Rate",
"Inflation Rate",
"Interest Rate",
"Imports",
"GDP",
"GDP Growth Rate",
"Non Farm Payrolls",
"Unemployment Rate",
"Business Confidence",
"Consumer Confidence",
"Industrial Production",
"Retail Sales YoY",
"Exports")
}
trim=function (x) gsub("^\\s+|\\s+$", "", x)
Split=function(x) {sapply(strsplit(x," "), paste, collapse=".")}
te.geomap=function(c,country="NULL",indicator,d1="",opts=NULL){
  if(is.null(opts)){opts="World"}
  if(opts=="World"){
    ct=te.countries(c,"G200")
  }else if(opts=="Africa"){
    ct=te.countries(c,"Africa200")
  }else if(opts=="Europe"){
    ct=te.countries(c,"Europe200")
  }else if(opts=="Asia"){
    ct=te.countries(c,"Asia200")
  }else{
    ct=te.countries(c,"G200")
  }
  #d=te.get.hist.multi.free.na(c,ct,indicator,"last")
  d=te.get.mat.new("all",indicator)
  d=d[d$Country %in% ct,]
  
  d$region[!is.na(countrycode(d$Country,"country.name","iso3c"))] <- countrycode(d$Country,"country.name","iso3c")[!is.na(countrycode(d$Country,"country.name","iso3c"))]
  d[is.na(countrycode(d$Country,"country.name","iso3c")),]$region <- substr(d[is.na(countrycode(d$Country,"country.name","iso3c")),]$Country,1,3)
  d$region[d$Country=="Congo"] <- "COD"
  d$Value <- as.numeric(d$Value)
  
  #cnames <-aggregate(cbind(long, lat) ~ region, data = all, FUN = function(x) mean(range(x)))
  #cnames$angle <-0
  #head(cnames)
  
  sPDF <- joinCountryData2Map(d,joinCode = "ISO3",nameJoinColumn="region")[-which(getMap()$ADMIN=="Antarctica"),]
  
  par(mai=c(0,0,0,0),mar=c(0,0,1,0),oma=c(0,0,0,0),xaxs="i",yaxs="i")
  colourPalette <- brewer.pal(20,"YlOrRd")
  mapCountryData(sPDF, 
                 nameColumnToPlot="Value", 
                 addLegend=FALSE,
                 colourPalette=colourPalette,
                 oceanCol="lightblue",
                 borderCol="black",
                 missingCountryCol="grey",
                 mapTitle="", #paste(opts,indicator,sep=" - "),
                 mapRegion=opts,
                 numCats=30)
  
  #ggplot(d, aes(map_id = region)) + 
  #  geom_map(aes(fill = Value),colour="grey10", map = all) + 
  #  expand_limits(x = all$long, y = all$lat) +
  #  scale_fill_gradient2(low="red", high=muted("green"),mid = "#669900", midpoint = (max(d$Value,na.rm=T)-min(d$Value,na.rm=T))/2) +
  #  theme(legend.position = "bottom",
  #        axis.ticks = element_blank(), 
  #        axis.title = element_blank(), 
  #        axis.text =  element_blank(),
  #        panel.background = element_rect(fill='#D6E0FF')) +
    #geom_text(cnames, aes(long, lat, label = region, angle=angle, map_id=NULL), size=2.5) + 
  #  guides(fill = guide_colorbar(barwidth = 10, barheight = 0.5)) 
}
te.heat.map=function(c,country,indicator,d1="NULL",opts=NULL){
  library(scales)
  library(plyr)
  #df=te.get.hist.multi.free(c,country,indicator,"last")
  df=te.get.mat.new(country,indicator)
  df <- df[c("Country","Category","DateTime","Value")]
  if(is.null(df)){stop("Return to Sender: No Such Country - Indicator Pair.")}
  if(length(df)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
  df <- ddply(df, .(Category), transform, rescale = rescale(Value))
  ggplot(df, aes(Category, Country)) + 
    geom_tile(aes(fill = rescale),colour = "white") + 
    scale_fill_gradient2(low = "steelblue",high = "red",midpoint=.5) +
    theme_grey(base_size = 9) + 
    labs(x = "",y = "") + 
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) + 
    theme(legend.position = "none",axis.ticks = element_blank(), 
          axis.text.x = element_text(size = 15, angle = 310, hjust = 0, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "#000052"))
  #+ ggtitle("Countries ~ Indicators")
}
te.tree.map=function(c,country,indicator,d1="NULL",opts=NULL){
  options(stringsAsFactors = FALSE)

  #df=te.get.hist.multi.free(c,country,indicator,"last")
  df=te.get.mat.new(country,indicator)
  df <- df[c("Country","Category","DateTime","Value")]
  
  if(is.null(df)){stop("Return to Sender: No Such Country - Indicator Pair.")}
  if(length(df)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
  
  df$Country[!is.na(countrycode(df$Country,"country.name","iso3c"))] <- countrycode(df$Country,"country.name","iso3c")[!is.na(countrycode(df$Country,"country.name","iso3c"))]
  df$Country[tolower(df$Country)=="euro area"] <- "EA17"
  
  df$Country[df$Value<0] <- paste("(-)",df$Country[df$Value<0],sep="")
  df$Value <- abs(df$Value)
  treemap::treemap(df, 
                   index=c("Country"), 
                   vSize="Value", 
                   title=indicator,
                   type="value",
                   vColor="Value",
                   palette="RdBu",
                   aspRatio=1,
                   range=c(min(df$Value),max(df$Value)))
}
#te.tree.map(c,countries,"GDP")

te.unit.converter=function(df){
  df$Unit<-tolower(df$Unit)
  for(rows in 1:dim(df)[1]){
    if(length(grep("thousand million", df[rows,]$Unit))>0){
      df[rows,]$Value = df[rows,]$Value*100000
    }
    else if(length(grep("hundred million", df[rows,]$Unit))>0){
      df[rows,]$Value = df[rows,]$Value*100000000
    }
    else if(length(grep("billion", df[rows,]$Unit))>0){
      df[rows,]$Value = df[rows,]$Value*1000000000
    }
    else if(length(grep("million", df[rows,]$Unit))>0){
      df[rows,]$Value = df[rows,]$Value*1000000
    }
    else if(length(grep("hundred", df[rows,]$Unit))>0){
      df[rows,]$Value = df[rows,]$Value*100
    }
    else if(length(grep("thousand", df[rows,]$Unit))>0){
      df[rows,]$Value = df[rows,]$Value*1000
    } 
  }  
  df
}

te.pie.chart=function(c,country,indicator,d1="NULL",opts=NULL){
  options(stringsAsFactors = FALSE)
  
  #df=te.get.hist.multi.free(c,country,indicator,"last")
  if(length(unique(country))>1 && length(indicator)>1){
  df=te.get.mat.new(country,indicator[1])}else{df=te.get.mat.new(country,indicator)}
  df <- df[c("Country","Category","DateTime","Value","Unit")]
  
  if(is.null(df)){stop("Return to Sender: No Such Country - Indicator Pair.")}
  if(length(df)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
  
  if(length(unique(df$Category))==1) df = te.unit.converter(df)
  
  for(rows in 1:dim(df)[1])
    df$Category[rows] <- paste(substr(strsplit(df$Category[rows]," ")[[1]],1,5),collapse=" ")
  
  df$Country[!is.na(countrycode(df$Country,"country.name","iso3c"))] <- countrycode(df$Country,"country.name","iso3c")[!is.na(countrycode(df$Country,"country.name","iso3c"))]
  df$Country[tolower(df$Country)=="euro area"] <- "EA17"
  df$Country[df$Value<0] <- paste("(-)",df$Country[df$Value<0],sep="")
  df$Value <- abs(df$Value)
  df$Value <- df$Value/sum(df$Value)
  df$p <- cumsum(df$Value)-diff(c(0,cumsum(df$Value)))*(1-0.5)
  
  df$Indicator <- df$Country
  if(length(unique(df$Country))==1) df$Indicator <- df$Category
  ggplot(data=df, aes(x=factor(1),y=Value, fill=factor(Indicator), weight=Value))+ 
    geom_bar(width=1,stat="identity",colour="black") + 
    coord_polar(theta="y")  + 
    geom_text(aes(x=1.5,y=p, angle=-p*360,label=Indicator,vjust=0)) +
    theme_bw() + 
    theme(axis.title.x = element_blank(), 
          axis.title.y= element_blank(), 
          axis.text.x=element_blank(), 
          axis.text.y=element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.border = element_blank(), 
          axis.ticks=element_blank(),
          legend.position = "none") 
  #+ ggtitle(indicator)
}
te.table=function(c,countries,indicators,d1="2005",what="NULL"){
  if(what=="te.plot.compare.scale"|| what=="te.plot.compare" || what=="te.geomap" || what=="te.tree.map" || what=="te.heat.map"){d1="last"}
  if(what=="te.geomap"){countries=te.countries(c,"G200")}
  if(what=="te.correlation.matrix"){d1="1950"}
  df=te.get.hist.multi.free.new(c,countries,indicators,d1)
  if(is.null(df)){stop("Return to Sender: No Such Country - Indicator Pair.")}
  if(length(df)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
  #df$DateTime <- as.character(as.yearmon(df$DateTime))
  #df$DateTime <- as.character(as.yearmon(df$DateTime))
  #df
  as.list(df)
}
#te.table(c,"argentina","youth unemployment rate")
te.correlation.matrix=function(c,country,indicator,d1="NULL",opts=NULL){
  options(stringsAsFactors = FALSE)
  
  #df=te.get.hist.multi.free(c,country,indicator,d1="2012")
  df=te.get.hist.multi.free.new(country,indicator,d1="2007-12-01")
  
  if(is.null(df)){stop("Return to Sender: No Such Country - Indicator Pair.")}
  if(length(df)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
  df$Country[!is.na(countrycode(df$Country,"country.name","iso3c"))] <- countrycode(df$Country,"country.name","iso3c")[!is.na(countrycode(df$Country,"country.name","iso3c"))]
  df$Country[tolower(df$Country)=="euro area"] <- "EA17"  
  if(length(country)==1){
    titl=paste(country,"Indicators",sep=" - ")
    df$Indicator <- sapply(df, function(x) paste(df$Category,sep=""))[,1] 
  }else if(length(indicator)==1){
    titl=paste(indicator,"Countries",sep=" - ")
    df$Indicator <- sapply(df, function(x) paste(substr(df$Country,1,4),sep=""))[,1] 
  }else{
    titl="TODO"
    df$Indicator <- sapply(df, function(x) paste(substr(df$Country,1,4),df$Category,sep=" - "))[,1] 
  }
  df$DateTime <- as.yearmon(df$DateTime)
  #df$DateTime <- as.yearqtr(df$DateTime)
  ndf = cast(df,DateTime ~ Indicator,value = "Close",fun.aggregate="mean")
  for(cols in 2:dim(ndf)[2]){ndf[!complete.cases(ndf[cols]),cols]<-NA; ndf[[cols]]<-na.approx(zoo(ndf[[cols]],as.yearmon(ndf[[1]],"%b%y")),na.rm=F)}
  #ndf
  names=names(ndf)
  #ndf=as.data.frame(na.omit(na.trim(na.approx(ndf[,2:length(names(ndf))]))))
  #names(ndf)=names[2:length(names)]
  #if(length(na.omit(na.trim(ndf))[,1])<3){stop("Too Little Info. One or more Indicators are sparse in data, re-do selection.")}
  #plot(na.approx(ndf[,2]))
  mdata=cor(na.approx(ndf[,2:length(names)],na.rm=F),use = "pairwise.complete.obs")
  #mdata=cor(as.data.frame(ndf[,1:length(names(ndf))]))
  plotcorrInternal <- function() {
    if (i == j && !diag) 
      return()
    if (!numbers) {
      mat[1, 2] <- mdata[i, j]
      mat[2, 1] <- mat[1, 2]
      ell <- ellipse(mat, t = 0.43)
      ell[, 1] <- ell[, 1] + j 
      ell[, 2] <- ell[, 2] + length(rows) + 1 - i
      polygon(ell, col = col[i, j])
      if (outline) 
        lines(ell)
    }
    else {
      text(j + 0.3, length(rows) + 1 - i , 
           round(100 * mdata[i, j], 0), adj=c(.5,.5),cex = .9,col=colorsnum[5*mdata[i,j] + 6])
    }
  }
  colors = c("#08519C","#3182BD","#6BAED6","#BDD7E7","#EFF3FF","white","#FEE5D9","#FCAE91","#FB6A4A","#DE2D26","#A50F15")
  colorsnum = c("#08519C","#3182BD","#6BAED6","#BDD7E7","#CCD2E5","#E4E2B7","#D4BEB4","#FCAE91","#FB6A4A","#DE2D26","#A50F15")
  savepar <- par(pty = "m", mar=c(0,0,1,0),oma=c(0,0,1,0))
  on.exit(par(savepar))
  plot.new()
  par(new = TRUE)
  rowdim <- dim(mdata)[1]
  coldim <- dim(mdata)[2]
  rowlabs <- dimnames(mdata)[[1]]
  collabs <- dimnames(mdata)[[2]]
  if (is.null(rowlabs)) 
    rowlabs <- 1:rowdim
  if (is.null(collabs)) 
    collabs <- 1:coldim
  rowlabs <- as.character(rowlabs)
  collabs <- as.character(collabs)
  
  col=colors[5*mdata + 6]
  col <- rep(col, length = length(mdata))
  dim(col) <- dim(mdata)
  cols <- 1:coldim
  rows <- 1:rowdim
  maxdim <- max(length(rows), length(cols))
  plt <- par("plt")
  xlabwidth <- max(strwidth(rowlabs[rows], units = "figure",cex = .1))/(plt[2] - plt[1])
  xlabwidth <- xlabwidth * maxdim/(1 - xlabwidth)
  ylabwidth <- max(strwidth(collabs[cols], units = "figure",cex = .1))/(plt[4] - plt[3])
  ylabwidth <- ylabwidth * maxdim/(1 - ylabwidth)
  
  plot(c(-xlabwidth - 0.5, maxdim + 0.5), c(0, maxdim + 1.5 + 
       ylabwidth), type = "n", bty = "n", axes = F, xlab = "", 
       ylab = "", asp = 1, cex.lab = .5)
  #title(titl,cex.main = 1)
  title("")
  text(rep(0, length(rows)), length(rows):1, labels = rowlabs[rows], 
       adj = 1, cex = .8)
  text(cols, rep(length(rows) + 1, length(cols)), 
       labels = collabs[cols], srt = 25, adj = 0, cex = .7)
  
  mat <- diag(c(1, 1))
  diag=F
  numbers=F
  outline=T
  for (i in 1:dim(mdata)[1]) {
    for (j in 1:dim(mdata)[2]) {
      
      if ((i >= j)) {
        numbers=F
        plotcorrInternal()
      }
      else if ((i <= j)) {
        numbers=T
        plotcorrInternal()
      }
    }
  }
}
#dataFrame=te.get.hist.multi.free(c,country,indicator,date)
#te.plot.multi(c,c("United States:Unemployment Rate","United States:Inflation Rate"),date="2005")
#te.plot.compare(c("r@tradingeconomics.com","Lisboa!1"),countries,indicators)
#c=set.auth("r@tradingeconomics.com","Lisboa!1")
#te.plot.compare(c,EUROPE[20:length(EUROPE)],indicators)
#te.plot.compare.scale(c,EUROPE[1:20],"unemployment rate")
#dataFrame=te.get.hist.multi.free(c,EUROPE,"unemployment rate",d1="last")
#te.plot.multi(c,reqArray)
#te.plot(c,"united states","unemployment rate")
#te.countries(c,"G20")
#te.geomap(c,"inflation rate")

#EUROPE=
#AFRICA=
#G20=
#AMERICAS=
#ASIA= 

te.testObj = function(obj){
  
  plot(1:10,title=obj)
}


indicators=c("gdp annual growth rate","inflation rate")
countries=c("portugal","greece","spain","united states","united kingdom","japan")

panel.cor=function(x, y, digits=2, prefix="", cex.cor){
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- cor(x, y,use="pairwise.complete.obs")   
  test <- cor.test(x,y,use="pairwise.complete.obs") 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("****", "***", "**", "*", " ")) 
  text(0.5, 0.5, round(100*r),cex=3) 
  text(.8, .85, Signif,col=2) 
}
te.simplecorrelation.matrix=function(c,country,indicator,d1="NULL",opts=NULL){
  options(stringsAsFactors = FALSE)
  #df=te.get.hist.multi.free(c,country,indicator,d1="2012")
  df=te.get.hist.multi.free.new(country,indicator,d1="2007-12-01")
  if(is.null(df)){stop("Return to Sender: No Such Country - Indicator Pair.")}
  if(length(df)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
  
  for(rows in 1:dim(df)[1])
  df$Category[rows] <- paste(substr(strsplit(df$Category[rows]," ")[[1]],1,5),collapse=" ")
  df$Country[!is.na(countrycode(df$Country,"country.name","iso3c"))] <- countrycode(df$Country,"country.name","iso3c")[!is.na(countrycode(df$Country,"country.name","iso3c"))]
  df$Country[tolower(df$Country)=="euro area"] <- "EA17"  
  if(length(country)==1){
  titl=paste(country,"Indicators",sep=" - ")
  df$Indicator <- sapply(df, function(x) paste(df$Category,sep=""))[,1] 
}else if(length(indicator)==1){
  titl=paste(indicator)
  df$Indicator <- sapply(df, function(x) paste(substr(df$Country,1,4),sep=""))[,1] 
}else{
  titl="TODO"
  df$Indicator <- sapply(df, function(x) paste(substr(df$Country,1,4),df$Category,sep=" - "))[,1] 
}
  df$DateTime <- as.yearmon(df$DateTime)
  #df$DateTime <- as.yearqtr(df$DateTime)
  ndf = cast(df,DateTime ~ Indicator,value = "Close",fun.aggregate="mean")
  for(cols in 2:dim(ndf)[2]){ndf[!complete.cases(ndf[cols]),cols]<-NA; ndf[[cols]]<-na.approx(zoo(ndf[[cols]],as.yearmon(ndf[[1]],"%b%y")),na.rm=F)}
  #ndf
  names=names(ndf)
  if(length(na.omit(na.trim(ndf))[,1])<3){stop("Too Little Info. One or more Indicators are sparse in data, re-do selection.")}
  ndf=as.data.frame(na.omit(na.approx(na.trim(ndf[,2:length(names(ndf))]))))
  names(ndf)=names[2:length(names)]
  pairs(as.data.frame(ndf[,1:length(names(ndf))]),
        lower.panel=panel.smooth, upper.panel=panel.cor,gap=0.1,
        cex=.1,cex.labels=1.2,main="")
}

## Function for arranging ggplots. use png(); arrange(p1, p2, ncol=1); dev.off() to save.
#require(grid)
#vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)

#pl1 = te.plot.multi(1,country,indicator[1])
#pl2 = te.plot.multi(1,country,indicator[2])
#pl3 = te.plot.multi(1,country,indicator[3])

#arrange_ggplot2(pl1,pl2,pl3,ncol=1)

#arrange_ggplot2 <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
#  dots <- list(...)
#  n <- length(dots)
#  if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
#  if(is.null(nrow)) { nrow = ceiling(n/ncol)}
#  if(is.null(ncol)) { ncol = ceiling(n/nrow)}
#  ## NOTE see n2mfrow in grDevices for possible alternative
#  grid.newpage()
#  pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
#  ii.p <- 1
#  for(ii.row in seq(1, nrow)){
#    ii.table.row <- ii.row	
#    if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
#    for(ii.col in seq(1, ncol)){
#      ii.table <- ii.p
#      if(ii.p > n) break
#      print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
#      ii.p <- ii.p + 1
#    }
#  }
#}



#historical.matrix = function(c,countries,indicators,d1="2005"){
#  options(stringsAsFactors = FALSE)
#  newdf=data.frame()
#  df=data.frame()
#  myTempDF=data.frame()
#  df=te.get.hist.multi.free(c,countries,indicators,d1)
#  for(i in 1:length(countries))
#  {
#    #print(i)
#    myTempDF=cbind(data.frame(t(df[tolower(df$Country)==tolower(countries[i]),c('Category','Value')])),countries[i])
#    if(length(myTempDF)<length(indicators)+1){next}
#    names(myTempDF)=c(myTempDF['Category',1:length(indicators)],'Country')
#    newdf=rbind(newdf,myTempDF['Value',])
#  }
#  #return(list(message=paste(names(newdf),collapse=" # ")))
#  
#  newdf[,1:length(indicators)]<-lapply(newdf[,1:length(indicators)],as.numeric)
#  #return(list(message=paste(paste(df,collapse=" J "),paste(newdf,collapse=" H "),collapse=" ! ")))
#  newdf
#}
if(F){
  #BUBBLE PLOT
  df=data.frame()
  assign("df",historicalToMatrix(c,country,indicator), envir = environment())
  if(is.null(df)){stop("Return to Sender: No Such Country - Indicator Pair.")}
  if(length(df)<2){stop("Return to Sender: No Such Country - Indicator Pair.")}
  df$Country[!is.na(countrycode(df$Country,"country.name","iso3c"))] <- countrycode(df$Country,"country.name","iso3c")[!is.na(countrycode(df$Country,"country.name","iso3c"))]
  df$Country[tolower(df$Country)=="euro area"] <- "RA17"
  titl=paste(indicator[2], indicator[3],sep=" ~ ")
  ggplot(df, aes(x=get(names(df)[2]), y=get(names(df)[3]), size=get(names(df)[1]), label=Country),legend=FALSE)+
  geom_point(colour="white", fill="red", shape=21) + scale_size_area(max_size = 25)+ 
  scale_x_continuous(indicator[2],limits=c(min(df[,2]), max(df[,2])))+
  scale_y_continuous(indicator[3],limits=c(min(df[,3]), max(df[,3])))+
  geom_text(size=4, hjust=0, vjust=0)+
  ggtitle(titl)
}