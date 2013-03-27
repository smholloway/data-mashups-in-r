library("XML")
library("rjson")
library("RCurl")
library("PBSmapping")

if (!file.exists("properties.html")) {
  download.file(url="http://web.archive.org/web/20080610132249/www.phillysheriff.com/properties.html", destfile="properties.html")
}

########################
# getAddressesFromHTML
# input:html filename
# returns:dataframe of geocoded addresses that can be plotted by PBSmapping
########################
getAddressesFromHTML<-function(myHTMLDoc){
  myStreets<-vector(mode="character",0)
  stNum<-"^[0-9]{2,5}(\\-[0-9]+)?"
  stName<-"([NSEW]\\. )?([0-9A-Z ]+)"
  stSuf<-"(St|Ave|Place|Blvd|Drive|Lane|Ln|Rd)(\\.?)$"
  badStrings<-
    "(\\r| a\\/?[kd]\\/?a.+$| - Premise.+$| assessed as.+$|, Unit.+
     |<font size=\"[0-9]\">|Apt\\..+| #.+$|[,\"]|\\s+$)"
  myStPat<-paste(stNum,stName,stSuf,sep=" ")
  for(line in readLines(myHTMLDoc)){
    line<-gsub(badStrings,'',line,perl=TRUE)
    matches<-grep(myStPat,line,perl=TRUE,
                  value=FALSE,ignore.case=TRUE)
    if(length(matches)>0){
       myStreets<-append(myStreets,line)
    }
  }
  myStreets 
}

####################
# geocodeAddresses
# input:vector of streets
# output:data frame containing lat/longs in PBSmapping-acceptable format
####################
geocodeAddresses<-function(myStreets){
  myGeoTable<-data.frame(address=character(),lat=numeric(),long=numeric(),EID=numeric())
  for(myStreet in myStreets){
     requestUrl<-paste(
          "http://maps.googleapis.com/maps/api/geocode/json?sensor=false&address=", 
          URLencode(myStreet),
          "+Philadelphia+PA",
          sep="")
    cat("geocoding:", requestUrl, "\n")
    tryCatch({
        json_data <- fromJSON(paste(readLines(requestUrl), collapse=""))
        lat <- unlist(lapply(json_data$results, function(x) {x$geometry[1]$location$lat}))
        lng <- unlist(lapply(json_data$results, function(x) {x$geometry[1]$location$lng}))

        myGeoTable<-rbind(myGeoTable,data.frame(address = myStreet, Y = lat, X = lng, EID=NA))
    }, error=function(err) {
        cat("parsing or http error:", conditionMessage(err), "\n")
    })
    Sys.sleep(0.1)
  }

  #let's use the built-in numbering as the event id that PBSmapping wants
  myGeoTable$EID<-as.numeric(rownames(myGeoTable))
  myGeoTable
}

streets<-getAddressesFromHTML("properties.html")

# http://www.temple.edu/ssdl/shpfiles/phila_tracts_2000.zip
myShapeFile<-importShapefile("tracts2000",readDBF=TRUE)
myPolyData<-attr(myShapeFile,"PolyData")
plotPolys(myShapeFile,axes=FALSE,bg="beige",main="Philadelphia County\n June 2009 Foreclosures",xlab="",ylab="")

geoTable<-geocodeAddresses(streets)

# page 17
#geoTable$X<-as.numeric(levels(geoTable$X))[geoTable$X] #do this 
#geoTable$Y<-as.numeric(levels(geoTable$Y))[geoTable$Y]
geoTable$X<-as.numeric(geoTable$X)
geoTable$Y<-as.numeric(geoTable$Y)
addressEvents<-as.EventData(geoTable,projection=NA)
addPoints(addressEvents,col="red",cex=.5)

addressPolys<-findPolys(addressEvents,myShapeFile)

# page 18
myTrtFC<- table(factor(addressPolys$PID,levels=levels(as.factor(myShapeFile$PID))))

# page 19
mapColors<-heat.colors(max(myTrtFC)+1,alpha=.6)[max(myTrtFC)-myTrtFC+1]

plotPolys(myShapeFile,axes=FALSE,bg="beige",main="Philadelphia County\n
              June 2009 Foreclosure Heat Map",xlab="",ylab="",col=mapColors)
legend("bottomright",legend=max(myTrtFC):0,
    fill=heat.colors(max(myTrtFC)+1,alpha=.6),
    title="Foreclosures")
