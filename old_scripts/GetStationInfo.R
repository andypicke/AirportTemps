
# I'm trying to get locations for the airport weather stations, so I can plot them on a map

# text file with weather station info
url <- "http://weather.rap.ucar.edu/surface/stations.txt"

url <- "https://www.wunderground.com/about/faq/US_cities.asp"

destfile = "~/AirportTemps/Data/StationInfo.txt"
download.file(url,destfile)


library(readr)

dat<-read_csv("~/AirportTemps/Data/StationsSmall.csv")
dat<-read_delim("~/AirportTemps/Data/Stationes.txt",delim="",col_names = c("Station","State","ID","Lat","Lon","Elev","WMO"),skip = 10)
dat<-read_tsv("~/AirportTemps/Data/Stationes.txt",col)
#dat <- read.table("~/AirportTemps/Data/StationInfo_Annotated.txt",skip = 1,colClasses = c("character","character","character","numeric","numeric","numeric","numeric"),fill=TRUE)
dat <- read.table("~/AirportTemps/Data/StationInfo_Annotated.txt",skip = 1,blank.lines.skip = TRUE)

dat <- read_delim("~/AirportTemps/Data/stations.txt",delim=" ",trim_ws = TRUE)
#,header=FALSE,sep="",skip=10,as.is=FALSE,allowEscapes = TRUE)
head(dat)
View(dat)


#### Finally I find this is built into weatherData package?

rm(list=ls())
head(USAirportWeatherStations)

library(maps)
#library(mapdata)

usa <- map_data("usa")
g<-ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill=NA, color="grey") + coord_fixed(1.3)
g

# select only stations from a lsit
st_list<-c("KPHX","KLAX","KDEN","KMIA","KBOI","KIND",'KDFW',"KMSY","KBOS")
st2<-stations[which(stations$airportCode %in% slist),]


# plot only the contiguous US for now
stations <- filter(USAirportWeatherStations,State!="AK")
stations <-filter(stations,State!="HI")
stations <-filter(stations,State!="PR")
stations <-filter(stations,State!="VI")
stations <-filter(stations,State!="GU")
stations <-filter(stations,State!="MP")
#g + geom_point(data = USAirportWeatherStations, aes(x = Lon, y = Lat), color = "black", size = 0.1) 

#g + geom_point(data = stations, aes(x = Lon, y = Lat, color=stations$Lat,size=stations$Elevation))

g + geom_point(data = st2, aes(x = Lon, y = Lat, color=Lat),size=3)


cl#+geom_point(data = labs, aes(x = long, y = lat), color = "yellow", size = 4)
