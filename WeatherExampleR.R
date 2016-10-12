
#### Goal: See if there is a trend in temperature recorded at US airports.

rm(list=ls())

library(weatherData)
library(lubridate)
library(ggplot2)
library(plyr)
library(maps)
library(RColorBrewer)

# Years we will get data for
year_list=1980:2016

# list of station codes to get data for
#st_list<-c("KPHX","KLAX","KDEN","KMIA","KBOI","KIND",'KDFW',"KMSY","KBOS")

#Nst<-length(USAirportWeatherStations$airportCode)
#st_list <- USAirportWeatherStations$airportCode[seq(1,Nst,10)]

# station KMMO 1993 not working?
st_list <-USAirportWeatherStations$airportCode
st_list <- st_list[-which(st_list=="KMMO")]
st_list <- st_list[-which(st_list=="KP75")]

# Define function to download daily weather for 1 year
get_yearly_weather <- function (year,st_code){
        url <- paste0("http://www.wunderground.com/history/airport/",st_code,"/",year,"/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=",year,"&req_city=NA&req_state=NA&req_statename=NA&format=1")
}

# Define a function to download the weather data for a given station code
get_all_years <- function(st_code){
        for (i in seq_along(year_list)) {
                year <- year_list[i]
                savefile<-file.path("~/AirportTemps/Data",paste0("wea",st_code,year,".csv"))
                if (file.exists(savefile)){
                        print(paste(savefile," already downloaded"))
                }else{
                        url=get_yearly_weather(year,st_code)
                        print(paste("Getting weather data for ",st_code," for year ",year))
                        download.file(url,savefile)
                }
        }
}

# Apply that function to station list to get data for all stations
tryapply(st_list, get_all_years)

# Remove bad temperature values (marked as -99999 in data)
rm_bad_wea_values <- function(dat){
        dat$Min.TemperatureF[which(dat$Min.TemperatureF==-99999)]<-NA
        dat$Max.TemperatureF[which(dat$Max.TemperatureF==-99999)]<-NA
        dat$Mean.TemperatureF[which(dat$Mean.TemperatureF==-99999)]<-NA
        dat<-subset(dat,Mean.TemperatureF>-50);
        dat<-subset(dat,Mean.TemperatureF<150);
        dat
}

# Define a function to load and combine all years for a station into one dataframe
combine_years_wea <- function(st_code){
        savefile <-file.path( "~/AirportTemps/Data",paste0("wea",st_code,"combined.csv"))
#        if (file.exists(savefile)){
#                print(paste(savefile," already combined"))
#        }else{
                print(paste("combining years for station ", st_code))
                # Load csv files for each year and combine into a single data frame
                year_list=1980:2015
                dat_all=data.frame()
                for (i in seq_along(year_list)) {
                        year <- year_list[i]
                        #print(year)
                        dat <- read.csv( file.path( "~/AirportTemps/Data",paste0("wea",st_code,year,".csv")))
                        dat<-rm_bad_wea_values(dat)
                        dat$dd <- ymd(dat[,1])
                        #                print(head(dat))
                        dat_all=rbind(dat_all,dat)
                        #print(dim(dat_all))
                }
                # Save csv with combined data...
                write.csv(dat_all,savefile)
                # dat_all
#        }
}

# some stations don't work, so need tryapply()
tryapply(st_list, combine_years_wea)

# Define a function to load a specified station and do fit

#fit_station <- function(st_code)

# save data frame with fit coeffs, p value etc.
temp_fits<-data.frame()
for (i in seq_along(st_list)) {
        fname<-file.path( "~/AirportTemps/Data",paste0("wea",st_list[i],"combined.csv"))
        if (file.exists(fname)){
                print(paste("Fitting to ",st_list[i]))
                dat<- read.csv(fname)
                dat$dd <- ymd(dat$dd)
                try(fit1 <- lm(Mean.TemperatureF~dd,data=dat)
                summary(fit1)
                temp_fits<-rbind(temp_fits,data.frame(airportCode=st_list[i],trend=fit1$coefficients[2],pval=summary(fit1)$coefficients[2,4])))
        }
}



# function to load combined 1980-2016 csv file for specified station
load_combined<-function(st_code){
        fname<-file.path( "~/AirportTemps/Data",paste0("wea",st_code,"combined.csv"))
        dat<- read.csv(fname)
        dat$dd <- ymd(dat$dd)
        dat
}

# save a vector of coeffs
#all_coefs <- NA*vector(length(st_list),mode="numeric")
#for (i in seq_along(st_list)) {
#        fname<-file.path( "~/AirportTemps/Data",paste0("wea",st_list[i],"combined.csv"))
#        if (file.exists(fname)){
#                print(paste("Fitting to ",st_list[i]))
#                dat<- read.csv(fname)
#                dat$dd <- ymd(dat$dd)
#                fit1 <- lm(Mean.TemperatureF~dd,data=dat)
#                summary(fit1)
#                # Only save is p-value is small enough
#                if (summary(fit1)$coefficients[2,4]<0.025){
#                        all_coefs[i]<-fit1$coefficients[2]
#                }
#        }
#}

# convert trend to deg/decade
temp_fits$trend <- temp_fits$trend*365*10
View(temp_fits)

# How many fits have significant/non sig. pvalues?
ig<-which(temp_fits$pval<0.025)
ib<-which(temp_fits$pval>0.025)
length(ig)
length(ib)

max(temp_fits$trend[ig])
max(temp_fits$trend[ib])

# Look at some of the stations where pvalue was large or small
id<-ib[18]
id<-ig[300]
print(temp_fits$airportCode[id])
d3 <- load_combined(temp_fits$airportCode[id])
g<-ggplot(d3,aes(x=dd,y=Max.TemperatureF))
g+geom_line()+geom_smooth(method="lm")

hist(temp_fits$trend[ib])
hist(temp_fits$trend[ig])

# look at big outliers
# looks like in the stations with absurdly huge trends, they have a couple of very small values

max(temp_fits$trend)
idb<-which(temp_fits$trend>6)
View(temp_fits[idb,])

id<-idb[1]
print(temp_fits$airportCode[id])
print(temp_fits$trend[id])
d3 <- load_combined(temp_fits$airportCode[id])
min(d3$Mean.TemperatureF,na.rm = TRUE)
d3<-subset(d3,Mean.TemperatureF>-100)
g<-ggplot(d3,aes(x=dd,y=Mean.TemperatureF))+geom_line()+geom_smooth(method="lm")
g
#}

# look at details of fit
fit1 <- lm(Mean.TemperatureF~dd,data=d3)
summary(fit1)

# Need to make a data frame w/ coefs and station info (lat,lon etc) for plotting
results <- data.frame(airportCode=st_list,trend=all_coefs)

# Join to data frame with station info
#results2 <- join(results,USAirportWeatherStations,by="airportCode")

results2 <- join(temp_fits,USAirportWeatherStations,by="airportCode")

# Convert from deg/day to deg/decade
#results2$trend <- results2$trend*365*10

# Keep only continental and lower 48 states (map doesn't look good with others)
results2<-subset(results2,State!="AK" & State!="MP" & State!="PR" & State!="HI" & abs(trend)<10)


## Plot map with circles proportional to trend
usa <- map_data("usa")
states <- map_data("state")

g<-ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill=NA, color="red") + coord_fixed(1.3) 

g2<-g+geom_polygon(data=states,aes(x = long, y = lat, group = group), fill="slategray",color = "white") + guides(fill=FALSE)  # do this to leave off the color legend

g2 + geom_point(data = results2, aes(x = Lon, y = Lat,size = abs(trend),color=trend)) + scale_color_gradient2(midpoint=0, low="blue", mid="white",high="red", space ="Lab" ,limits=c(-3,3)) +ggtitle("Linear trend [deg/decade] in mean temp., 1980-2016") + labs(x="Longitude",y="Latitude")

# same size
g2 + geom_point(data = results2, aes(x = Lon, y = Lat,color=trend)) + scale_color_gradient2(midpoint=0, low="blue", mid="white",high="red", space ="Lab" ,limits=c(-3,3)) +ggtitle("Linear trend [deg/decade] in mean temp., 1980-2016") + labs(x="Longitude",y="Latitude")

# should plot all stations, make the oens where fits not signficant a different color/marker

# just plot all station locations
usa <- map_data("usa")
states <- map_data("state")

g<-ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill=NA, color="red") + coord_fixed(1.3) 
g2<-g+geom_polygon(data=states,aes(x = long, y = lat, group = group), fill="slategray",color = "white") + guides(fill=FALSE)  # do this to leave off the color legend
all_cont=subset(USAirportWeatherStations,State!="AK" & State!="MP" & State!="PR" & State!="HI" & State!="VI" & State!="GU")
g2+geom_point(data=all_cont,aes(x=Lon,y=Lat),color="red",size=0.5)

####
