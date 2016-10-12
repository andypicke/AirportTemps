

library(leaflet)
#library(map)
library(weatherData)

cont_us <- subset(USAirportWeatherStations,State!="AK" & State!="MP" & State!="PR" & State!="HI" & State!="VI" & State!="GU")

m <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
addMarkers(lng=cont_us$Lon,lat=cont_us$Lat,popup=cont_us$Station,clusterOptions = markerClusterOptions())
#addCircles(lng=cont_us$Lon,lat=cont_us$Lat)
m  # Print the map

