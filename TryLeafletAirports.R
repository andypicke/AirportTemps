

library(leaflet)
#library(map)
library(weatherData)

cont_us <- subset(USAirportWeatherStations,State!="AK" & State!="MP" & State!="PR" & State!="HI" & State!="VI" & State!="GU")

m <- leaflet() %>%
        setMaxBounds(-125,23,-67,50)%>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
addCircleMarkers(lng=cont_us$Lon,lat=cont_us$Lat,popup=cont_us$Station,radius=2,fill=TRUE,color="grey")#,clusterOptions = markerClusterOptions())
#addCircles(lng=cont_us$Lon,lat=cont_us$Lat)
m  # Print the map

