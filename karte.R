# Karte
## koordinaten punkt 25/0: 47.954579, 7.798374
# 47.954405, 7.798380
# 47.954365, 7.798366
coordis <- read.csv("nele.csv")
coordis <- coordis[c(7,4),]
coordinates(coordis) <- ~lon + lat
coordis@proj4string <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

library(leaflet)
leaflet() %>% 
  addTiles %>% 
  addProviderTiles(provider = "Esri.WorldImagery", 
                   options = providerTileOptions(opacity = 0.7)) %>% 
  addMarkers(coordis, lng = coordinates(coordis)[,1], lat= coordinates(coordis)[,2])

coornercords <- data.frame(lon=c( 7.798357,7.798894, 7.798860,7.798321), 
                           lat=c(47.954827,  47.954798, 47.954349, 47.954385))
# 2.1:  47.954827, 7.798357
# 2.4:  47.954798, 7.798894
# 6.4:  47.954349, 7.798860
# 6.1: 47.954385, 7.798321
leaflet() %>% 
  addTiles %>% 
  addProviderTiles(provider = "Esri.WorldImagery", 
                   options = providerTileOptions(opacity = 0.7)) %>% 
  addPolygons(coornercords, lng=coornercords$lon, lat=coornercords$lat) %>% 
  addMarkers(coordis, lng = coordinates(coordis)[,1], lat= coordinates(coordis)[,2])
  
library(raster)
library(rgdal)
Map <- raster("Rplot01.tiff")  

extent(Map) <- c(7.798357,7.798860, 47.954349 ,47.954827)
projection(Map) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

pal <- colorBin("Greys", domain = 255:0, na.color = "transparent", reverse = T)
leaflet() %>% 
  addTiles %>% 
  addProviderTiles(provider = "Esri.WorldImagery", 
                   options = providerTileOptions(opacity = 0.5)) %>% 
  addScaleBar(position = c("bottomright"), options = scaleBarOptions(imperial = FALSE) ) %>% 
  #addPolygons(coornercords, color = "red", lng=coornercords$lon, lat=coornercords$lat) #%>% 
  addRasterImage(Map, color = pal, opacity = 0.3, maxBytes = 123123123) %>% 
  addMiniMap("topleft",
    tiles = providers$Esri.WorldStreetMap, height = 300, zoomLevelOffset = -6
    )
