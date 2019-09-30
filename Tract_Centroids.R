rm(list=ls());gc()

library(tidyverse)
library(viridis)
library(mapview)
library(sf)
library(rgdal)    # for readOGR and others
library(leaflet)  # for interactive maps (NOT leafletR here)
library(maptools)
library(rgeos)
library(rmapshaper)
library(geojsonio)
library(rmapshaper)
library(rgdal)
library(geojsonsf)

map_ab = readOGR("/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Final_Shapefile/Cluster_final_ab.geojson")


trueCentroids_ab = gCentroid(map_ab,byid=TRUE)
# plot(map)
# points(coordinates(map),pch=1)
# points(trueCentroids,pch=2)

cent_ab <- SpatialPointsDataFrame(gCentroid(map_ab, byid=TRUE), map_ab@data, match.ID=FALSE)
cent_ab <- as.data.frame(cent_ab)
cent_ab=cent_ab[c("GEOID", 'x', 'y')]
names(cent_ab)[names(cent_ab) == 'x'] = 'lng'
names(cent_ab)[names(cent_ab) == 'y'] = 'lat'

glimpse(cent_ab)
write.csv(cent_ab ,'/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/states_shapfiles/Tract_cent_ab.csv')

### 2 - Non Albers Coordinates ####
rm(list=ls());gc()

map = readOGR("/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Final_Shapefile/Cluster_final.geojson")

trueCentroids = gCentroid(map,byid=TRUE)
# plot(map)
# points(coordinates(map),pch=1)
# points(trueCentroids,pch=2)

cent <- SpatialPointsDataFrame(gCentroid(map, byid=TRUE), map@data, match.ID=FALSE)
cent <- as.data.frame(cent)
cent=cent[c("GEOID", 'x', 'y')]
names(cent)[names(cent) == 'x'] = 'lng'
names(cent)[names(cent) == 'y'] = 'lat'

glimpse(cent)
write.csv(cent ,'/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/states_shapfiles/Tract_cent.csv')
