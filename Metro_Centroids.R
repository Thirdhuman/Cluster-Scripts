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


metro_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Opportunity Zones - Shapefiles/Metro Areas/tl_2017_us_cbsa/tl_2017_us_cbsa.shp")
# metro_shapes=metro_shapes[c("NAMELSAD", 'geometry')]
names(metro_shapes)[names(metro_shapes) == 'NAMELSAD'] = 'METRO_NAME'
attr(metro_shapes, "sf_column") <- "geometry"
metro_shapes[["geometry"]] <- metro_shapes$geom
metro_shapes$geom <- NULL
map <- geojson_sf(metro_shapes)


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
write.csv(cent ,'/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/states_shapfiles/Metro_cent.csv')

mynewspdf <- merge(map, cent)
# geojson_write(input = mynewspdf, file = "/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Final_Shapefile/state_shapes_ab_update.geojson", overwrite = TRUE)


