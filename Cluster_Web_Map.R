library(tidycensus)
library(tidyverse)
library(viridis)
library(mapview)
library(sf)
library(rgdal)    # for readOGR and others
library(leaflet)  # for interactive maps (NOT leafletR here)
library(dplyr)    # for working with data frames
library(ggplot2)  # for plotting
library(maptools)
library(htmlwidgets)
library(haven)
library(rgeos)
options(tigris_use_cache = TRUE)

Clusters_df=openxlsx::read.xlsx("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/Cluster_Final.xlsx")
tract_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/Shapefiles/tract_files.shp")

names(Clusters_df);head(tract_shapes);
oz_info$GEOID10=str_pad(oz_info$GEOID10, 11, pad = "0")

matched <- intersect(opp_z$GEOID10, oz_info$GEOID10)
all <-  union(opp_z$GEOID10, oz_info$GEOID10)
non.matched <- all[!all %in% matched]

oz_merge=merge(opp_z,oz_info, by=c("GEOID10"), all.x=T)
st_write(oz_merge, "/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Opportunity Zones - Shapefiles/OZ_info.shp", update=TRUE, delete_layer = TRUE)

#write.csv(oz_merge,'/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Opportunity Zones - Shapefiles/oz_merge.csv')