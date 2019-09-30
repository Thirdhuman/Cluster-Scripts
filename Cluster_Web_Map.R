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


# Best Cluster: Cluster_Final

# Clusters_df=openxlsx::read.xlsx("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Cluster_Final_2.xlsx")
Clusters_df=openxlsx::read.xlsx("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Final_Clusters/Cluster_14-15_f2.xlsx")

names(Clusters_df)

Clusters_df$Final_Cluster = ifelse(Clusters_df$Final_Cluster == 'African American Adversity','Concentrated Poverty',
	Clusters_df$Final_Cluster)

# African American Adversity

# Clusters_df=Clusters_df[c('FIPS', 'OZ')]
Clusters_df=Clusters_df[c('FIPS', 'Final_Cluster','OZ')]
# Clusters_df=merge(Clusters_df,Clusters_names, by = "FIPS", all = T)
# names(Clusters_df)[names(Clusters_df) == 'FIPS'] = 'GEOID'
tract_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Shapefiles/tract_files.shp")
tract_shapes=subset(tract_shapes, ALAND > 0.1)
tract_shapes$GEOID=as.character(tract_shapes$GEOID)

metro_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Opportunity Zones - Shapefiles/Metro Areas/tl_2017_us_cbsa/tl_2017_us_cbsa.shp")
metro_shapes=metro_shapes[c("NAME", 'geometry')]
names(metro_shapes)[names(metro_shapes) == 'NAME'] = 'METRO_NAME'

state_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/states_shapfiles/states.shp")
state_shapes=state_shapes[c("STATE_NAME", 'geometry')]

county_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/county_shapefiles/tl_2016_us_county.shp")
county_shapes=county_shapes[c("NAMELSAD", 'geometry')]
names(county_shapes)[names(county_shapes) == 'NAMELSAD'] = 'COUNTY_NAME'

st_crs(tract_shapes) <- 4326
st_crs(state_shapes) <- 4326
st_crs(county_shapes) <- 4326
st_crs(metro_shapes) <- 4326
# 
# st_transform(tract_shapes) <- 4326
# st_transform(state_shapes) <- 4326
# st_transform(county_shapes) <- 4326
# st_transform(metro_shapes) <- 4326

tract_shapes=st_join(tract_shapes, state_shapes, left = T, largest = T)
tract_shapes=st_join(tract_shapes, county_shapes, left = T, largest = T)
tract_shapes=st_join(tract_shapes, metro_shapes, left = T, largest = T)

LIC_tracts=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/Extra_Shapefiles/LIC Tracts/tabula-Tract-Table.csv', stringsAsFactors = F)
LIC_tracts$Tract_ID=str_pad(LIC_tracts$Tract_ID, 11, pad = "0")
LIC_tracts=LIC_tracts[c("Tract_ID", 'LIC')]

oz_info=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Opportunity Zones - Shapefiles/Opportunity_Zones.csv', stringsAsFactors = F)
oz_info$GEOID10=str_pad(oz_info$GEOID10, 11, pad = "0")
oz_info=oz_info[c('GEOID10','TYPE')]
names(oz_info)[names(oz_info) == 'TYPE'] = 'OZ_type'
IDX.dat=openxlsx::read.xlsx("~/Desktop/Welfare_Policy/Struggling_Regions/Index/final_index.xlsx")

tract_df=merge(tract_shapes,oz_info, by.x = "GEOID", by.y = "GEOID10", all.x = T, sort = F )
tract_df=merge(tract_df,Clusters_df, by.x = "GEOID", by.y = "FIPS", all.x = T, sort = F )
tract_df=merge(tract_df,LIC_tracts, by.x = "GEOID", by.y = "Tract_ID", all.x = T, sort = F )

tract_df$LIC = ifelse(tract_df$OZ_type == 'Low-Income Community','Low-Income Community',
		   ifelse(tract_df$OZ_type == 'Undesignated Low-Income Community', 'Low-Income Community',
					"Non-LIC"))

# tract_df$lat <- as.numeric( sub("+", "", tract_df$INTPTLAT) )
# tract_df$lng <- as.numeric( sub("+", "", tract_df$INTPTLON) )
tract_df[is.na(tract_df)] <- "N/A"

tract_df=merge(tract_df,IDX.dat, by.x = "GEOID", by.y = "FIPS", all.x = T, sort = F )

# tract_df=subset(tract_df, STATEFP != "02" & STATEFP != "15" & STATEFP != "72")
# tract_df=subset(tract_df, STATEFP == "48")
tract_df=subset(tract_df, STATEFP != "72")

tract_centr_ab=read.csv('/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/states_shapfiles/Tract_cent_ab.csv')
tract_df <- merge(tract_df, tract_centr_ab, all.x = T)

# tract_centr=read.csv('/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/states_shapfiles/Tract_cent.csv')
# tract_df <- merge(tract_df, tract_centr, all.x = T)

# tract_df$lat=as.numeric(tract_df$INTPTLAT)
# tract_df$lng=as.numeric(tract_df$INTPTLON)

names(tract_df)
tract_df=within(tract_df,rm(STATEFP, COUNTYFP,TRACTCE,NAME,NAMELSAD,MTFCC,FUNCSTAT,ALAND,AWATER,INTPTLAT,INTPTLON))
glimpse(tract_df)

# tract_df$lng <- -1*as.numeric( sub("-", "", tract_df$INTPTLON) )
 
st_crs(tract_df) ="+proj=longlat +init=epsg:4326"
glimpse(tract_df)

tract_df.sp=as_Spatial(tract_df, cast = TRUE, IDs = paste0("ID", 1:length(tract_df)))

geojson_write(input = tract_df.sp, file = "/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Final_Shapefile/Cluster_Final.geojson", overwrite = TRUE)
# geojson_write(input = tract_df.sp.samp, file = "/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Final_Shapefile/Cluster_Final_samp.geojson", overwrite = TRUE)


# # States
state_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/states_shapfiles/states.shp")

state_shapes$area <- st_area(state_shapes) / 100000000

centr=read.csv('/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/states_shapfiles/ST_cent.csv')
state_shapes <- merge(state_shapes, centr, all.x = T)
# st_is_valid(state_shapes)
# df_union_cast <- st_cast(state_shapes, "POLYGON")
state_shapes=as_Spatial(state_shapes, cast = TRUE, IDs = paste0("ID", 1:length(state_shapes)))
# state_shapes_simple <- gSimplify(state_shapes, topologyPreserve = TRUE, tol = 0.025)

geojson_write(input = state_shapes, file = "/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Final_Shapefile/state_shapes.geojson", overwrite = TRUE)


# 