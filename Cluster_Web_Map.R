gc;

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
names(Clusters_names)

# Clusters_df=Clusters_df[c('FIPS', 'OZ')]
Clusters_df=Clusters_df[c('FIPS', 'Final_Cluster','OZ')]
# Clusters_df=merge(Clusters_df,Clusters_names, by = "FIPS", all = T)
# names(Clusters_df)[names(Clusters_df) == 'FIPS'] = 'GEOID'
tract_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Shapefiles/tract_files.shp")
tract_shapes=subset(tract_shapes, ALAND > 1)

LIC_tracts=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/Extra_Shapefiles/LIC Tracts/tabula-Tract-Table.csv', stringsAsFactors = F)
LIC_tracts$Tract_ID=str_pad(LIC_tracts$Tract_ID, 11, pad = "0")
LIC_tracts=LIC_tracts[c("Tract_ID", 'LIC')]

oz_info=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Opportunity Zones - Shapefiles/Opportunity_Zones.csv', stringsAsFactors = F)
oz_info$GEOID10=str_pad(oz_info$GEOID10, 11, pad = "0")
oz_info=oz_info[c('GEOID10', "STATENAME", 'COUNTYNAME','TYPE')]
names(oz_info)[names(oz_info) == 'TYPE'] = 'OZ_type'

tract_df=merge(tract_shapes,oz_info, by.x = "GEOID", by.y = "GEOID10", all.x = T, sort = F )
tract_df=merge(tract_df,Clusters_df, by.x = "GEOID", by.y = "FIPS", all.x = T, sort = F )
tract_df=merge(tract_df,LIC_tracts, by.x = "GEOID", by.y = "Tract_ID", all.x = T, sort = F )

unique(tract_df$OZ_type)
unique(tract_df$LIC)

tract_df$LIC = ifelse(tract_df$OZ_type == 'Low-Income Community','Low-Income Community',
			ifelse(tract_df$OZ_type == 'Undesignated Low-Income Community', 'Low-Income Community',
					"No"))

# tract_df=subset(tract_df, STATEFP != "02" & STATEFP != "15" & STATEFP != "72")
# tract_df=subset(tract_df, STATEFP == "48")
tract_df=subset(tract_df, STATEFP != "72")


# tract_df=merge(tract_df,oz_info, by.x = "GEOID", by.y = "GEOID10", all.x = T, sort = F )
# tract_df <- tract_df %>% st_set_crs(4326)
# names(tract_df)[names(tract_df) == 'INTPTLAT'] = 'lat'
# names(tract_df)[names(tract_df) == 'INTPTLON'] = 'lng'

tract_df$clusterAbbvr=str_replace_all(tract_df$Cluster_Final, "[[:punct:]]", "")

tract_df$clusterAbbvr <- abbreviate( tract_df$clusterAbbvr, minlength = 12 )
tract_df$ind1 <- sample(100, size = nrow(tract_df), replace = TRUE) 
tract_df$ind2 <- sample(100, size = nrow(tract_df), replace = TRUE) 
tract_df$ind3 <- sample(100, size = nrow(tract_df), replace = TRUE) 
tract_df$ind4 <- sample(100, size = nrow(tract_df), replace = TRUE) 
tract_df$ind5 <- sample(100, size = nrow(tract_df), replace = TRUE) 

names(tract_df)
tract_df=within(tract_df,rm(STATEFP,COUNTYFP,TRACTCE,NAME,NAMELSAD,MTFCC,FUNCSTAT,ALAND,AWATER))
# names(tract_df) = str_replace_all(names(tract_df), "[[:punct:]]", "")
# names(tract_df) = abbreviate( names(tract_df), minlength = 9)
names(tract_df)

unique(tract_df$clusterAbbvr)
glimpse(tract_df)

tract_df$lat <- as.numeric( sub("+", "", tract_df$INTPTLAT) )
tract_df$lng <- as.numeric( sub("+", "", tract_df$INTPTLON) )
tract_df[is.na(tract_df)] <- "N/A"

# tract_df$lng <- -1*as.numeric( sub("-", "", tract_df$INTPTLON) )
# 
# tract_df$lat=as.numeric(tract_df$INTPTLAT)
# tract_df$lng=as.numeric(tract_df$INTPTLON)
# tract_df$lat=NULL
# tract_df$lng=NULL
# tract_df$id <- 1:nrow(tract_df)

cluster_dat=tract_df 
glimpse(cluster_dat)
cluster_dat=within(cluster_dat,rm(INTPTLAT,INTPTLON,ind1,ind2,ind3,ind4,ind5,STATENAME,COUNTYNAME,OZ_type,Final_Cluster))
cluster_dat[is.na(cluster_dat)] <- "N/A"

cluster_dat=as.data.frame(cluster_dat)
cluster_dat$GEOID=as.character(cluster_dat$GEOID)
cluster_dat=within(cluster_dat,rm(geometry))
save(cluster_dat, file =  '~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/cluster_df.Rdata')
write.csv(cluster_dat, file =  '~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/cluster_df.csv')
# tract_df=lwgeom::st_make_valid(tract_df)

names(tract_df)
st_crs(tract_df) ="+proj=longlat +init=epsg:4326"
glimpse(tract_df)

tract_geodf=geojson_json(tract_df)
tract_geodf$features$id <- 1:nrow(tract_geodf)

inspect(tract_geodf)
tract_df
geojson_write(input = tract_df, file = "/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Final_Shapefile/Cluster_Final.geojson", overwrite = TRUE)

# States
state_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/states_shapfiles/states.shp")
state_shapes
geojson_write(input = state_shapes, file = "/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Final_Shapefile/state_shapes.geojson", overwrite = TRUE)


# 