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
Clusters_df=Clusters_df[c('FIPS', 'OZ','k_pca_15_1')]

tract_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/Shapefiles/tract_files.shp")
tract_shapes=subset(tract_shapes, ALAND > 0)

oz_info=read.csv('/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Opportunity Zones - Shapefiles/Opportunity_Zones.csv', stringsAsFactors = F)
oz_info=oz_info[c('GEOID10', 'COUNTYNAME','TYPE')]
oz_info$GEOID10=str_pad(oz_info$GEOID10, 11, pad = "0")

tract_df=merge(tract_shapes,Clusters_df, by.x = "GEOID", by.y = "FIPS", all.x = T, sort = F )
tract_df=merge(tract_df,oz_info, by.x = "GEOID", by.y = "GEOID10", all.x = T, sort = F )

# names(tract_pop)[names(tract_pop) == 'FIPS'] <- 'tfips'
# names(tract_pop)[names(tract_pop) == 'ChangePopulation'] <- 't_ChangePopulation'
# names(tract_pop)[names(tract_pop) == 'Population2000'] <- 't_Population2000'
# names(tract_pop)[names(tract_pop) == 'Population2017'] <- 't_Population2017'
# names(tract_pop)[names(tract_pop) == 'MedHHInc2000'] <- 't_MedHHInc2000'
# names(tract_pop)[names(tract_pop) == 'MedHHInc2017'] <- 'MedHHInc2017'


# IDX.dat$FIPS= ifelse(IDX.dat$FIPS == "51515050100", "51019050100",
#        ifelse(IDX.dat$FIPS == "02270000100", "02158000100",
#        ifelse(IDX.dat$FIPS == "46113940500", "46102940500",
#        ifelse(IDX.dat$FIPS == "46113940800", "46102940800",
#        ifelse(IDX.dat$FIPS == "46113940900", "46102940900",IDX.dat$FIPS)))))

# 
# names(Clusters_df);head(tract_shapes);
# 
# matched <- intersect(opp_z$GEOID10, oz_info$GEOID10)
# all <-  union(opp_z$GEOID10, oz_info$GEOID10)
# non.matched <- all[!all %in% matched]

# oz_merge=merge(opp_z,oz_info, by=c("GEOID10"), all.x=T)

tract_df=st_as_sf(tract_df, 4269)%>% st_transform(4269)


st_write(tract_df, "~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/Web_Map/Cluster_OZ_file.shp", update=TRUE, delete_layer = TRUE)

#write.csv(oz_merge,'/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Opportunity Zones - Shapefiles/oz_merge.csv')