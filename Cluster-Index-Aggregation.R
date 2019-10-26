
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

Clusters_df=openxlsx::read.xlsx("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Final_Clusters/Cluster_14-15_f2.xlsx")

LIC_tracts=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/Extra_Shapefiles/LIC Tracts/tabula-Tract-Table.csv', stringsAsFactors = F)
LIC_tracts$Tract_ID=str_pad(LIC_tracts$Tract_ID, 11, pad = "0")
LIC_tracts=LIC_tracts[c("Tract_ID", 'LIC')]

oz_info=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Opportunity Zones - Shapefiles/Opportunity_Zones.csv', stringsAsFactors = F)
oz_info$GEOID10=str_pad(oz_info$GEOID10, 11, pad = "0")
oz_info=oz_info[c('GEOID10','TYPE')]
names(oz_info)[names(oz_info) == 'TYPE'] = 'OZ_type'

IDX.dat=openxlsx::read.xlsx("~/Desktop/Welfare_Policy/Struggling_Regions/Index/final_index_uncapped.xlsx")

names(IDX.dat)

cluster_index_df=merge(Clusters_df,IDX.dat, by.x = "FIPS", by.y = "FIPS", all.x = T, sort = F )

glimpse(cluster_index_df)
names(cluster_index_df)


labor_agg=aggregate( cluster_index_df$labor_idx, by = list(cluster_index_df$Final_Cluster),  FUN = mean, na.rm=T)
mig_agg=aggregate( cluster_index_df$mig_idx, by = list(cluster_index_df$Final_Cluster),  FUN = mean, na.rm=T)
pov_agg=aggregate( cluster_index_df$pov_idx, by = list(cluster_index_df$Final_Cluster),  FUN = mean, na.rm=T)
edu_agg=aggregate( cluster_index_df$edu_idx, by = list(cluster_index_df$Final_Cluster),  FUN = mean, na.rm=T)
afford_agg=aggregate( cluster_index_df$afford_idx, by = list(cluster_index_df$Final_Cluster),  FUN = mean, na.rm=T)

names(labor_agg)[names(labor_agg) == 'x'] = 'labor_agg'
names(mig_agg)[names(mig_agg) == 'x'] = 'mig_agg'
names(pov_agg)[names(pov_agg) == 'x'] = 'pov_agg'
names(afford_agg)[names(afford_agg) == 'x'] = 'afford_agg'
names(edu_agg)[names(edu_agg) == 'x'] = 'edu_agg'

names(labor_agg)[names(labor_agg) == 'Group.1'] = 'Cluster'
names(mig_agg)[names(mig_agg) == 'Group.1'] = 'Cluster'
names(pov_agg)[names(pov_agg) == 'Group.1'] = 'Cluster'
names(afford_agg)[names(afford_agg) == 'Group.1'] = 'Cluster'
names(edu_agg)[names(edu_agg) == 'Group.1'] = 'Cluster'

cluster_index_agg=list(labor_agg,mig_agg,pov_agg,edu_agg,afford_agg) %>% reduce(full_join, by = "Cluster")

glimpse(cluster_index_agg)

write.csv(cluster_index_agg, "/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Website/Opportunity_Zones/Opportunity Zones_Data/cluster_index_agg.csv")

openxlsx::write.xlsx(cluster_index_agg, "~/Desktop/Welfare_Policy/Struggling_Regions/Index/cluster_index_agg.xlsx")


