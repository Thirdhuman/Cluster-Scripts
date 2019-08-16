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
library(rmapshaper)
library(geojsonio)
library(rmapshaper)
options(tigris_use_cache = TRUE)

Clusters_df=openxlsx::read.xlsx("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/Cluster_Final.xlsx")
Clusters_df=Clusters_df[c('FIPS', 'OZ','k_pca_15_1')]

tract_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/Shapefiles/tract_files.shp")
tract_shapes=subset(tract_shapes, ALAND > 0)

oz_info=read.csv('/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Opportunity Zones - Shapefiles/Opportunity_Zones.csv', stringsAsFactors = F)
oz_info=oz_info[c('GEOID10', 'COUNTYNAME','TYPE')]
oz_info$GEOID10=str_pad(oz_info$GEOID10, 11, pad = "0")

tract_df=merge(tract_shapes,Clusters_df, by.x = "GEOID", by.y = "FIPS", all.x = T, sort = F )
# tract_df=merge(tract_df,oz_info, by.x = "GEOID", by.y = "GEOID10", all.x = T, sort = F )
# tract_df <- tract_df %>% st_set_crs(4326)

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
tract_df$k_pca_15_1[tract_df$k_pca_15_1 == "Puerto Rico & US Territories"] = "56"
tract_df$k_pca_15_1[tract_df$k_pca_15_1 == "Sparsely Populated"] = "0"
tract_df=subset(tract_df, k_pca_15_1 != "56")
tract_df <- ms_simplify(tract_df)

# 
# names(Clusters_df);head(tract_shapes);
# 
# matched <- intersect(opp_z$GEOID10, oz_info$GEOID10)
# all <-  union(opp_z$GEOID10, oz_info$GEOID10)
# non.matched <- all[!all %in% matched]

# oz_merge=merge(opp_z,oz_info, by=c("GEOID10"), all.x=T)
# anyNA(tract_df)
# proj4string(tract_df)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
test=tract_df %>% group_split(k_pca_15_1,OZ)
tract_df.sp <- as(tract_df, "Spatial")
proj4string(tract_df.sp)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# 
# geojson_write(tract_df,"/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/Web_Map/Cluster.geojson")
# st_write(tract_df, "/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/Web_Map/Cluster_OZ_file.shp", update=TRUE, delete_layer = TRUE)
# st_write(tract_df.sp, "/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/Web_Map/Cluster_OZ_file_2.shp", update=TRUE, delete_layer = TRUE)
# geojson_write(tract_df.sp,"/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/Web_Map/Cluster_2.geojson")

for (i in 1:length(test)) {
	assign(paste0("Cluster_",i), (test[[i]]))
	print(paste0("Cluster_",i))}

list_dfs = list(Cluster_1,Cluster_2,Cluster_3,Cluster_4,Cluster_5,Cluster_6,Cluster_7,Cluster_8,Cluster_9,Cluster_10,Cluster_11,Cluster_12,Cluster_13,Cluster_14,Cluster_15,Cluster_16,Cluster_17,Cluster_18,Cluster_19,Cluster_20,Cluster_21,Cluster_22,Cluster_23,Cluster_24,Cluster_25,Cluster_26,Cluster_27,Cluster_28,Cluster_29,Cluster_30,Cluster_31,Cluster_32,Cluster_33,Cluster_34)

rm(Cluster_1,Cluster_2,Cluster_3,Cluster_4,Cluster_5,Cluster_6,Cluster_7,Cluster_8,Cluster_9,Cluster_10,Cluster_11,Cluster_12,Cluster_13,Cluster_14,Cluster_15,Cluster_16,Cluster_17,Cluster_18,Cluster_19,Cluster_20,Cluster_21,Cluster_22,Cluster_23,Cluster_24,Cluster_25,Cluster_26,Cluster_27,Cluster_28,Cluster_29,Cluster_30,Cluster_31,Cluster_32,Cluster_33,Cluster_34)

for (i in 1:length(list_dfs)) {
	c=list_dfs[[i]]$k_pca_15_1[1]
	z=list_dfs[[i]]$OZ[1]
	assign(paste0("Cluster_",c,"_OZ_",z), (test[[i]]))
	print(paste0("Cluster_",c,"_OZ_",z))}

Cluster_uploads=LIST(Cluster_0_OZ_0,Cluster_0_OZ_1,Cluster_1_OZ_0,Cluster_1_OZ_1,Cluster_10_OZ_0,Cluster_10_OZ_1,Cluster_11_OZ_0,Cluster_11_OZ_1,Cluster_12_OZ_0,Cluster_12_OZ_1,Cluster_13_OZ_0,Cluster_13_OZ_1,Cluster_14_OZ_0,Cluster_14_OZ_1,Cluster_15_OZ_0,Cluster_15_OZ_1,Cluster_2_OZ_0,Cluster_2_OZ_1,Cluster_3_OZ_0,Cluster_3_OZ_1,Cluster_4_OZ_0,Cluster_4_OZ_1,Cluster_5_OZ_0,Cluster_5_OZ_1,Cluster_56_OZ_0,Cluster_56_OZ_1,Cluster_6_OZ_0,Cluster_6_OZ_1,Cluster_7_OZ_0,Cluster_7_OZ_1,Cluster_8_OZ_0,Cluster_8_OZ_1,Cluster_9_OZ_0,Cluster_9_OZ_1)

Cluster_uploads=pbmcapply::pbmclapply(Cluster_uploads,geojson_json)
names(Cluster_uploads[1])

Cluster_uploads[sapply(Cluster_uploads, is.null)] <- NA

# Cluster_1 <- geojson_json(Cluster_1)
# 
# test[[i]] <- geojson_json(test[[i]])
# tract_df <- geojson_json(tract_df)
names(Cluster_uploads)
tract_df.sp.arc <- arc.shape(tract_df.sp)
LIST <- function(...) {
    nms <- sapply(as.list(substitute(list(...))), deparse)[-1]
    setNames(list(...), nms)
}


for (i in 1:length(Cluster_uploads)) {
	dl.me=geojson_json(Cluster_uploads[[i]])
	geojson_write((dl.me),
	file = paste0("/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/Web_Map/Cluster_GEOJSON/Native_Multi_JG/",names(Cluster_uploads[i]),".geojson"))
	# st_write(as.data.frame(test[[i]]) , paste0("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/Web_Map/Multi-file/c",i,"_Cluster.shp"),
	# 	update=TRUE, delete_layer = TRUE)
	}

writeOGR(tract_df.sp, dsn = '/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/Web_Map/Cluster_OZ_7.shp', layer = "Cluster_OZ_7", driver = "ESRI Shapefile", layer_options = "RESIZE=YES"
	#, update=TRUE, delete_layer = TRUE
	)
writeOGR(tract_df.sp, dsn = save.dir, layer = filename.save, driver = 'ESRI Shapefile')


#write.csv(oz_merge,'/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Opportunity Zones - Shapefiles/oz_merge.csv')