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

oz_info=read.csv('/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Opportunity Zones - Shapefiles/Opportunity_Zones.csv', stringsAsFactors = F)
oz_info$GEOID10=str_pad(oz_info$GEOID10, 11, pad = "0")
oz_info=oz_info[c('GEOID10', "STATENAME", 'COUNTYNAME','TYPE')]
names(oz_info)[names(oz_info) == 'TYPE'] = 'OZ_type'

tract_df=merge(tract_shapes,oz_info, by.x = "GEOID", by.y = "GEOID10", all.x = T, sort = F )
tract_df=merge(tract_df,Clusters_df, by.x = "GEOID", by.y = "FIPS", all.x = T, sort = F )
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
tract_df[is.na(tract_df)] <- '0'

# tract_df$lng <- -1*as.numeric( sub("-", "", tract_df$INTPTLON) )
# 
# tract_df$lat=as.numeric(tract_df$INTPTLAT)
# tract_df$lng=as.numeric(tract_df$INTPTLON)
# tract_df$lat=NULL
# tract_df$lng=NULL
tract_df$id <- 1:nrow(tract_df)

cluster_dat=tract_df 
glimpse(cluster_dat)
cluster_dat=within(cluster_dat,rm(INTPTLAT,INTPTLON,ind1,ind2,ind3,STATENAME,COUNTYNAME,OZ_type,Final_Cluster))
cluster_dat[is.na(cluster_dat)] <- "N/A"

cluster_dat=as.data.frame(cluster_dat)
cluster_dat$GEOID=as.character(cluster_dat$GEOID)
cluster_dat_geo <- geojson_json(cluster_dat)

geojson_write(input = cluster_dat_geo, file = "/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Final_Shapefile/OZ_Tract.geojson", overwrite = TRUE)

cluster_dat=within(cluster_dat,rm(geometry))
save(cluster_dat, file =  '~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/cluster_df.Rdata')
write.csv(cluster_dat, file =  '~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/cluster_df.csv')
# tract_df=st_cast(tract_df, "MULTIPOLYGON") %>% st_cast("POLYGON")
# tract_df=lwgeom::st_make_valid(tract_df)

names(tract_df)
# tract_df <- st_transform(tract_df,"+proj=longlat +init=epsg:4326")
# tract_df=st_transform(st_sf(tract_df), "+proj=longlat +init=epsg:4326")
# st_transform(structure(p1, proj4string = "+init=epsg:4326"), "+init=epsg:3857")
# proj4string(tract_df) <- CRS("+proj=longlat +datum=WGS84")
st_crs(tract_df) ="+proj=longlat +init=epsg:4326"
glimpse(tract_df)

tract_df
geojson_write(input = tract_df, file = "/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Final_Shapefile/Cluster_Final.geojson", overwrite = TRUE)

# States


# 
# cluster_split=tract_df %>% group_split(OZ)
# for (i in 1:length(cluster_split)) {
# 	assign(paste0("Cluster_",i), (cluster_split[[i]]))
# 	print(paste0("Cluster_",i))}
# 
# list_dfs = list(Cluster_1,Cluster_2)
# rm(Cluster_1,Cluster_2)
# names(tract_df)
# 
# for (i in 1:length(list_dfs)) {
# 	z=list_dfs[[i]]$OZ[1]
# 	assign(paste0("Cluster_OZ_",z), (cluster_split[[i]]))
# 	print(paste0("Cluster_OZ_",z))}
# 
# 
# LIST <- function(...) {
#     nms <- sapply(as.list(substitute(list(...))), deparse)[-1]
#     setNames(list(...), nms)
# }
# 
# 
# 
# Cluster_uploads=LIST(Cluster_OZ_0,Cluster_OZ_1)
# 
# for (i in 1:length(Cluster_uploads)) {
# 	geojson_write((Cluster_uploads[[i]]),
# 	file = paste0("/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Multi-file/Cluster_GEOJSON/",names(Cluster_uploads[i]),"_split.geojson"))}
# 	
# cluster_split=tract_df %>% group_split(clusterAbbvr,OZ)

# proj4string(tract_df.sp)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# tract_df.sp=tract_df 
# writeOGR(tract_df.sp, dsn = '/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Final_Shapefile/Cluster_OZ_Final_test.shp', layer = "Cluster_OZ_Final_Test", driver = "ESRI Shapefile", overwrite_layer=T, delete_dsn =T)

# st_write(tract_df.sp, "/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Cluster_OZ_file_2.shp", update=TRUE, delete_layer = TRUE)


# geojson_write(tract_df_gj,file ="/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Final_Shapefile/tract_df.geojson")

# geojson_write(tract_df,"/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Cluster.geojson")
# st_write(tract_df, "/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Cluster_OZ_file.shp", update=TRUE, delete_layer = TRUE)
# st_write(tract_df.sp, "/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Cluster_OZ_file_2.shp", update=TRUE, delete_layer = TRUE)
# geojson_write(tract_df.sp,"/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Cluster_2.geojson")
unique(tract_df$Cluster_Final)

for (i in 1:length(cluster_split)) {
	assign(paste0("Cluster_",i), (cluster_split[[i]]))
	print(paste0("Cluster_",i))}

list_dfs = list(Cluster_1,Cluster_2,Cluster_3,Cluster_4,Cluster_5,Cluster_6,Cluster_7,Cluster_8,Cluster_9,Cluster_10,Cluster_11,Cluster_12,Cluster_13,Cluster_14,Cluster_15,Cluster_16,Cluster_17,Cluster_18,Cluster_19,Cluster_20,Cluster_21,Cluster_22,Cluster_23,Cluster_24,Cluster_25,Cluster_26,Cluster_27,Cluster_28,Cluster_29,Cluster_30,Cluster_31,Cluster_32)

rm(Cluster_1,Cluster_2,Cluster_3,Cluster_4,Cluster_5,Cluster_6,Cluster_7,Cluster_8,Cluster_9,Cluster_10,Cluster_11,Cluster_12,Cluster_13,Cluster_14,Cluster_15,Cluster_16,Cluster_17,Cluster_18,Cluster_19,Cluster_20,Cluster_21,Cluster_22,Cluster_23,Cluster_24,Cluster_25,Cluster_26,Cluster_27,Cluster_28,Cluster_29,Cluster_30,Cluster_31,Cluster_32)
names(tract_df)

for (i in 1:length(list_dfs)) {
	c=list_dfs[[i]]$clusterAbbvr[1]
	z=list_dfs[[i]]$OZ[1]
	assign(paste0("",c,"_OZ_",z), (cluster_split[[i]]))
	print(paste0("",c,"_OZ_",z))}


LIST <- function(...) {
    nms <- sapply(as.list(substitute(list(...))), deparse)[-1]
    setNames(list(...), nms)
}


Cluster_uploads=LIST(AfrcnAmrcnAd_OZ_0,AfrcnAmrcnAd_OZ_1,CllgsandUnvr_OZ_0,CllgsandUnvr_OZ_1,DnsDvrsUrbnt_OZ_0,DnsDvrsUrbnt_OZ_1,DvrsLwIncmSb_OZ_0,DvrsLwIncmSb_OZ_1,EldrlyandRtr_OZ_0,EldrlyandRtr_OZ_1,ImmgrntHspnA_OZ_0,ImmgrntHspnA_OZ_1,LowDnstyUrbn_OZ_0,LowDnstyUrbn_OZ_1,MddlIncmSbrb_OZ_0,MddlIncmSbrb_OZ_1,MixdUsUrbnCr_OZ_0,MixdUsUrbnCr_OZ_1,MltryBssandF_OZ_0,MltryBssandF_OZ_1,NtvAmrcnAdvr_OZ_0,NtvAmrcnAdvr_OZ_1,PrtRcUSTrrtr_OZ_0,PrtRcUSTrrtr_OZ_1,RuralAmerica_OZ_0,RuralAmerica_OZ_1,SparslyPpltd_OZ_0,SparslyPpltd_OZ_1,UltrWlthySbr_OZ_0,UltrWlthySbr_OZ_1,UpprIncmSbrb_OZ_0,UpprIncmSbrb_OZ_1)

for (i in 1:length(Cluster_uploads)) {
dl.me=(Cluster_uploads[[i]])
st_write(dl.me , paste0("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Multi-file/",names(Cluster_uploads[i]),".shp"),
		update=TRUE, delete_layer = TRUE)
}
# Cluster_uploads=pbmcapply::pbmclapply(Cluster_uploads,geojson_json)
names(Cluster_uploads[1])

Cluster_uploads[sapply(Cluster_uploads, is.null)] <- NA

# Cluster_1 <- geojson_json(Cluster_1)
test_geojson=geojson_json(Cluster_uploads[[1]])
# test[[i]] <- geojson_json(test[[i]])
# tract_df <- geojson_json(tract_df)
# names(Cluster_uploads)
# tract_df.sp.arc <- arc.shape(tract_df.sp)
names(Cluster_uploads)
for (i in 1:length(Cluster_uploads)) {
	dl.me=geojson_json(Cluster_uploads[[i]])
	geojson_write((dl.me),
	file = paste0("/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Cluster_GEOJSON/Native_Multi_JG/",names(Cluster_uploads[i]),".geojson"))
	# st_write(as.data.frame(test[[i]]) , paste0("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Multi-file/c",i,"_Cluster.shp"),
	# 	update=TRUE, delete_layer = TRUE)
	}

cluster_split=tract_df %>% group_split(clusterAbbvr)

for (i in 1:length(cluster_split)) {
	assign(paste0("Cluster_",i), (cluster_split[[i]]))
	print(paste0("Cluster_",i))}

list_dfs = list(Cluster_1,Cluster_2,Cluster_3,Cluster_4,Cluster_5,Cluster_6,Cluster_7,Cluster_8,Cluster_9,Cluster_10,Cluster_11,Cluster_12,Cluster_13,Cluster_14,Cluster_15,Cluster_16)

rm(Cluster_1,Cluster_2,Cluster_3,Cluster_4,Cluster_5,Cluster_6,Cluster_7,Cluster_8,Cluster_9,Cluster_10,Cluster_11,Cluster_12,Cluster_13,Cluster_14,Cluster_15,Cluster_16)
names(tract_df)

for (i in 1:length(list_dfs)) {
	c=list_dfs[[i]]$clusterAbbvr[1]
	z=list_dfs[[i]]$OZ[1]
	assign(paste0("",c), (cluster_split[[i]]))
	print(paste0("",c))}


LIST <- function(...) {
    nms <- sapply(as.list(substitute(list(...))), deparse)[-1]
    setNames(list(...), nms)
}
# AfrcnAmrcnAd,CllgsandUnvr,DnsDvrsUrbnt,DvrsLwIncmSb,EldrlyandRtr,ImmgrntHspnA,LowDnstyUrbn,MddlIncmSbrb,MixdUsUrbnCr,MltryBssandF,NtvAmrcnAdvr,PrtRcUSTrrtr,RuralAmerica,SparslyPpltd,UltrWlthySbr,UpprIncmSbrb


Cluster_uploads=LIST(AfrcnAmrcnAd,CllgsandUnvr,DnsDvrsUrbnt,DvrsLwIncmSb,EldrlyandRtr,ImmgrntHspnA,LowDnstyUrbn,MddlIncmSbrb,MixdUsUrbnCr,MltryBssandF,NtvAmrcnAdvr,PrtRcUSTrrtr,RuralAmerica,SparslyPpltd,UltrWlthySbr,UpprIncmSbrb)

for (i in 1:length(Cluster_uploads)) {
dl.me=(Cluster_uploads[[i]])
st_write(dl.me , paste0("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Multi-file/",names(Cluster_uploads[i]),".shp"),
		update=TRUE, delete_layer = TRUE)
}

for (i in 1:length(Cluster_uploads)) {
	dl.me=geojson_json(Cluster_uploads[[i]])
	geojson_write((dl.me),
	file = paste0("/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Multi-file/Cluster_GEOJSON/Small_Multi_GeoJason/",names(Cluster_uploads[i]),".geojson"))
	# st_write(as.data.frame(test[[i]]) , paste0("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Multi-file/c",i,"_Cluster.shp"),
	# 	update=TRUE, delete_layer = TRUE)
	}


# writeOGR(tract_df.sp, dsn = save.dir, layer = filename.save, driver = 'ESRI Shapefile')


#write.csv(oz_merge,'/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Opportunity Zones - Shapefiles/oz_merge.csv')