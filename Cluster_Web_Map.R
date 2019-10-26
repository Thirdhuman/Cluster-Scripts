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

Clusters_df=openxlsx::read.xlsx("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Final_Clusters/Cluster_14-15_f2.xlsx")

names(county_shapes)[names(county_shapes) == 'Cluster.Name'] = 'Final_Cluster'
names(county_shapes)[names(county_shapes) == 'Opportunity.Zone.Status'] = 'OZ'

Clusters_df$Final_Cluster = ifelse(Clusters_df$OZ == 'Opportunity Zone','OZ',
	ifelse(Clusters_df$OZ == 'Undesignated','Non-OZ',
	Clusters_df$Final_Cluster))

unique(Clusters_df$Final_Cluster)

Clusters_df$Final_Cluster = ifelse(Clusters_df$Final_Cluster == 'African American Adversity','Concentrated Adversity',
	Clusters_df$Final_Cluster)

Clusters_df$Final_Cluster = ifelse(Clusters_df$Final_Cluster == 'Prosperous Urban Core','Vibrant Urban Core',
	Clusters_df$Final_Cluster)

Clusters_df$Final_Cluster = ifelse(Clusters_df$Final_Cluster == 'Colleges and Universities','Colleges & Universities',
	Clusters_df$Final_Cluster)

Clusters_df$Final_Cluster = ifelse(Clusters_df$Final_Cluster == 'Elderly and Retirees','Elderly & Retirees',
	Clusters_df$Final_Cluster)

Clusters_df$Final_Cluster = ifelse(Clusters_df$Final_Cluster == 'Military Bases and Facilities','Military Facilities & Residences',
	Clusters_df$Final_Cluster)

Clusters_df$Final_Cluster = ifelse(Clusters_df$Final_Cluster == 'Diverse Middle-Income Suburban','Diverse Inner-Suburban',
	Clusters_df$Final_Cluster)
Clusters_df=Clusters_df[c('FIPS', 'Final_Cluster','OZ')]

native_tracts=sf::st_read('~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Shapefiles/Native American Tracts', crs = 4269)
native_tracts$Native_Tract = "Yes"

# Read Tract Shapes
tract_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Shapefiles/", crs = 4269)
tract_shapes=subset(tract_shapes, as.numeric(ALAND) > 0.1)
tract_shapes$GEOID=as.character(tract_shapes$GEOID)

# Read Metro Shapes
metro_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Opportunity Zones - Shapefiles/Metro Areas/tl_2017_us_cbsa/", crs = 4269)
metro_shapes=metro_shapes[c("NAME", 'geometry')]
names(metro_shapes)[names(metro_shapes) == 'NAME'] = 'METRO_NAME'

# Read State Shapes
state_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/states_shapfiles/", crs = 4269)
state_shapes=state_shapes[c("STATE_NAME", 'STATE_FIPS')]

# Read County Shapes
county_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/county_shapefiles/", crs = 4269)
HMRC=openxlsx::read.xlsx('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/High-Migration-Rural-Counties/CDFI-Fund-High-Migration-Rural-Counties.xlsx')
HMRC=HMRC[c("FIPS", 'HMRC')]
county_shapes=merge(county_shapes,HMRC,by.x="GEOID", by.y="FIPS", all = T)
county_shapes=county_shapes[c("NAMELSAD","HMRC", 'geometry')]
names(county_shapes)[names(county_shapes) == 'NAMELSAD'] = 'COUNTY_NAME'


# Read Empowerment Zone Shapes
emp_zone=sf::st_read('~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Shapefiles/Empowerment_Zones_and_Enterprise_Communities')
emp_zone=subset(emp_zone, TYPE == "Empowerment Zone")
names(emp_zone)[names(emp_zone) == 'TYPE'] = 'Empowerment Zone'
emp_zone=emp_zone[c("Empowerment Zone", 'geometry')]

metro_shapes=st_transform(metro_shapes, 4269)
tract_shapes=st_transform(tract_shapes, 4269)
state_shapes=st_transform(state_shapes, 4269)
county_shapes=st_transform(county_shapes, 4269)
emp_zone=st_transform(emp_zone, 4269)

st_crs(tract_shapes) <- 4269
st_crs(state_shapes) <- 4269
st_crs(county_shapes) <- 4269
st_crs(metro_shapes) <- 4269
st_crs(emp_zone) <- 4269

st_geometry(state_shapes) <- NULL
state_shapes=as.data.frame(state_shapes)
tract_shapes=merge(tract_shapes, state_shapes, by.x="STATEFP", by.y ="STATE_FIPS", all.x = T)
tract_shapes=st_join(tract_shapes, county_shapes, left = T, largest = T)
tract_shapes=st_join(tract_shapes,  metro_shapes, left = T, largest = T)
tract_shapes=st_join(tract_shapes,  emp_zone, left = T, largest = T)

tract_15=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/2015_Poverty&FamilyInc.csv', stringsAsFactors = F)
tract_16=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/2016_Poverty&FamilyInc.csv', stringsAsFactors = F)
tract_17=read.csv("~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/2017_Poverty&FamilyInc.csv", stringsAsFactors = F)

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

(unique(tract_df$LIC))
(unique(tract_df$OZ_type))

tract_df$LIC_detailed = tract_df$LIC

tract_df$LIC=ifelse(tract_df$OZ_type=='Low-Income Community' | tract_df$LIC=='Undesignated Low-Income Community','Low-Income Community',tract_df$LIC)
tract_df$LIC	=	ifelse(is.na(tract_df$LIC)==T, "Non-LIC", tract_df$LIC)
(unique(tract_df$LIC))

tract_df$OZ_type = ifelse(tract_df$OZ_type == 'Low-Income Community','Low-Income Community (OZ)', tract_df$OZ_type)
tract_df$OZ_type = ifelse(tract_df$OZ_type == 'Non-LIC Contiguous','Non-LIC Contiguous (OZ)', tract_df$OZ_type)
tract_df$OZ_type = ifelse(is.na(tract_df$OZ_type)==T, "Undesignated Tract", tract_df$OZ_type)
unique(tract_df$OZ_type)

tract_df$tract_type = NA
tract_df$tract_type = ifelse(tract_df$OZ_type == 'Low-Income Community (OZ)' | tract_df$OZ_type == 'Non-LIC Contiguous (OZ)','Opportunity Zone',# tract_df$tract_type)
ifelse(tract_df$LIC_detailed == 'Undesignated Low-Income Community','Undesignated Low-Income Community', tract_df$tract_type))
tract_df$tract_type = ifelse(is.na(tract_df$tract_type)==T, "Undesignated Tract", tract_df$tract_type)
unique(tract_df$tract_type)

tract_df$tract_type_alt = NA
tract_df$tract_type_alt = ifelse(tract_df$OZ_type == 'Low-Income Community (OZ)','Opportunity Zone (LIC)',# tract_df$tract_type_alt
ifelse(tract_df$OZ_type == 'Non-LIC Contiguous (OZ)','Opportunity Zone (Contiguous)', #tract_df$tract_type
ifelse(tract_df$LIC_detailed == 'Undesignated Low-Income Community' ,'Undesignated Low-Income Community', tract_df$tract_type_alt)))
tract_df$tract_type_alt = ifelse(is.na(tract_df$tract_type_alt)==T, "Undesignated Non-LIC", tract_df$tract_type_alt)
unique(tract_df$tract_type_alt)

tract_df[is.na(tract_df)] <- "N/A"
tract_df=merge(tract_df,IDX.dat, by.x = "GEOID", by.y = "FIPS", all.x = T, sort = F )
tract_df=subset(tract_df, STATEFP != "72") # remove Puerto Rico

lic_df = as.data.frame(tract_df[c("GEOID", 'LIC', 'OZ_type','tract_type','tract_type_alt','labor_idx','pov_idx','edu_idx','afford_idx','mig_idx','combined_index')])
lic_df = as.data.frame(lic_df[c("GEOID", 'LIC', 'OZ_type','tract_type','tract_type_alt','labor_idx','pov_idx','edu_idx','afford_idx','mig_idx','combined_index')])
glimpse(lic_df)
openxlsx::write.xlsx(lic_df,'~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/Extra_Shapefiles/LIC Tracts/LIC.xlsx')
(unique(lic_df$LIC))


#### Albers-USA Projection ####

# tract_centr_ab=read.csv('/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/states_shapfiles/Tract_cent_ab.csv')
# tract_df <- merge(tract_df, tract_centr_ab, all.x = T, sort = F)

#### Standard Projection ####
tract_df$lat <- as.numeric( sub("+", "", tract_df$INTPTLAT) )
tract_df$lng <- as.numeric( sub("+", "", tract_df$INTPTLON) )

#### Alt Standard Projection ####

# tract_centr=read.csv('/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/states_shapfiles/Tract_cent.csv')
# tract_df <- merge(tract_df, tract_centr, all.x = T)


names(tract_df)
tract_df=within(tract_df,rm(STATEFP, COUNTYFP,TRACTCE,NAME,NAMELSAD,MTFCC,FUNCSTAT,ALAND,AWATER,INTPTLAT,INTPTLON, LIC_detailed, tract_type_alt, tract_type))
tract_df=within(tract_df,rm(State_Name,County_Name))
tract_df=within(tract_df,rm(X))
glimpse(tract_df)

# tract_df$lng <- -1*as.numeric( sub("-", "", tract_df$INTPTLON) )
 
# st_crs(tract_df) ="+proj=longlat +init=epsg:4326"
glimpse(tract_df)
tract_df.sp=as_Spatial(tract_df, cast = TRUE, IDs = paste0("ID", 1:length(tract_df)))
glimpse(tract_df.sp)

# writeOGR(tract_df.sp, layer = 'tract_df.sp', '~/Desktop/Welfare_Policy/Struggling_Regions/Focused Analyses/Metro Analyses/', driver="ESRI Shapefile")

# st_write(tract_df, "~/Desktop/Welfare_Policy/Struggling_Regions/Focused Analyses/Metro Analyses/tract_df.shp", driver="ESRI Shapefile", update = TRUE)  # create to a shapefile
# write_sf(obj=tract_df, dsn="~/Desktop/Welfare_Policy/Struggling_Regions/Focused Analyses/Metro Analyses/", layer="tract_df", driver="ESRI Shapefile",update=T)

geojson_write(input = tract_df.sp, file = "/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Final_Shapefile/Cluster_Final.geojson", overwrite = TRUE)
# geojson_write(input = tract_df.sp.samp, file = "/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Final_Shapefile/Cluster_Final_samp.geojson", overwrite = TRUE)


###### States ######
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