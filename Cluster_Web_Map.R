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

names(Clusters_df)[names(Clusters_df) == 'Cluster.Name'] = 'Final_Cluster'
names(Clusters_df)[names(Clusters_df) == 'Opportunity.Zone.Status'] = 'OZ'

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


### Read Tract Shapes ### 
tract_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Shapefiles/", crs = 4269)
tract_shapes=subset(tract_shapes, as.numeric(ALAND) > 0.1)
tract_shapes$country_fips <- paste0(tract_shapes$STATEFP,tract_shapes$COUNTYFP)
tract_shapes$GEOID=as.character(tract_shapes$GEOID)

# Tract 15
tract_15=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/2015_Poverty&FamilyInc.csv', stringsAsFactors = F)
tract_15$Geo_FIPS=str_pad(tract_15$Geo_FIPS, 11, pad = "0")
names(tract_15)[names(tract_15) == 'SE_A00001_001'] = 'TPop_15'
names(tract_15)[names(tract_15) == 'SE_A14010_001'] = 'tract_MFI_15'
names(tract_15)[names(tract_15) == 'PCT_SE_B13004_002'] = 'perc_fpl'
tract_15$cond_pov15=ifelse(tract_15$perc_fpl >= 20, T, F)
tract_15=tract_15[c("Geo_FIPS", 'TPop_15', 'tract_MFI_15', 'cond_pov15')]

# Tract 16
tract_16=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/2016_Poverty&FamilyInc.csv', stringsAsFactors = F)
tract_16$Geo_FIPS=str_pad(tract_16$Geo_FIPS, 11, pad = "0")
names(tract_16)[names(tract_16) == 'SE_A00001_001'] = 'TPop_16'
names(tract_16)[names(tract_16) == 'SE_A14010_001'] = 'tract_MFI_16'
names(tract_16)[names(tract_16) == 'PCT_SE_B13004_002'] = 'perc_fpl'
tract_16$cond_pov16=ifelse(tract_16$perc_fpl >= 20, T, F)
tract_16=tract_16[c("Geo_FIPS", 'TPop_16', 'tract_MFI_16', 'cond_pov16')]

tract_cond=merge(tract_15,tract_16)
tract_shapes=merge(tract_shapes,tract_cond,by.x='GEOID',by.y='Geo_FIPS')

### Read State Shapes ### 
state_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/states_shapfiles/", crs = 4269)
state_shapes=state_shapes[c("STATE_NAME", 'STATE_FIPS')]

# State 2015
state_15=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/2015_StateFamilyInc.csv', stringsAsFactors = F)
state_15$Geo_FIPS=str_pad(state_15$Geo_FIPS, 2, pad = "0")
names(state_15)[names(state_15) == 'SE_A14010_001'] = 'state_MFI_15'
state_15=state_15[c("Geo_FIPS", 'state_MFI_15')]

# State 2016
state_16=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/2016_StateFamilyInc.csv', stringsAsFactors = F)
state_16$Geo_FIPS=str_pad(state_16$Geo_FIPS, 2, pad = "0")
names(state_16)[names(state_16) == 'SE_A14010_001'] = 'state_MFI_16'
state_16=state_16[c("Geo_FIPS", 'state_MFI_16')]

state_cond=merge(state_15,state_16)
state_shapes=merge(state_shapes,state_cond,by.x='STATE_FIPS',by.y='Geo_FIPS', all=T)
st_geometry(state_shapes) <- NULL
state_shapes=as.data.frame(state_shapes)

### Read Metro Shapes ### 
metro_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Opportunity Zones - Shapefiles/Metro Areas/tl_2019_us_cbsa/", crs = 4269)
metro_shapes=metro_shapes[c("NAME", 'geometry', 'CBSAFP')]
names(metro_shapes)[names(metro_shapes) == 'NAME'] = 'METRO_NAME'

metro_shp_15=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/Historical Geographies/tl_2015_us_cbsa/", crs = 4269)
metro_shp_15=metro_shp_15[c("NAME", 'geometry', 'CBSAFP')]
metro_shp_15$metro_15=T
names(metro_shp_15)[names(metro_shp_15) == 'NAME'] = 'METRO_NAME_15'
metro_shp_15$METRO_NAME_15=NULL

metro_shp_16=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/Historical Geographies/tl_2016_us_cbsa/", crs = 4269)
metro_shp_16=metro_shp_16[c("NAME", 'geometry', 'CBSAFP')]
metro_shp_16$metro_16=T
names(metro_shp_16)[names(metro_shp_16) == 'NAME'] = 'METRO_NAME_16'
metro_shp_16$METRO_NAME_16=NULL

# Metro 2015
metro_15=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/2015_MetroFamilyInc.csv', stringsAsFactors = F)
dupmet15=metro_15[duplicated(metro_15$Geo_CBSA),]
dupmet15
metro_15$Geo_CBSA=str_pad(metro_15$Geo_CBSA, 5, pad = "0")
names(metro_15)[names(metro_15) == 'SE_A00001_001'] = 'MPop_15'
names(metro_15)[names(metro_15) == 'SE_A14010_001'] = 'metro_MFI_15'
names(metro_15)[names(metro_15) == 'PCT_SE_B13004_002'] = 'perc_fpl'
# metro_15$cond_metro_MFI_15=ifelse(metro_15$metro_MFI_15 >= 80, T, F)
metro_15=metro_15[c("Geo_CBSA", 'metro_MFI_15')]

# Metro 2016
metro_16=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/2016_MetroFamilyInc.csv', stringsAsFactors = F)
metro_16$Geo_CBSA=str_pad(metro_16$Geo_CBSA, 5, pad = "0")
names(metro_16)[names(metro_16) == 'SE_A00001_001'] = 'MPop_16'
names(metro_16)[names(metro_16) == 'SE_A14010_001'] = 'metro_MFI_16'
names(metro_16)[names(metro_16) == 'PCT_SE_B13004_002'] = 'perc_fpl'
# metro_16$cond_metro_MFI_16=ifelse(metro_16$metro_MFI_16 >= 80, T, F)
metro_16=metro_16[c("Geo_CBSA", 'metro_MFI_16')]

metro_15=merge(metro_shp_15,metro_15,by.x='CBSAFP',by.y='Geo_CBSA', all=T)
any(is.na(metro_15))

metro_16=merge(metro_shp_16,metro_16,by.x='CBSAFP',by.y='Geo_CBSA', all=T)
any(is.na(metro_16))


### Read County Shapes ###
county_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/county_shapefiles/", crs = 4269)
HMRC_dat=openxlsx::read.xlsx('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/High-Migration-Rural-Counties/CDFI-Fund-High-Migration-Rural-Counties.xlsx')
HMRC_dat=HMRC[c("FIPS", 'HMRC')]
county_shapes=merge(county_shapes,HMRC_dat,by.x="GEOID", by.y="FIPS", all = T)
county_shapes=county_shapes[c("GEOID","NAMELSAD","HMRC", 'geometry')]
county_shapes$HMRC[is.na(county_shapes$HMRC)] <- F
names(county_shapes)[names(county_shapes) == 'NAMELSAD'] = 'COUNTY_NAME'
st_geometry(county_shapes) <- NULL
county_shapes=as.data.frame(county_shapes)

### Read Empowerment Zone Shapes ### 
emp_zone=sf::st_read('~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Shapefiles/Empowerment_Zones_and_Enterprise_Communities')
emp_zone=subset(emp_zone, TYPE == "Empowerment Zone")
names(emp_zone)[names(emp_zone) == 'TYPE'] = 'Empowerment_Zone'
emp_zone=emp_zone[c("Empowerment_Zone", 'geometry')]

# Merge files
metro_shapes=st_transform(metro_shapes, 4269)
tract_shapes=st_transform(tract_shapes, 4269)
emp_zone=st_transform(emp_zone, 4269)

st_crs(tract_shapes) <- 4269
st_crs(metro_shapes) <- 4269
st_crs(emp_zone) <- 4269
emp_zone=emp_zone%>% lwgeom::st_make_valid()

tract_shapes=merge(tract_shapes, state_shapes, by.x="STATEFP", by.y ="STATE_FIPS", all.x = T)
tract_shapes=merge(tract_shapes, county_shapes, by.x="country_fips", by.y ="GEOID", all.x = T)
tract_shapes=st_join(tract_shapes,  metro_shapes, left = T, largest = T)
tract_shapes=st_join(tract_shapes,  metro_15, left = T, largest = T)
tract_shapes=st_join(tract_shapes,  metro_16, left = T, largest = T)
tract_shapes=st_join(tract_shapes,  emp_zone, left = T, largest = T)

tract_shapes$metro = T
tract_shapes$metro[is.na(tract_shapes$METRO_NAME)] <- F
tract_shapes$metro_15[is.na(tract_shapes$metro_15)] <- F
tract_shapes$metro_16[is.na(tract_shapes$metro_16)] <- F

names(tract_shapes)

tract_shapes$empzone_tf=ifelse( as.character(tract_shapes$Empowerment_Zone)=="Empowerment Zone", T ,F)
tract_shapes$empzone_tf[is.na(tract_shapes$Empowerment_Zone)] <- F
unique(tract_shapes$Empowerment_Zone)
unique(tract_shapes$empzone_tf)
save(tract_shapes, file = '~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/tract_shapes.Rdata')

############# Check LIC Conditions ##############
load(file = '~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/tract_shapes.Rdata')
tract_shapes$Empowerment_Zone=NULL

table(is.na(tract_shapes$cond_pov15))
table(is.na(tract_shapes$cond_pov16))

table(is.na(tract_shapes$tract_MFI_15))
table(is.na(tract_shapes$tract_MFI_16))

table(is.na(tract_shapes$metro_MFI_15))
table(is.na(tract_shapes$metro_MFI_16))

table(is.na(tract_shapes$state_MFI_15))
table(is.na(tract_shapes$state_MFI_16))

table(is.na(tract_shapes$metro))
table(is.na(tract_shapes$metro))

# Make NA Values False
tract_shapes$cond_pov15[is.na(tract_shapes$cond_pov15)] <- F
tract_shapes$cond_pov16[is.na(tract_shapes$cond_pov16)] <- F

#### Conditions set for 2015 ####
tract_shapes$LIC_2015=F
unique(tract_shapes$LIC_2015)
glimpse(tract_shapes)

# 2015 Condition #1: Is this tract poverty rate >= 20%
tract_shapes$LIC_2015=ifelse(tract_shapes$cond_pov15 == T, T, tract_shapes$LIC_2015)
unique(tract_shapes$LIC_2015)
tract_shapes$cond1_2015=tract_shapes$LIC_2015

# 2015 Condition #2: Is this tract in a metropolitan area AND MFI <= 80% of state OR metropolitan MFI?
tract_shapes$LIC_2015=ifelse(tract_shapes$cond_pov15 == F & tract_shapes$metro_15 == T & (tract_shapes$tract_MFI_15 <= (tract_shapes$metro_MFI_15 * .80)) | (tract_shapes$tract_MFI_15 <= (tract_shapes$state_MFI_15 * .80)), T, tract_shapes$LIC_2015)
unique(tract_shapes$LIC_2015)
tract_shapes$cond2_2015=tract_shapes$LIC_2015

# 2015 Condition #3: Is this tract, non-metro AND statewide MFI <= 80% of the statewide MFI?
tract_shapes$LIC_2015=ifelse(tract_shapes$cond_pov15 == F & tract_shapes$metro_15 == F & tract_shapes$tract_MFI_15 <= (tract_shapes$state_MFI_15 * .80), T, tract_shapes$LIC_2015)
tract_shapes$cond3_2015=tract_shapes$LIC_2015

# 2015 Condition #4: Is this tract, non-metro AND statewide MFI >= 80% AND a HMRC AND has statewide MFI <= 85% ?
tract_shapes$LIC_2015=ifelse(tract_shapes$cond_pov15 == F & tract_shapes$metro_15 == F & 
		tract_shapes$tract_MFI_15 > (tract_shapes$state_MFI_15 * .80)
		& tract_shapes$HMRC==T & tract_shapes$tract_MFI_15 <= (tract_shapes$state_MFI_15 * .85),
		T, tract_shapes$LIC_2015)
unique(tract_shapes$LIC_2015)
tract_shapes$LIC_2015[is.na(tract_shapes$LIC_2015)] <- F
tract_shapes$cond4_2015=tract_shapes$LIC_2015


### Is Contiguous to LIC 2015? ###
LIC_contig_15 = subset(tract_shapes, LIC_2015 == T)
non_contig_15=subset(tract_shapes,LIC_2015 == F)
non_contig_15=non_contig_15[c("GEOID",'geometry')]
intersect_ts_15 <- st_intersects(non_contig_15, LIC_contig_15)
inter_15 = as.character(intersect_ts_15)
C_int_15=cbind(inter_15,non_contig_15)
C_int_15$inter_15=ifelse(C_int_15$inter_15=='integer(0)',F,T)
st_geometry(C_int_15) <- NULL
C_int_15=as.data.frame(C_int_15)
tract_shapes=merge(tract_shapes,C_int_15,all.x =T)
tract_shapes$inter_15[is.na(tract_shapes$inter_15)] <- F
unique(tract_shapes$inter_15)

# 2015 Condition #5: Is this tract in a metropolitan area AND MFI >= 80% of state OR metropolitan MFI AND population < 2000 AND in an empowerment zone AND is contiguous with LIC tracts?
tract_shapes$LIC_2015_f=ifelse(tract_shapes$cond_pov15 == F & tract_shapes$metro_15 == T &
		tract_shapes$TPop_15 < 2000 & tract_shapes$inter_15 == T & tract_shapes$empzone_tf == T &
		((tract_shapes$tract_MFI_15 > tract_shapes$metro_MFI_15 * .80) |
		(tract_shapes$tract_MFI_15 > tract_shapes$state_MFI_15 * .80)), 
		T, tract_shapes$LIC_2015)
tract_shapes$cond5_2015=tract_shapes$LIC_2015_f
unique(tract_shapes$LIC_2015_f)


# 2015 Condition #6: Is this tract, non-metro AND statewide MFI >= 80% AND a HMRC AND population < 2000 AND in an empowerment zone AND is contiguous with LIC tracts?
tract_shapes$LIC_2015_f=ifelse(tract_shapes$cond_pov15 == F & tract_shapes$metro_15 == F & 
		tract_shapes$tract_MFI_15 > (tract_shapes$state_MFI_15 * .80) & tract_shapes$HMRC==F
		& tract_shapes$TPop_15 < 2000 & tract_shapes$inter_15 == T & tract_shapes$empzone_tf == T,
		T, tract_shapes$LIC_2015_f)

unique(tract_shapes$LIC_2015_f)
tract_shapes$LIC_2015_f[is.na(tract_shapes$LIC_2015_f)] <- F


#### Conditions set for 2016 ####
tract_shapes$LIC_2016=F
unique(tract_shapes$LIC_2016)

# 2016 Condition #1: Is this tract poverty rate >= 20%
tract_shapes$LIC_2016=ifelse(tract_shapes$cond_pov16 == T, T, tract_shapes$LIC_2016)
tract_shapes$cond1_2016=tract_shapes$LIC_2016
unique(tract_shapes$LIC_2016)

# 2016 Condition #2: Is this tract in a metropolitan area AND MFI <= 80% of state OR metropolitan MFI?
tract_shapes$LIC_2016=ifelse(tract_shapes$cond_pov16 == F & tract_shapes$metro_16 == T & 
		((tract_shapes$tract_MFI_16 <= (tract_shapes$metro_MFI_16 * .80)) | 
		(tract_shapes$tract_MFI_16 <= (tract_shapes$state_MFI_16 * .80))) 
		, T, tract_shapes$LIC_2016)
unique(tract_shapes$LIC_2016)
tract_shapes$LIC_2016[is.na(tract_shapes$LIC_2016)] <- F
tract_shapes$cond2_2016=tract_shapes$LIC_2016

# 2016 Condition #3: Is this tract, non-metro AND statewide MFI <= 80% of the statewide MFI?
tract_shapes$LIC_2016=ifelse(tract_shapes$cond_pov16 == F & tract_shapes$metro_16 == F & 
		tract_shapes$tract_MFI_16 <= (tract_shapes$state_MFI_16 * .80)
		, T, tract_shapes$LIC_2016)
unique(tract_shapes$LIC_2016)
tract_shapes$LIC_2016[is.na(tract_shapes$LIC_2016)] <- F
tract_shapes$cond3_2016=tract_shapes$LIC_2016

# 2016 Condition #4: Is this tract, non-metro AND statewide MFI >= 80% AND a HMRC AND has statewide MFI <= 85% ?
tract_shapes$LIC_2016=ifelse(tract_shapes$cond_pov16 == F & tract_shapes$metro_16 == F 
		& tract_shapes$tract_MFI_16 > (tract_shapes$state_MFI_16 * .80)
		& tract_shapes$tract_MFI_16 <= (tract_shapes$state_MFI_16 * .85) & tract_shapes$HMRC==T ,
		T, tract_shapes$LIC_2016)
unique(tract_shapes$LIC_2016)
tract_shapes$LIC_2016[is.na(tract_shapes$LIC_2016)] <- F
tract_shapes$cond4_2016=tract_shapes$LIC_2016

## Is Contiguous to LIC 2016? ## 

LIC_contig_16 = subset(tract_shapes, LIC_2016 == T)
non_contig_16=subset(tract_shapes,LIC_2016 == F)
non_contig_16=non_contig_16[c("GEOID",'geometry')]
intersect_ts_16 <- st_intersects(non_contig_16, LIC_contig_16)
inter_16 = as.character(intersect_ts_16)
C_int_16=cbind(inter_16,non_contig_16)
C_int_16$inter_16=ifelse(C_int_16$inter_16=='integer(0)',F,T)
st_geometry(C_int_16) <- NULL
C_int_16=as.data.frame(C_int_16)
tract_shapes=merge(tract_shapes,C_int_16,all.x =T)
tract_shapes$inter_16[is.na(tract_shapes$inter_16)] <- F
unique(tract_shapes$inter_16)

# 2016 Condition #5: Is this tract in a metropolitan area AND MFI >= 80% of state OR metropolitan MFI AND population < 2000 AND in an empowerment zone AND is contiguous with LIC tracts?
tract_shapes$LIC_2016_f=ifelse(tract_shapes$cond_pov16 == F & tract_shapes$metro_16 == T 
		& tract_shapes$TPop_16 < 2000 & tract_shapes$inter_16 == T & tract_shapes$empzone_tf == T &
		((tract_shapes$tract_MFI_16 > tract_shapes$metro_MFI_16 * .80) |
		(tract_shapes$tract_MFI_16 > tract_shapes$state_MFI_16 * .80)) ,
		T, tract_shapes$LIC_2016)
tract_shapes$LIC_2016_f[is.na(tract_shapes$LIC_2016_f)] <- F
tract_shapes$cond5_2016=tract_shapes$LIC_2016_f
unique(tract_shapes$LIC_2016)

# 2016 Condition #6: Is this tract, non-metro AND statewide MFI >= 80% AND a HMRC AND population < 2000 AND in an empowerment zone AND is contiguous with LIC tracts?
tract_shapes$LIC_2016_f=ifelse(tract_shapes$cond_pov16 == F & tract_shapes$metro_16 == F & 
		tract_shapes$tract_MFI_16 > (tract_shapes$state_MFI_16 * .80) & tract_shapes$HMRC==F
		& tract_shapes$TPop_16 < 2000 & tract_shapes$inter_16 == T & tract_shapes$empzone_tf == T,
		T, tract_shapes$LIC_2016_f)
tract_shapes$LIC_2016_f[is.na(tract_shapes$LIC_2016_f)] <- F

# Combine 2015 & 2016 LIC calcs
tract_shapes$final_LIC=ifelse(tract_shapes$LIC_2016_f == T | tract_shapes$LIC_2015_f == T, T, F)
unique(tract_shapes$final_LIC)

# Non-LIC Contiguous Tract

# LIC_contig_15_f = subset(tract_shapes, LIC_2015_f == T)
# non_contig_f=subset(tract_shapes,LIC_2015_f == F)
# non_contig_f=non_contig_f[c("GEOID",'geometry')]
# inter_contig_f <- st_intersects(non_contig_f, LIC_contig_15_f)
# inter_contig_f = as.character(inter_contig_f)
# C_int_f=cbind(inter_contig_f,non_contig_f)
# C_int_f$inter_contig_f=ifelse(C_int_f$inter_contig_f=='integer(0)',F,T)
# st_geometry(C_int_f) <- NULL
# C_int_f=as.data.frame(C_int_f)
# tract_shapes=merge(tract_shapes,C_int_f,all.x =T)
# tract_shapes$inter_contig_f[is.na(tract_shapes$inter_contig_f)] <- F

# Contiguous Eligible Tracts
# tract_shapes$f_CONTIG=ifelse(tract_shapes$LIC_2016_f == T | tract_shapes$LIC_2015_f == T, T, F)

# Compare to other 

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

tract_df$LIC_tf=ifelse(tract_df$OZ_type=='Low-Income Community' | tract_df$LIC=='Undesignated Low-Income Community',T,F)
tract_df$LIC_tf = ifelse(is.na(tract_df$LIC_tf), F, tract_df$LIC_tf)
(unique(tract_df$OZ_type))

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

names(tract_df)

lic_df = as.data.frame(tract_df[c("GEOID", 'LIC', 'OZ_type','tract_type','tract_type_alt','labor_idx','pov_idx','edu_idx','afford_idx','mig_idx','combined_index',
	"cond1_2015" , "cond2_2015" , "cond3_2015" ,"cond4_2015",'cond5_2015'
	,"cond1_2016" , "cond2_2016" , "cond3_2016" ,"cond4_2016",'cond5_2016',
	'LIC_tf','final_LIC')])
lic_df = as.data.frame(lic_df[c("GEOID", 'LIC', 'OZ_type','tract_type','tract_type_alt','labor_idx','pov_idx','edu_idx','afford_idx','mig_idx','combined_index',
	"cond1_2015" , "cond2_2015" , "cond3_2015" ,"cond4_2015",'cond5_2015'
	,"cond1_2016" , "cond2_2016" , "cond3_2016" ,"cond4_2016",'cond5_2016',
	'LIC_tf','final_LIC')])
glimpse(lic_df)
openxlsx::write.xlsx(lic_df,'~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/Extra_Shapefiles/LIC Tracts/LIC.xlsx')
(unique(lic_df$final_LIC))


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