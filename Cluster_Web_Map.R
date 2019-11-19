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

names(Clusters_df)[names(Clusters_df) == 'Promising_Cluster'] = 'Final_Cluster'
names(Clusters_df)[names(Clusters_df) == 'OZ1'] = 'OZ'
Clusters_df
Clusters_df$Final_Cluster = ifelse(Clusters_df$OZ1 == 'Opportunity Zone','OZ',
	ifelse(Clusters_df$OZ1 == 'Undesignated','Non-OZ',
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
Clusters_df=Clusters_df[c('Fips', 'Final_Cluster','OZ')]

native_tracts=sf::st_read('~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Shapefiles/Native American Tracts', crs = 4269)
native_tracts$Native_Tract = "Yes"
native_tracts=native_tracts[c("geometry", 'Native_Tract')]

### Read Tract Shapes ### 
tract_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Shapefiles/", crs = 4269)
tract_shapes$country_fips <- paste0(tract_shapes$STATEFP,tract_shapes$COUNTYFP)
tract_shapes$GEOID=as.character(tract_shapes$GEOID)

# Tract 15
tract_15=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/2015_Poverty&FamilyInc.csv', stringsAsFactors = F)
tract_15$Geo_FIPS=str_pad(tract_15$Geo_FIPS, 11, pad = "0")
names(tract_15)[names(tract_15) == 'SE_A00001_001'] = 'TPop_15'
names(tract_15)[names(tract_15) == 'SE_A14010_001'] = 'tract_MFI_15'
names(tract_15)[names(tract_15) == 'PCT_SE_B13004_002'] = 'Tperc_fpl_15'
# tract_15$cond_pov15=ifelse(round(tract_15$Tperc_fpl_15,0) >= 20, T, F)
tract_15=tract_15[c("Geo_FIPS", 'TPop_15', 'tract_MFI_15','Tperc_fpl_15')]

tract_15$Geo_FIPS= ifelse(
	        tract_15$Geo_FIPS == "51515050100", "51019050100",
	ifelse(tract_15$Geo_FIPS ==  "04019002701", "04019002704",
	ifelse(tract_15$Geo_FIPS ==  "04019002903", "04019002906",
	ifelse(tract_15$Geo_FIPS ==  "04019410501", "04019004118",
	ifelse(tract_15$Geo_FIPS ==  "04019410502", "04019004121",
	ifelse(tract_15$Geo_FIPS ==  "04019410503", "04019004125",
	ifelse(tract_15$Geo_FIPS ==  "04019470400", "04019005200",
	ifelse(tract_15$Geo_FIPS ==  "04019470500", "04019005300",
	ifelse(tract_15$Geo_FIPS ==  "06037930401", "06037137000",
	ifelse(tract_15$Geo_FIPS ==  "02270000100", "02158000100",
	ifelse(tract_15$Geo_FIPS ==  "46113940500", "46102940500",
	ifelse(tract_15$Geo_FIPS ==  "46113940800", "46102940800",
	ifelse(tract_15$Geo_FIPS ==  "46113940900", "46102940900",
	ifelse(tract_15$Geo_FIPS ==  "36053940101", "36053030101",
	ifelse(tract_15$Geo_FIPS ==  "36053940102", "36053030102",
	ifelse(tract_15$Geo_FIPS ==  "36053940103", "36053030103",
	ifelse(tract_15$Geo_FIPS ==  "36053940200", "36053030200",
	ifelse(tract_15$Geo_FIPS ==  "36053940300", "36053030300",
	ifelse(tract_15$Geo_FIPS ==  "36053940401", "36053030401",
	ifelse(tract_15$Geo_FIPS ==  "36053940403", "36053030403",
	ifelse(tract_15$Geo_FIPS ==  "36053940600", "36053030600",
	ifelse(tract_15$Geo_FIPS ==  "36053940700", "36053030402",
	ifelse(tract_15$Geo_FIPS ==  "36065940000", "36065024800",
	ifelse(tract_15$Geo_FIPS ==  "36065940100", "36065024700",
	ifelse(tract_15$Geo_FIPS ==  "36065940200", "36065024900", tract_15$Geo_FIPS)))))))))))))))))))))))))


# Tract 16
tract_16=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/2016_Poverty&FamilyInc.csv', stringsAsFactors = F)
tract_16$Geo_FIPS=str_pad(tract_16$Geo_FIPS, 11, pad = "0")
names(tract_16)[names(tract_16) == 'SE_A00001_001'] = 'TPop_16'
names(tract_16)[names(tract_16) == 'SE_A14010_001'] = 'tract_MFI_16'
names(tract_16)[names(tract_16) == 'PCT_SE_B13004_002'] = 'Tperc_fpl_16'
# tract_16$cond_pov16=ifelse(round(tract_16$Tperc_fpl_16,0) >= 20, T, F)
tract_16=tract_16[c("Geo_FIPS", 'TPop_16', 'tract_MFI_16','Tperc_fpl_16')]

tract_16$Geo_FIPS= ifelse(
	        tract_16$Geo_FIPS == "51515050100", "51019050100",
	ifelse(tract_16$Geo_FIPS ==  "04019002701", "04019002704",
	ifelse(tract_16$Geo_FIPS ==  "04019002903", "04019002906",
	ifelse(tract_16$Geo_FIPS ==  "04019410501", "04019004118",
	ifelse(tract_16$Geo_FIPS ==  "04019410502", "04019004121",
	ifelse(tract_16$Geo_FIPS ==  "04019410503", "04019004125",
	ifelse(tract_16$Geo_FIPS ==  "04019470400", "04019005200",
	ifelse(tract_16$Geo_FIPS ==  "04019470500", "04019005300",
	ifelse(tract_16$Geo_FIPS ==  "06037930401", "06037137000",
	ifelse(tract_16$Geo_FIPS ==  "02270000100", "02158000100",
	ifelse(tract_16$Geo_FIPS ==  "46113940500", "46102940500",
	ifelse(tract_16$Geo_FIPS ==  "46113940800", "46102940800",
	ifelse(tract_16$Geo_FIPS ==  "46113940900", "46102940900",
	ifelse(tract_16$Geo_FIPS ==  "36053940101", "36053030101",
	ifelse(tract_16$Geo_FIPS ==  "36053940102", "36053030102",
	ifelse(tract_16$Geo_FIPS ==  "36053940103", "36053030103",
	ifelse(tract_16$Geo_FIPS ==  "36053940200", "36053030200",
	ifelse(tract_16$Geo_FIPS ==  "36053940300", "36053030300",
	ifelse(tract_16$Geo_FIPS ==  "36053940401", "36053030401",
	ifelse(tract_16$Geo_FIPS ==  "36053940403", "36053030403",
	ifelse(tract_16$Geo_FIPS ==  "36053940600", "36053030600",
	ifelse(tract_16$Geo_FIPS ==  "36053940700", "36053030402",
	ifelse(tract_16$Geo_FIPS ==  "36065940000", "36065024800",
	ifelse(tract_16$Geo_FIPS ==  "36065940100", "36065024700",
	ifelse(tract_16$Geo_FIPS ==  "36065940200", "36065024900", tract_16$Geo_FIPS)))))))))))))))))))))))))


tract_cond=merge(tract_15,tract_16,all=T)
tract_shapes=merge(tract_shapes,tract_cond,by.x='GEOID',by.y='Geo_FIPS',all.x=T)

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
# metro_shp_15=subset(metro_shp_15, LSAD=='M1')
metro_shp_15$metro_15=ifelse(metro_shp_15$LSAD=='M1',T,F)
metro_shp_15=metro_shp_15[c("NAME", 'geometry', 'CBSAFP','metro_15')]
names(metro_shp_15)[names(metro_shp_15) == 'NAME'] = 'METRO_NAME_15'
# metro_shp_15$METRO_NAME_15=NULL

metro_shp_16=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/Historical Geographies/tl_2016_us_cbsa/", crs = 4269)
unique(metro_shp_16$LSAD)
# metro_shp_16=subset(metro_shp_16, LSAD=='M1')
metro_shp_16$metro_16=ifelse(metro_shp_16$LSAD=='M1',T,F)
metro_shp_16=metro_shp_16[c("NAME", 'geometry', 'CBSAFP','metro_16')]
names(metro_shp_16)[names(metro_shp_16) == 'NAME'] = 'METRO_NAME_16'
# metro_shp_16$METRO_NAME_16=NULL

# Metro 2015
metro_15=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/2015_MetroFamilyInc.csv', stringsAsFactors = F)
metro_15$Geo_CBSA=str_pad(metro_15$Geo_CBSA, 5, pad = "0")
names(metro_15)[names(metro_15) == 'SE_A00001_001'] = 'MPop_15'
names(metro_15)[names(metro_15) == 'SE_A14010_001'] = 'metro_MFI_15'
names(metro_15)[names(metro_15) == 'PCT_SE_B13004_002'] = 'perc_fpl_m15'
names(metro_15)[names(metro_15) == 'Geo_CBSA'] = 'CBSAFP_15'
metro_15=metro_15[c("CBSAFP_15", 'metro_MFI_15')]

# Metro 2016
metro_16=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/2016_MetroFamilyInc.csv', stringsAsFactors = F)
metro_16$Geo_CBSA=str_pad(metro_16$Geo_CBSA, 5, pad = "0")
names(metro_16)[names(metro_16) == 'SE_A00001_001'] = 'MPop_16'
names(metro_16)[names(metro_16) == 'SE_A14010_001'] = 'metro_MFI_16'
names(metro_16)[names(metro_16) == 'PCT_SE_B13004_002'] = 'perc_fpl_m16'
names(metro_16)[names(metro_16) == 'Geo_CBSA'] = 'CBSAFP_16'
metro_16=metro_16[c("CBSAFP_16", 'metro_MFI_16')]

metro_15=merge(metro_shp_15,metro_15,by.x='CBSAFP',by.y='CBSAFP_15', all.x=T)
metro_16=merge(metro_shp_16,metro_16,by.x='CBSAFP',by.y='CBSAFP_16', all.x=T)

metro_15$CBSAFP=NULL
metro_16$CBSAFP=NULL
metro_shapes$CBSAFP=NULL

any(is.na(metro_16))
any(is.na(metro_15))

### Read County Shapes ###
county_shapes=sf::st_read("~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/county_shapefiles/", crs = 4269)
HMRC_dat=openxlsx::read.xlsx('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/High-Migration-Rural-Counties/CDFI-Fund-High-Migration-Rural-Counties.xlsx')
HMRC_dat=HMRC_dat[c("FIPS", 'HMRC')]

county_15=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/2015_CountyFamilyInc.csv', stringsAsFactors = F)
county_15$Geo_FIPS=str_pad(county_15$Geo_FIPS, 5, pad = "0")
names(county_15)[names(county_15) == 'SE_A14010_001'] = 'county_MFI_15'
names(county_15)[names(county_15) == 'PCT_SE_B13004_002'] = 'county_fpl_15'
county_15=county_15[c("Geo_FIPS", 'county_MFI_15','county_fpl_15')]

county_16=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/2016_CountyFamilyInc.csv', stringsAsFactors = F)
county_16$Geo_FIPS=str_pad(county_16$Geo_FIPS, 5, pad = "0")
names(county_16)[names(county_16) == 'SE_A14010_001'] = 'county_MFI_16'
names(county_16)[names(county_16) == 'PCT_SE_B13004_002'] = 'county_fpl_16'

county_16=county_16[c("Geo_FIPS", 'county_MFI_16','county_fpl_16')]

county_shapes=merge(county_shapes,HMRC_dat,by.x="GEOID", by.y="FIPS", all.x = T)
county_shapes=merge(county_shapes,county_15,by.x="GEOID", by.y="Geo_FIPS", all.x = T)
county_shapes=merge(county_shapes,county_16,by.x="GEOID", by.y="Geo_FIPS", all.x = T)

county_shapes=county_shapes[c("GEOID","NAMELSAD","HMRC", 'geometry', 'county_MFI_15', 'county_MFI_16')]
county_shapes$HMRC[is.na(county_shapes$HMRC)] <- F
names(county_shapes)[names(county_shapes) == 'NAMELSAD'] = 'COUNTY_NAME'
st_geometry(county_shapes) <- NULL
county_shapes=as.data.frame(county_shapes)

### Read Empowerment Zone Shapes ### 
emp_zone=sf::st_read('~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Shapefiles/Empowerment_Zones_and_Enterprise_Communities', crs = 4269)
emp_zone=subset(emp_zone, TYPE == "Empowerment Zone")
names(emp_zone)[names(emp_zone) == 'TYPE'] = 'Empowerment_Zone'
emp_zone=emp_zone[c("Empowerment_Zone", 'geometry')]


# op_zone=sf::st_read('/Users/rorr/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/Opportunity_Zones', crs = 4269)
# op_zone=subset(emp_zone, TYPE == "Opportunity Zone")
# names(op_zone)[names(emp_zone) == 'TYPE'] = 'Opportunity Zone'
# op_zone=op_zone[c("Opportunity Zone", 'geometry')]

# Merge files
tract_shapes=st_transform(tract_shapes, 4269)
metro_shapes=st_transform(metro_shapes, 4269)
metro_15=st_transform(metro_15, 4269)
metro_16=st_transform(metro_16, 4269)
emp_zone=st_transform(emp_zone, 4269)

st_crs(tract_shapes) <- 4269
st_crs(metro_shapes) <- 4269
st_crs(metro_15) <- 4269
st_crs(metro_16) <- 4269
st_crs(emp_zone) <- 4269
emp_zone=emp_zone%>% lwgeom::st_make_valid()
tract_shapes=subset(tract_shapes, as.numeric(ALAND) > 0)

tract_shapes=merge(tract_shapes, state_shapes, by.x="STATEFP", by.y ="STATE_FIPS", all.x = T)
tract_shapes=merge(tract_shapes, county_shapes, by.x="country_fips", by.y ="GEOID", all.x = T)
tract_shapes=st_join(tract_shapes,  metro_shapes, left = T, largest = T, join = st_intersects)
tract_shapes=st_join(tract_shapes,  metro_15, left = T, largest = T, join = st_intersects)
tract_shapes=st_join(tract_shapes,  metro_16, left = T, largest = T, join = st_intersects)
tract_shapes=st_join(tract_shapes,  emp_zone, left = T, largest = T, join = st_intersects)
tract_shapes=st_join(tract_shapes,  native_tracts, left = T, largest = T, join = st_intersects)

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

## Strange Designations ##
# 40109103200 - OK City; LIC mislabeling seems innocent. Would have gotten in as a contiguous tract.
# 06037206020 - LA; close, but fails population requirement and creates a contiguous tract branch. Can’t get in as a contiguous tract. Unsalvageable
# 26163517200 - Detroit; close, but barely fails population requirement and creates a contiguous tract branch. While it can ultimately retain its OZ designation as a contiguous tract, the branching designation it created can no longer be considered legitimate. 
# 
# 06037206031 - Improper LA branching Contiguous Tract. Unsalvageable.
# 26163517000 - Improper Detroit branching Contiguous Tract. Unsalvageable.

############# Check LIC Conditions ##############
load(file = '~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/tract_shapes.Rdata')
tract_shapes$Empowerment_Zone=NULL
tract_shapes$CBSAFP.x=NULL
tract_shapes$CBSAFP.y=NULL
tract_shapes$CBSAFP=NULL

table(is.na(tract_shapes$Tperc_fpl_15))
table(is.na(tract_shapes$Tperc_fpl_16))

table(is.na(tract_shapes$tract_MFI_15))
table(is.na(tract_shapes$tract_MFI_16))

table(is.na(tract_shapes$metro_MFI_15))
table(is.na(tract_shapes$metro_MFI_16))

table(is.na(tract_shapes$state_MFI_15))
table(is.na(tract_shapes$state_MFI_16))

table(is.na(tract_shapes$metro))
table(is.na(tract_shapes$metro))

tract_shapes$metro_15[(tract_shapes$HMRC)==T] <- F
tract_shapes$metro_16[(tract_shapes$HMRC)==T] <- F


# Make NA Values False

# Side conditions
tract_shapes$tract_MFI_15[(tract_shapes$tract_MFI_15)==0] <- NA
tract_shapes$tract_MFI_16[(tract_shapes$tract_MFI_16)==0] <- NA

tract_shapes$Tperc_fpl_15[(tract_shapes$Tperc_fpl_15)==0] <- NA
tract_shapes$Tperc_fpl_16[(tract_shapes$Tperc_fpl_16)==0] <- NA

# Tract FPL 

tract_shapes$no_dat_MFI_15=ifelse(is.na(tract_shapes$tract_MFI_15), T, F)
tract_shapes$no_dat_MFI_16=ifelse(is.na(tract_shapes$tract_MFI_16), T, F)
tract_shapes$no_dat_pov_15=ifelse(is.na(tract_shapes$Tperc_fpl_15), T, F)
tract_shapes$no_dat_pov_16=ifelse(is.na(tract_shapes$Tperc_fpl_16), T, F)

# tract_shapes$tract_MFI_15[is.na(tract_shapes$tract_MFI_15)] <- tract_shapes$county_MFI_15
# tract_shapes$tract_MFI_16[is.na(tract_shapes$tract_MFI_16)] <- tract_shapes$county_MFI_16
# 
# tract_shapes$Tperc_fpl_15[is.na(tract_shapes$Tperc_fpl_15)] <- tract_shapes$county_fpl_15
# tract_shapes$Tperc_fpl_16[is.na(tract_shapes$Tperc_fpl_16)] <- tract_shapes$county_fpl_16

tract_shapes$cond_pov15=ifelse((as.numeric(tract_shapes$Tperc_fpl_15)) >= 19.95, T, F)
tract_shapes$cond_pov16=ifelse((as.numeric(tract_shapes$Tperc_fpl_16)) >= 19.95, T, F)

tract_shapes$cond_state_MFI_15=ifelse((tract_shapes$tract_MFI_15/tract_shapes$state_MFI_15) <= (.80), T, F)
tract_shapes$cond_state_MFI_15_2=ifelse((tract_shapes$tract_MFI_15/tract_shapes$state_MFI_15) <= (.85), T, F)
tract_shapes$cond_metro_MFI_15=ifelse((tract_shapes$tract_MFI_15/tract_shapes$metro_MFI_15) <=(.80), T, F)
tract_shapes$cond_tpop_15=ifelse(tract_shapes$TPop_15 < 2000, T, F)

tract_shapes$cond_state_MFI_16=ifelse((tract_shapes$tract_MFI_16/tract_shapes$state_MFI_16) <= (.80), T, F)
tract_shapes$cond_state_MFI_16_2=ifelse((tract_shapes$tract_MFI_16/tract_shapes$state_MFI_16) <= (.85), T, F)
tract_shapes$cond_metro_MFI_16=ifelse((tract_shapes$tract_MFI_16/tract_shapes$metro_MFI_16) <=(.80), T, F)
tract_shapes$cond_tpop_16=ifelse(tract_shapes$TPop_16 < 2000, T, F)

tract_shapes$cond_pov15[is.na(tract_shapes$cond_pov15)] <- F
tract_shapes$cond_pov16[is.na(tract_shapes$cond_pov16)] <- F

tract_shapes$check_state_MFI_15=(tract_shapes$tract_MFI_15/tract_shapes$state_MFI_15)
tract_shapes$check_metro_MFI_15=(tract_shapes$tract_MFI_15/tract_shapes$metro_MFI_15)
tract_shapes$check_state_MFI_16=(tract_shapes$tract_MFI_16/tract_shapes$state_MFI_16)
tract_shapes$check_metro_MFI_16=(tract_shapes$tract_MFI_16/tract_shapes$metro_MFI_16)

tract_shapes$cond_state_MFI_15[is.na(tract_shapes$cond_state_MFI_15)] <- F
tract_shapes$cond_metro_MFI_15[is.na(tract_shapes$cond_metro_MFI_15)] <- F
tract_shapes$cond_state_MFI_15_2[is.na(tract_shapes$cond_state_MFI_15_2)] <- F
tract_shapes$cond_tpop_15[is.na(tract_shapes$cond_tpop_15)] <- F

tract_shapes$cond_state_MFI_16[is.na(tract_shapes$cond_state_MFI_16)] <- F
tract_shapes$cond_metro_MFI_16[is.na(tract_shapes$cond_metro_MFI_16)] <- F
tract_shapes$cond_state_MFI_16_2[is.na(tract_shapes$cond_state_MFI_16_2)] <- F
tract_shapes$cond_tpop_16[is.na(tract_shapes$cond_tpop_16)] <- F

table(is.na(tract_shapes$cond_state_MFI_15))
table(is.na(tract_shapes$cond_state_MFI_16))

############################################################
################ LIC Conditions set for 2015 ###############
############################################################
tract_shapes$LIC_2015=F
unique(tract_shapes$LIC_2015)
glimpse(tract_shapes)

# 2015 Condition #1: Is this tract poverty rate >= 20%
tract_shapes$LIC_2015=ifelse(tract_shapes$cond_pov15 == T, T, tract_shapes$LIC_2015)
unique(tract_shapes$LIC_2015)
# tract_shapes$cond1_2015=tract_shapes$LIC_2015

# 2015 Condition #2: Is this tract in a metropolitan area AND MFI <= 80% of state OR metropolitan MFI?
tract_shapes$LIC_2015=ifelse(tract_shapes$cond_pov15 == F & tract_shapes$metro_15 == T & 
		(tract_shapes$cond_state_MFI_15 == T | tract_shapes$cond_metro_MFI_15 == T)
		, T, tract_shapes$LIC_2015)
unique(tract_shapes$LIC_2015)
# tract_shapes$cond2_2015=tract_shapes$LIC_2015

# 2015 Condition #3: Is this tract, non-metro AND statewide MFI <= 80% of the statewide MFI?
tract_shapes$LIC_2015=ifelse(tract_shapes$cond_pov15 == F & tract_shapes$metro_15 == F & 
		tract_shapes$cond_state_MFI_15 == T
		, T, tract_shapes$LIC_2015)
# tract_shapes$cond3_2015=tract_shapes$LIC_2015
unique(tract_shapes$LIC_2015)

# 2015 Condition #4: Is this tract, non-metro AND statewide MFI >= 80% AND a HMRC AND has statewide MFI <= 85% ?
tract_shapes$LIC_2015=ifelse(tract_shapes$cond_pov15 == F &
		tract_shapes$metro_15 == F & 
		tract_shapes$cond_state_MFI_15 == F
		& tract_shapes$HMRC==T & tract_shapes$cond_state_MFI_15_2 == T,
		T, tract_shapes$LIC_2015)
unique(tract_shapes$LIC_2015)
# tract_shapes$LIC_2015[is.na(tract_shapes$LIC_2015)] <- F
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
		tract_shapes$cond_tpop_15 == T & tract_shapes$inter_15 == T & tract_shapes$empzone_tf == T &
		tract_shapes$cond_state_MFI_15 == F & tract_shapes$cond_metro_MFI_15 == F, 
		T, tract_shapes$LIC_2015)
# tract_shapes$cond5_2015=tract_shapes$LIC_2015_f
unique(tract_shapes$LIC_2015_f)


# 2015 Condition #6: Is this tract, non-metro AND statewide MFI >= 80% AND a HMRC AND population < 2000 AND in an empowerment zone AND is contiguous with LIC tracts?
tract_shapes$LIC_2015_f=ifelse(tract_shapes$cond_pov15 == F & tract_shapes$metro_15 == F & 
		tract_shapes$cond_state_MFI_15 == F & tract_shapes$HMRC== T
		& tract_shapes$cond_tpop_15 == T & tract_shapes$inter_15 == T & tract_shapes$empzone_tf == T,
		T, tract_shapes$LIC_2015_f)
unique(tract_shapes$LIC_2015_f)
# tract_shapes$LIC_2015_f[is.na(tract_shapes$LIC_2015_f)] <- F

############################################################
################ LIC Conditions set for 2016 ###############
############################################################
tract_shapes$LIC_2016=F
unique(tract_shapes$LIC_2016)

# 2016 Condition #1: Is this tract poverty rate >= 20%
tract_shapes$LIC_2016=ifelse(tract_shapes$cond_pov16 == T, T, tract_shapes$LIC_2016)
# tract_shapes$cond1_2016=tract_shapes$LIC_2016
unique(tract_shapes$LIC_2016)

# 2016 Condition #2: Is this tract in a metropolitan area AND MFI <= 80% of state OR metropolitan MFI?
tract_shapes$LIC_2016=ifelse(tract_shapes$cond_pov16 == F & tract_shapes$metro_16 == T & 
		(tract_shapes$cond_metro_MFI_16 == T | tract_shapes$cond_state_MFI_16 == T) 
		, T, tract_shapes$LIC_2016)

unique(tract_shapes$LIC_2016)
# tract_shapes$LIC_2016[is.na(tract_shapes$LIC_2016)] <- F
tract_shapes$cond2_2016=tract_shapes$LIC_2016

# 2016 Condition #3: Is this tract, non-metro AND statewide MFI <= 80% of the statewide MFI?
tract_shapes$LIC_2016=ifelse(tract_shapes$cond_pov16 == F & tract_shapes$metro_16 == F & 
		tract_shapes$cond_state_MFI_16 == T
		, T, tract_shapes$LIC_2016)
unique(tract_shapes$LIC_2016)
# tract_shapes$LIC_2016[is.na(tract_shapes$LIC_2016)] <- F
tract_shapes$cond3_2016=tract_shapes$LIC_2016

# 2016 Condition #4: Is this tract, non-metro AND statewide MFI >= 80% AND a HMRC AND has statewide MFI <= 85% ?
tract_shapes$LIC_2016=ifelse(tract_shapes$cond_pov16 == F & tract_shapes$metro_16 == F 
		& tract_shapes$cond_state_MFI_16 == F
		& tract_shapes$cond_state_MFI_16_2 == T & tract_shapes$HMRC==T ,
		T, tract_shapes$LIC_2016)
unique(tract_shapes$LIC_2016)
# tract_shapes$LIC_2016[is.na(tract_shapes$LIC_2016)] <- F
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
		& tract_shapes$cond_tpop_16 == T & tract_shapes$inter_16 == T & tract_shapes$empzone_tf == T &
		tract_shapes$cond_state_MFI_16 == T & tract_shapes$cond_metro_MFI_16 == T ,
		T, tract_shapes$LIC_2016)
# tract_shapes$LIC_2016_f[is.na(tract_shapes$LIC_2016_f)] <- F
tract_shapes$cond5_2016=tract_shapes$LIC_2016_f
unique(tract_shapes$LIC_2016_f)

# 2016 Condition #6: Is this tract, non-metro AND statewide MFI >= 80% AND a HMRC AND population < 2000 AND in an empowerment zone AND is contiguous with LIC tracts?
tract_shapes$LIC_2016_f=ifelse(tract_shapes$cond_pov16 == F & tract_shapes$metro_16 == F & 
		tract_shapes$cond_state_MFI_16 == F & tract_shapes$HMRC==F
		& tract_shapes$cond_tpop_16 == T & tract_shapes$inter_16 == T & tract_shapes$empzone_tf == T,
		T, tract_shapes$LIC_2016_f)
# tract_shapes$LIC_2016_f[is.na(tract_shapes$LIC_2016_f)] <- F
unique(tract_shapes$LIC_2016_f)

# Combine 2015 & 2016 LIC calcs
table(is.na(tract_shapes$LIC_2016_f))
table(is.na(tract_shapes$LIC_2015_f))

tract_shapes$intermediate_LIC=ifelse(tract_shapes$LIC_2016_f == T | tract_shapes$LIC_2015_f == T, T, F)
table(is.na(tract_shapes$intermediate_LIC))

unique(tract_shapes$intermediate_LIC)

#### Non-LIC Contiguous Tract ####

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

#### Generalized Taxonomy of Eligibility ####

# Poverty 
tract_shapes$poverty_cond=ifelse(
	tract_shapes$cond_pov16 == T | tract_shapes$cond_pov15 == T, T, F )

# Median Family Income
tract_shapes$income_cond=ifelse(
		(tract_shapes$metro_16 == T & 
		(tract_shapes$cond_metro_MFI_16 == T | tract_shapes$cond_state_MFI_16 == T))
			|
		(tract_shapes$metro_15 == T & 
		(tract_shapes$cond_metro_MFI_15 == T | tract_shapes$cond_state_MFI_15 == T))
			|
		(tract_shapes$metro_16 == F & 
		tract_shapes$cond_state_MFI_16 == T)
			|
		(tract_shapes$metro_15 == F & 
		tract_shapes$cond_state_MFI_15 == T)
			, T, F )

# HMRC
tract_shapes$rural_cond=ifelse(
		(tract_shapes$cond_pov16 == F & tract_shapes$metro_16 == F 
		& tract_shapes$cond_state_MFI_16 == F
		& tract_shapes$cond_state_MFI_16_2 == T & tract_shapes$HMRC==T)
		|
		(tract_shapes$cond_pov15 == F & tract_shapes$metro_15 == F 
		& tract_shapes$cond_state_MFI_15 == F
		& tract_shapes$cond_state_MFI_15_2 == T & tract_shapes$HMRC==T)
		, T, F)

# Small population
tract_shapes$population_cond=ifelse(
		(tract_shapes$cond_tpop_16 == T & tract_shapes$inter_16 == T & tract_shapes$empzone_tf == T)
		|
		(tract_shapes$cond_tpop_15 == T & tract_shapes$inter_15 == T & tract_shapes$empzone_tf == T)
		, T, F)

##### Compare to other data sources ####

PM_LIC_tracts=openxlsx::read.xlsx('~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/PolicyMap_OZ_Eligibility.xlsx')

PM_LIC_tracts$Census.Tract=str_pad(PM_LIC_tracts$Census.Tract, 11, pad = "0")
names(PM_LIC_tracts)[names(PM_LIC_tracts) == 'Census.Tract'] = 'Tract_ID'
PM_LIC_tracts$Tract_ID= ifelse(
	        PM_LIC_tracts$Tract_ID == "51515050100", "51019050100",
	ifelse(PM_LIC_tracts$Tract_ID ==  "04019002701", "04019002704",
	ifelse(PM_LIC_tracts$Tract_ID ==  "04019002903", "04019002906",
	ifelse(PM_LIC_tracts$Tract_ID ==  "04019410501", "04019004118",
	ifelse(PM_LIC_tracts$Tract_ID ==  "04019410502", "04019004121",
	ifelse(PM_LIC_tracts$Tract_ID ==  "04019410503", "04019004125",
	ifelse(PM_LIC_tracts$Tract_ID ==  "04019470400", "04019005200",
	ifelse(PM_LIC_tracts$Tract_ID ==  "04019470500", "04019005300",
	ifelse(PM_LIC_tracts$Tract_ID ==  "06037930401", "06037137000",
	ifelse(PM_LIC_tracts$Tract_ID ==  "02270000100", "02158000100",
	ifelse(PM_LIC_tracts$Tract_ID ==  "46113940500", "46102940500",
	ifelse(PM_LIC_tracts$Tract_ID ==  "46113940800", "46102940800",
	ifelse(PM_LIC_tracts$Tract_ID ==  "46113940900", "46102940900",
	ifelse(PM_LIC_tracts$Tract_ID ==  "36053940101", "36053030101",
	ifelse(PM_LIC_tracts$Tract_ID ==  "36053940102", "36053030102",
	ifelse(PM_LIC_tracts$Tract_ID ==  "36053940103", "36053030103",
	ifelse(PM_LIC_tracts$Tract_ID ==  "36053940200", "36053030200",
	ifelse(PM_LIC_tracts$Tract_ID ==  "36053940300", "36053030300",
	ifelse(PM_LIC_tracts$Tract_ID ==  "36053940401", "36053030401",
	ifelse(PM_LIC_tracts$Tract_ID ==  "36053940403", "36053030403",
	ifelse(PM_LIC_tracts$Tract_ID ==  "36053940600", "36053030600",
	ifelse(PM_LIC_tracts$Tract_ID ==  "36053940700", "36053030402",
	ifelse(PM_LIC_tracts$Tract_ID ==  "36065940000", "36065024800",
	ifelse(PM_LIC_tracts$Tract_ID ==  "36065940100", "36065024700",
	ifelse(PM_LIC_tracts$Tract_ID ==  "36065940200", "36065024900", PM_LIC_tracts$Tract_ID)))))))))))))))))))))))))
names(PM_LIC_tracts)[names(PM_LIC_tracts) == 'New.Markets.Tax.Credit.(NMTC).eligibility.and.Qualified.Opportun'] = 'NMTC_elig_type'
PM_LIC_tracts=PM_LIC_tracts[c("Tract_ID", 'elig_type')]
unique(PM_LIC_tracts$elig_type)

Und_LIC_tracts=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/Extra_Shapefiles/LIC Tracts/tabula-Tract-Table.csv', stringsAsFactors = F)

Und_LIC_tracts$Tract_ID=str_pad(Und_LIC_tracts$Tract_ID, 11, pad = "0")
Und_LIC_tracts$Tract_ID= ifelse(
	       Und_LIC_tracts$Tract_ID == "51515050100", "51019050100",
	ifelse(Und_LIC_tracts$Tract_ID ==  "04019002701", "04019002704",
	ifelse(Und_LIC_tracts$Tract_ID ==  "04019002903", "04019002906",
	ifelse(Und_LIC_tracts$Tract_ID ==  "04019410501", "04019004118",
	ifelse(Und_LIC_tracts$Tract_ID ==  "04019410502", "04019004121",
	ifelse(Und_LIC_tracts$Tract_ID ==  "04019410503", "04019004125",
	ifelse(Und_LIC_tracts$Tract_ID ==  "04019470400", "04019005200",
	ifelse(Und_LIC_tracts$Tract_ID ==  "04019470500", "04019005300",
	ifelse(Und_LIC_tracts$Tract_ID ==  "06037930401", "06037137000",
	ifelse(Und_LIC_tracts$Tract_ID ==  "02270000100", "02158000100",
	ifelse(Und_LIC_tracts$Tract_ID ==  "46113940500", "46102940500",
	ifelse(Und_LIC_tracts$Tract_ID ==  "46113940800", "46102940800",
	ifelse(Und_LIC_tracts$Tract_ID ==  "46113940900", "46102940900",
	ifelse(Und_LIC_tracts$Tract_ID ==  "36053940101", "36053030101",
	ifelse(Und_LIC_tracts$Tract_ID ==  "36053940102", "36053030102",
	ifelse(Und_LIC_tracts$Tract_ID ==  "36053940103", "36053030103",
	ifelse(Und_LIC_tracts$Tract_ID ==  "36053940200", "36053030200",
	ifelse(Und_LIC_tracts$Tract_ID ==  "36053940300", "36053030300",
	ifelse(Und_LIC_tracts$Tract_ID ==  "36053940401", "36053030401",
	ifelse(Und_LIC_tracts$Tract_ID ==  "36053940403", "36053030403",
	ifelse(Und_LIC_tracts$Tract_ID ==  "36053940600", "36053030600",
	ifelse(Und_LIC_tracts$Tract_ID ==  "36053940700", "36053030402",
	ifelse(Und_LIC_tracts$Tract_ID ==  "36065940000", "36065024800",
	ifelse(Und_LIC_tracts$Tract_ID ==  "36065940100", "36065024700",
	ifelse(Und_LIC_tracts$Tract_ID ==  "36065940200", "36065024900", Und_LIC_tracts$Tract_ID)))))))))))))))))))))))))

Und_LIC_tracts=Und_LIC_tracts[c("Tract_ID", 'LIC')]

oz_info=read.csv('~/Desktop/Welfare_Policy/Struggling_Regions/Opportunity Zones - Shapefiles/Opportunity_Zones.csv', stringsAsFactors = F)
oz_info$GEOID10=str_pad(oz_info$GEOID10, 11, pad = "0")
oz_info=oz_info[c('GEOID10','TYPE')]
oz_info$GEOID10= ifelse(
	       oz_info$GEOID10 == "51515050100", "51019050100",
	ifelse(oz_info$GEOID10 == "04019002701", "04019002704",
	ifelse(oz_info$GEOID10 == "04019002903", "04019002906",
	ifelse(oz_info$GEOID10 == "04019410501", "04019004118",
	ifelse(oz_info$GEOID10 == "04019410502", "04019004121",
	ifelse(oz_info$GEOID10 == "04019410503", "04019004125",
	ifelse(oz_info$GEOID10 == "04019470400", "04019005200",
	ifelse(oz_info$GEOID10 == "04019470500", "04019005300",
	ifelse(oz_info$GEOID10 == "06037930401", "06037137000",
	ifelse(oz_info$GEOID10 == "02270000100", "02158000100",
	ifelse(oz_info$GEOID10 == "46113940500", "46102940500",
	ifelse(oz_info$GEOID10 == "46113940800", "46102940800",
	ifelse(oz_info$GEOID10 == "46113940900", "46102940900",
	ifelse(oz_info$GEOID10 == "36053940101", "36053030101",
	ifelse(oz_info$GEOID10 == "36053940102", "36053030102",
	ifelse(oz_info$GEOID10 == "36053940103", "36053030103",
	ifelse(oz_info$GEOID10 == "36053940200", "36053030200",
	ifelse(oz_info$GEOID10 == "36053940300", "36053030300",
	ifelse(oz_info$GEOID10 == "36053940401", "36053030401",
	ifelse(oz_info$GEOID10 == "36053940403", "36053030403",
	ifelse(oz_info$GEOID10 == "36053940600", "36053030600",
	ifelse(oz_info$GEOID10 == "36053940700", "36053030402",
	ifelse(oz_info$GEOID10 == "36065940000", "36065024800",
	ifelse(oz_info$GEOID10 == "36065940100", "36065024700",
	ifelse(oz_info$GEOID10 == "36065940200", "36065024900", oz_info$GEOID10)))))))))))))))))))))))))
names(oz_info)[names(oz_info) == 'TYPE'] = 'OZ_type'

IDX.dat=openxlsx::read.xlsx("~/Desktop/Welfare_Policy/Struggling_Regions/Index/final_index.xlsx")
IDX.dat$FIPS= ifelse(
	       IDX.dat$FIPS == "51515050100", "51019050100",
	ifelse(IDX.dat$FIPS == "04019002701", "04019002704",
	ifelse(IDX.dat$FIPS == "04019002903", "04019002906",
	ifelse(IDX.dat$FIPS == "04019410501", "04019004118",
	ifelse(IDX.dat$FIPS == "04019410502", "04019004121",
	ifelse(IDX.dat$FIPS == "04019410503", "04019004125",
	ifelse(IDX.dat$FIPS == "04019470400", "04019005200",
	ifelse(IDX.dat$FIPS == "04019470500", "04019005300",
	ifelse(IDX.dat$FIPS == "06037930401", "06037137000",
	ifelse(IDX.dat$FIPS == "02270000100", "02158000100",
	ifelse(IDX.dat$FIPS == "46113940500", "46102940500",
	ifelse(IDX.dat$FIPS == "46113940800", "46102940800",
	ifelse(IDX.dat$FIPS == "46113940900", "46102940900",
	ifelse(IDX.dat$FIPS == "36053940101", "36053030101",
	ifelse(IDX.dat$FIPS == "36053940102", "36053030102",
	ifelse(IDX.dat$FIPS == "36053940103", "36053030103",
	ifelse(IDX.dat$FIPS == "36053940200", "36053030200",
	ifelse(IDX.dat$FIPS == "36053940300", "36053030300",
	ifelse(IDX.dat$FIPS == "36053940401", "36053030401",
	ifelse(IDX.dat$FIPS == "36053940403", "36053030403",
	ifelse(IDX.dat$FIPS == "36053940600", "36053030600",
	ifelse(IDX.dat$FIPS == "36053940700", "36053030402",
	ifelse(IDX.dat$FIPS == "36065940000", "36065024800",
	ifelse(IDX.dat$FIPS == "36065940100", "36065024700",
	ifelse(IDX.dat$FIPS == "36065940200", "36065024900", IDX.dat$FIPS)))))))))))))))))))))))))

tract_shapes=subset(tract_shapes, STATEFP != "72") # remove Puerto Rico
tract_df=merge(tract_shapes,oz_info, by.x = "GEOID", by.y = "GEOID10", all.x = T, sort = F )
tract_df=merge(tract_df,Clusters_df, by.x = "GEOID", by.y = "Fips", all.x = T, sort = F )
tract_df=merge(tract_df,Und_LIC_tracts, by.x = "GEOID", by.y = "Tract_ID", all.x = T, sort = F )
tract_df=merge(tract_df,PM_LIC_tracts, by.x = "GEOID", by.y = "Tract_ID", all.x = T, sort = F )
tract_df=merge(tract_df,IDX.dat, by.x = "GEOID", by.y = "FIPS", all.x = T, sort = F )

# LIC Types
tract_df$LIC_detailed = tract_df$LIC
tract_df$LIC_tf=ifelse(tract_df$OZ_type=='Low-Income Community'|tract_df$LIC=='Undesignated Low-Income Community',T,F)
tract_df$LIC_tf = ifelse(is.na(tract_df$LIC_tf), F, tract_df$LIC_tf)

tract_df$LIC=ifelse(tract_df$OZ_type=='Low-Income Community' | tract_df$LIC=='Undesignated Low-Income Community','Low-Income Community',tract_df$LIC)
tract_df$LIC	=	ifelse(is.na(tract_df$LIC)==T, "Non-LIC", tract_df$LIC)

# OZ Type 
tract_df$OZ_type = ifelse(tract_df$OZ_type == 'Low-Income Community','Low-Income Community (OZ)', tract_df$OZ_type)
tract_df$OZ_type = ifelse(tract_df$OZ_type == 'Non-LIC Contiguous','Non-LIC Contiguous (OZ)', tract_df$OZ_type)
tract_df$OZ_type = ifelse(is.na(tract_df$OZ_type)==T, "Undesignated Tract", tract_df$OZ_type)

# Tract Type
tract_df$tract_type = NA
tract_df$tract_type = ifelse(tract_df$OZ_type == 'Low-Income Community (OZ)' | tract_df$OZ_type == 'Non-LIC Contiguous (OZ)','Opportunity Zone',ifelse(tract_df$LIC_detailed == 'Undesignated Low-Income Community','Undesignated Low-Income Community', tract_df$tract_type))
tract_df$tract_type = ifelse(is.na(tract_df$tract_type)==T, "Undesignated Tract", tract_df$tract_type)

# Tract Type Full (alt)
tract_df$tract_type_alt = NA
tract_df$tract_type_alt = ifelse(tract_df$OZ_type == 'Low-Income Community (OZ)','Opportunity Zone (LIC)',# tract_df$tract_type_alt
ifelse(tract_df$OZ_type == 'Non-LIC Contiguous (OZ)','Opportunity Zone (Contiguous)', #tract_df$tract_type
ifelse(tract_df$LIC_detailed == 'Undesignated Low-Income Community' ,'Undesignated Low-Income Community', tract_df$tract_type_alt)))
tract_df$tract_type_alt = ifelse(is.na(tract_df$tract_type_alt)==T, "Undesignated Non-LIC", tract_df$tract_type_alt)

tract_df$corruption = tract_df$OZ_type
tract_df$corruption = ifelse(tract_df$GEOID == '40109103200' | tract_df$GEOID == '06037206020' | tract_df$GEOID == '26163517200',
	'Mislabeled LICs',
	ifelse(tract_df$GEOID == '06037206031' | tract_df$GEOID == '26163517000',
	'Improper Contiguous Tracts',
	tract_df$OZ_type))

tract_df$final_LIC = ifelse(tract_df$intermediate_LIC == T & tract_df$LIC_tf == F,F,tract_df$intermediate_LIC)

# Conditions
tract_shapes$poverty_cond=ifelse(tract_df$poverty_cond == T & tract_df$LIC_tf == F,F,tract_df$poverty_cond)
tract_shapes$population_cond=ifelse(tract_df$population_cond == T & tract_df$LIC_tf == F,F,tract_df$population_cond)
tract_shapes$income_cond=ifelse(tract_df$income_cond == T & tract_df$LIC_tf == F,F,tract_df$income_cond)
tract_shapes$rural_cond	=ifelse(tract_df$rural_cond == T & tract_df$LIC_tf == F,F,tract_df$rural_cond)
	
tract_shapes[is.na(tract_shapes)] <- "N/A"

lic_df = as.data.frame(tract_df[c("GEOID", 'LIC', 'OZ_type','tract_type','tract_type_alt','labor_idx','pov_idx','edu_idx','afford_idx','mig_idx','combined_index', 'LIC_2016', 'LIC_2015', 'LIC_2016_f', 'LIC_2015_f','Tperc_fpl_15','Tperc_fpl_16','NMTC_elig_type','METRO_NAME','COUNTY_NAME',
	# "cond1_2015" , "cond2_2015" , "cond3_2015" ,"cond4_2015",'cond5_2015'
	# ,"cond1_2016" , "cond2_2016" , "cond3_2016" ,"cond4_2016",'cond5_2016',
	'poverty_cond', 'population_cond', 'rural_cond', 'income_cond',
	'metro_MFI_15','metro_MFI_16','state_MFI_15','state_MFI_16','intermediate_LIC','Native_Tract',
	'check_state_MFI_15', 'check_metro_MFI_15','check_state_MFI_16', 'check_metro_MFI_16','corruption',
	'cond_state_MFI_15','cond_metro_MFI_15','cond_state_MFI_15_2','cond_tpop_15',
	'TPop_16','TPop_15','tract_MFI_15','tract_MFI_16',
	'no_dat_MFI_15','no_dat_MFI_16','no_dat_pov_15','no_dat_pov_16',
	'cond_state_MFI_16','cond_metro_MFI_16','cond_state_MFI_16_2','cond_tpop_16','METRO_NAME_16','METRO_NAME_15',
	'LIC_tf','final_LIC','inter_15','inter_16','empzone_tf','metro_15','metro_16','cond_pov15','cond_pov16','HMRC')])

lic_df = as.data.frame(lic_df[c("GEOID", 'LIC', 'OZ_type','tract_type','tract_type_alt','labor_idx','pov_idx','edu_idx','afford_idx','mig_idx','combined_index', 'LIC_2016', 'LIC_2015', 'LIC_2016_f', 'LIC_2015_f','Tperc_fpl_15','Tperc_fpl_16','NMTC_elig_type','METRO_NAME','COUNTY_NAME',
	'poverty_cond', 'population_cond', 'rural_cond', 'income_cond',
	# "cond1_2015" , "cond2_2015" , "cond3_2015" ,"cond4_2015",'cond5_2015'
	# ,"cond1_2016" , "cond2_2016" , "cond3_2016" ,"cond4_2016",'cond5_2016',
	'metro_MFI_15','metro_MFI_16','state_MFI_15','state_MFI_16','intermediate_LIC','Native_Tract',
	'check_state_MFI_15', 'check_metro_MFI_15','check_state_MFI_16', 'check_metro_MFI_16','corruption',
	'cond_state_MFI_15','cond_metro_MFI_15','cond_state_MFI_15_2','cond_tpop_15',
	'TPop_16','TPop_15','tract_MFI_15','tract_MFI_16',
	'no_dat_MFI_15','no_dat_MFI_16','no_dat_pov_15','no_dat_pov_16',
	'cond_state_MFI_16','cond_metro_MFI_16','cond_state_MFI_16_2','cond_tpop_16','METRO_NAME_16','METRO_NAME_15',
	'LIC_tf','final_LIC','inter_15','inter_16','empzone_tf','metro_15','metro_16','cond_pov15','cond_pov16','HMRC')])

openxlsx::write.xlsx(lic_df,'~/Desktop/Welfare_Policy/Struggling_Regions/Cluster_Analyses/Web_Map/Resources/Extra_Shapefiles/LIC Tracts/LIC.xlsx')
save(tract_df, file = '~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/tract_df.Rdata')
unique(tract_df$final_LIC)
glimpse(tract_df)

#####################################################################
############# Choose Final Variables and Lat-Lng style ##############
#####################################################################

load(file = '~/Desktop/Welfare_Policy/Struggling_Regions/Eligibility Criteria/tract_df.Rdata')


names(tract_df)
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
tract_df=within(tract_df,rm(STATEFP, COUNTYFP,TRACTCE,NAME,NAMELSAD,MTFCC,FUNCSTAT,ALAND,AWATER,INTPTLAT,INTPTLON, LIC_detailed, tract_type))
tract_df=within(tract_df,rm(LIC_2016, LIC_2015, LIC_2016_f, LIC_2015_f,pov_by,Tperc_fpl_15,Tperc_fpl_16,NMTC_elig_type,
	# "cond1_2015" , "cond2_2015" , "cond3_2015" ,"cond4_2015",cond5_2015
	# ,"cond1_2016" , "cond2_2016" , "cond3_2016" ,"cond4_2016",cond5_2016,
	metro_MFI_15,metro_MFI_16,state_MFI_15,state_MFI_16,intermediate_LIC,
	check_state_MFI_15, check_metro_MFI_15,check_state_MFI_16, check_metro_MFI_16,corruption,
	cond_state_MFI_15,cond_metro_MFI_15,cond_state_MFI_15_2,cond_tpop_15,TPop_16,TPop_15,tract_MFI_15,tract_MFI_16,
	cond_state_MFI_16,cond_metro_MFI_16,cond_state_MFI_16_2,cond_tpop_16,
	LIC_tf,final_LIC,inter_15,inter_16,empzone_tf,metro_15,metro_16,cond_pov15,cond_pov16,HMRC))
tract_df=within(tract_df,rm(county_MFI_15,county_MFI_16,cond2_2016,cond4_2015, cond3_2016, cond4_2016, cond5_2016,FIPS.Code, Formatted.FIPS,State,country_fips ))
tract_df=within(tract_df,rm(State_Name,County_Name,METRO_NAME_15,METRO_NAME_16,no_dat_MFI_15,no_dat_pov_15,no_dat_MFI_16,no_dat_pov_16))
tract_df=within(tract_df,rm(poverty_cond,income_cond,rural_cond,population_cond))
tract_df=within(tract_df,rm(X))
glimpse(tract_df)
names(tract_df)

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