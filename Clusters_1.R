rm(list=ls());gc()

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(zoo)
library(mice)
library(scales)
library(lattice)

options(scipen = 999)

OZ_dat_1    = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/cluster_inputs/TrimmedData_Jun24.dta")
OZ_dat_lang = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/cluster_inputs/languagesupplement.dta")
OZ_dat_extra= readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/cluster_inputs/Supplement2.dta")
OZ_dat_sup3 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/cluster_inputs/Supplement3.dta")
OZ_dat_sup4 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/cluster_inputs/supplement4.dta")
OZ_dat_sup5 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/cluster_inputs/supplement5.dta")
OZ_dat_sup6 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/cluster_inputs/AgeBrackets_2.dta")
OZ_dat_sup7 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/cluster_inputs/trajectory_2.dta")
OZ_dat_sup8 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/cluster_inputs/supplement8_2.dta")
OZ_dat_sup9 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/cluster_inputs/HousingSupplement.dta")
OZ_dat_sup10 = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Index/Raw Data/2013_2017_ACS.dta")
OZ_dat_sup11 = readstata13::read.dta13('/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Index/Raw Data/IncomeSupplement.dta')

HUD_vac=readxl::read_excel("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/cluster_inputs/hud_addresses.xlsx")
change_lt=readxl::read_excel("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/cluster_inputs/tract_longterm.xlsx")


var.labels_1 = attr(OZ_dat_1,"var.labels")
var.labels_2 = attr(OZ_dat_lang,"var.labels")
var.labels_extra = attr(OZ_dat_extra,"var.labels")
var.labels_sup3 = attr(OZ_dat_sup3,"var.labels")
var.labels_sup4 = attr(OZ_dat_sup4,"var.labels")
var.labels_sup5 = attr(OZ_dat_sup5,"var.labels")
var.labels_sup6 = attr(OZ_dat_sup6,"var.labels")
var.labels_sup7 = attr(OZ_dat_sup7,"var.labels")
var.labels_sup8 = attr(OZ_dat_sup8,"var.labels")
var.labels_sup9 = attr(OZ_dat_sup9,"var.labels")
var.labels_sup10 = attr(OZ_dat_sup10,"var.labels")
var.labels_sup11 = attr(OZ_dat_sup11,"var.labels")

OZ_dat_sup7=OZ_dat_sup7[-c(2:15)]
OZ_dat_sup7[c(2:9)]=OZ_dat_sup7[c(2:9)]*100
OZ_dat_sup6[c(3:11)]=OZ_dat_sup6[c(3:11)]*100
# OZ_dat_sup8=OZ_dat_sup8[c(1,12:16)]
# HUD_vac$res_prop=HUD_vac$res_prop*100
# HUD_vac$bus_prop=HUD_vac$bus_prop*100
OZ_dat_sup9=OZ_dat_sup9[c(1,13:22)]
var.labels_sup9=var.labels_sup9[c(1,3:12)]
OZ_dat_sup9[c(2:11)]=OZ_dat_sup9[c(2:11)]*100
pct_vars <- grepl('PCT_', colnames(var.labels_sup10))
OZ_dat_sup10[pct_vars] =  OZ_dat_sup10[pct_vars] * 100
OZ_dat_sup10$LFPR2554 = OZ_dat_sup10$LFPR2554 *100
OZ_dat_sup10$EPOP2554=OZ_dat_sup10$Employed2554/OZ_dat_sup10$Pop2554 * 100
OZ_dat_sup10$pov_200=(OZ_dat_sup10$PCT_C17002002+OZ_dat_sup10$PCT_C17002003+OZ_dat_sup10$PCT_C17002004+OZ_dat_sup10$PCT_C17002005+OZ_dat_sup10$PCT_C17002006+OZ_dat_sup10$PCT_C17002007)
OZ_dat_sup10$pov_200=OZ_dat_sup10$pov_200 *100
OZ_dat_sup10$armed_forces_perc = (OZ_dat_sup10$B23025006 / OZ_dat_sup10$B23025001) * 100
OZ_dat_sup10=OZ_dat_sup10[c('FIPS', 'PCT_C17002002','LFPR2554','EPOP2554','pov_200','armed_forces_perc')]
PCT_C17002002$PCT_C17002002 = PCT_C17002002$PCT_C17002002*100
OZ_dat_sup11=subset(OZ_dat_sup11,Year==2017)
as.data.frame(OZ_dat_sup11) %>% summarise_all(funs(sum(is.na(.)))) %>% gather()
OZ_dat_sup11=OZ_dat_sup11[c('FIPS', 'PerCapitaIncome','PerWorkerWageIncome')]

data.key.c=data.frame(
	var.name=c(names(OZ_dat_1),names(OZ_dat_lang[2:5]),names(OZ_dat_extra[2:15]),names(OZ_dat_sup3[-c(1:2)]),names(OZ_dat_sup4[-c(1:2)]),
		names(OZ_dat_sup5[-c(1)]),names(OZ_dat_sup6[-c(1:2)]),names(OZ_dat_sup7[-c(1)]),names(OZ_dat_sup8[-c(1)]),names(OZ_dat_sup9[-c(1)])),
	var.label=c(var.labels_1,var.labels_2[2:5],var.labels_extra[2:15], var.labels_sup3[-c(1:2)],var.labels_sup4[-c(1:2)],
		var.labels_sup5[-c(1)],var.labels_sup6[-c(1:2)],var.labels_sup7[-c(1:15)],var.labels_sup8[-c(1)],var.labels_sup9[-c(1)]))

OZ_dat_1=subset(OZ_dat_1,select=-c(PCT_B25034010,PCT_B25034003,PCT_B25034002))
OZ_dat=list(OZ_dat_lang,OZ_dat_1,OZ_dat_extra,OZ_dat_sup3[-c(2)],OZ_dat_sup4[-c(2)],
	HUD_vac, OZ_dat_sup5,OZ_dat_sup6[-c(2)],change_lt,OZ_dat_sup7,OZ_dat_sup8,OZ_dat_sup9,OZ_dat_sup10,OZ_dat_sup11) %>% reduce(full_join, by = "FIPS")

rm(var.labels_1,var.labels_2,var.labels_sup3,var.labels_sup4,var.labels_sup5,
		var.labels_sup6,var.labels_sup7,var.labels_sup8,var.labels_extra,var.labels_sup9,var.labels_sup10,
		OZ_dat_1,OZ_dat_sup3,OZ_dat_sup4,OZ_dat_sup5,OZ_dat_sup6,
		OZ_dat_sup7,OZ_dat_sup8,OZ_dat_lang,HUD_vac,change_lt,OZ_dat_extra,OZ_dat_sup9,OZ_dat_sup10)
gv()
data.key.c=unique(data.key.c)
OZ_dat = subset(OZ_dat, select = -c( `_merge`, merge1, B12007001, B12007002)) # Remove Merge labels
# openxlsx::write.xlsx(OZ_dat[,c("FIPS",'A00002_002','B01001001')],"~/Desktop/Welfare_Policy/Struggling Regions/Vacancies/all_tracts.xlsx")

data.key.c[grep('PCT_B190010',data.key.c$var.name),]
OZ_dat$PCT_B190010_IncQ1 = (OZ_dat$PCT_B19001002+OZ_dat$PCT_B19001003+OZ_dat$PCT_B19001004+OZ_dat$PCT_B19001005+OZ_dat$PCT_B19001006)
OZ_dat$PCT_B190010_IncQ2 = (OZ_dat$PCT_B19001007+OZ_dat$PCT_B19001008+OZ_dat$PCT_B19001009+OZ_dat$PCT_B19001010+OZ_dat$PCT_B19001011)
OZ_dat$PCT_B190010_IncQ3 = (OZ_dat$PCT_B19001012+OZ_dat$PCT_B19001013)
OZ_dat$PCT_B190010_IncQ4 = (OZ_dat$PCT_B19001014+OZ_dat$PCT_B19001015+OZ_dat$PCT_B19001016+OZ_dat$PCT_B19001017)

data.key.c[grep('PCT_B250240',data.key.c$var.name),]
OZ_dat$PCT_H_type_1 = (	OZ_dat$PCT_B25024002+OZ_dat$PCT_B25024003)
OZ_dat$PCT_H_type_2t4 = (OZ_dat$PCT_B25024004+OZ_dat$PCT_B25024005)
OZ_dat$PCT_H_type_5t19 = (OZ_dat$PCT_B25024006+OZ_dat$PCT_B25024007)
OZ_dat$PCT_H_type_20p = (OZ_dat$PCT_B25024008+OZ_dat$PCT_B25024009)

data.key.c[grep('PCT_B083030',data.key.c$var.name),]
OZ_dat$PCT_commute_1 = (OZ_dat$PCT_B08303003+OZ_dat$PCT_B08303004)
OZ_dat$PCT_commute_2 = (OZ_dat$PCT_B08303005+OZ_dat$PCT_B08303006+OZ_dat$PCT_B08303007+OZ_dat$PCT_B08303008)
OZ_dat$PCT_commute_3 = (OZ_dat$PCT_B08303009+OZ_dat$PCT_B08303010+OZ_dat$PCT_B08303011+OZ_dat$PCT_B08303012)
OZ_dat$PCT_commute_over_hour = (OZ_dat$PCT_B08303012+OZ_dat$PCT_B08303013)

OZ_dat$vehicles_2plus=(OZ_dat$PCT_B08014004+OZ_dat$PCT_B08014005+OZ_dat$PCT_B08014006+OZ_dat$PCT_B08014007)

OZ_dat$housebuilt_after2010=(OZ_dat$PCT_B25034002 + OZ_dat$PCT_B25034003)
OZ_dat$Other_Race_incl_Pacific_Islander=(OZ_dat$PCT_B02001007 + OZ_dat$PCT_B02001006)

OZ_dat$PercentFemale=(OZ_dat$PercentFemale * 100)
OZ_dat$EnrolledCollege=(OZ_dat$EnrolledCollege * 100)
OZ_dat$PCT_B16001003=(OZ_dat$PCT_B16001003 * 100)
OZ_dat$PCT_B16001002=(OZ_dat$PCT_B16001002 * 100)
OZ_dat$St_Code=stringi::stri_sub(OZ_dat$FIPS, from=1, to=2); unique(OZ_dat$St_Code)
OZ_dat=subset(OZ_dat ,as.numeric(OZ_dat$St_Code) <= 56) 
OZ_dat = OZ_dat %>%  dplyr::select(GEOID, FIPS, OZ, everything())
OZ_dat$OZ = as.character(OZ_dat$OZ)

OZ_dat = subset(OZ_dat, is.na(B01001001) == F)
OZ_dat = subset(OZ_dat, (B01001001) > 150 )
OZ_dat = subset(OZ_dat, select = -c(GEOID, St_Code)) # Remove GEO Vars

# menon=subset(OZ_dat, German >= 50 | Spanish >= 50)
# res = cor(menon[,-c(1:2)],use = "complete.obs")
# res=round(res, 2)
# res=as.data.frame(res[,c('German','Spanish')])
# res$least_corr = -1*(res$German - res$Spanish)
# res = tibble::rownames_to_column(res, "VALUE")
# res %>% arrange(least_corr)
# res %>% arrange(desc(least_corr))

OZ_dat$changeepop00_17=squish(OZ_dat$changeepop00_17,quantile(OZ_dat$changeepop00_17,c(.005,.995),na.rm=T))
OZ_dat$changeepop13_17=squish(OZ_dat$changeepop13_17,quantile(OZ_dat$changeepop13_17,c(.005,.995),na.rm=T))
OZ_dat$changeunempl00_17=squish(OZ_dat$changeunempl00_17,quantile(OZ_dat$changeunempl00_17,c(.005,.995),na.rm=T))
OZ_dat$changeunempl13_17=squish(OZ_dat$changeunempl13_17,quantile(OZ_dat$changeunempl13_17,c(.005,.995),na.rm=T))
OZ_dat$changepop00_17=squish(OZ_dat$changepop00_17,quantile(OZ_dat$changepop00_17,c(.005,.995),na.rm=T))
OZ_dat$changepop13_17=squish(OZ_dat$changepop13_17,quantile(OZ_dat$changepop13_17,c(.005,.995),na.rm=T))
OZ_dat$changemanufacturing00_17=squish(OZ_dat$changemanufacturing00_17,quantile(OZ_dat$changemanufacturing00_17,c(.005,.995),na.rm=T))
OZ_dat$changemanufacturing13_17=squish(OZ_dat$changemanufacturing13_17,quantile(OZ_dat$changemanufacturing13_17,c(.005,.995),na.rm=T))
OZ_dat$bus_pct_change=squish(OZ_dat$bus_pct_change,quantile(OZ_dat$bus_pct_change,c(.005,.995),na.rm=T))
OZ_dat$res_pct_change=squish(OZ_dat$res_pct_change,quantile(OZ_dat$res_pct_change,c(.005,.995),na.rm=T))
OZ_dat$ams_pct_change=squish(OZ_dat$ams_pct_change,quantile(OZ_dat$ams_pct_change,c(.005,.995),na.rm=T))
OZ_dat$lt_perc_pop=squish(OZ_dat$lt_perc_pop,quantile(OZ_dat$lt_perc_pop,c(.005,.995),na.rm=T))
OZ_dat$lt_perc_inc=squish(OZ_dat$lt_perc_inc,quantile(OZ_dat$lt_perc_inc,c(.005,.995),na.rm=T))
OZ_dat$ChangeMedHomePrice=squish(OZ_dat$ChangeMedHomePrice,quantile(OZ_dat$ChangeMedHomePrice,c(.005,.995),na.rm=T))
OZ_dat$ChangeIncome=squish(OZ_dat$ChangeIncome,quantile(OZ_dat$ChangeIncome,c(.005,.995),na.rm=T))
OZ_dat$decline_manufacturing_absolute=squish(OZ_dat$decline_manufacturing_absolute,quantile(OZ_dat$decline_manufacturing_absolute,c(.005,.995),na.rm=T))
OZ_dat$decline_population=squish(OZ_dat$decline_population,quantile(OZ_dat$decline_population,c(.005,.995),na.rm=T))
OZ_dat$decline_employment_rate=squish(OZ_dat$decline_employment_rate,quantile(OZ_dat$decline_employment_rate,c(.005,.995),na.rm=T))
OZ_dat$decline_manufacturing_rel_ind=squish(OZ_dat$decline_manufacturing_rel_ind,quantile(OZ_dat$decline_manufacturing_rel_ind,c(.005,.995),na.rm=T))
OZ_dat$decline_manufacturing_rel_pop=squish(OZ_dat$decline_manufacturing_rel_pop,quantile(OZ_dat$decline_manufacturing_rel_pop,c(.005,.995),na.rm=T))
OZ_dat$changeMedHHinc00_17=squish(OZ_dat$changeMedHHinc00_17,quantile(OZ_dat$changeMedHHinc00_17,c(.005,.995),na.rm=T))
OZ_dat$changeMedHHinc13_17=squish(OZ_dat$changeMedHHinc13_17,quantile(OZ_dat$changeMedHHinc13_17,c(.005,.995),na.rm=T))

OZ_dat$Naturalized = OZ_dat$ForeignBorn - OZ_dat$PCT_B05001006 
OZ_dat$PCT_Age45to54 = OZ_dat$PCT_B01001016 + OZ_dat$PCT_B01001015 + OZ_dat$PCT_B01001040 + OZ_dat$PCT_B01001039
OZ_dat$PCT_Age55to64 = OZ_dat$PCT_B01001019 + OZ_dat$PCT_B01001018 + OZ_dat$PCT_B01001017 + OZ_dat$PCT_B01001043 + OZ_dat$PCT_B01001042+ OZ_dat$PCT_B01001041
OZ_dat$non_familyHH =OZ_dat$PCT_B11011018+OZ_dat$PCT_B11011017+OZ_dat$PCT_B11011019 # % Households: Nonfamily Households   
OZ_dat$EnglishOrGerman = pmin(OZ_dat$PCT_B16001002 + OZ_dat$German, 100)

OZ_dat = subset(OZ_dat, select = -c( # Remove GEO Vars
#              var.name                                                                        var.label             
		PCT_H_type_20p #                                                                              <NA>       
		,PCT_H_type_2t4 #                                                                             <NA>       
		,PCT_H_type_5t19 #                                                                             <NA>      
		,PCT_H_type_1 #                                                                             <NA>
		,PCT_commute_over_hour #                                                                     <NA>        
		,PCT_commute_3 #                                                                             <NA>        
		,PCT_commute_2 #                                                                             <NA>        
		,PCT_commute_1 #                                                                             <NA>        
		,PCT_B190010_IncQ4 #                                                                             <NA>    
		,PCT_B190010_IncQ3 #                                                                             <NA>    
		,PCT_B190010_IncQ2 #                                                                             <NA>    
		,PCT_B190010_IncQ1 #                                                                             <NA>    
		# ,UninsuredRate #                                            % Total: No Health Insurance Coverage   
		# ,PCT_B13010002 # % Women 15 to 50 Years for Whom Poverty Status Is Determined: Women Who Had a Birth
		# ,UnemploymentRate #               % Civilian Population in Labor Force 16 Years and Over: Unemployed   
		# ,SSIRate #                                                Percent of households with SSI income
		# ,PovertyRate #                                              Percent of population under the FPL        
		# ,PCT_C24050071 # % Civilian Employed Population 16 Years and Over: Production, Transportation, an      
		# ,PCT_C24050057 # % Civilian Employed Population 16 Years and Over: Natural Resources, Constructio      
		# ,PCT_C24050043 #   % Civilian Employed Population 16 Years and Over: Sales and Office Occupations      
		# ,PCT_C24050029 #            % Civilian Employed Population 16 Years and Over: Service Occupations      
		# ,PCT_C24050015 # % Civilian Employed Population 16 Years and Over: Management, Business, Science,      
		# ,PCT_C24050014 #          % Civilian Employed Population 16 Years and Over: Public Administration      
		# ,PCT_C24050013 # % Civilian Employed Population 16 Years and Over: Other Services, Except Public       
		# ,PCT_C24050012 # % Civilian Employed Population 16 Years and Over: Arts, Entertainment, and Recre      
		# ,PCT_C24050011 # % Civilian Employed Population 16 Years and Over: Educational Services, and Heal      
		# ,PCT_C24050010 # % Civilian Employed Population 16 Years and Over: Professional, Scientific, and       
		# ,PCT_C24050009 # % Civilian Employed Population 16 Years and Over: Finance and Insurance, and Rea      
		# ,PCT_C24050008 #                    % Civilian Employed Population 16 Years and Over: Information      
		# ,PCT_C24050007 # % Civilian Employed Population 16 Years and Over: Transportation and Warehousing      
		# ,PCT_C24050006 #                   % Civilian Employed Population 16 Years and Over: Retail Trade      
		# ,PCT_C24050005 #                % Civilian Employed Population 16 Years and Over: Wholesale Trade      
		# ,PCT_C24050004 #                  % Civilian Employed Population 16 Years and Over: Manufacturing      
		# ,PCT_C24050003 #                   % Civilian Employed Population 16 Years and Over: Construction      
		# ,PCT_C24050002 # % Civilian Employed Population 16 Years and Over: Agriculture, Forestry, Fishing      
		,housebuilt_after2010 #
		# ,PCT_B25034002        #                                      Housing Units: Built 2014 or Later
		# ,PCT_B25034003        #                                       Housing Units: Built 2010 to 2013
		,PCT_B25034004        #                                       Housing Units: Built 2000 to 2009
		,PCT_B25034005        #                                       Housing Units: Built 1990 to 1999
		,PCT_B25034006        #                                       Housing Units: Built 1980 to 1989
		,PCT_B25034007        #                                       Housing Units: Built 1970 to 1979
		,PCT_B25034008        #                                       Housing Units: Built 1960 to 1969
		,PCT_B25034009        #                                       Housing Units: Built 1950 to 1959
		,PCT_B25034010        #                                       Housing Units: Built 1940 to 1949
		# ,PCT_B25034011        #                                    Housing Units: Built 1939 or Earlier
		,PCT_B25024011 #      #                                      % Housing Units: Boat, Rv, Van, Etc.
		# ,PCT_B25024010 #                                                     % Housing Units: Mobile Home      
		# ,PCT_B25024009 #                                                      % Housing Units: 50 or More      
		# ,PCT_B25024008 #                                                        % Housing Units: 20 to 49      
		# ,PCT_B25024007 #                                                        % Housing Units: 10 to 19      
		# ,PCT_B25024006 #                                                          % Housing Units: 5 to 9      
		# ,PCT_B25024005 #                                                          % Housing Units: 3 or 4      
		# ,PCT_B25024004 #                                                               % Housing Units: 2      
		# ,PCT_B25024003 #                                                     % Housing Units: 1, Attached
		# ,PCT_B25024002 #                                                     % Housing Units: 1, Detached
		# ,PCT_B25003003 #                                        % Occupied Housing Units: Renter Occupied      
		# ,PCT_B25002003 #                                                          % Housing Units: Vacant      
		# ,PCT_B19059002 #                                             % Households: With Retirement Income      
		# ,PCT_B19058002 #                    % Households: With Cash Public Assistance or Food Stamps/Snap      
		# ,PCT_B19052003 #                                           % Households: No Wage or Salary Income      
		# ,PCT_B19001017 #                                                   % Households: $200,000 or More      
		# ,PCT_B19001016 #                                               % Households: $150,000 to $199,999      
		# ,PCT_B19001015 #                                               % Households: $125,000 to $149,999      
		# ,PCT_B19001014 #                                               % Households: $100,000 to $124,999      
		# ,PCT_B19001013 #                                                 % Households: $75,000 to $99,999      
		# ,PCT_B19001012 #                                                 % Households: $60,000 to $74,999      
		# ,PCT_B19001011 #                                                 % Households: $50,000 to $59,999      
		# ,PCT_B19001010 #                                                 % Households: $45,000 to $49,999      
		# ,PCT_B19001009 #                                                 % Households: $40,000 to $44,999      
		# ,PCT_B19001008 #                                                 % Households: $35,000 to $39,999      
		# ,PCT_B19001007 #                                                 % Households: $30,000 to $34,999      
		# ,PCT_B19001006 #                                                 % Households: $25,000 to $29,999      
		# ,PCT_B19001005 #                                                 % Households: $20,000 to $24,999      
		# ,PCT_B19001004 #                                                 % Households: $15,000 to $19,999      
		# ,PCT_B19001003 #                                                 % Households: $10,000 to $14,999      
		# ,PCT_B19001002 #                                                  % Households: Less than $10,000      
		# ,PCT_B16001003 #                                        Share who speak spanish, 5 years or older      
		,PCT_B16001002 #                                   Share who only speak english, 5 years or older
		# ,PCT_B15003025 #                                 % Population 25 Years and Over: Doctorate Degree      
		,PCT_B15003024 #                       % Population 25 Years and Over: Professional School Degree        
		,PCT_B15002035 #                         % Population 25 Years and Over: Female: Doctorate Degree        
		,PCT_B15002034 #               % Population 25 Years and Over: Female: Professional School Degree        
		,PCT_B15002033 #                          % Population 25 Years and Over: Female: Master's Degree        
		,PCT_B15002032 #                        % Population 25 Years and Over: Female: Bachelor's Degree        
		,PCT_B15002031 #                       % Population 25 Years and Over: Female: Associate's Degree        
		,PCT_B15002030 # % Population 25 Years and Over: Female: Some College, 1 or More Years, No Degree        
		,PCT_B15002029 #           % Population 25 Years and Over: Female: Some College, Less than 1 Year        
		,PCT_B15002028 # % Population 25 Years and Over: Female: High School Graduate (Includes Equivalen        
		,PCT_B15002027 #                   % Population 25 Years and Over: Female: 12th Grade, No Diploma        
		,PCT_B15002026 #                               % Population 25 Years and Over: Female: 11th Grade        
		,PCT_B15002025 #                               % Population 25 Years and Over: Female: 10th Grade        
		,PCT_B15002024 #                                % Population 25 Years and Over: Female: 9th Grade        
		,PCT_B15002023 #                        % Population 25 Years and Over: Female: 7th and 8th Grade        
		,PCT_B15002022 #                        % Population 25 Years and Over: Female: 5th and 6th Grade        
		,PCT_B15002021 #                     % Population 25 Years and Over: Female: Nursery to 4th Grade        
		,PCT_B15002020 #                   % Population 25 Years and Over: Female: No Schooling Completed        
		,PCT_B15002018 #                           % Population 25 Years and Over: Male: Doctorate Degree        
		,PCT_B15002017 #                 % Population 25 Years and Over: Male: Professional School Degree        
		,PCT_B15002016 #                            % Population 25 Years and Over: Male: Master's Degree        
		,PCT_B15002015 #                          % Population 25 Years and Over: Male: Bachelor's Degree        
		,PCT_B15002014 #                         % Population 25 Years and Over: Male: Associate's Degree        
		,PCT_B15002013 #   % Population 25 Years and Over: Male: Some College, 1 or More Years, No Degree        
		,PCT_B15002012 #             % Population 25 Years and Over: Male: Some College, Less than 1 Year        
		,PCT_B15002011 # % Population 25 Years and Over: Male: High School Graduate (Includes Equivalency        
		,PCT_B15002010 #                     % Population 25 Years and Over: Male: 12th Grade, No Diploma        
		,PCT_B15002009 #                                 % Population 25 Years and Over: Male: 11th Grade        
		,PCT_B15002008 #                                 % Population 25 Years and Over: Male: 10th Grade        
		,PCT_B15002007 #                                  % Population 25 Years and Over: Male: 9th Grade        
		,PCT_B15002006 #                          % Population 25 Years and Over: Male: 7th and 8th Grade        
		,PCT_B15002005 #                          % Population 25 Years and Over: Male: 5th and 6th Grade        
		,PCT_B15002004 #                       % Population 25 Years and Over: Male: Nursery to 4th Grade        
		,PCT_B15002003 #                     % Population 25 Years and Over: Male: No Schooling Completed        
		,PCT_B14005029 # % Population 16 to 19 Years: Female: Not Enrolled in School: Not High School Gra        
		,PCT_B14005028 # % Population 16 to 19 Years: Female: Not Enrolled in School: Not High School Gra        
		,PCT_B14005027 # % Population 16 to 19 Years: Female: Not Enrolled in School: Not High School Gra        
		,PCT_B14005024 # % Population 16 to 19 Years: Female: Not Enrolled in School: High School Graduat        
		,PCT_B14005017 #                        % Population 16 to 19 Years: Female: Enrolled in School  
		,PCT_B14005015 # % Population 16 to 19 Years: Male: Not Enrolled in School: Not High School Gradu        
		,PCT_B14005014 # % Population 16 to 19 Years: Male: Not Enrolled in School: Not High School Gradu        
		,PCT_B14005013 # % Population 16 to 19 Years: Male: Not Enrolled in School: Not High School Gradu        
		,PCT_B14005010 # % Population 16 to 19 Years: Male: Not Enrolled in School: High School Graduate         
		,PCT_B14005003 #                          % Population 16 to 19 Years: Male: Enrolled in School  
		,PCT_B14004024 # % Population 15 Years and Over: Female: Enrolled in Private College or Graduate         
		,PCT_B14004019 # % Population 15 Years and Over: Female: Enrolled in Public College or Graduate S        
		,PCT_B14004008 # % Population 15 Years and Over: Male: Enrolled in Private College or Graduate Sc        
		,PCT_B14004003 # % Population 15 Years and Over: Male: Enrolled in Public College or Graduate Sch        
		,PCT_B11011019 #    % Households: Nonfamily Households: Mobile Homes and All Other Types of Units        
		,PCT_B11011018 #                    % Households: Nonfamily Households: 2-or-More-Unit Structures        
		,PCT_B11011017 #                            % Households: Nonfamily Households: 1-Unit Structures        
		,PCT_B11011006 # % Households: Family Households: Married-Couple Family: Mobile Homes and All Oth        
		,PCT_B11011005 # % Households: Family Households: Married-Couple Family: 2-or-More-Unit Structure        
		,PCT_B11011004 #        % Households: Family Households: Married-Couple Family: 1-Unit Structures
		,PCT_B11009005 # % Households: Unmarried-Partner Households: Female Householder and Female Partne        
		,PCT_B11009003 #    % Households: Unmarried-Partner Households: Male Householder and Male Partner        
		#,PCT_B11001003 #                           % Households: Family Households: Married-Couple Family      
		#,PCT_B09005005 #		% Percent Children In Single Female HH	         
		#,PCT_B08303013 #         % Workers 16 Years and Over Who Did Not Work At Home: 90 or More Minutes      
		#,PCT_B08303012 #         % Workers 16 Years and Over Who Did Not Work At Home: 60 to 89 Minutes      
		#,PCT_B08303011 #         % Workers 16 Years and Over Who Did Not Work At Home: 45 to 59 Minutes      
		#,PCT_B08303010 #         % Workers 16 Years and Over Who Did Not Work At Home: 40 to 44 Minutes      
		#,PCT_B08303009 #         % Workers 16 Years and Over Who Did Not Work At Home: 35 to 39 Minutes      
		#,PCT_B08303008 #         % Workers 16 Years and Over Who Did Not Work At Home: 30 to 34 Minutes      
		#,PCT_B08303007 #         % Workers 16 Years and Over Who Did Not Work At Home: 25 to 29 Minutes      
		#,PCT_B08303006 #         % Workers 16 Years and Over Who Did Not Work At Home: 20 to 24 Minutes      
		#,PCT_B08303005 #         % Workers 16 Years and Over Who Did Not Work At Home: 15 to 19 Minutes      
		#,PCT_B08303004 #         % Workers 16 Years and Over Who Did Not Work At Home: 10 to 14 Minutes      
		#,PCT_B08303003 #        % Workers 16 Years and Over Who Did Not Work At Home: 5 to 9 Minutes      
		#,PCT_B08301010 #        % Workers 16 Years and Over: Public Transportation (Excluding Taxicab)      
		#,vehicles_2plus #	  % Workers 16 Years and Over:  2 or more vehicles
		,PCT_B08301002 #          % Workers 16 Years and Over: Car, Truck, or Van               
		,PCT_B08014007 #          % Workers 16 Years and Over in Households: 5 or More Vehicles Available        
		,PCT_B08014006 #          % Workers 16 Years and Over in Households: 4 Vehicles Available        
		,PCT_B08014005 #          % Workers 16 Years and Over in Households: 3 Vehicles Available        
		,PCT_B08014004 #          % Workers 16 Years and Over in Households: 2 Vehicles Available        
		,PCT_B08014003 #          % Workers 16 Years and Over in Households: 1 Vehicle Available        
		# ,PCT_B08014002 #        % Workers 16 Years and Over in Households: No Vehicle Available      
		,PCT_B07204015   #   % Population Elsewhere_Different_County_Different_State_West					   
		,PCT_B07204014   #   % Population Elsewhere_Different_County_Different_State_South			         
		,PCT_B07204013   #   % Population Elsewhere_Different_County_Different_State_Midwest			         
		,PCT_B07204012   #   % Population Elsewhere_Different_County_Different_State_Northeast			         
		,PCT_B07204011   #   % Population Elsewhere_Different_County_Different_State				         
		,PCT_B07204010   #   % Population Elsewhere_Different_County_Same_State					         
		,PCT_B07204009   #   % Population Elsewhere_Different_County							        
		,PCT_B07204008   #   % Population Elsewhere_Same_County								         
		# ,PCT_B07204007 # % Population Different County and Town   							   
		,PCT_B07204006   # % Population Same_City_Or_Town_Different_County_(Same_State)				         
		# ,PCT_B07204005 # % Population Same_city_or_town_same_County 							   
		,PCT_B07204004   # % Population Same_city_or_town									
		,PCT_B07204003   # % PCT_Different_House_1_Year_Ago     								   
		# ,PCT_B07009006 # % Population 25 Years and Over in the United States: Graduate or Professional De      
		# ,PCT_B07009005 #           % Population 25 Years and Over in the United States: Bachelor's Degree      
		# ,PCT_B07009004 # % Population 25 Years and Over in the United States: Some College or Associate's      
		# ,PCT_B07009003 # % Population 25 Years and Over in the United States: High School Graduate (Inclu      
		# ,PCT_B07009002 # % Population 25 Years and Over in the United States: Less than High School Gradu      
		# ,PCT_B05001006 #                      % Total Population in the United States: Not a U.S. Citizen      
		# ,PCT_B03001003 #                                           % Total Population: Hispanic or Latino      
		,Other_Race_incl_Pacific_Islander
		,PCT_B02001010 # % Total Population: Two or More Races: Two Races Excluding Some Other Race, and         
		,PCT_B02001009 #       % Total Population: Two or More Races: Two Races Including Some Other Race        
		,PCT_B02001008 #                                            % Total Population: Two or More Races        
		,PCT_B02001007 #                                        % Total Population: Some Other Race Alone        
		,PCT_B02001006 #             % Total Population: Native Hawaiian and Other Pacific Islander Alone        
		# ,PCT_B02001005 #                                                  % Total Population: Asian Alone      
		# ,PCT_B02001004 #          % Total Population: American Indian and Alaska Native Alone      
		# ,PCT_B02001003 #                  % Total Population: Black or African American Alone      
		# ,PCT_B02001002 #                                      % Total Population: White Alone      
		# ,PCT_B01001049 #                        % Total Population: Female: 85 Years and Over
		# ,PCT_B01001048 #                           % Total Population: Female: 80 to 84 Years
		# ,PCT_B01001047 #                           % Total Population: Female: 75 to 79 Years
		# ,PCT_B01001046 #                           % Total Population: Female: 70 to 74 Years
		# ,PCT_B01001045 #                           % Total Population: Female: 67 to 69 Years
		# ,PCT_B01001044 #                          % Total Population: Female: 65 and 66 Years
		# ,PCT_B01001043 #                           % Total Population: Female: 62 to 64 Years
		# ,PCT_B01001042 #                          % Total Population: Female: 60 and 61 Years
		# ,PCT_B01001041 #                           % Total Population: Female: 55 to 59 Years
		# ,PCT_B01001040 #                           % Total Population: Female: 50 to 54 Years
		# ,PCT_B01001039 #                           % Total Population: Female: 45 to 49 Years
		# ,PCT_B01001038 #                           % Total Population: Female: 40 to 44 Years
		# ,PCT_B01001037 #                           % Total Population: Female: 35 to 39 Years
		# ,PCT_B01001036 #                           % Total Population: Female: 30 to 34 Years
		# ,PCT_B01001035 #                           % Total Population: Female: 25 to 29 Years
		# ,PCT_B01001034 #                           % Total Population: Female: 22 to 24 Years
		# ,PCT_B01001033 #                                 % Total Population: Female: 21 Years
		# ,PCT_B01001032 #                                 % Total Population: Female: 20 Years
		# ,PCT_B01001031 #                          % Total Population: Female: 18 and 19 Years
		# ,PCT_B01001030 #                           % Total Population: Female: 15 to 17 Years
		# ,PCT_B01001029 #                           % Total Population: Female: 10 to 14 Years
		# ,PCT_B01001028 #                             % Total Population: Female: 5 to 9 Years
		# ,PCT_B01001027 #                            % Total Population: Female: Under 5 Years
		# ,PCT_B01001025 #                          % Total Population: Male: 85 Years and Over
		# ,PCT_B01001024 #                             % Total Population: Male: 80 to 84 Years
		# ,PCT_B01001023 #                             % Total Population: Male: 75 to 79 Years
		# ,PCT_B01001022 #                             % Total Population: Male: 70 to 74 Years
		# ,PCT_B01001021 #                             % Total Population: Male: 67 to 69 Years
		# ,PCT_B01001020 #                            % Total Population: Male: 65 and 66 Years
		# ,PCT_B01001019 #                             % Total Population: Male: 62 to 64 Years
		# ,PCT_B01001018 #                            % Total Population: Male: 60 and 61 Years
		# ,PCT_B01001017 #                             % Total Population: Male: 55 to 59 Years
		# ,PCT_B01001016 #                             % Total Population: Male: 50 to 54 Years
		# ,PCT_B01001015 #                             % Total Population: Male: 45 to 49 Years
		# ,PCT_B01001014 #                             % Total Population: Male: 40 to 44 Years
		# ,PCT_B01001013 #                             % Total Population: Male: 35 to 39 Years
		# ,PCT_B01001012 #                             % Total Population: Male: 30 to 34 Years
		# ,PCT_B01001011 #                             % Total Population: Male: 25 to 29 Years
		# ,PCT_B01001010 #                             % Total Population: Male: 22 to 24 Years
		# ,PCT_B01001009 #                                   % Total Population: Male: 21 Years
		# ,PCT_B01001008 #                                   % Total Population: Male: 20 Years
		# ,PCT_B01001007 #                            % Total Population: Male: 18 and 19 Years
		# ,PCT_B01001006 #                             % Total Population: Male: 15 to 17 Years
		# ,PCT_B01001005 #                             % Total Population: Male: 10 to 14 Years
		# ,PCT_B01001004 #                               % Total Population: Male: 5 to 9 Years
		# ,PCT_B01001003 #                              % Total Population: Male: Under 5 Years
		# ,PCT_16_19_school #                              % Population 16-19 enrolled in school  
		# ,GroupQuarters #                      Share of population living in group quarters    
		# ,Naturalized
		,ForeignBorn #        Not a citizen or naturalized (doesn't include born abroad to Americans)
		# ,NonHispanicWhite #                    % Share of total population that is non-Hispanic white   
		,CentralAmerican #                       % of total populationof Central American origin      
		,PuertoRican #                           % of total populationof Puerto Rican origin          
		,SouthAmerican #                         % of total populationof South American origin        
		,Mexican #                               % of total populationof Mexican origin              
		 ,German #                    Language  -  German
		,French #                     Language  - French, Haitian, Cajun           
		,English #                    Language  - English        
		,Spanish #                    Language  - Spanish    			 
		,Slavic #                     Language  -Slavic languages           
		,IndoEuropean #               Language - Other Indo-European Language         
		,Chinese #                    Language  - Chinese          
		,Korean #                     Language  -  Korean           
		,Arabic #                     Language  -     Arabic           
		,Tagalog #                    Language  - Tagalog 	         
		,Vietnamese #                 Language  -	Vietnamese			    
		,OtherAsian #                 Language - Other Asian languages           
		,Unspecifiedlang #            Language  -	Unspecified language	
		#,DisabilityRate #       Share of civilian noninstitutionalized population classified as disabled   
		,B25109001 #                              Owner-Occupied Housing Units: Median Value -- Total            
		,B25105001 #  Occupied Housing Units with Monthly Housing Costs: Median Monthly Housing Costs            
		,B25088003 # Owner-Occupied Housing Units: Median Selected Monthly Owner Costs (Dollars) -- T            
		,B25088002 # Owner-Occupied Housing Units: Median Selected Monthly Owner Costs (Dollars) -- T            
		# ,B25078001 #                     Owner-Occupied Housing Units: Upper Value Quartile (Dollars)          
		# ,B25077001 #                             Owner-Occupied Housing Units: Median Value (Dollars)          
		# ,B25076001 #                     Owner-Occupied Housing Units: Lower Value Quartile (Dollars)          
		# ,B25064001 #                Renter-Occupied Housing Units Paying Cash Rent: Median Gross Rent          
		# ,B25035001 #                                     Housing Units: Median Year Structure Built   
		# ,B19083001 #                                                           Households: Gini Index          
		# ,B19081006 #                                      Households: Quintile Means: Top 5 Percent
		# ,B19081005 #                                      Households: Quintile Means: Highest Quintile           
		# ,B19081004 #                                      Households: Quintile Means: Fourth Quintile           
		# ,B19081003 #                                      Households: Quintile Means: Third Quintile           
		# ,B19081002 #                                      Households: Quintile Means: Second Quintile           
		# ,B19081001 #                                      Households: Quintile Means: Lowest Quintile
		,B19049005 # 	Households: Median Household Income in the Past 12 Months (In 2017 Inflation-Adj -U25       
		,B19049004 # 	Households: Median Household Income in the Past 12 Months (In 2017 Inflation-Adj -25-44     
		,B19049003 # 	Households: Median Household Income in the Past 12 Months (In 2017 Inflation-Adj -45-65     
		,B19049002 # 	Households: Median Household Income in the Past 12 Months (In 2017 Inflation-Adj -O65       
		# ,B01002001 #                            Median Age -- Total              
		# ,B01001001 #                            Total Population             
		# ,A00002_002 #                           Population Density (Per Sq. Mile)            
		,decline_manufacturing_absolute #         decline in the number of manufacturing workers 
		,decline_population #                     Percent decline in population (2013-2017 vs 2009-2013)
		,decline_employment_rate #                Percentage point decline in employment rate
		,decline_manufacturing_rel_ind #    Percentage point decline in manufacturing's share of employed 
		,decline_manufacturing_rel_pop #   	Percentage point decline in manufacturing workers' share of the population
		,bus_pct_change  #				business address change 2016-2017
		,res_pct_change #					residential address change 2016-2017
		,ams_pct_change #					total address change 2016-2017
		#,res_prop
		#,bus_prop
		#,MedHHInc2017
		,ChangeIncome #                    Percent change in Median Household Income              
		,ChangeMedHomePrice #              Percent change in median owner-occupied house price   
		,lt_perc_pop
		# ,mt_perc_pop
		,lt_perc_inc
		# ,EnrolledColleg
		,ShareMasters
		# ,PercentFemale
		# ,PCT_A01001_002 #		 "Total Population: Under 5 Years"
		# ,PCT_Age5to17   #		 "Total Population: 5 to 17 Years"
		# ,PCT_A01001_006 #		 "Total Population: 18 to 24 Years"
		# ,PCT_A01001_007 #		 "Total Population: 25 to 34 Years"
		# ,PCT_A01001_008 #		 "Total Population: 35 to 44 Years"
		,PCT_Age45to64  #		 	"Total Population: 45 to 64 Years"
		# ,PCT_Age45to54   	 	"Split Middle-Age #1: 45 to 54"
		# ,PCT_Age55to64   # 	 	"Split Middle-Age #2: 55 to 64"
		# ,PCT_A01001_011 #		 "Total Population: 65 to 74 Years"
		# ,PCT_A01001_012 #		 "Total Population: 75 to 84 Years"
		# ,PCT_A01001_013 #		 "Total Population: 85 Years and Over"
		,PCT_65over #                	"% Pop 65 and over"
		,PCT_25to64 #                 "% Pop 25 to 64"
		,PCT_18to24 #                	"% Pop 18 to 24"
		,PCT_Under18 #            	"% Pop under 18"
		,changeepop00_17
		,changeepop13_17
		,changeunempl00_17
		,changeunempl13_17
		# ,changepop00_17
		# ,changepop13_17
		,changemanufacturing00_17
		,changemanufacturing13_17
		,changeMedHHinc00_17
		,changeMedHHinc13_17
		,changeMedHomeValue00_17
		,changeMedHomeValue13_17
		#, non_familyHH
		# ,LFPR2554
		# ,EPOP2554
		# ,pov_200
		# ,PCT_C17002002
		# ,PerCapitaIncome
		# ,armed_forces_perc
		# ,PerWorkerWageIncome
		# ,EnglishOrGerman
	))

df1 = OZ_dat[,-c(1:2)]
df1 = sapply( df1, as.numeric )
df1_cols=names(df1)

df1_names = OZ_dat[,1:2]
rownames(df1)=df1_names[,1]
missings_df1=as.data.frame(df1) %>% summarise_all(funs(sum(is.na(.)))) %>% gather()
print(missings_df1) %>% arrange(value)

# vif https://stats.stackexchange.com/questions/82984/how-to-test-and-avoid-multicollinearity-in-mixed-linear-model
vif_scores=usdm::vif(x=as.data.frame(df1))
data.key.c=subset(data.key.c, var.label!= '% Housing Units: Built 2010 to 2013' & var.label!= '% Housing Units: Built 2014 or Later')
data.key.i=merge(data.key.c, vif_scores, by.x = 'var.name', by.y = 'Variables', all = T) 
data.key.i=merge(data.key.i,missings_df1, by.x = c('var.name'),by.y = c('key'),  all = T)  %>% arrange(desc(var.name))
data.key.i=(data.key.i[!is.na(data.key.i$VIF),] %>% arrange(VIF))
data.key.i

# md_pattern_df=md.pattern(df1)
gc()
#### Imputation Function ####
df1_imp = parlmice(df1, m=1
	,meth='pmm'
	#, cluster.seed=3
	,pred = quickpred(df1, method = "spearman", mincor=0.2875, minpuc=0.4 ,exc=c(
	 'B01001001'     # Total Population
	,'B25078001' # Owner-Occupied Housing Units: Upper Value Quartile (Dollars)
	,'B25076001' # Owner-Occupied Housing Units: Lower Value Quartile (Dollars)
	,'NonHispanicWhite'
	,'ForeignBorn'
	,"German"
	,'B19081006'  # Households: Quintile Means: Top 5 Percent
	,'B19081005'  # Households: Quintile Means: Highest Quintile
	,'B19081004'  # Households: Quintile Means: Fourth Quintile
	,'B19081003'  # Households: Quintile Means: Third Quintile
	,'B19081002'  # Households: Quintile Means: Second Quintile
	,'B19081001'  # Households: Quintile Means: Lowest Quintile
	,'changepop00_17'
	,'changepop13_17'
	,'pov_200' # Near Poverty
	,'PCT_C17002002' # Deep Poverty
	,'PerCapitaIncome'
	,'PerWorkerWageIncome'
	,'LFPR2554'
	,'EPOP2554'
	,'vehicles_2plus'
	,'mt_perc_pop'
		)
		),
	cl.type = "FORK")
df1_completed = (complete(df1_imp))

openxlsx::write.xlsx(df1_completed, "~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/df_imputed.xlsx")
df1_completed=openxlsx::read.xlsx( "~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/df_imputed.xlsx")
# openxlsx::write.xlsx(df1_completed, "~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/df_imputed_alt.xlsx")
# df1_completed=openxlsx::read.xlsx( "~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/df_imputed_alt.xlsx")

df1_completed %>% summarise_all(funs(sum(.<0 )))
ACS_labels=as.data.frame(colnames(df1_completed));names(ACS_labels)[names(ACS_labels) == 'colnames(df1_completed)'] = 'var.name'
ACS_labels$var.name=as.character(ACS_labels$var.name)

ACS_labels=merge(ACS_labels,data.key.i, by = 'var.name', all.y = T, sort = F)
ACS_labels$var.label=ifelse(is.na(ACS_labels$var.label ), as.character(ACS_labels$var.name), as.character(ACS_labels$var.label) )

ACS_labels$var.label=ifelse(ACS_labels$var.name == 'PCT_B07204011', "Different_State", ACS_labels$var.label)
ACS_labels$var.label=ifelse(ACS_labels$var.name == 'PCT_B07204010', "Different_County", ACS_labels$var.label)
ACS_labels$var.label=ifelse(ACS_labels$var.name == 'PCT_B07204005', "Same_CityOrCounty", ACS_labels$var.label)

df1_unscaled=df1_completed
colnames( df1_unscaled )=(ACS_labels$var.label)
df1_unscaled=cbind(df1_names,df1_unscaled)

openxlsx::write.xlsx(df1_unscaled, "~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/ACS_imp_unscaled.xlsx")
df1_completed = scale(as.matrix(df1_completed))
ACS_dat = cbind(df1_names, df1_completed)
ACS_dat %>% summarise_all(funs(sum(is.na(.))))

#### finishing up data prep 

openxlsx::write.xlsx(ACS_dat, "~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/ACS_dat_clean.xlsx")
ACS_dat=openxlsx::read.xlsx("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/ACS_dat_clean.xlsx")

# openxlsx::write.xlsx(ACS_dat, "~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/ACS_dat_clean_alt.xlsx")
# ACS_dat=openxlsx::read.xlsx("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/ACS_dat_clean_alt.xlsx")


gc()
ACS_dat=subset(ACS_dat, select = -c(B01001001 # Remove population count and other vars from cluster algorithm
	,MedHHInc2017
	# ,PCT_B13010002 # % Poverty - Had a Child in last 12
	,B19081006  # Households: Quintile Means: Top 5 Percent
	,B19081005  # Households: Quintile Means: Highest Quintile
	,B19081004  # Households: Quintile Means: Fourth Quintile
	,B19081003  # Households: Quintile Means: Third Quintile
	,B19081002  # Households: Quintile Means: Second Quintile
	,B19081001  # Households: Quintile Means: Lowest Quintile
	,B25078001  # Home Price - High
	,B25076001  # Home Price - Low
	,pov_200 # Near Poverty
	,PCT_C17002002 # Deep Poverty
	# ,PerCapitaIncome
	,non_familyHH
	,PerWorkerWageIncome
	,LFPR2554
	,EPOP2554
	# ,German
	# ,ForeignBorn
	,changepop00_17
	,changepop13_17
	,NonHispanicWhite
	,changepop00_17
	,vehicles_2plus
	# ,PercentFemale
	# ,PCT_A01001_002 #		 "Total Population: Under 5 Years"
	# ,PCT_Age5to17   #		 "Total Population: 5 to 17 Years"
	# ,PCT_A01001_006 #		 "Total Population: 18 to 24 Years"
	# ,PCT_A01001_007 #		 "Total Population: 25 to 34 Years"
	# ,PCT_A01001_008 #		 "Total Population: 35 to 44 Years"
	# ,PCT_Age45to64  #		 "Total Population: 45 to 64 Years"
	# ,PCT_Age45to54   	 		"Split Middle-Age #1: 45 to 54"
	# ,PCT_Age55to64   # 	 		"Split Middle-Age #2: 55 to 64"
	# ,PCT_A01001_011 #		 "Total Population: 65 to 74 Years"
	# ,PCT_A01001_012 #		 "Total Population: 75 to 84 Years"
	# ,PCT_A01001_013 #		 "Total Population: 85 Years and Over"
	,PCT_B01001049 #             % Total Population: Female: 85 Years and Over
	,PCT_B01001048 #                % Total Population: Female: 80 to 84 Years
	,PCT_B01001047 #                % Total Population: Female: 75 to 79 Years
	,PCT_B01001046 #                % Total Population: Female: 70 to 74 Years
	,PCT_B01001045 #                % Total Population: Female: 67 to 69 Years
	,PCT_B01001044 #               % Total Population: Female: 65 and 66 Years
	,PCT_B01001043 #                % Total Population: Female: 62 to 64 Years
	,PCT_B01001042 #               % Total Population: Female: 60 and 61 Years
	,PCT_B01001041 #                % Total Population: Female: 55 to 59 Years
	,PCT_B01001040 #                % Total Population: Female: 50 to 54 Years
	,PCT_B01001039 #                % Total Population: Female: 45 to 49 Years
	,PCT_B01001038 #                % Total Population: Female: 40 to 44 Years
	,PCT_B01001037 #                % Total Population: Female: 35 to 39 Years
	,PCT_B01001036 #                % Total Population: Female: 30 to 34 Years
	,PCT_B01001035 #                % Total Population: Female: 25 to 29 Years
	,PCT_B01001034 #                % Total Population: Female: 22 to 24 Years
	,PCT_B01001033 #                      % Total Population: Female: 21 Years
	,PCT_B01001032 #                      % Total Population: Female: 20 Years
	,PCT_B01001031 #               % Total Population: Female: 18 and 19 Years
	,PCT_B01001030 #                % Total Population: Female: 15 to 17 Years
	,PCT_B01001029 #                % Total Population: Female: 10 to 14 Years
	,PCT_B01001028 #                  % Total Population: Female: 5 to 9 Years
	,PCT_B01001027 #                 % Total Population: Female: Under 5 Years
	,PCT_B01001025 #               % Total Population: Male: 85 Years and Over
	,PCT_B01001024 #                  % Total Population: Male: 80 to 84 Years
	,PCT_B01001023 #                  % Total Population: Male: 75 to 79 Years
	,PCT_B01001022 #                  % Total Population: Male: 70 to 74 Years
	,PCT_B01001021 #                  % Total Population: Male: 67 to 69 Years
	,PCT_B01001020 #                 % Total Population: Male: 65 and 66 Years
	,PCT_B01001019 #                  % Total Population: Male: 62 to 64 Years
	,PCT_B01001018 #                 % Total Population: Male: 60 and 61 Years
	,PCT_B01001017 #                  % Total Population: Male: 55 to 59 Years
	,PCT_B01001016 #                  % Total Population: Male: 50 to 54 Years
	,PCT_B01001015 #                  % Total Population: Male: 45 to 49 Years
	,PCT_B01001014 #                  % Total Population: Male: 40 to 44 Years
	,PCT_B01001013 #                  % Total Population: Male: 35 to 39 Years
	,PCT_B01001012 #                  % Total Population: Male: 30 to 34 Years
	,PCT_B01001011 #                  % Total Population: Male: 25 to 29 Years
	,PCT_B01001010 #                  % Total Population: Male: 22 to 24 Years
	,PCT_B01001009 #                        % Total Population: Male: 21 Years
	,PCT_B01001008 #                        % Total Population: Male: 20 Years
	,PCT_B01001007 #                 % Total Population: Male: 18 and 19 Years
	,PCT_B01001006 #                  % Total Population: Male: 15 to 17 Years
	,PCT_B01001005 #                  % Total Population: Male: 10 to 14 Years
	,PCT_B01001004 #                    % Total Population: Male: 5 to 9 Years
	,PCT_B01001003 #                   % Total Population: Male: Under 5 Years
))

ACS_dat %>% summarise_all(funs(sum(is.na(.) )))

ACS_dat_names = ACS_dat[,1]
ACS_dat_OZs = ACS_dat[,1:2]
ACS_matrix = ACS_dat[,-c(1:2)]
rownames(ACS_matrix) = ACS_dat_names

pc=prcomp(ACS_matrix)
##compute variance
std_dev = pc$sdev
pr_var = std_dev^2
prop_varex = pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component",
             ylab = "Proportion of Variance Explained",
             type = "b")

plot(cumsum(prop_varex), xlab = "Principal Component",
              ylab = "Cumulative Proportion of Variance Explained",
              type = "b")

cumsum(prop_varex)

# Primary Components
comp1 = data.frame(pc$x[,1:87])
comp2 = data.frame(pc$x[,1:94])

# Alt Components
# alt_comp1 = data.frame(pc$x[,1:98])
# alt_comp2 = data.frame(pc$x[,1:87])

#### K-Means Implimentation #### 
gc()

# k_pca_12_1= kmeans(comp1, centers = 12, nstart=31, iter.max=5500);gc()
# k_pca_13_1= kmeans(comp1, centers = 13, nstart=45, iter.max=5000);gc()
k_pca_14_1= kmeans(comp1, centers = 14, nstart=50, iter.max=5000);gc()
k_pca_15_1= kmeans(comp1, centers = 15, nstart=50, iter.max=5000);gc()
k_pca_16_1= kmeans(comp1, centers = 16, nstart=75, iter.max=5000);gc()
k_pca_17_1= kmeans(comp1, centers = 17, nstart=75, iter.max=5000);gc()
# k_pca_18_1= kmeans(comp1, centers = 18, nstart=50, iter.max=30500);gc()
# k_pca_19_1= kmeans(comp1, centers = 19, nstart=50, iter.max=30500);gc()
# k_pca_20_1= kmeans(comp1, centers = 20, nstart=50, iter.max=9500);gc()

# k_pca_12_2= kmeans(comp2, centers = 12, nstart=50, iter.max=5500);gc()
k_pca_13_2= kmeans(comp2, centers = 13,  nstart=50, iter.max=5000);gc()
k_pca_14_2= kmeans(comp2, centers = 14, nstart=75, iter.max=5000);gc()
k_pca_15_2= kmeans(comp2, centers = 15, nstart=75, iter.max=5000);gc()
k_pca_16_2= kmeans(comp2, centers = 16, nstart=75, iter.max=5000);gc()
# k_pca_17_2= kmeans(comp2, centers = 17, nstart=75, iter.max=5000);gc()
# k_pca_18_2= kmeans(comp2, centers = 18, nstart=50, iter.max=9500);gc()
# k_pca_19_2= kmeans(comp2, centers = 19, nstart=50, iter.max=9500);gc()
# k_pca_20_2= kmeans(comp2, centers = 20, nstart=50, iter.max=9500);gc()

# k_pca_12_3= kmeans(comp3, centers = 12, nstart=50, iter.max=5000);gc()
# k_pca_13_3= kmeans(comp3, centers = 13, nstart=50, iter.max=5000);gc()
# k_pca_14_3= kmeans(ACS_matrix, centers = 14, nstart=75, iter.max=5000);gc()
# k_pca_15_3= kmeans(ACS_matrix, centers = 15, nstart=75, iter.max=5000);gc()
# k_pca_16_3= kmeans(ACS_matrix, centers = 16, nstart=75, iter.max=5000);gc()
# k_pca_17_3= kmeans(comp3, centers = 17, nstart=50, iter.max=9500);gc()
# k_pca_18_3= kmeans(comp3, centers = 18, nstart=50, iter.max=9500);gc()
# k_pca_19_3= kmeans(comp3, centers = 19, nstart=50, iter.max=5000);gc()

#### Generate groups ####

# ACS_test=cbind(ACS_dat_OZs,k_pca_12_1=k_pca_12_1$cluster)
# ACS_test=cbind(ACS_dat_OZs,k_pca_13_1=k_pca_13_1$cluster)
ACS_test=cbind(ACS_dat_OZs,k_pca_14_1=k_pca_14_1$cluster)
ACS_test=cbind(ACS_test,k_pca_15_1=k_pca_15_1$cluster)
ACS_test=cbind(ACS_test,k_pca_16_1=k_pca_16_1$cluster)
ACS_test=cbind(ACS_test,k_pca_17_1=k_pca_17_1$cluster)
# ACS_test=cbind(ACS_test,k_pca_18_1=k_pca_18_1$cluster)
# ACS_test=cbind(ACS_test,k_pca_19_1=k_pca_19_1$cluster)
# ACS_test=cbind(ACS_test,k_pca_20_1=k_pca_20_1$cluster)

# ACS_test=cbind(ACS_test,k_pca_12_2=k_pca_12_2$cluster)
ACS_test=cbind(ACS_test,k_pca_13_2=k_pca_13_2$cluster)
ACS_test=cbind(ACS_test,k_pca_14_2=k_pca_14_2$cluster)
ACS_test=cbind(ACS_test,k_pca_15_2=k_pca_15_2$cluster)
ACS_test=cbind(ACS_test,k_pca_16_2=k_pca_16_2$cluster)
ACS_test=cbind(ACS_test,k_pca_17_2=k_pca_17_2$cluster)
# ACS_test=cbind(ACS_test,k_pca_18_2=k_pca_18_2$cluster)
# ACS_test=cbind(ACS_test,k_pca_19_2=k_pca_19_2$cluster)
# ACS_test=cbind(ACS_test,k_pca_20_2=k_pca_20_2$cluster)

# ACS_test=cbind(ACS_test,k_pca_12_3=k_pca_12_3$cluster)
# ACS_test=cbind(ACS_test,k_pca_13_3=k_pca_13_3$cluster)
ACS_test=cbind(ACS_test,k_pca_14_3=k_pca_14_3$cluster)
ACS_test=cbind(ACS_test,k_pca_15_3=k_pca_15_3$cluster)
ACS_test=cbind(ACS_test,k_pca_16_3=k_pca_16_3$cluster)
# ACS_test=cbind(ACS_test,k_pca_17_3=k_pca_17_3$cluster)
# ACS_test=cbind(ACS_test,k_pca_18_3=k_pca_18_3$cluster)
# ACS_test=cbind(ACS_test,k_pca_19_3=k_pca_19_3$cluster)
# ACS_test=cbind(ACS_test,k_pca_20_3=k_pca_20_3$cluster)

# ACS_test=cbind(ACS_test,k_pca_12_4=k_pca_12_4$cluster)
# ACS_test=cbind(ACS_test,k_pca_13_4=k_pca_13_4$cluster)
# ACS_test=cbind(ACS_test,k_pca_14_4=k_pca_14_4$cluster)
# ACS_test=cbind(ACS_test,k_pca_15_4=k_pca_15_4$cluster)
# ACS_test=cbind(ACS_test,k_pca_16_4=k_pca_16_4$cluster)

# ACS_test=cbind(ACS_test,pca_clara_12_1=pca_c12_1$clustering)
# ACS_test=cbind(ACS_test,pca_clara_13_1=pca_c13_1$clustering)
# ACS_test=cbind(ACS_test,pca_clara_14_1=pca_c14_1$clustering)
# 
# ACS_test=cbind(ACS_test,pca_clara_12_2=pca_c12_2$clustering)
# ACS_test=cbind(ACS_test,pca_clara_13_2=pca_c13_2$clustering)
# ACS_test=cbind(ACS_test,pca_clara_14_2=pca_c14_2$clustering)
# ACS_test=cbind(ACS_test,pca_clara_15_2=pca_c15_2$clustering)

OZ_dat_lang = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/cluster_inputs/languagesupplement.dta")
OZ_dat_lang$St_Code=stringi::stri_sub(OZ_dat_lang$FIPS, from=1, to=2)
OZ_dat_lang$admin_type=ifelse(as.numeric(OZ_dat_lang$St_Code) <= 56, "1", "0") 
ACS_test=merge(ACS_test, OZ_dat_lang[,c(1,5:7)], by = c("FIPS", "OZ"), all.y = T)
rm(OZ_dat_lang)

##### Fill empties #####
# ACS_test$k_pca_11_1[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
# ACS_test$k_pca_12_1[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
# ACS_test$k_pca_13_1[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
ACS_test$k_pca_14_1[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
ACS_test$k_pca_15_1[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
ACS_test$k_pca_16_1[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
ACS_test$k_pca_17_1[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
# ACS_test$k_pca_18_1[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
# ACS_test$k_pca_19_1[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
# ACS_test$k_pca_20_1[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"

# ACS_test$k_pca_11_2[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
# ACS_test$k_pca_12_2[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
ACS_test$k_pca_13_2[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
ACS_test$k_pca_14_2[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
ACS_test$k_pca_15_2[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
ACS_test$k_pca_16_2[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
ACS_test$k_pca_17_2[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
# ACS_test$k_pca_18_2[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
# ACS_test$k_pca_19_2[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
# ACS_test$k_pca_20_2[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"

# ACS_test$k_pca_11_3[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
# ACS_test$k_pca_12_3[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
# ACS_test$k_pca_13_3[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
ACS_test$k_pca_14_3[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
ACS_test$k_pca_15_3[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
ACS_test$k_pca_16_3[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
# ACS_test$k_pca_17_3[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
# ACS_test$k_pca_18_3[ACS_test$admin_type == "0"] = "Puerto Rico & US Territories"
 

###
# ACS_test$k_pca_12_1[is.na(ACS_test$k_pca_12_1)] = "Sparsely Populated"
# ACS_test$k_pca_13_1[is.na(ACS_test$k_pca_13_1)] = "Sparsely Populated"
ACS_test$k_pca_14_1[is.na(ACS_test$k_pca_14_1)] = "Sparsely Populated"
ACS_test$k_pca_15_1[is.na(ACS_test$k_pca_15_1)] = "Sparsely Populated"
ACS_test$k_pca_16_1[is.na(ACS_test$k_pca_16_1)] = "Sparsely Populated"
ACS_test$k_pca_17_1[is.na(ACS_test$k_pca_17_1)] = "Sparsely Populated"
# ACS_test$k_pca_18_1[is.na(ACS_test$k_pca_18_1)] = "Sparsely Populated"
# ACS_test$k_pca_19_1[is.na(ACS_test$k_pca_19_1)] = "Sparsely Populated"
# ACS_test$k_pca_20_1[is.na(ACS_test$k_pca_20_1)] = "Sparsely Populated"

# ACS_test$k_pca_12_2[is.na(ACS_test$k_pca_12_2)] = "Sparsely Populated"
ACS_test$k_pca_13_2[is.na(ACS_test$k_pca_13_2)] = "Sparsely Populated"
ACS_test$k_pca_14_2[is.na(ACS_test$k_pca_14_2)] = "Sparsely Populated"
ACS_test$k_pca_15_2[is.na(ACS_test$k_pca_15_2)] = "Sparsely Populated"
ACS_test$k_pca_16_2[is.na(ACS_test$k_pca_16_2)] = "Sparsely Populated"
ACS_test$k_pca_17_2[is.na(ACS_test$k_pca_17_2)] = "Sparsely Populated"
# ACS_test$k_pca_18_2[is.na(ACS_test$k_pca_18_2)] = "Sparsely Populated"
# ACS_test$k_pca_19_2[is.na(ACS_test$k_pca_19_2)] = "Sparsely Populated"
# ACS_test$k_pca_20_2[is.na(ACS_test$k_pca_20_2)] = "Sparsely Populated"

# ACS_test$k_pca_12_3[is.na(ACS_test$k_pca_12_3)] = "Sparsely Populated"
# ACS_test$k_pca_13_3[is.na(ACS_test$k_pca_13_3)] = "Sparsely Populated"
ACS_test$k_pca_14_3[is.na(ACS_test$k_pca_14_3)] = "Sparsely Populated"
ACS_test$k_pca_15_3[is.na(ACS_test$k_pca_15_3)] = "Sparsely Populated"
ACS_test$k_pca_16_3[is.na(ACS_test$k_pca_16_3)] = "Sparsely Populated"
# ACS_test$k_pca_17_3[is.na(ACS_test$k_pca_17_3)] = "Sparsely Populated"

##### Save Output ####
ACS_test=subset(ACS_test, select = -c(admin_type,St_Code))

# Add back in raw data
ACS_unscale=openxlsx::read.xlsx("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/ACS_imp_unscaled.xlsx")
ACS_test=merge(ACS_test, ACS_unscale , by = c("FIPS","OZ"), all = T , sort = F)
names(ACS_test);names(ACS_unscale)

# Save tables
openxlsx::write.xlsx(ACS_test, "~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/ACS_test_kmeans.xlsx")
# openxlsx::write.xlsx(ACS_test, "~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/Cluster_Final.xlsx")
  
# names(ACS_test)
# rm(list=ls())
gc()

