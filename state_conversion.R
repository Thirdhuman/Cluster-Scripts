library(readstata13)
library(tidyverse)
library(reshape2)
library(scales)


# ACS_Stats = read.dta13("/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Demographic Data/RawACS.dta")
ACS_Stats_2 = readstata13::read.dta13("/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Demographic Data/ACSwithNewClusters.dta")
normed_clusters = readstata13::read.dta13("/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Demographic Data/NormalizedDataset.dta")
raw_OZ_data = readstata13::read.dta13("/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Demographic Data/June19RawData.dta")

normed_clusters = subset(normed_clusters, is.na(population) == F)
normed_clusters = subset(normed_clusters, is.na(population) == F)


new_DF <- raw_OZ_data[is.na(raw_OZ_data$A00001_001),]
new_DF <- normed_clusters[is.na(normed_clusters$A00001_001),]

normed_clusters %>%
  summarise_all(funs(sum(is.na(.))))

names(normed_clusters)
raw_OZ_data %>%
  summarise_all(funs(sum(is.na(.))))

# names_df=readxl::read_xlsx("/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Demographic Data/ACS_Stats_Codebook.xlsx")
# names(ACS_Stats)

# ACS_Stats$GEOID=gsub("14000US","",ACS_Stats$GEOID)
# ACS_Stats$GEOID

# openxlsx::write.xlsx(ACS_Stats, "/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Demographic Data/ACS_Stats.xlsx")
# openxlsx::write.xlsx(ACS_Stats_2, "/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Demographic Data/ACS_Stats_2.xlsx")
openxlsx::write.xlsx(normed_clusters, "/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Demographic Data/normed_clusters.xlsx")

openxlsx::write.xlsx(raw_OZ_data, "/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Demographic Data/raw_OZ_data.xlsx")

a <- readstata13::read.dta13("/Users/rorr/Desktop/Welfare_Policy/Struggling Regions/Demographic Data/June19RawData.dta")
var.labels <- attr(a,"var.labels")

data.key <- data.frame(var.name=names(a),var.labels)
data.key <- data.frame(var.name=names(a),var.labels)


