library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(zoo)
library(mice)
library(scales)
library(lattice)
library(GWmodel)
library(mvoutlier)
library(robustbase)
library(sp)
library(tmap)
library(raster)
library(RColorBrewer)
library(usmap)
library(geosphere)
library(parallelDist)
options(scipen = 999)

ACS_dat=openxlsx::read.xlsx("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/ACS_dat_clean_alt.xlsx")
tract_coord=readxl::read_excel("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/cluster_inputs/cluster_coords.xlsx")

tract_coord$lat=as.numeric(tract_coord$Intptlat);tract_coord$Intptlat=NULL
tract_coord$long=as.numeric(tract_coord$Intptlon);tract_coord$Intptlon=NULL
ACS_dat=merge(tract_coord,ACS_dat, by.x = "Geoid", by.y ="FIPS", sort = F, all.x = T)

# ACS_dat$long <- as.numeric(levels(ACS_dat$long))[ACS_dat$long]
# ACS_dat$lat <- as.numeric(levels(ACS_dat$lat))[ACS_dat$lat]
# mdist <- distm(ACS_dat)

ACS_dat_OZs <- ACS_dat[,1:4]
Coords1 = as.matrix(cbind(as.numeric(ACS_dat$long),as.numeric(ACS_dat$lat)))
# mdist <- distm(Coords1)
# rm(tract_coord,ACS_dat,ACS_dat_OZs)
# gc()
mdist=parDist(Coords1, method = "euclidean")
gw.mdist=gw.weight(mdist, bw = 1000, k=2)
d <- gw.dist(mdist,longlat=TRUE)

ACS_dat_names <- ACS_dat[,1]
ACS_matrix <- ACS_dat[,-c(1:4)]
rownames(ACS_matrix) = ACS_dat_names

d1s <- SpatialPointsDataFrame(Coords1,as.data.frame(ACS_matrix)	)

bw.choice <- bw.gwpca(d1s,vars=colnames(d1s@data),k = 90, robust = FALSE, adaptive = TRUE)
head(mdist)
pca.gw.auto  <- gwpca(d1s,vars=colnames(d1s@data),bw=bw.choice,k=90)

# 
# gc()
# 
# pca.gw.auto  <- gwpca(d1s,vars=colnames(d1s@data),bw=1000000,k=30)
# pca.gw <- gwpca(d1s,vars=colnames(d1s@data),bw=1000000,k=10)

# load("/Users/rorr/Google Drive - Niskanen/Data for Windows/pca.gw.auto.rdata")

pca.gw.auto$CV

local.loadings <- pca.gw.auto$loadings[,,1] # note first component only - would need to explore all components..
lead.item <- colnames(local.loadings)[max.col(abs(local.loadings))]
df1p = SpatialPointsDataFrame(Coords1, data.frame(lead=lead.item))
plot_usmap()
colour <- brewer.pal(8,"Dark2")[match(d1s$lead,unique(d1s$lead))]
plot(df1p,pch=18,col=colour,add=TRUE)
legend('topleft',as.character(unique(df1p$lead)),pch=18,col=brewer.pal(8,"Dark2"))
