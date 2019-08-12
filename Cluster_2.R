# Cluster Analyses
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(zoo)
library(mice)
library(scales)
library(lattice)
library(diceR)
library(pander)
# library(usdm)

options(scipen = 999)

# https://cran.r-project.org/web/packages/diceR/vignettes/overview.html

ACS_dat=openxlsx::read.xlsx("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/ACS_dat_clean.xlsx")

ACS_dat_OZs <- ACS_dat[,1:2]
ACS_dat_names <- ACS_dat[,1]
ACS_matrix <- ACS_dat[,-c(1,2)]
rownames(ACS_matrix) = ACS_dat_names
glimpse(ACS_matrix)

# df1.f_M <- cov(ACS_matrix)
# corrplot::corrplot(df1.f_M, method = "circle", order = "hclust", addrect = 8)

gc()
set.seed(123)
# k5 <- kmeans(ACS_matrix, centers = 5, nstart = 25, algorithm="Hartigan-Wong", iter.max = 650)
k6 <- kmeans(ACS_matrix, centers = 6, nstart = 25, algorithm="Hartigan-Wong", iter.max = 2000)
gc()
k7 <- kmeans(ACS_matrix, centers = 7, nstart = 40, algorithm="Hartigan-Wong", iter.max = 2000)
gc()
k8 <- kmeans(ACS_matrix, centers = 8, nstart = 40, algorithm="Hartigan-Wong", iter.max = 2000)
gc()
k9 <- kmeans(ACS_matrix, centers = 9, nstart = 40, algorithm="Hartigan-Wong", iter.max = 2000)
gc()
k10 <- kmeans(ACS_matrix, centers = 10, nstart = 45, algorithm="Hartigan-Wong", iter.max = 2000)
# k11 <- kmeans(ACS_matrix, centers = 11, nstart = 25, algorithm="Hartigan-Wong", iter.max = 850)
# k12 <- kmeans(ACS_matrix, centers = 12, nstart = 50, algorithm="Hartigan-Wong", iter.max = 750)
# k13 <- kmeans(ACS_matrix, centers = 13, nstart = 35, algorithm="Hartigan-Wong", iter.max = 850)

# plots to compare
# p1 <- fviz_cluster(k10, geom = "point",  data = ACS_dat) + ggtitle("k = 10")
# p2 <- fviz_cluster(k11, geom = "point",  data = ACS_dat) + ggtitle("k = 11")
# p3 <- fviz_cluster(k12, geom = "point",  data = ACS_dat) + ggtitle("k = 12")
# p4 <- fviz_cluster(k13, geom = "point",  data = ACS_dat) + ggtitle("k = 13")

# library(gridExtra)
# grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3)

# ACS_test=cbind(ACS_dat_OZs,cluster_5=k5$cluster)
ACS_test=cbind(ACS_dat_OZs,cluster_6=k6$cluster)
ACS_test=cbind(ACS_test,cluster_7=k7$cluster)
ACS_test=cbind(ACS_test,cluster_8=k8$cluster)
ACS_test=cbind(ACS_test,cluster_9=k9$cluster)
ACS_test=cbind(ACS_test,cluster_10=k10$cluster)
# ACS_test=cbind(ACS_test,cluster_11=k11$cluster)
# ACS_test=cbind(ACS_test,cluster_12=k12$cluster)
# ACS_test=cbind(ACS_test,cluster_13=k13$cluster)

OZ_dat_lang = readstata13::read.dta13("~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/cluster_inputs/languagesupplement.dta")
ACS_test=merge(ACS_test, OZ_dat_lang[,c(1,5)], by = c("FIPS", "OZ"), all.y = T)
rm(OZ_dat_lang)

# ACS_test$cluster_5[is.na(ACS_test$cluster_5)] = "Sparsely Populated"
ACS_test$cluster_6[is.na(ACS_test$cluster_6)] = "Sparsely Populated"
ACS_test$cluster_7[is.na(ACS_test$cluster_7)] = "Sparsely Populated"
ACS_test$cluster_8[is.na(ACS_test$cluster_8)] = "Sparsely Populated"
ACS_test$cluster_9[is.na(ACS_test$cluster_9)] = "Sparsely Populated"
ACS_test$cluster_10[is.na(ACS_test$cluster_10)] = "Sparsely Populated"
# ACS_test$cluster_11[is.na(ACS_test$cluster_11)] = "Sparsely Populated"
# ACS_test$cluster_12[is.na(ACS_test$cluster_12)] = "Sparsely Populated"
# ACS_test$cluster_13[is.na(ACS_test$cluster_13)] = "Sparsely Populated"
ACS_exam=merge(ACS_test, ACS_dat, by = c("FIPS", "OZ"), all.y = T)

examine_clust_6=ACS_exam %>%
  group_by(cluster_6) %>%
       select_if(is.numeric) %>%
	summarise_all(funs(mean))

examine_clust_7=ACS_exam %>%
  group_by(cluster_7) %>%
       select_if(is.numeric) %>%
	summarise_all(funs(mean))

examine_clust_8=ACS_exam %>%
  group_by(cluster_8) %>%
       select_if(is.numeric) %>%
	summarise_all(funs(mean))

examine_clust_9=ACS_exam %>%
  group_by(cluster_9) %>%
       select_if(is.numeric) %>%
	summarise_all(funs(mean))

examine_clust_10=ACS_exam %>%
  group_by(cluster_10) %>%
       select_if(is.numeric) %>%
	summarise_all(funs(mean))
rm(ACS_exam,ACS_dat)

examine_clust_10['PCT_B02001004']
examine_clust_9['PCT_B02001004']
examine_clust_8['PCT_B02001004']
examine_clust_7['PCT_B02001004']

openxlsx::write.xlsx(ACS_test, "~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/ACS_test_kmeans.xlsx")
# write.csv(ACS_test, "~/Desktop/Welfare_Policy/Struggling Regions/Cluster Analyses/ACS_test_kmeans.csv")
# 
# kpam_6=pam(OZ_sub_dat, 6, metric = "manhattan", stand = FALSE)
# kpam_7=pam(OZ_sub_dat, 7, metric = "manhattan", stand = FALSE)
# kpam_8=pam(OZ_sub_dat, 8, metric = "manhattan", stand = FALSE)
# kpam_10=pam(OZ_sub_dat, 10, metric = "manhattan", stand = FALSE)
# kpam_11=pam(OZ_sub_dat, 11, metric = "manhattan", stand = FALSE)
# kpam_12=pam(OZ_sub_dat, 12, metric = "manhattan", stand = FALSE)
# kpam_13=pam(OZ_sub_dat, 13, metric = "manhattan", stand = FALSE) 
# 
# # function to compute total within-cluster sum of square 
# wss <- function(k) {
#   kmeans(ACS_dat, k, nstart = 25 , iter.max = 250)$tot.withinss
# }
# 
# # Compute and plot wss for k = 1 to k = 15
# k.values <- 2:25
# 
# # extract wss for 2-15 clusters
# wss_values <- map_dbl(k.values, wss)
# 
# k_means_plot=plot(k.values, wss_values,
#        type="b", pch = 19, frame = FALSE, 
#        xlab="Number of clusters K",
#        ylab="Total within-clusters sum of squares")
# 
# 
# # OZ_df=subset(ACS_dat, OZ == "1")
# 
# # https://walkerke.github.io/2017/05/tidycensus-every-tract/
# 
# df %>%
#   as_tibble() %>%
#   mutate(cluster = k2$cluster,
#          state = row.names(USArrests)) %>%
#   ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
#   geom_text()
