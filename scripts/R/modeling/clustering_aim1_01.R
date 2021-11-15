library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
options(scipen = 999)
set.seed(1125)

df_tr <-  readRDS(file = "/home/guest/sem_3/707/health_retirement/model_matrix_training.RDS")
fulltrain <- readRDS("../../../HRSdat_presplit_mice_rf01.rds")
#make copy of original data
tdf <- fulltrain
df <- scale(df_tr)
# Dissimilarity matrix
dismat <- dist(df, method = "euclidean")

#Hclust via Complete Linkage
hc_complete <- hclust(dismat, method = "complete" )

#jpeg(file="dendro_plots/dendro_complete.jpeg")
# Plot the obtained dendrogram
plot(hc_complete, cex = 0.6, hang = -1, main = "Cluster Dendrogram\nComplete Euclidean",
     xlab = "")
#rect.hclust(hc_complete)
#dev.off()


#Hclust via Average Linkage
hc_avg <- hclust(dismat, method = "average" )
# Plot the obtained dendrogram
#jpeg(file="dendro_plots/dendro_average.jpeg")
plot(hc_avg, cex = 0.6, hang = -1, main = "Cluster Dendrogram\nAverage Euclidean",
     xlab = "")
#rect.hclust(hc_avg)
#dev.off()



#Hclust via Single Linkage
hc_single <- hclust(dismat, method = "single" )
# Plot the obtained dendrogram
#jpeg(file="dendro_plots/dendro_single.jpeg")
plot(hc_single, cex = 0.6, hang = -1, main = "Cluster Dendrogram\nSingle Euclidean",
     xlab = "")
#rect.hclust(hc_single)
#dev.off()

ward_den <- hclust(dismat, method = "ward.D2")
#plot dendrogram
#jpeg(file="dendro_plots/dendro_ward.jpeg")
plot(ward_den, cex = 0.6, hang = -1, main = "Cluster Dendrogram \nWard's",
     xlab = "")
# dev.off()

#jpeg(file="dendro_plots/dendro_ward_k_4.jpeg")
plot(ward_den, cex = 0.6, hang = -1, main = "Cluster Dendrogram \nWard's k=4",
     xlab = "")
rect.hclust(ward_den, k = 4)
#dev.off()

# Cut tree into 4 groups
df_grp <- cutree(ward_den, k = 4)
#add these groupings to the data:
tdf <- tdf %>%
  mutate(cluster = df_grp)


tdf_analysis <- tdf %>%
  group_by(cluster) %>%
  mutate(mean_age = mean(age),
         mean_children = mean(children),
         mean_netassets = mean(net_assets),
         #rowcount = n(),
         prop_male = length(which(sex == "Male")) / n() ,
         prop_retired = length(which(retirement == "Retired")) / n(),
         prop_married = length(which(marital_status == "Married")) / n(),
         prop_satisfied = length(which(life_satisfaction %in% c("Very Satisfied", "Completely Satisfied"))) / n(),
         prop_health = length(which(health_rate %in% c("Good", "Very Good", "Excellent"))) / n()) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  select(cluster, mean_age, mean_children, mean_netassets, prop_male, prop_retired,
         prop_married, prop_satisfied, prop_health) %>%
  data.frame()

# saveRDS(tdf_analysis, file = "dendrodat.RDS")