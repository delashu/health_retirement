library(gbm)
library(dplyr)
#load in training data
train <- readRDS(file="/home/guest/sem_3/707/health_retirement/training_001HRS.rds")
set.seed(1125)
mydat <- readRDS(file = "/home/guest/sem_3/707/health_retirement/scripts/R/modeling/hypergrid.rds") %>% 
  data.frame()
mydat <- mydat[which(mydat$optimal_trees == 2469), ]


#fit the gbm
mygbm <- gbm(train$net_assets~.,
             data = train,
             distribution = "gaussian",
             interaction.depth = mydat$interaction.depth,
             cv.folds = 5,
             shrinkage = mydat$shrinkage,
             n.trees = mydat$optimal_trees,
             n.cores = NULL,
             verbose = FALSE)
saveRDS(mygbm, "/home/guest/sem_3/707/health_retirement/scripts/R/modeling/gbm_01.rds")
