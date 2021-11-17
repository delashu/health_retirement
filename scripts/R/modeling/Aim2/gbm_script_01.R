
library(gbm)
#load in training data
train <- readRDS(file="/home/guest/sem_3/707/health_retirement/training_001HRS.rds")
set.seed(1125)

# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(0.001, .05, .01),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10),
  bag.fraction = c(0.2, .5, 1), 
  optimal_trees = 0,     
  min_RMSE = 0
)


# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # train model
  gbm.tune <- gbm(
    train$net_assets~.,
    data = train,
    distribution = "gaussian",
    n.trees = 6000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL,
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

saveRDS(hyper_grid, file = "/home/guest/sem_3/707/health_retirement/scripts/R/modeling/hypergrid.rds")

