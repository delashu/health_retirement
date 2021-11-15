library(tidyverse)
library(caret)
library(glmnet)
library(rpart)
library(randomForest)
library(ipred)   
library(e1071)
options(scipen = 999)

set.seed(11+8+2021)

#read in training dataset
train <- readRDS("/home/guest/sem_3/707/health_retirement/HRSdat_mice_rf01.rds")
#create k folds where k=10
idx <- createFolds(train$retirement, k = 10)
#create skeleton dataframe to bind to:
baseframe <- data.frame(
  metric = c("Accuracy","Kappa","AccuracyLower","AccuracyUpper","AccuracyNull",
             "AccuracyPValue","McnemarPValue","Sensitivity","Specificity","Pos Pred Value",
             "Neg Pred Value","Precision","Recall","F1","Prevalence","Detection Rate",
             "Detection Prevalence","Balanced Accuracy"   )
)
logistic_vec <- baseframe
ridge_vec <- baseframe
lasso_LSE_vec <- baseframe
lasso_min_vec <- baseframe
rpart_vec <- baseframe
tree_vec <- baseframe
LDA_vec <- baseframe
bagg_vec <- baseframe
SVM_vec_poly <- baseframe
SVM_vec_radial <- baseframe
SVM_vec_sigmoid <- baseframe



for (i in 1:length(idx)) {
  ################ set up k-fold training and test sets
  #set training data
  train_dat <- train[as.numeric(unlist(idx[setdiff(c(1:length(idx)), i)])), ]
  #set training data
  test_dat <- train[idx[[i]],]
  
  
  ################ Models
  #logistic
  logistic_fit <- glm(retirement ~ ., 
                      data = train_dat,
                      family = "binomial")
  
  #set up for LASSO
  X <-  model.matrix(retirement ~  ., 
                     data = train_dat)
  #test matrix
  x_test_mat <- model.matrix(retirement ~  ., 
                             data = test_dat)
  Y <- train_dat$retirement
  #fit base LASSO model
  cv.model<- cv.glmnet(x=X,y=Y, 
                       family = "binomial", 
                       alpha=1)  
  l.min <- cv.model$lambda.min
  l.lse <- cv.model$lambda.1se
  
  #LASSO LSE Model
  lasso_LSE_model <- glmnet(x=X,y=Y, 
                            family = "binomial", 
                            alpha=1, 
                            lambda = l.lse)
  
  
  
  #LASSO L min Model
  lasso_min_model <- glmnet(x=X,y=Y, 
                            family = "binomial", 
                            alpha=1, 
                            lambda = l.min)
  #Ridge Model
  ridge_model <- cv.glmnet(X, Y, alpha = 0,
                           family = "binomial")
  min_lambda_ridge <- ridge_model$lambda.min
  #rpart fit :
  rpart_fit  <- rpart(retirement ~ ., data = train_dat,method = "class")
  #regression tree fit: 
  tree_fit <- randomForest(retirement ~ ., data = train_dat,method = "class")
  #bagging method fit
  bagg_fit <-bagging(retirement ~ ., data = train_dat, coob = TRUE)
  #LDA fit 
  lda_fit <- MASS::lda(retirement ~ ., data = train_dat)
  #SVM fit 
  SVM_fit_poly <- svm(retirement~., data = train_dat, kernel = "polynomial", type = 'C-classification', scale = FALSE)
  SVM_fit_radial <- svm(retirement~., data = train_dat, kernel = "radial", type = 'C-classification', scale = FALSE)
  SVM_fit_sigmoid <- svm(retirement~., data = train_dat, kernel = "sigmoid", type = 'C-classification', scale = FALSE)
  
  
  
  ################ Predictions
  log_predict <- predict(logistic_fit, 
                         test_dat %>% dplyr::select(-retirement),
                         type="response")
  rpart_pred <- predict(rpart_fit, test_dat %>% dplyr::select(-retirement), type = "class")
  tree_pred <- predict(tree_fit, test_dat %>% dplyr::select(-retirement), type = "class")
  logistic_predictions <- ifelse(log_predict>0.5, "Retired", "Not Retired")
  ridge_pred <- predict(ridge_model, s = min_lambda_ridge, newx = x_test_mat,
                        type="class")
  lasso_LSE_predict <- predict(lasso_LSE_model,x_test_mat,type="class")
  lasso_min_predict <- predict(lasso_min_model,x_test_mat,type="class")
  bagg_pred <- predict(bagg_fit, test_dat, type = "class")
  lda_pred<- predict(lda_fit, test_dat, type = "class")[["class"]]
  SVM_predict_poly <- predict(SVM_fit_poly, test_dat %>% dplyr::select(-retirement))
  SVM_predict_radial <- predict(SVM_fit_radial, test_dat %>% dplyr::select(-retirement))
  SVM_predict_sigmoid <- predict(SVM_fit_sigmoid, test_dat %>% dplyr::select(-retirement))
  
  
  ################ Evaluation Metrics
  #create confusion matrices
  log_mat <-confusionMatrix(factor(logistic_predictions), factor(test_dat$retirement))
  ridge_mat  <-confusionMatrix(factor(ridge_pred), factor(test_dat$retirement))
  lasso_LSE_mat <-confusionMatrix(factor(lasso_LSE_predict), factor(test_dat$retirement))
  lasso_min_mat  <-confusionMatrix(factor(lasso_min_predict), factor(test_dat$retirement))
  rpart_mat <-confusionMatrix(factor(rpart_pred), factor(test_dat$retirement))
  tree_mat <-confusionMatrix(factor(tree_pred), factor(test_dat$retirement))
  bagg_mat <- confusionMatrix(factor(bagg_pred), factor(test_dat$retirement))
  lda_mat <- confusionMatrix(factor(lda_pred), factor(test_dat$retirement))
  SVM_mat_poly <-confusionMatrix(factor(SVM_predict_poly), factor(test_dat$retirement))
  SVM_mat_radial <-confusionMatrix(factor(SVM_predict_radial), factor(test_dat$retirement))
  SVM_mat_sigmoid <-confusionMatrix(factor(SVM_predict_sigmoid), factor(test_dat$retirement))
  
  ################ Vectorization
  logistic_metrics <- data.frame(c(log_mat$overall, log_mat$byClass))
  names(logistic_metrics) <- paste0("fold_",i)
  ridge_metrics <- data.frame(c(ridge_mat$overall, ridge_mat$byClass))
  names(ridge_metrics) <- paste0("fold_",i)
  lasso_LSE_metrics <- data.frame(c(lasso_LSE_mat$overall, lasso_LSE_mat$byClass))
  names(lasso_LSE_metrics) <- paste0("fold_",i)
  lasso_min_metrics <- data.frame(c(lasso_min_mat$overall, lasso_min_mat$byClass))
  names(lasso_min_metrics) <- paste0("fold_",i)
  rpart_metrics <- data.frame(c(rpart_mat$overall, rpart_mat$byClass))
  names(rpart_metrics) <- paste0("fold_",i)
  tree_metrics <- data.frame(c(tree_mat$overall, tree_mat$byClass))
  names(tree_metrics) <- paste0("fold_",i)
  bagg_metrics <- data.frame(c(bagg_mat$overall, bagg_mat$byClass))
  names(bagg_metrics) <- paste0("fold_",i)
  lda_metrics <- data.frame(c(lda_mat$overall, lda_mat$byClass))
  names(lda_metrics) <- paste0("fold_",i)
  SVM_metrics_poly <- data.frame(c(SVM_mat_poly$overall, SVM_mat_poly$byClass))
  names(SVM_metrics_poly) <- paste0("fold_",i)
  SVM_metrics_radial <- data.frame(c(SVM_mat_radial$overall, SVM_mat_radial$byClass))
  names(SVM_metrics_radial) <- paste0("fold_",i)
  SVM_metrics_sigmoid <- data.frame(c(SVM_mat_sigmoid$overall, SVM_mat_sigmoid$byClass))
  names(SVM_metrics_sigmoid) <- paste0("fold_",i)
  
  #append
  logistic_vec <- cbind(logistic_vec, logistic_metrics)
  ridge_vec <- cbind(ridge_vec, ridge_metrics)
  lasso_LSE_vec <- cbind(lasso_LSE_vec, lasso_LSE_metrics)
  lasso_min_vec <- cbind(lasso_min_vec, lasso_min_metrics)
  rpart_vec <- cbind(rpart_vec, rpart_metrics)
  tree_vec <- cbind(tree_vec, tree_metrics)
  bagg_vec <- cbind(bagg_vec, bagg_metrics)
  LDA_vec <- cbind(LDA_vec, lda_metrics)
  SVM_vec_poly <- cbind(SVM_vec_poly, SVM_metrics_poly)
  SVM_vec_radial <- cbind(SVM_vec_radial, SVM_metrics_radial)
  SVM_vec_sigmoid <- cbind(SVM_vec_sigmoid, SVM_metrics_sigmoid)
}




logistic_vec$model <- "logistic"
ridge_vec$model <- "ridge"
lasso_LSE_vec$model <- "lasso_lse"
lasso_min_vec$model <- "lasso_min"
rpart_vec$model <- "rpart"
tree_vec$model <- "reg_tree"
bagg_vec$model <- "bagging"
LDA_vec$model <- "LDA"
SVM_vec_poly$model <- "SVM polynomial"
SVM_vec_radial$model <- "SVM radial"
SVM_vec_sigmoid$model <- "SVM sigmoid"

all_models <- rbind(logistic_vec, ridge_vec, lasso_LSE_vec,
                    lasso_min_vec,rpart_vec,tree_vec, bagg_vec, 
                    LDA_vec,
                    SVM_vec_poly, SVM_vec_radial, SVM_vec_sigmoid)
rownames(all_models) <- NULL

#row wise grouped operations on fold columns:
all_models$mean_metric = rowMeans(all_models[,grep('^fold_', colnames(all_models))])


saveRDS(all_models,"/home/guest/sem_3/707/health_retirement/models_all_01.rds")


#reshape the data: 
summarydat <- reshape(all_models[,c("metric","model","mean_metric")], 
                      timevar = c("metric"),
                      idvar = c("model") ,
                      direction = "wide")
colnames(summarydat) <- sub("mean_metric.", "", colnames(summarydat))


saveRDS(summarydat,"/home/guest/sem_3/707/health_retirement/model_summary_01.rds")