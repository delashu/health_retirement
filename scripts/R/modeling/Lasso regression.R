#Load train miced data
test <- readRDS(file = "test_001HRS.rds")
train <- readRDS(file="HRSdat_mice_rf01.rds")

#Aim 1 build a logistic regression model to predict retirement and non-retirement.
table(train$retirement)
model_complete <- glm(retirement ~ ., family=binomial(link='logit'), data=train)

#Lasso to select variables
X <-  model.matrix(retirement ~  ., 
                   data=train)
Y <- train[,"retirement"]
cv.model<- cv.glmnet(x=X,y=Y, 
                     family = "binomial", 
                     alpha=1)  
l.min <- cv.model$lambda.min
l.lse <- cv.model$lambda.1se
lasso.model <- glmnet(x=X,y=Y, 
                      family = "binomial", 
                      alpha=1, 
                      lambda = l.lse)
lasso.model$beta
model_lasso <- glm(retirement ~ age+health_rate+high_BP+depression+
                     days_in_bed+social_security+medicare+life_insurance, family=binomial(link='logit'), data=train)
summary(model_lasso)
