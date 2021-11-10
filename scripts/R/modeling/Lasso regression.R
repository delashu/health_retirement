#Load train miced data
test <- readRDS(file = "test_001HRS.rds")
train <- readRDS(file="HRSdat_mice_rf01.rds")

#Aim 1 build a logistic regression model to predict retirement and non-retirement unbalanced.
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

###Use the complete miced data set to select variables to make inference for Aim1.
#load complete data 
cpt <- readRDS(file = "HRSdat_presplit_mice_rf01.rds")

#Aim1 build a logistic regression model to predict retirement and non-retirement making inference using the complete dataset
table(cpt$retirement)
fit1 <- glm(retirement ~ ., family=binomial(link='logit'), data = cpt)
summary(fit1)

#StepwiseAIC to select the variables
step.fit1 <- stepAIC(fit1, direction = 'both', trace = FALSE)
step.fit1$anova
summary(step.fit1)
###The variables selected in the final model by stepwise selection age, life satisfaction, heatth rate
###high blood pressure, cancer, depression, days in bed, lonely pw, enjoy life, help friends, own or rent home,
###social security, medicare, follow stock market, life insurance, net assets.

#Ridge regression to select variables
X <-  model.matrix(retirement ~  ., 
                   data=cpt)[,-1]
Y <- cpt[,"retirement"]

cv.ridge<- cv.glmnet(x=X,y=Y, 
                     family = "binomial", 
                     alpha=0)  
best.ridge <- cv.ridge$lambda.min

ridge.fit1 <- glmnet(x=X,
                     y=Y,
                     alpha=0,
                     family=binomial,
                     lambda=best.ridge)

coef(ridge.fit1)
###All variables were selected in ridge regression.

#Lasso regression to select variables
cv.lasso <- cv.glmnet(x=X,y=Y, 
                      family = "binomial", 
                      alpha=1)  
best.lasso <- cv.lasso$lambda.min
lasso.fit1 <- glmnet(x=X,y=Y,
                     alpha = 1,
                     family = binomial,
                     lambda = best.lasso)
coef(lasso.fit1)
###Variables selected by lasso regression sex, age, children, marital_status, life satisfaction,
###health rate, high BP, diabetes, cancer, depression, days in bed, memory, lonely pw, enjoy life, 
###help fridends, have friends, difficulty managing mny, own or rent home, social security, medicare.
