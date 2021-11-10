#Load complete miced data
cpt <- readRDS(file = "HRSdat_presplit_mice_rf01.rds")
#Aim1 build a logistic regression model to predict retirement and non-retirement making inference using the complete dataset
table(cpt$retirement)

fit1 <- glm(retirement ~ ., family=binomial(link='logit'), data = cpt)

summary(fit1)

#StepwiseAIC to select the variables
step.fit1 <- stepAIC(fit1, direction = 'both', trace = FALSE)

step.fit1$aic

summary(step.fit1)
###The variables selected in the final model by stepwise selection age, life satisfaction, health rate
###high blood pressure, cancer, depression, days in bed, lonely pw, enjoy life, help friends, own or rent home,
###social security, medicare, follow stock market, life insurance, net assets with AIC 10656.94.

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
###All variables were selected in ridge regression with AIC 10672.
#Use glm() to get AIC
ridge.model <- glm(retirement ~ ., family=binomial(link='logit'), data = cpt)

#Lasso regression to select variables
cv.lasso <- cv.glmnet(x=X,y=Y, 
                       family = "binomial", 
                       alpha=1)  

#Use l.min
best.lasso.lmin <- cv.lasso$lambda.min
lasso.fit1.lmin <- glmnet(x=X,y=Y,
                     alpha = 1,
                     family = "binomial",
                     lambda = best.lasso.lmin)
coef(lasso.fit1.lmin)
###ALL variables were selected by lasso l.min.
#Use glm() to get AIC 
lasso.model.lmin <- glm(retirement ~ ., family=binomial(link='logit'), data = cpt)

summary(lasso.model.lmin)

#Use l.lse
best.lasso.llse <- cv.lasso$lambda.1se

lasso.fit1.llse <- glmnet(x=X,y=Y,
                          alpha = 1,
                          family = "binomial",
                          lambda = best.lasso.llse)
coef(lasso.fit1.llse)
###Age, health rate, high blood pressure, depression, days in bed, social security, medicare, life_insurance were selected by l.lse.
#Use glm() to get AIC 
lasso.model.llse <- glm(retirement ~ age+health_rate+high_BP+depression+
                     days_in_bed+social_security+medicare+life_insurance, family=binomial(link='logit'), data=cpt)

summary(lasso.model.llse) 

###A to compare AIC for each model
result <- data.frame(cbind(model = c('stepwise', 'ridge regression', 
                                     'lasso regression l.min', 'lasso regression l.lse'),
                           AIC = c(step.fit1$aic, ridge.model$aic, 
                                   lasso.model.lmin$aic, lasso.model.llse$aic)))
result
