library(MASS)
library(quantreg)
library(glmnet)
library(rqPen)
options(scipen = 999)

#multiple linear regression script
dat <- readRDS(file = "/home/guest/sem_3/707/health_retirement/HRSdat_presplit_mice_rf01.rds")

#fit multiple linear regression
fit1 <- lm(net_assets ~ ., data = dat)

#check the assumptions:
plot(fit1,1, main = c("naive multiple linear regression")) 
#assumption of linearity between Xis and Y does not hold. 

#try to transform the outcome: 

#fit the model: 
fit2 <- lm(log(net_assets+1) ~ ., data = dat)
#check the assumptions:
plot(fit2,1, main = c("naive multiple linear regression")) 
#assumption of linearity between Xis and Y does not hold. 


#fit the model: 
fit2_a <- lm(sqrt(net_assets) ~ ., data = dat)
#check the assumptions:
plot(fit2_a,1, main = c("multiple linear regression with sqrt transform")) 
#assumption of linearity between Xis and Y does not hold. 


#assumption of linearity between Xis and Y does not hold after transformation 

#since transformations do not hold, we will try another regression method: 
#################### Robust Regression ####################
modmat <- model.matrix(net_assets ~., dat)
y_train <- dat$net_assets
cv_lasso <- cv.glmnet(modmat, y_train, alpha = 1)
lasso_lam <- cv_lasso$lambda.min
lassofit1 <- glmnet(modmat, 
                    y_train, 
                    alpha = 1, 
                    lambda = lasso_lam) 
coef(lassofit1)
"
lasso fit removes:
depression, days_in_bed, lonely_pw, life_insurance
Refit the multiple regression removing these variables: 
"

#fit multiple linear regression number 2
fit3 <- lm(net_assets ~ retirement + sex + age + children + marital_status + 
             life_satisfaction + health_rate + high_BP + diabetes + cancer + 
             memory + enjoy_life_pw + help_friends_py + have_friends + 
             difficulty_managing_mny + own_rent_home + social_security + 
             medicare+ follow_stockmarket, data = dat)



#check the assumptions:
plot(fit3,1, main = c("multiple linear regression\npost variable selection")) 

#assumption of linearity between Xis and Y does not hold. 
#try the log transform
fit4 <- lm(log(net_assets+1)  ~ retirement + sex + age + children + marital_status + 
             life_satisfaction + health_rate + high_BP + diabetes + cancer + 
             memory + enjoy_life_pw + help_friends_py + have_friends + 
             difficulty_managing_mny + own_rent_home + social_security + 
             medicare+ follow_stockmarket, data = dat)
plot(fit4,1, main = c("log transform multiple linear regression\npost variable selection"))

#try the squareroot transform 
#try the log transform
fit4_a <- lm(sqrt(net_assets)  ~ retirement + sex + age + children + marital_status + 
             life_satisfaction + health_rate + high_BP + diabetes + cancer + 
             memory + enjoy_life_pw + help_friends_py + have_friends + 
             difficulty_managing_mny + own_rent_home + social_security + 
             medicare+ follow_stockmarket, data = dat)
plot(fit4_a, 1,
     main = c("square-root transform multiple linear regression\npost variable selection"))



#################### Quantile Regression ####################
#derived from 'clustering_aim1_01.Rmd'
mymat <- readRDS(file = "/home/guest/sem_3/707/health_retirement/model_matrix_training.RDS")
xmat <- as.matrix(mymat[, names(mymat) != "net_assets"])
ymat <- mymat$net_assets

qlassofit1 <- rq(net_assets ~ ., data = dat,method = "lasso",tau = 0.25)
ql1 <- data.frame(qlassofit1$coefficients)
qlassofit2 <- rq(net_assets ~ ., data = dat,method = "lasso",tau = 0.5)
ql2 <- data.frame(qlassofit2$coefficients)
qlassofit3 <- rq(net_assets ~ ., data = dat,method = "lasso",tau = 0.9)
ql3 <- data.frame(qlassofit3$coefficients)
lasso_coefs <- cbind(ql1, ql2, ql3)
#After fitting three separate LASSO models on quantiles of 0.25, 0.5, 0.9, we see that 
#the following variables should be removed from the model:


"
FIT 0.25:
widowed, enjoylifepw, memoryGood, retired, depression, medicare, children, age, daysinbed, 
healthrateGood, lonely_pw, social_security, sex, havefriends, lifesatisfactionNOT


FIT 0.5:
healthrateGpd, retire, lonelypw, sex, marital separated, medicare, age, daysinbed, 
children, memorypoor, enjoylifepw, socialsecity, maritalNEVER, depression, havefreinds, 
ownrenthomeOTHER, marital Widowed


FIT 0.9:
marital separated, difficultymanaginyes, socialsecityyes, maritalNEVER, 
ownrenthomeRENT, lifeNOTVERY, memoryFAIR, highBPyes, enjoylifepw, sexmale, lifeSOMEWHAT
age, children, daysinbed, havefriends, lonelypw, healthrateGood, 
medicare, depression, memoryPOOR

"
#remove the variables from the LASSO output
reducedat1 <- dat %>% 
  dplyr::select(-c(enjoy_life_pw, retirement, depression, medicare, days_in_bed, 
           lonely_pw, social_security, have_friends)) %>% 
  data.frame()

reducedat2 <- dat %>% 
  dplyr::select(-c(retirement, lonely_pw, medicare, days_in_bed, children, 
            enjoy_life_pw, social_security, depression, have_friends)) %>% 
  data.frame()
reducedat3 <- dat %>% 
  dplyr::select(-c(difficulty_managing_mny, social_security, high_BP, enjoy_life_pw, 
            children, days_in_bed, have_friends, lonely_pw, medicare,
            depression)) %>% 
  data.frame()

quant_fit1 <- rq(net_assets ~ ., data = reducedat1, tau = 0.25)
summary(quant_fit1)

quant_fit2 <- rq(net_assets ~ ., data = reducedat2, tau = 0.5)
summary(quant_fit2)


quant_fit3 <- rq(net_assets ~ ., data = reducedat3, tau = 0.9)
summary(quant_fit3)

#https://support.sas.com/resources/papers/proceedings17/SAS0525-2017.pdf
#http://www.statisticalanalysisconsulting.com/wp-content/uploads/2011/06/SA04.pdf
#https://stats.idre.ucla.edu/stata/faq/how-do-i-interpret-quantile-regression-coefficients/










#################### Robust Regression ####################
robust_fit1 <- rlm(net_assets ~ ., data = dat)
summary(robust_fit1)
