library(MASS)
library(quantreg)

#multiple linear regression script
dat <- readRDS(file = "HRSdat_mice_rf01.rds")

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
#assumption of linearity between Xis and Y does not hold after transformation 

#since transformations do not hold, we will try another regression method: 


#################### Robust Regression ####################
robust_fit1 <- rlm(net_assets ~ ., data = dat)
summary(robust_fit1)



#################### Quantile Regression ####################
quant_fit1 <- rq(net_assets ~ ., data = dat)
summary(quant_fit1)
