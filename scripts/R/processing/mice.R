library(dplyr)
library(mice)
set.seed(11+03+21)
#read in data to perform 'mice' POST SPLIT  
dat <- readRDS("/home/guest/sem_3/707/health_retirement/training_001HRS.rds")

#read in data to perform 'mice' PRE SPLIT
dat_comp <- readRDS("/home/guest/sem_3/707/health_retirement/training_001HRS_complete.rds")

'
We assume that data is Missing at Random (MAR)
Thus, we will utilize mice to impute all missing independent variables
https://cran.r-project.org/web/packages/mice/mice.pdf
'

#check for missingness before imputation
sapply(dat, function(x) sum(is.na(x)))

sapply(dat_comp, function(x) sum(is.na(x)))


all_impute <- mice(dat_comp, m = 5, method = "rf")
complete_all_dat <- complete(all_impute)

#saveRDS(complete_all_dat, file="HRSdat_presplit_mice_rf01.rds")



imputation_one <- mice(dat, m = 5, method = "rf")
complete_dat <- complete(imputation_one)


#check for missingness after imputation
sapply(complete_dat, function(x) sum(is.na(x)))
#saveRDS(complete_dat, file="HRSdat_mice_rf01.rds")


imputation_cart <- mice(dat, m = 5, method = "cart")
complete_dat_cart <- complete(imputation_cart)


#check for missingness after imputation
sapply(complete_dat_cart, function(x) sum(is.na(x)))
#saveRDS(complete_dat_cart, file="HRSdat_mice_cart01.rds")



"
The data is assumed to be Missing at Random (MAR) because the missingness is explained by groups within the observed data.  
For example, approximately 89% of the missingness in the *life_satisfaction* variable is from retired individuals.  
Those with missingness in the *life_satisfaction* were 79 years old and those without missing values were 67 years old.  
These clear groupings within missing columns is seen for all independent variables.  
This led to the assumption that data is MAR.   
Since the data is MAR, Mutiple Imputation by Chained Equations (MICE) is used to impute missing data in the independent variables. 
The Classification and Regression Tree (CART)/Random Forest method is used to impute values for missing data. 
CART is selected for its robustness and ability to impute data on continuous and categorical variables. 
Furthermore, the data shows complex colinearity, interactions, non-linear relationships, and multimodal distributions.  
CART was preferred over traditional linear and logistic regression MICE techniques to handle these issues. 

Resources: 
https://www.bmj.com/content/338/bmj.b2393
https://stats.stackexchange.com/questions/462507/how-to-decide-whether-missing-values-are-mar-mcar-or-mnar
"