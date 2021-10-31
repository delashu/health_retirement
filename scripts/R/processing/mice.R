library(dplyr)
library(mice)
#read in data to perform 'mice' on  

dat <- readRDS("processed_001HRS.rds")


'
We assume that data is Missing at Random (MAR)

Thus, we will utilize mice to impute all missing independent variables
'
#check for missingness before imputation
sapply(dat, function(x) sum(is.na(x)))


#set methods of imputation for specific variables: 


# meth[c("life_satisfaction")]="norm" 
# meth[c("health_rate")]="logreg" 
# meth[c("high_BP")]="polyreg"
# meth[c("diabetes")]="polyreg"
# meth[c("cancer")]="polyreg"
# meth[c("depression")]="polyreg"
# meth[c("days_in_bed")]="polyreg"
# meth[c("memory")]="polyreg"
# meth[c("lonely_pw")]="polyreg"
# meth[c("enjoy_life_pw")]="polyreg"
# meth[c("job_status")]="polyreg"
# meth[c("help_friends_py")]="polyreg"
# meth[c("have_friends")]="polyreg"
# meth[c("difficulty_managing_mny")]="polyreg"
# meth[c("own_rent_home")]="polyreg"
# meth[c("social_security")]="polyreg"
# meth[c("medicare")]="polyreg"
# meth[c("follow_stockmarket")]="polyreg"
# meth[c("life_insurance")]="polyreg"


#https://cran.r-project.org/web/packages/mice/mice.pdf
#https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/
imputation_one <- mice(dat, m = 5, method = "rf")
complete_dat <- complete(imputation_one)


#check for missingness after imputation
sapply(complete_dat, function(x) sum(is.na(x)))

