#load libraries
library(arsenal)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(haven)
library(patchwork)
options(scipen = 999)

"
Data Download Link:
  https://hrsdata.isr.umich.edu/data-products/2018-rand-hrs-fat-file

Data Codebook:
  https://hrs.isr.umich.edu/sites/default/files/meta/2018/core/codebook/h18_00.html

The raw SAS file was read into R and saved as an RDS file. 
Read in the file below
"

dat <- readRDS(file="h18e1a_SAS/h18e1a.rds")


#Data processing
mydat <- dat %>% 
  select(QX060_R, QX065_R, QA019, QA099,
         QA100, QA101, QA113, QB063,
         QB000,QC001,QC005,QC010, QC018, QC257, QC053,
         QC271,QC272, QC080,QC117,QC129, QC229,
         QC150, QD101,QD114,QD115, QJ005M1, QJ612,
         QJK014, QJ067,QJ547, QJ549, QG059, 
         QH004, QJ179, QJ3568, QJ3570, QJ3478,
         QN001,QP097,QT011, QT012, 
         QJ3578, QQ317, QQ331,QQ345,
         QQ357, QQ371,QQ376, QJ2W009_1,
         QJ2W009_2, QJ2W009_3, QJ2W009_4,QJ2W009_5,
         QJ2W009_6,QJ2W009_7,QJ2W009_8,
         QJ2W009_9,QJ2W009_10, 
         #friends 
         QG198, QG097,
         #housing - mortage
         QH020,QH016,QH166, QH032, 
         #debts
         QQ478, QQ519)%>% 
  filter(QJ3578 %in% c(1,3,5)) %>% 
  mutate(retirement = ifelse(QJ3578 %in% c(1,3), "Retired", "Not Retired"), 
         sex = ifelse(QX060_R ==1, "Male","Female"), 
         partner_status = ifelse(QX065_R ==1, "Married",
                                 ifelse(QX065_R ==3, "Partnered",
                                        ifelse(QX065_R ==6, "Other",NA))), 
         age = QA019,
         resident_children = QA099,
         nonresident_children = QA100,
         children_nonspouse = QA101,
         children = resident_children + nonresident_children + children_nonspouse,
         #grandchildren = QA113, #don't think we need this - "COUNT OF CHILD CHILDLAW AND GRANDCHILD"
         marital_status =ifelse(QB063 ==1, "Married",
                                ifelse(QB063 ==2, "Annulled",
                                       ifelse(QB063 ==3, "Separated",
                                              ifelse(QB063 ==4, "Divorced",
                                                     ifelse(QB063 ==5, "Widowed",
                                                            ifelse(QB063 ==6, "Never Married",
                                                                   ifelse(QB063 ==7, "Other",
                                                                          ifelse(QB063 == 8,"DontKnow", 
                                                                                 ifelse(QB063 == 9,"Refused", NA))))))))), 
         life_satisfaction = ifelse(QB000 ==1, "Completely Satisfied",
                                    ifelse(QB000 ==2, "Very Satisfied",
                                           ifelse(QB000 ==3, "Somewhat Satisfied",
                                                  ifelse(QB000 ==4, "Not Very Satisfied",
                                                         ifelse(QB000 ==5, "Not At All Satisfied",
                                                                ifelse(QB000 == 8 | QB000 == 9, NA,QB000)))))), 
         health_rate = ifelse(QC001 ==1, "Excellent",
                              ifelse(QC001 ==2, "Very Good",
                                     ifelse(QC001 ==3, "Good",
                                            ifelse(QC001 ==4, "Fair",
                                                   ifelse(QC001 ==5, "Poor",
                                                          ifelse(QC001 == 8 | QC001 == 9, NA,QC001)))))),
         high_BP = ifelse(QC005 ==1, "Yes",
                          ifelse(QC005 == 4, "No",
                                 ifelse(QC005 == 5, "No",
                                        ifelse(QC005 == 6, "Had it Before Not Now",
                                               ifelse(QC005 == 8 | QC001 == 9, NA,QC005))))),
         diabetes = ifelse(QC010 ==1, "Yes",
                           ifelse(QC010 ==4, "No",
                                  ifelse(QC010 ==5, "No",
                                         ifelse(QC010 ==6, "Had it Before Not Now",
                                                ifelse(QC010 == 8 | QC010 == 9, NA,QC010))))), 
         cancer = ifelse(QC018 == 1, "Yes",
                         ifelse(QC018 ==4, "No",
                                ifelse(QC018 ==5, "No",
                                       ifelse(QC018 == 8 | QC018 == 9, NA, QC018)))),
         heart_attack = ifelse(QC257 == 1, "Yes",
                               ifelse(QC257 ==4, "No",
                                      ifelse(QC257 ==5, "No",
                                             ifelse(QC257 == 8 | QC257 == 9, NA, QC257)))),
         
         depression = ifelse(QC271 == 1, "Yes",
                             ifelse(QC271 ==6, "Had it Before Not Now",
                                 ifelse(QC271 ==4, "No",
                                        ifelse(QC271 ==5, "No",
                                               ifelse(QC271 == 8 | QC271 == 9, NA, QC271))))),
         times_fallen = ifelse(QC080 == 99 | QC080 ==98, NA, QC080), 
         alc_days = ifelse(QC129 == 9 | QC129 ==8, NA, QC129),
         days_in_bed = ifelse(QC229 == 98 | QC229 ==99, NA, QC229), 
         memory = ifelse(QD101 ==1, "Excellent",
                         ifelse(QD101 ==2, "Very Good",
                                ifelse(QD101 ==3, "Good",
                                       ifelse(QD101 ==4, "Fair",
                                              ifelse(QD101 ==5, "Poor",
                                                     ifelse(QD101 == 8 | QD101 == 9, NA,QD101)))))), 
         lonely_pw = ifelse(QD114 == 1, "Yes",
                            ifelse(QD114 == 5, "No",
                                   ifelse(QD114 == 8 | QD114 == 9, NA, QD114))),
         enjoy_life_pw = ifelse(QD115 == 1, "Yes",
                                ifelse(QD115 == 5, "No",
                                       ifelse(QD115 == 8 | QD115 == 9, NA, QD115))),
         
         help_friends_py = ifelse(QG198 == 1, "Yes",
                                  ifelse(QG198 ==4, "No",
                                         ifelse(QG198 == 8 | QG198 == 9, NA, QG198))),
         have_friends = ifelse(QG097 == 1, "Yes",
                               ifelse(QG097 ==5, "No",
                                      ifelse(QG097 == 8 | QG097 == 9, NA, QG097))),
         
         job_status =ifelse(QJ005M1 ==1, "Working Now",
                            ifelse(QJ005M1 ==2, "Unemployed",
                                   ifelse(QJ005M1 ==3, "Laid Off",
                                          ifelse(QJ005M1 ==4, "Disabled",
                                                 ifelse(QJ005M1 ==5, "Retired",
                                                        ifelse(QJ005M1 ==6, "Homemaker",
                                                               ifelse(QJ005M1 ==7, "Other",
                                                                      ifelse(QJ005M1 == 8,"On Leave", 
                                                                             ifelse(QJ005M1 == 98 | QJ005M1 == 99, NA, NA))))))))),
         weeks_worked_py = ifelse(QJ612 == 98 | QJ612 ==99, NA, QJ612),
         amount_earn_when_left = ifelse(QJ067 ==  9999998 | QJ067 == 9999999, NA, 
                                        QJ067),
         difficulty_managing_mny = ifelse(QG059 ==1, "Yes",
                                          ifelse(QG059 == 5, "No",
                                                 ifelse(QG059 == 8 | 
                                                          QG059 == 9 | 
                                                          QG059 == 6 | QG059 == 7,  NA,QG059))),
         own_rent_home = ifelse(QH004 ==1, "Own",
                                ifelse(QH004 ==2, "Rent",
                                       ifelse(QH004 ==3, "Lives Rent Free",
                                              ifelse(QH004 ==7, "Other",
                                                     ifelse(QH004 == 8 | QH004 == 9, NA,QH004))))),
         age_plan_stop_wrk = ifelse(QJ3568 == 96, NA,
                                    ifelse(QJ3568 == 98 |QJ3568 == 99, NA,QJ3568)),
         #age to plan to stop working: if never (95), then we will go to the maximum (95)
         social_security = ifelse(QJ3478 == 1, "Yes",
                                  ifelse(QJ3478 == 5, "No",
                                         ifelse(QJ3478 == 8 | QJ3478 == 9, NA, QJ3478))),
         medicare = ifelse(QN001 == 1, "Yes",
                           ifelse(QN001 == 5, "No",
                                  ifelse(QN001 == 8 | QN001 == 9, NA, QN001))),
         follow_stockmarket = ifelse(QP097 == 1, "Very Closely",
                                     ifelse(QP097 == 2, "Somewhat Closely",
                                            ifelse(QP097 == 3, "Not At All",
                                                   ifelse(QP097 == 8 | QP097 == 9, NA, QP097)))),
         life_insurance = ifelse(QT011 == 1, "Yes",
                                 ifelse(QT011 == 5, "No",
                                        ifelse(QT011 == 8 | QT011 == 9, NA, QT011))),
         num_lifeinsur_policies =  ifelse(QT012 ==1, "1",
                                          ifelse(QT012 ==2, "2",
                                                 ifelse(QT012 ==3, "3",
                                                        ifelse(QT012 ==4, "4",
                                                               ifelse(QT012 ==5, "5 or more",       
                                                                      ifelse(QT012 == 8 | QT012 == 9, NA,QT012)))))),
         stocks_mf = ifelse(QQ317 ==  9999998 | QQ317 == 9999999, NA, QQ317),
         bonds = ifelse(QQ331 ==  99999998 | QQ331 == 99999999, NA, QQ331),
         savings = ifelse(QQ345 ==  99999998 | QQ345 == 99999999, NA, QQ345),
         cds = ifelse(QQ357 ==  99999998 | QQ357 == 99999999, NA, QQ357),
         vehicles = ifelse(QQ371 ==  99999998 | QQ371 == 99999999, NA, QQ371),
         other_savings = ifelse(QQ376 ==  99999998 | QQ376 == 99999999, NA, QQ376), 
         pension_1= ifelse(QJ2W009_1 ==  99999998 | QJ2W009_1 == 99999999, NA, QJ2W009_1),
         pension_2= ifelse(QJ2W009_2 ==  99999998 | QJ2W009_2 == 99999999, NA, QJ2W009_2),
         pension_3= ifelse(QJ2W009_3 ==  99999998 | QJ2W009_3 == 99999999, NA, QJ2W009_3),
         pension_4= ifelse(QJ2W009_4 ==  99999998 | QJ2W009_4 == 99999999, NA, QJ2W009_4),
         pension_5= ifelse(QJ2W009_5 ==  99999998 | QJ2W009_5 == 99999999, NA, QJ2W009_5),
         pension_6= ifelse(QJ2W009_6 ==  99999998 | QJ2W009_6 == 99999999, NA, QJ2W009_6),
         pension_7= ifelse(QJ2W009_7 ==  99999998 | QJ2W009_7 == 99999999, NA, QJ2W009_7),
         pension_8= ifelse(QJ2W009_8 ==  99999998 | QJ2W009_8 == 99999999, NA, QJ2W009_8),
         pension_9= ifelse(QJ2W009_9 ==  99999998 | QJ2W009_9 == 99999999, NA, QJ2W009_9),
         pension_10= ifelse(QJ2W009_10 ==  99999998 | QJ2W009_10 == 99999999, NA, QJ2W009_10),
         pension = sum(pension_1, pension_2,pension_3,pension_4,pension_5,
                       pension_6,pension_7,pension_8,pension_9,pension_10, na.rm = TRUE),
         home_value = ifelse(QH020 ==  9999999998 | QH020 == 9999999999, NA, QH020),
         mobile_home_value = ifelse(QH016 ==  99999998 | QH016 == 99999999, NA, QH016),
         second_home_value = ifelse(QH166 ==  9999999998 | QH166 == 9999999999, NA, QH166),
         mortgage = ifelse(QH032 ==  99999998 | QH032 == 99999999, NA, QH032), 
         debts = ifelse(QQ478 == 9999998 | QQ478 == 9999999, NA, QQ478),
         credit_card_debt = ifelse(QQ519 == 9999998 | QQ519 == 9999999, NA, QQ519)) %>% 
  
  select(retirement, sex, age, children, marital_status, 
         life_satisfaction, health_rate, high_BP, diabetes, 
         cancer, heart_attack, depression, times_fallen, alc_days, days_in_bed, 
         memory, lonely_pw, enjoy_life_pw, job_status, weeks_worked_py,
         help_friends_py,have_friends,
         amount_earn_when_left, difficulty_managing_mny, 
         own_rent_home, age_plan_stop_wrk, social_security, medicare, follow_stockmarket, life_insurance, 
         num_lifeinsur_policies, 
         stocks_mf, bonds, savings, cds, vehicles, other_savings, pension, home_value, mobile_home_value, 
         second_home_value, mortgage, debts, credit_card_debt) %>%
  rowwise() %>% 
  mutate(assets = sum(stocks_mf, bonds, savings, cds, vehicles, 
                      other_savings, pension, 
                      na.rm = TRUE),
         housing_assets = sum(home_value, second_home_value,mobile_home_value, -mortgage, 
                              na.rm = TRUE),
         net_assets = assets + housing_assets)
