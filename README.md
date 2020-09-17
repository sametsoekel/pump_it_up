# pump_it_up
 Using data from Taarifa and the Tanzanian Ministry of Water, Predicting which pumps are functional, which need some repairs, and which don't work at all.
 
 ## Notes day-to-day
 
 ### 14 September 2020
 
 -   Feature `construction_year` has many of zeros to indicate NA's they need some interest.
 
 -   Because the feature `scheme_name` has %47 missing variables, rid it off.
 
 -   After ridding off unnecesary features (it may change!), there is no observations with over 20% missing rate
 
 -   Most missing values are indicated by nothing, they need to be filled with NA.
 
 -   IT MEANS THERE ABSOLUTELY ARE OBSERVATIONS OVER %20 MISSING RATIO.
 
 
 ### 16 September 2020

 -   `construction_year` filled by a Light GBM Model which saved in " Predictors " with today's date.

 -   TO DO : After imputing, %20 over limit observations need to be checked again. (again and again)
 
 ### 17 September 2020

 -   First submission has just given. Even with a very basic mode imputation, **0.7127 Accuracy** 🚀🚀
 
 
