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
 
 
 ### 18 September 2020
 
 -   Target of independent variables (in /Data/test_x.csv) has been predictive imputed very sensitively. (Predictive imputation in that principle is what I try to do with plasteR)
 
 -   Just to check, trained 3 model with only complete cases (about 35k observations.) and gave 3 submissions by each.

 -   In total, pushed 4 submissions whose accuracies are **0.7127**, **0.6320**, **0.6238**, **0.6059** respectively.
 
 -   The reason of failure in the last 3 submissions is that train set has been processed cursoryly.
 
 -   TO DO : Train set will be imputed predicatively, observations with more than 20% missing rate will be deleted and three different model will be deployed for 5th, 6th and 7th submissions.
 
 
 ### 19 September 2020 
 
 -   The last three submissions (5th, 6th and 7th) has just been given (modeled by NeuralNet,LightGBM,LightGBM respectively).
 
 -   Their accuracies are **0.7504**, **0.7615** and **0.7662** respectively 🚀🚀
 
 -   TO DO : For 20 September's submission; a catboost model will be deployed to make submission. According to its succes, imputation of training and target sets may be checked again.
 
 ### 20 September 2020
 
 -   8th and 9th submissions were given, accuracy scores are **0.8015** and **0.7838** respectively 🚀🚀🚀
   
 -   Decided to use catboost in next few submissions thanks to high classification score.
 
 -   Imputation, imputation, imputation !
 
 -   TO DO : This day is not finished yet, just begun. Will impute original prediction set by catboost and give the last submission of the day.
 
  

 
 
