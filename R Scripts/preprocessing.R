train_x <- read.csv("Data/train_x.csv", na.strings=c("NA",""),stringsAsFactors = TRUE)

train_y <- read_csv("Data/train_y.csv")



###### SOME VARIABLES I FOUND INSIGNIFICANT GETS OUT -- 14/09/2020 #######

train_xx <- train_x %>% select(-c("id","wpt_name","num_private","date_recorded","recorded_by","scheme_name",))

#plasteR::na.outline(train_xx) ### checked current missing values

#Freq(as.factor(train_xx$construction_year)) ### checked faulty frequencies

wholeset <- cbind(
  train_xx,train_y %>% select(-"id")
) %>% data.frame()

#### Filling zero dates with NA

wholeset$construction_year <- ifelse(wholeset$construction_year==0,NA,wholeset$construction_year)


year_train <- wholeset %>%
  filter(!is.na(construction_year))

year_train<-na.omit(year_train)


year_target <- wholeset %>%
  filter(is.na(construction_year)) %>%
  select(-"construction_year") %>% na.omit()



##### TO DO : AFTER FILLING NA CONSTRUCTION YEARS (year_target), MERGE year_target and year_train
#### fitting a year predict model is deploying in year_predict.R using year_train set


##### LGB_FİNAL MODEL DEPLOYED


predicted_years <- lgb_final_model$predict(as.matrix(year_target))

#########################################

# MERGE MERGE MERGE MERGE MERGE MERGE

#########################################


repairedpart <- data.frame(year_target,construction_year=round(predicted_years))

yearkeeper <- year_train$construction_year

unchangedpart <- data.frame(year_train %>% select(-"construction_year"),construction_year=yearkeeper)



mergedperfect <- rbind(repairedpart,unchangedpart)

summary(mergedperfect$amount_tsh)


write.csv(mergedperfect,file="1st_merge.csv")


