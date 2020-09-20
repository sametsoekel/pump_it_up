df <- read.csv("~/Desktop/pump_it_up/Data/test_x.csv", na.strings=c("NA",""), stringsAsFactors=TRUE)

df$construction_year <- ifelse(df$construction_year==0,NA,df$construction_year)
df <- df %>% select(-c("wpt_name","num_private","date_recorded","recorded_by","scheme_name",))


full <- df[complete.cases(df),]
full_noid <- full %>% select(-"id")


#### Picking just NA construction years
logvector <- is.na(df$construction_year)&complete.cases(df %>% select(-"construction_year"))

catnames <- full %>% purrr::keep(is.factor) %>% names()

construction_year_target <- df[logvector,] %>% select(-"construction_year")
construction_year_target_noid <- construction_year_target %>% select(-"id")

index <- sample(1:nrow(full),nrow(full))
train_index <- index[1:5661]

train<-full_noid[train_index,]
train_x<-train %>% select(-c("construction_year"))
train_y<-train$construction_year
test<-full_noid[-train_index,]
test_x<-test %>% select(-c("construction_year"))
test_y<-test$construction_year


learnpool <- catboost.load_pool(train_x,label = train_y,cat_features = catnames)
testpool <- catboost.load_pool(test_x,label = test_y,cat_features = catnames)


catparam <- list(use_best_model=T,loss_function='RMSE',eval_metric='RMSE',
                 depth=12,learning_rate=0.06,iterations=200)


catmodel1 <- catboost.train(learnpool,testpool,catparam)


imputepred <- catboost.predict(catmodel1,catboost.load_pool(construction_year_target_noid))


construction_year_target$construction_year <- imputepred

repairedpart <- rbind(full,construction_year_target)
repairedid <- repairedpart$id

df[match(repairedid,df$id),] <- repairedpart




############## UPDATING FULL ############

full <- df[complete.cases(df),]
full_noid <- full %>% select(-"id")

############## UPDATING FULL ############


############# Imputing public meeting

#### Picking just NA public_meeting
logvector <- is.na(df$public_meeting)&complete.cases(df %>% select(-"public_meeting"))

public_meeting_target <- df[logvector,] %>% select(-"public_meeting")
public_meeting_target_noid <- public_meeting_target %>% select(-"id")

index <- sample(1:nrow(full),nrow(full))
train_index <- index[1:9097]



train<-full_noid[train_index,]
train_x<-train %>% select(-c("public_meeting"))
train_y<-as.numeric(train$public_meeting)-1
test<-full_noid[-train_index,]
test_x<-test %>% select(-c("public_meeting"))
test_y<-as.numeric(test$public_meeting)-1


learnpool <- catboost.load_pool(train_x,label = train_y,cat_features = catnames)
testpool <- catboost.load_pool(test_x,label = test_y,cat_features = catnames)

catparam <- list(use_best_model=T,loss_function='Logloss',eval_metric='Accuracy',
                 depth=12,learning_rate=0.37,iterations=90,prediction_type='Class')

catmodel2 <- catboost.train(learnpool,testpool,catparam)


imputepred2 <- catboost.predict(catmodel2,catboost.load_pool(public_meeting_target_noid),prediction_type = 'Class')

imputepred2 <- ifelse(imputepred2==0,"False","True")
imputepred2 <- as.factor(imputepred2)


public_meeting_target$public_meeting <- imputepred2

repairedpart <- rbind(full,public_meeting_target)
repairedid <- repairedpart$id


df[match(repairedid,df$id),] <- repairedpart






############## UPDATING FULL ############

full <- df[complete.cases(df),]
full_noid <- full %>% select(-"id")

############## UPDATING FULL ############





############# Imputing permit

#### Picking just NA public_meeting
logvector <- is.na(df$permit)&complete.cases(df %>% select(-"permit"))

permit_target <- df[logvector,] %>% select(-"permit")
permit_target_noid <- permit_target %>% select(-"id")

index <- sample(1:nrow(full),nrow(full))
train_index <- index[1:9544]

train<-full_noid[train_index,]
train_x<-train %>% select(-c("permit"))
train_y<-as.numeric(train$permit)-1
test<-full_noid[-train_index,]
test_x<-test %>% select(-c("permit"))
test_y<-as.numeric(test$permit)-1


#### 0 false 1 true

catnames <- train %>% purrr::keep(is.factor) %>% names()

learnpool <- catboost.load_pool(train_x,label = train_y,cat_features = catnames)
testpool <- catboost.load_pool(test_x,label = test_y,cat_features = catnames)



catparam <- list(use_best_model=T,loss_function='Logloss',eval_metric='Accuracy',
                 depth=12,learning_rate=0.37,iterations=50,prediction_type='Class')


catmodel3 <- catboost.train(learnpool,testpool,catparam)



imputepred3 <- catboost.predict(catmodel3,catboost.load_pool(permit_target_noid),prediction_type = 'Class')

imputepred3 <- ifelse(imputepred3==0,"False","True")
imputepred3 <- as.factor(imputepred3)

permit_target$permit <- imputepred3

repairedpart <- rbind(full,permit_target)
repairedid <- repairedpart$id


df[match(repairedid,df$id),] <- repairedpart




############## UPDATING FULL ############

full <- df[complete.cases(df),]
full_noid <- full %>% select(-"id")

############## UPDATING FULL ############

full %>% View()









############# Imputing

#### Picking just NA public_meeting


median(df$construction_year,na.rm = T)
mean(df$construction_year,na.rm=T)

df$construction_year <- ifelse(is.na(df$construction_year),mean(df$construction_year,na.rm=T),df$construction_year)


df$permit <- as.character(df$permit)
df$permit <- ifelse(is.na(df$permit),"True",df$permit)



df$funder <- as.character(df$funder)
val <- unique(df$funder[!is.na(df$funder)])
mode <- val[which.max(tabulate(match(df$funder, val)))]

df$funder <- ifelse(is.na(df$funder),mode,df$funder)




df$installer <- as.character(df$installer)

val <- unique(df$installer[!is.na(df$installer)])
mode <- val[which.max(tabulate(match(df$installer, val)))]

df$installer <- ifelse(is.na(df$installer),mode,df$installer)




df$subvillage <- as.character(df$subvillage)

val <- unique(df$subvillage[!is.na(df$subvillage)])
mode <- val[which.max(tabulate(match(df$subvillage, val)))]

df$subvillage <- ifelse(is.na(df$subvillage),mode,df$subvillage)



df$public_meeting <- as.character(df$public_meeting)

val <- unique(df$public_meeting[!is.na(df$public_meeting)])
mode <- val[which.max(tabulate(match(df$public_meeting, val)))]

df$public_meeting <- ifelse(is.na(df$public_meeting),mode,df$public_meeting)





df$scheme_management <- as.character(df$scheme_management)


val <- unique(df$scheme_management[!is.na(df$scheme_management)])
mode <- val[which.max(tabulate(match(df$scheme_management, val)))]

df$scheme_management <- ifelse(is.na(df$scheme_management),mode,df$scheme_management)






write_csv(df,"ImputedCatBoostModMedian2.csv")



