df <- read.csv("~/Desktop/pump_it_up/Data/train_x.csv", na.strings = c("NA",""),stringsAsFactors=TRUE)
trgt <- read.csv("~/Desktop/pump_it_up/Data/train_y.csv", na.strings = c("NA",""),stringsAsFactors=TRUE)
trgt <- trgt$status_group
df$construction_year <- ifelse(df$construction_year==0,NA,df$construction_year)
df <- df %>% select(-c("wpt_name","num_private","date_recorded","recorded_by","scheme_name"))
df <- cbind(df,trgt) %>% data.frame()


scaler <- function(x){(x-min(x))/(max(x)-min(x))}


full <- df[complete.cases(df),]
full_num <- as.data.frame(lapply(full, as.numeric))
full_scaled <- full_num %>% select(-"id") %>% apply(2,scaler) %>% data.frame() %>% cbind(id =full_num$id)


#### Picking just NA construction years
logvector <- is.na(df$construction_year)&complete.cases(df %>% select(-"construction_year"))

construction_year_target <- df[logvector,] %>% select(-"construction_year")
construction_year_target_num <- as.data.frame(lapply(construction_year_target, as.numeric))
construction_year_target_scaled <- construction_year_target_num %>% select(-"id") %>% apply(2,scaler) %>% data.frame() %>% cbind(id=construction_year_target$id)


index <- sample(1:nrow(full),nrow(full))
train_index <- index[1:22400]

catnames <- full %>% purrr::keep(is.factor) %>% names()

dataset <- full

lgbtrain <- dataset[train_index,]
lgbtrainid <- lgbtrain %>% select("id")
lgbtrainx <- lgbtrain %>% select(-c("construction_year","id"))
lgbtest <- dataset[-train_index,]
lgbtestid <- lgbtest %>% select("id")
lgbtestx <- lgbtest %>% select(-c("construction_year","id"))




dtrain <- lgb.Dataset(as.matrix(lgbtrainx),label=lgbtrain$construction_year,categorical_feature = catnames)
dtest <- lgb.Dataset(as.matrix(lgbtestx),label=lgbtest$construction_year,categorical_feature = catnames)
valids <- list(test = dtest)

params <- list(objective = "regression", metric = "rmse")
lgbmodel <- lgb.train(
  params = params
  , data = dtrain
  , nrounds = 3000
  , valids = valids
  , min_data = 1L
  , learning_rate = 0.037
  , early_stopping_rounds = 150
) ###2000 iter 8.7 test hata

pred <- lgbmodel$predict(as.matrix(lgbtestx))
obs <- lgbtest$construction_year
hata <- data.frame(pred = pred,obs=obs)
defaultSummary(hata)



############ IMPUTING CONSTRUCTION_YEAR

targetx <- construction_year_target %>% select(-"id")
targetxid <- construction_year_target$id

yearstotarget <- lgbmodel$predict(as.matrix(targetx))

donetarget <- cbind(id = targetxid,targetx,construction_year=yearstotarget) %>% data.frame()

repairedpart <- rbind(full,donetarget)
repairedid <- repairedpart$id


df[match(repairedid,df$id),] <- repairedpart

########### Construction year imputed









############ checking / updating complete cases ###########

full <- df[complete.cases(df),]
full_num <- as.data.frame(lapply(full, as.numeric))
full_scaled <- full_num %>% select(-"id") %>% apply(2,scaler) %>% data.frame() %>% cbind(id =full_num$id)

############ checking / updating complete cases ###########



############# IMPUTING PUBLIC MEETING


#### Picking just NA public_meeting
logvector <- is.na(df$public_meeting)&complete.cases(df %>% select(-"public_meeting"))

public_meeting_target <- df[logvector,] %>% select(-"public_meeting")
public_meeting_target_num <- as.data.frame(lapply(public_meeting_target, as.numeric))
public_meeting_target_scaled <- public_meeting_target_num %>% select(-"id") %>% apply(2,scaler) %>% data.frame() %>% cbind(id=public_meeting_target$id)



index <- sample(1:nrow(full),nrow(full))
train_index <- index[1:33740]


train<-full_scaled[train_index,]
train$public_meeting <- as.factor(train$public_meeting)
train_y<-train$public_meeting
test<-full_scaled[-train_index,]
test_x<-test %>% select(-c("public_meeting","id"))
test_y<-test$public_meeting

withnoid<-train %>% select(-"id")


nnetmodel <- nnet(public_meeting~.,data=withnoid,
                  size = 15,decay=0.01)


pred <- predict(nnetmodel,withnoid,type="class") %>% as.factor()
obs <- train$public_meeting

confusionMatrix(pred,obs,positive="0")

#### %94 train


pred <- predict(nnetmodel,test_x,type="class") %>% as.factor()
obs <- test$public_meeting %>% as.factor()

confusionMatrix(pred,obs,positive="0")


############ Model tamamdır imputation başlıyor

################ IMPUTING PUBLIC MEETING


targetx <- public_meeting_target_scaled %>% select(-"id")
targetxid <- public_meeting_target$id
targetxnotscaled <- public_meeting_target %>% select(-"id")


publicmeetingtotarget <- predict(nnetmodel,targetx,type="class")
publicmeetingtotarget <- ifelse(publicmeetingtotarget=="1","True","False")
publicmeetingtotarget <- as.factor(publicmeetingtotarget)


####merging imputed years

donetarget <- cbind(id = targetxid,targetxnotscaled,public_meeting=publicmeetingtotarget) %>% data.frame()


repairedpart <- rbind(full,donetarget)
repairedid <- repairedpart$id

df[match(repairedid,df$id),] <- repairedpart

############ Public_meeting imputed ##########







############ checking / updating complete cases ###########

full <- df[complete.cases(df),]
full_num <- as.data.frame(lapply(full, as.numeric))
full_scaled <- full_num %>% select(-"id") %>% apply(2,scaler) %>% data.frame() %>% cbind(id =full_num$id)

############ checking / updating complete cases ###########







############### IMPUTING PERMIT

#### Picking just NA permit
logvector <- is.na(df$permit)&complete.cases(df %>% select(-"permit"))

permit_target <- df[logvector,] %>% select(-"permit")
permit_target_num <- as.data.frame(lapply(permit_target, as.numeric))
permit_target_scaled <- permit_target_num %>% select(-"id") %>% apply(2,scaler) %>% data.frame() %>% cbind(id=permit_target$id)
index <- sample(1:nrow(full),nrow(full))
train_index <- index[1:35000]


train<-full_scaled[train_index,]
train$permit <- as.factor(train$permit)
train_y<-train$permit
test<-full_scaled[-train_index,]
test_x<-test %>% select(-c("permit","id"))
test_y<-test$permit

withnoid<-train %>% select(-"id")



nnetmodel3<- nnet(permit~.,data=withnoid,
                  size=22,decay=0.05)


##### train hatası check

pred <- predict(nnetmodel3,withnoid,type="class") %>% as.factor()
obs <- train$permit

confusionMatrix(pred,obs,positive="0")

########## %91 train...


##### test hatası check

pred <- predict(nnetmodel3,test_x,type="class") %>% as.factor()
obs <- test$permit %>% as.factor()

confusionMatrix(pred,obs,positive="0")

############## %90 test...




############ Model tamamdır imputation başlıyor


################ IMPUTING PERMIT


targetx <- permit_target_scaled %>% select(-"id")
targetxid <- permit_target_scaled$id
targetxnotscaled <- permit_target %>% select(-"id")



permittotarget <- predict(nnetmodel3,targetx,type="class")
permittotarget <- ifelse(permittotarget=="1","True","False")
permittotarget <- as.factor(permittotarget)


donetarget <- cbind(id = targetxid,targetxnotscaled,permit=permittotarget) %>% data.frame()


repairedpart <- rbind(full,donetarget)
repairedid <- repairedpart$id

df[match(repairedid,df$id),] <- repairedpart





############ checking / updating complete cases ###########

full <- df[complete.cases(df),]
full_num <- as.data.frame(lapply(full, as.numeric))
full_scaled <- full_num %>% select(-"id") %>% apply(2,scaler) %>% data.frame() %>% cbind(id =full_num$id)

############ checking / updating complete cases ###########





write_csv(full,"PredictiveImputedNoMode50k.csv")



############ devamına mod basıyorum





median(df$construction_year,na.rm = T)
val <- unique(df$construction_year[!is.na(df$construction_year)])
mode <- val[which.max(tabulate(match(df$construction_year, val)))]
df$construction_year <- ifelse(is.na(df$construction_year),mode,df$construction_year)



val <- unique(df$scheme_management[!is.na(df$scheme_management)])
mode <- val[which.max(tabulate(match(df$scheme_management, val)))]

df$scheme_management <- ifelse(is.na(df$scheme_management),mode,df$scheme_management)


val <- unique(df$public_meeting[!is.na(df$public_meeting)])
mode <- val[which.max(tabulate(match(df$public_meeting, val)))]

df$public_meeting <- ifelse(is.na(df$public_meeting),mode,df$public_meeting)




val <- unique(df$subvillage[!is.na(df$subvillage)])
mode <- val[which.max(tabulate(match(df$subvillage, val)))]

df$subvillage <- ifelse(is.na(df$subvillage),mode,df$subvillage)




val <- unique(df$permit[!is.na(df$permit)])
mode <- val[which.max(tabulate(match(df$permit, val)))]

df$permit <- ifelse(is.na(df$permit),mode,df$permit)



val <- unique(df$funder[!is.na(df$funder)])
mode <- val[which.max(tabulate(match(df$funder, val)))]

df$funder <- ifelse(is.na(df$funder),mode,df$funder)


val <- unique(df$installer[!is.na(df$installer)])
mode <- val[which.max(tabulate(match(df$installer, val)))]

df$installer <- ifelse(is.na(df$installer),mode,df$installer)




write_csv(df,"ImputePredictiveandModeFull59k.csv")
