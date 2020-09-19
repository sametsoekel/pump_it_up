df <- read.csv("~/Desktop/pump_it_up/Data/test_x.csv", na.strings = c("NA",""),stringsAsFactors=TRUE)

df$construction_year <- ifelse(df$construction_year==0,NA,df$construction_year)
df <- df %>% select(-c("wpt_name","num_private","date_recorded","recorded_by","scheme_name",))

full <- df[complete.cases(df),]
full_num <- as.data.frame(lapply(full, as.numeric))
full_scaled <- full_num %>% select(-"id") %>% apply(2,scaler) %>% data.frame() %>% cbind(id =full_num$id)

scaler <- function(x){(x-min(x))/(max(x)-min(x))}

#### Picking just NA construction years
logvector <- is.na(df$construction_year)&complete.cases(df %>% select(-"construction_year"))



############ Imputing construction year

construction_year_target <- df[logvector,] %>% select(-"construction_year")
construction_year_target_num <- as.data.frame(lapply(construction_year_target, as.numeric))
construction_year_target_scaled <- construction_year_target_num %>% select(-"id") %>% apply(2,scaler) %>% data.frame() %>% cbind(id=construction_year_target$id)
index <- sample(1:nrow(full),nrow(full))
train_index <- index[1:3906]


train<-full_scaled[train_index,]
train_y<-train$construction_year
test<-full_scaled[-train_index,]
test_x<-test %>% select(-c("construction_year","id"))
test_y<-test$construction_year

withnoid<-train %>% select(-"id")



#data.frame(pred=val,obs=full_num$construction_year) %>% View()

nnetmodel <- nnet(construction_year~.,data =withnoid,
                  size=12,rang=.1,decay=3,maxit=1000)

trainpred <- nnetmodel$fitted.values* (min(full_num$construction_year) - max(full_num$construction_year)) + max(full_num$construction_year)
obs <- train_y* (min(full_num$construction_year) - max(full_num$construction_year)) + max(full_num$construction_year)

defaultSummary(data.frame(obs=obs,pred=trainpred))



nnetgrid <- expand.grid(
  size = 2:30,
  decay = c(0.001,0.01,0.1),
  maxit = c(100,200,300,400,500,1000),
  rang = c(.1,.2,.5,.7,1)
)

ysa_grid <- expand.grid(
  decay = c(0.001,0.01,0.1),
  size = 1:10
)
nnetgrid$decay

ctrl <- trainControl(method = "cv", number =10)


x <- withnoid %>% select(-c("construction_year"),"id")
nnettune <- train(
  construction_year~.,data=withnoid,
  method = "mlpWeightDecay",
  metric="RMSE",
  trControl = ctrl,
  tuneGrid = ysa_grid,
  verbose=T
)
plot(nnettune)


tunedpred <- (predict(nnettune$finalModel,x)* (min(full_num$construction_year) - max(full_num$construction_year)) + max(full_num$construction_year)) %>% round()
obstrain <- train$construction_year* (min(full_num$construction_year) - max(full_num$construction_year)) + max(full_num$construction_year)


hata <- data.frame(pred = tunedpred,obs=obs)
hata %>% View()

defaultSummary(hata)

############ random forest model
withnoid <- full_num %>% select(-"id")

rf <- randomForest(construction_year~.,data=withnoid)


########################### Lightgbm model

catnames <- full %>% purrr::keep(is.factor) %>% names()


dataset <- full

lgbtrain <- dataset[1:6061,]
lgbtrainid <- lgbtrain %>% select("id")
lgbtrainx <- lgbtrain %>% select(-c("construction_year","id"))
lgbtest <- dataset[6062:8061,]
lgbtestid <- lgbtest %>% select("id")
lgbtestx <- lgbtest %>% select(-c("construction_year","id"))




dtrain <- lgb.Dataset(as.matrix(lgbtrainx),label=lgbtrain$construction_year,categorical_feature = catnames)
dtest <- lgb.Dataset(as.matrix(lgbtestx),label=lgbtest$construction_year,categorical_feature = catnames)
valids <- list(test = dtest)

params <- list(objective = "regression", metric = "rmse")
lgbmodel <- lgb.train(
  params = params
  , data = dtrain
  , nrounds = 2000
  , valids = valids
  , min_data = 1L
  , learning_rate = 0.037
  , early_stopping_rounds = 150
)   ##### 5.6 train hatası, 9.62 test hatası. Harika <3


lgbmodel2 <- lgb.train(
  params = params
  , data = dtrain
  , nrounds = 1500
  , valids = valids
  , min_data = 5
  , learning_rate = 0.00076
  , early_stopping_rounds = 50
)

pred <- lgbmodel$predict(as.matrix(lgbtestx))
obs <- lgbtest$construction_year

hata <- data.frame(pred = round(pred),obs=obs)
defaultSummary(hata)


################ IMPUTING CONSTRUCTION_YEAR


targetx <- construction_year_target %>% select(-"id")
targetxid <- construction_year_target$id

yearstotarget <- lgbmodel$predict(as.matrix(targetx))

####merging imputed years

donetarget <- cbind(id = targetxid,targetx,construction_year=yearstotarget) %>% data.frame()



repairedpart <- rbind(full,donetarget)
repairedid <- repairedpart$id

repairedpart
df[match(repairedid,df$id),] <- repairedpart

###################### repaired part merged with main submission format


full <- df[complete.cases(df),]
full_num <- as.data.frame(lapply(full, as.numeric))
full_scaled <- full_num %>% select(-"id") %>% apply(2,scaler) %>% data.frame() %>% cbind(id =full_num$id)












############# Imputing public meeting

#### Picking just NA public_meeting
logvector <- is.na(df$public_meeting)&complete.cases(df %>% select(-"public_meeting"))

public_meeting_target <- df[logvector,] %>% select(-"public_meeting")
public_meeting_target_num <- as.data.frame(lapply(public_meeting_target, as.numeric))
public_meeting_target_scaled <- public_meeting_target_num %>% select(-"id") %>% apply(2,scaler) %>% data.frame() %>% cbind(id=public_meeting_target$id)
index <- sample(1:nrow(full),nrow(full))
train_index <- index[1:3906]


index <- sample(1:nrow(full),nrow(full))
train_index <- index[1:9097]


train<-full_scaled[train_index,]
train$public_meeting <- as.factor(train$public_meeting)
train_y<-train$public_meeting
test<-full_scaled[-train_index,]
test_x<-test %>% select(-c("public_meeting","id"))
test_y<-test$public_meeting

withnoid<-train %>% select(-"id")

nnetmodel <- nnet(public_meeting~.,data=withnoid,
                  size = 15,decay=0.01)
## size 22 devay 0.2

##### train hatası check

pred <- predict(nnetmodel,withnoid,type="class") %>% as.factor()
obs <- train$public_meeting

confusionMatrix(pred,obs,positive="0")

########## %96 train...

##### test hatası check

pred <- predict(nnetmodel,test_x,type="class") %>% as.factor()
obs <- test$public_meeting %>% as.factor()

confusionMatrix(pred,obs,positive="0")

############## %92 test...

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







full <- df[complete.cases(df),]
full_num <- as.data.frame(lapply(full, as.numeric))
full_scaled <- full_num %>% select(-"id") %>% apply(2,scaler) %>% data.frame() %>% cbind(id =full_num$id)




############### Imputing Permit

#### Picking just NA permit
logvector <- is.na(df$permit)&complete.cases(df %>% select(-"permit"))

permit_target <- df[logvector,] %>% select(-"permit")
permit_target_num <- as.data.frame(lapply(permit_target, as.numeric))
permit_target_scaled <- permit_target_num %>% select(-"id") %>% apply(2,scaler) %>% data.frame() %>% cbind(id=permit_target$id)
index <- sample(1:nrow(full),nrow(full))
train_index <- index[1:9500]


train<-full_scaled[train_index,]
train$permit <- as.factor(train$permit)
train_y<-train$permit
test<-full_scaled[-train_index,]
test_x<-test %>% select(-c("permit","id"))
test_y<-test$permit

withnoid<-train %>% select(-"id")


nnetmodel2<- nnet(permit~.,data=withnoid,
                  size=22,decay=0.05)

##### train hatası check

pred <- predict(nnetmodel2,withnoid,type="class") %>% as.factor()
obs <- train$permit

confusionMatrix(pred,obs,positive="0")

########## %93 train...

##### test hatası check

pred <- predict(nnetmodel2,test_x,type="class") %>% as.factor()
obs <- test$permit %>% as.factor()

confusionMatrix(pred,obs,positive="0")

############## %90 test...

############ Model tamamdır imputation başlıyor


################ IMPUTING PERMIT


targetx <- permit_target_scaled %>% select(-"id")
targetxid <- permit_target_scaled$id
targetxnotscaled <- permit_target %>% select(-"id")



permittotarget <- predict(nnetmodel2,targetx,type="class")
permittotarget <- ifelse(permittotarget=="1","True","False")
permittotarget <- as.factor(permittotarget)


donetarget <- cbind(id = targetxid,targetxnotscaled,permit=permittotarget) %>% data.frame()


repairedpart <- rbind(full,donetarget)
repairedid <- repairedpart$id

df[match(repairedid,df$id),] <- repairedpart




########### Devamı Imputing by Mode !!!




val <- unique(df$permit[!is.na(df$permit)])
mode <- val[which.max(tabulate(match(df$permit, val)))]

df$permit <- ifelse(is.na(df$permit),mode,df$permit)



val <- unique(df$funder[!is.na(df$funder)])
mode <- val[which.max(tabulate(match(df$funder, val)))]

df$funder <- ifelse(is.na(df$funder),mode,df$funder)


val <- unique(df$installer[!is.na(df$installer)])
mode <- val[which.max(tabulate(match(df$installer, val)))]

df$installer <- ifelse(is.na(df$installer),mode,df$installer)



val <- unique(df$subvillage[!is.na(df$subvillage)])
mode <- val[which.max(tabulate(match(df$subvillage, val)))]

df$subvillage <- ifelse(is.na(df$subvillage),mode,df$subvillage)



val <- unique(df$public_meeting[!is.na(df$public_meeting)])
mode <- val[which.max(tabulate(match(df$public_meeting, val)))]

df$public_meeting <- ifelse(is.na(df$public_meeting),mode,df$public_meeting)



val <- unique(df$scheme_management[!is.na(df$scheme_management)])
mode <- val[which.max(tabulate(match(df$scheme_management, val)))]

df$scheme_management <- ifelse(is.na(df$scheme_management),mode,df$scheme_management)





val <- unique(df$construction_year[!is.na(df$construction_year)])
mode <- val[which.max(tabulate(match(df$construction_year, val)))]
df$construction_year <- ifelse(is.na(df$construction_year),mode,df$construction_year)




write_csv(df,"fullsubmission.csv")
