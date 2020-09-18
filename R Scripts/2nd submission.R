df <- read.csv("~/Desktop/pump_it_up/Data/train_x.csv", na.strings=c("NA",""), stringsAsFactors=TRUE)
train_y <- read.csv("~/Desktop/pump_it_up/Data/train_y.csv", stringsAsFactors=TRUE)

df <- df %>% select(-c("wpt_name","num_private","date_recorded","recorded_by","scheme_name","id"))
train_y <- train_y$status_group

df <- cbind(df,status_group=train_y)
df$construction_year <- ifelse(df$construction_year==0,NA,df$construction_year)

full<-na.omit(df)
full$status_group <- as.numeric(full$status_group)-1

#full$status_group <- as.factor(full$status_group)

##0 functional 2 non functional 1 functional needs repair


dataset<-full

############## NA OMIT ILE MODELLEME


catnames <- full %>% purrr::keep(is.factor) %>% names()


lgbtrain <- dataset[1:25473,]
lgbtrainx <- lgbtrain %>% select(-c("status_group"))
lgbtest <- dataset[25474:32473,]
lgbtestx <- lgbtest %>% select(-c("status_group"))
lgbtesty <- lgbtest$status_group



dtrain <- lgb.Dataset(as.matrix(lgbtrainx),label=lgbtrain$status_group)
dtest <- lgb.Dataset(as.matrix(lgbtestx),label=lgbtest$status_group)
valids <- list(test = dtest)

params <- list(objective = "multiclass", metric = "multi_logloss",num_class=3)
lgbmodel <- lgb.train(
  params = params
  , data = dtrain
  , nrounds = 1000
  , min_data = 1L
  , valids = valids
  , learning_rate = 0.1
  , categorical_feature = catnames
)

lgbtesty %>% View()
pred <- predict(lgbmodel,as.matrix(lgbtestx),reshape = T)

tahmin <- pred %>% data.frame()

myf <- function(x){
  match(max(x),x)
}
preds <- apply(tahmin,1,myf)-1
preds <- as.factor(preds)
obstest <- lgbtesty %>% as.factor()

confusionMatrix(preds,obstest)

########### %75 accuracy



#### let's give the 2nd submission

### fullsubmission.csv is independent variables.

fullsubmission <- read.csv("~/Desktop/pump_it_up/fullsubmission.csv", stringsAsFactors=TRUE)
fullsubmission <- fullsubmission %>% select(-"id")

submpred <- predict(lgbmodel,as.matrix(fullsubmission),reshape = T)

tahmin <- submpred %>% data.frame()

myf <- function(x){
  match(max(x),x)
}
preds <- apply(tahmin,1,myf)-1
preds <- as.factor(preds)



textpred <- ifelse(preds==0,"functional",
                   ifelse(preds==1,"functional needs repair","non functional"))

SubmissionFormat$status_group<-textpred


write_csv(SubmissionFormat,"2nd_submission.csv")




#### boktan impute edilmiş test_x

boktanimpute <- read.csv("~/Desktop/pump_it_up/Data/test_x.csv", na.strings=c("NA",""), stringsAsFactors=TRUE)
boktanimpute <- boktanimpute %>% select(-c("id","wpt_name","num_private","date_recorded","recorded_by","scheme_name",))


numerizedmain <- boktanimpute
numerizedmain$construction_year <- ifelse(numerizedmain$construction_year==0,2010,numerizedmain$construction_year)


############  devamında tüm NA'lere mod bastım, izaha gerek yok 2.2nd submission


boktanpred <- predict(lgbmodel,as.matrix(numerizedmain),reshape = T)
preds <- boktanpred %>% data.frame()
preds <- apply(preds,1,myf)-1
preds <- as.factor(preds)

textpred <- ifelse(preds==0,"functional",
                   ifelse(preds==1,"functional needs repair","non functional"))

SubmissionFormat$status_group<-textpred


write_csv(SubmissionFormat,"2.3nd_submission.csv")

