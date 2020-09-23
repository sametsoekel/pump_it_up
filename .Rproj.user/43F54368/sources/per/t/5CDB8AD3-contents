df <- read.csv("~/Desktop/pump_it_up/Imputed Train Set/19EylImputedCatBOOST.csv", stringsAsFactors=TRUE)

df <- df %>% select(-c("id"))

train_index <- sample(1:nrow(df),nrow(df)*0.74)


train_x <- df[train_index,] %>% select(-"status_group")
train_y <- as.numeric(df[train_index,]$status_group)-1

test_x <-  df[-train_index,] %>% select(-"status_group")
test_y <- as.numeric(df[-train_index,]$status_group)-1

## 0 functional, 1 functional needs repair, 2 non functional


catnames <- train_x %>% purrr::keep(is.factor) %>% names()

learnpool <- catboost.load_pool(train_x,label = train_y,cat_features = catnames)
testpool <- catboost.load_pool(test_x,label = test_y,cat_features = catnames)


catparam <- list(use_best_model=T,loss_function='MultiClass',custom_loss='Accuracy',
                 prediction_type='Class',learning_rate=0.4455,depth=9,iterations=400)



catmodel <- catboost.train(learnpool,testpool,catparam)

pred <- catboost.predict(catmodel,catboost.load_pool(test_x),prediction_type = 'Class') %>% as.factor()

confusionMatrix(pred,as.factor(test_y))


####### giving submission

z <- read.csv("~/Desktop/pump_it_up/ImputedCatBoostModMedian2.csv", stringsAsFactors=TRUE)

z <- z %>% select(-"id")

subpred <- catboost.predict(catmodel,catboost.load_pool(z),prediction_type = 'Class') %>% as.factor()


textpred <- ifelse(subpred==0,"functional",
                   ifelse(subpred==1,"functional needs repair","non functional"))


SubmissionFormat$status_group <- textpred

write_csv(SubmissionFormat,"22EylCatBOOSTSub.csv")






######### Deploying a lightgbm model



dtrain <- lgb.Dataset(as.matrix(train_x),label = train_y,categorical_feature = catnames)
dtest <- lgb.Dataset(as.matrix(test_x),label = test_y,categorical_feature = catnames)
lgbparams <- list(objective = "multiclass", metric = "multi_logloss",num_class=3)
valids <- list(test = dtest)

lgbmodel <- lgb.train(
  params = lgbparams
  , data = dtrain
  , nrounds = 3000
  , min_data = 1L
  , valids = valids
  , learning_rate = .01
  , early_stopping_rounds = 50
)

predsss <- predict(lgbmodel,as.matrix(test_x),reshape = T)

tahmin <- predsss %>% data.frame()

myf <- function(x){
  match(max(x),x)
}
predsss <- apply(tahmin,1,myf)-1

confusionMatrix(as.factor(predsss),as.factor(test_y))


predsss <- predict(lgbmodel,as.matrix(z),reshape = T)

tahmin <- predsss %>% data.frame()
predsss <- apply(tahmin,1,myf)-1

textpred <- ifelse(predsss==0,"functional",
                   ifelse(predsss==1,"functional needs repair","non functional"))


SubmissionFormat$status_group <- textpred

write_csv(SubmissionFormat,"22EylLightGBMsub.csv")

