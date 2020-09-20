df <- df %>% select(-"id")

index <- sample(1:nrow(df),nrow(df))

train_index <- index[1:round(0.7*nrow(df))]


train <- df[train_index,]
train_x <- train %>% select(-"status_group")
train_y <- as.numeric(train$status_group)-1

test <- df[-train_index,]
test_x <- test %>% select(-"status_group")
test_y <- as.numeric(test$status_group)-1

catnames <- test %>% purrr::keep(is.factor) %>% names()

##### 0 functional 1 functional needs repair 2 non functional


learnpool <- catboost.load_pool(train_x,label=train_y,cat_features = catnames)
testpool <- catboost.load_pool(test_x,label=test_y,cat_features = catnames)


catparam <- list(use_best_model=T,loss_function='MultiClass',eval_metric='Accuracy',
                 depth=8,learning_rate=0.069,iterations=220,prediction_type='Class')


catmodel <- catboost.train(learnpool,testpool,catparam)



target <- read.csv("~/Desktop/pump_it_up/ImputedCatBoostModMedian2.csv", stringsAsFactors=TRUE)
target <- target %>% select(-"id")

catpred <- catboost.predict(catmodel,catboost.load_pool(target),prediction_type = 'Class')

textpred <- ifelse(catpred==0,"functional",
                   ifelse(catpred==1,"functional needs repair","non functional"))



SubmissionFormat$status_group <- textpred


write_csv(SubmissionFormat,"11thSubByCatBOOST.csv")




catparam2 <- list(use_best_model=T,loss_function='MultiClass',eval_metric='Accuracy',
                 depth=8,learning_rate=0.08,iterations=220,prediction_type='Class')


catmodel2 <- catboost.train(learnpool,testpool,catparam2)



catpred2 <- catboost.predict(catmodel2,catboost.load_pool(target),prediction_type = 'Class')

textpred2 <- ifelse(catpred2==0,"functional",
                   ifelse(catpred2==1,"functional needs repair","non functional"))

SubmissionFormat$status_group <- textpred2



write_csv(SubmissionFormat,"12thSubByCatBOOST.csv")








catparam3 <- list(use_best_model=T,loss_function='MultiClass',eval_metric='Accuracy',
                  depth=7,learning_rate=0.088,iterations=400,prediction_type='Class')


catmodel3 <- catboost.train(learnpool,testpool,catparam3)

catpred3 <- catboost.predict(catmodel3,catboost.load_pool(target),prediction_type = 'Class')

textpred3 <- ifelse(catpred3==0,"functional",
                    ifelse(catpred3==1,"functional needs repair","non functional"))

SubmissionFormat$status_group <- textpred3



write_csv(SubmissionFormat,"13thSubByCatBOOST.csv")



confusionMatrix(as.factor(catpred2),as.factor(catpred3))
