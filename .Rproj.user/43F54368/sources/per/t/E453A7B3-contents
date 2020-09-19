df <- read.csv("~/Desktop/pump_it_up/19EylImputedCatBOOST.csv", stringsAsFactors=TRUE)


index <- sample(1:nrow(df),nrow(df))

train_index <- index[1:35603]


train <- df[train_index,] %>% select(-"id")
train_x <- train %>% select(-"status_group")
train_y <- as.numeric(train$status_group)-1


###### 0 functional, 1 functional needs repair, 2 non functional

catnames <- train %>% purrr::keep(is.factor) %>% names



test <- df[-train_index,] %>% select(-"id")
test_x <- test %>% select(-"status_group")
test_y <- as.numeric(test$status_group)-1



learnpool <- catboost.load_pool(train_x,label = train_y,cat_features = catnames)
testpool <- catboost.load_pool(test_x,label = test_y,cat_features = catnames)


catparam <- list(use_best_model=T,loss_function='MultiClass',eval_metric='Accuracy',
                 depth=12,learning_rate=0.37,iterations=50,prediction_type='Class')


catmodel <- catboost.train(learnpool,testpool,catparam) ### %80.31 test acc.





catparam2 <- list(use_best_model=T,loss_function='MultiClass',eval_metric='Accuracy',
                 depth=12,learning_rate=0.69,iterations=100,prediction_type='Class')


catmodel2 <- catboost.train(learnpool,testpool,catparam2) ### %80.12 test acc.




catparam3 <- list(use_best_model=T,loss_function='MultiClass',eval_metric='Accuracy',
                 depth=7,learning_rate=0.37,iterations=200,prediction_type='Class')


catmodel3 <- catboost.train(learnpool,testpool,catparam3) ### %81.36 test acc






######## giving submission


fullsubmission <- read.csv("~/Desktop/pump_it_up/Imputed Submission Var./fullsubmission.csv", stringsAsFactors=TRUE)


fullsubmission <- fullsubmission %>% select(-"id")

pred <- catboost.predict(catmodel3,catboost.load_pool(fullsubmission),prediction_type = 'Class')

pred <- ifelse(pred==0,"functional",
               ifelse(pred==1,"functional needs repair","non functional"))



SubmissionFormat$status_group <- pred


write_csv(SubmissionFormat,"9thSubByCatBOOST.csv")



