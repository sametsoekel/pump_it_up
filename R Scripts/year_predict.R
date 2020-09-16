## deploying a new lightgbm model using year_train


df<-year_train

set.seed(42)
train_index <- createDataPartition(df$construction_year,
                                   p = .7,
                                   list = F,
                                   times = 1)


yt_train <- df[train_index,]

yt_test <- df[-train_index,]


yt_train_y <- yt_train$construction_year

yt_train_x <- yt_train %>% dplyr::select(-c("construction_year"))

yt_test_y <- yt_test$construction_year

yt_test_x <- yt_test %>% dplyr::select(-"construction_year")



catnames <- names(purrr::keep(yt_train_x,is.factor))

yt_train_lgb <- lgb.Dataset(as.matrix(yt_train_x), label = yt_train_y,categorical_feature = catnames)
data_file <- tempfile(fileext = ".data")
lgb.Dataset.save(yt_train_lgb, data_file)
yt_train_lgb <- lgb.Dataset(data_file)
lgb.Dataset.construct(yt_train_lgb)







#### BUILDING MODEL ####


lgbmodel <- lgb.train(data=yt_train_lgb,
                   objective = "regression",
                   alpha = 0.1,
                   nrounds = 1000,
                   learning_rate = .1
)


lgbpred <- lgbmodel$predict(as.matrix(yt_test_x))

hata <- data.frame(pred=lgbpred,obs=yt_test_y)
defaultSummary(hata)














lgb.grid <- list(objective = "regression",
                 metric = "RMSE",
                 min_sum_hessian_in_leaf = 1,
                 feature_fraction = 0.8,
                 bagging_fraction = 0.8,
                 bagging_freq = 5,
                 min_data = 200,
                 max_bin = 50,
                 lambda_l1 = 5,
                 lambda_l2 = 1,
                 min_data_in_bin=200,
                 min_gain_to_split = 5,
                 min_data_in_leaf = 20,
                 is_unbalance = TRUE)




lgb.normalizedgini <- function(preds, dtrain){
  actual = getinfo(dtrain, "label")
  score  = NormalizedGini(preds,actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}

yt_train_lgb_cv <- lgb.Dataset(data=as.matrix(yt_train_x), label=yt_train_y)

lgb.model.cv <- lgb.cv(params = lgb.grid, data = yt_train_lgb_cv, aplha=0.01,learning_rate = 0.1, num_leaves = 25,
                      num_threads = 2 , nrounds = 10000, early_stopping_rounds = 50,
                      eval_freq = 100, eval = lgb.normalizedgini,
                      categorical_feature = catnames, nfold = 13, stratified = TRUE)






best_iter <- lgb.model.cv$best_iter



########## deploy a final model #############

lgb_final_model = lgb.train(params = lgb.grid, data = yt_train_lgb_cv, learning_rate = 0.1,
                      num_leaves = 25, num_threads = 2 , nrounds = best_iter,
                      eval_freq = 20, eval = lgb.normalizedgini,
                      categorical_feature = catnames)

pred <- lgb_final_model$predict(as.matrix(yt_test_x))

hata <- data.frame(pred=round(pred),obs=yt_test_y)
defaultSummary(hata)



