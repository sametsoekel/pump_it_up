train_index <- createDataPartition(year_train$construction_year,
                                   p = .7,
                                   list = F,
                                   times = 1)

train <- year_train[train_index,]

test <- year_train[-train_index,]


train_y <- train$construction_year

train_x <- train %>% dplyr::select(-"construction_year")

test_y <- test$construction_year

test_x <- test %>% dplyr::select(-"construction_year")


catnames <- names(purrr::keep(train_x,is.factor))

dtrain <- lgb.Dataset(as.matrix(train_x), label = train_y,categorical_feature = catnames)
data_file <- tempfile(fileext = ".data")
lgb.Dataset.save(dtrain, data_file)
dtrain <- lgb.Dataset(data_file)
lgb.Dataset.construct(dtrain)



model <- lgb.train(data=dtrain,
                   objective = "multiclass",
                   alpha = 0.1,
                   nrounds = 1000,
                   learning_rate = .1
                   )

pred <- model$predict(as.matrix(test_x))

hata <- data.frame(pred=pred,obs=test_y)
defaultSummary(hata)
##### 8.6 yıl ortalama hata


