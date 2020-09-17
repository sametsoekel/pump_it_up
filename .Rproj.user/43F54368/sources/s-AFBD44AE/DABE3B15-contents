df <- read.csv("~/Desktop/pump_it_up/1st_merge.csv", stringsAsFactors=TRUE) %>% select(-"X")

df$status_group <- ((df$status_group %>% as.numeric()) -1) %>% as.factor()

#### 2: non functional
#### 0: functional
#### 1: functional needs repair


set.seed(37)


partition_index <- sample(1:nrow(df),nrow(df))

seventyfivepercent <- nrow(df)*.75

train_index <- partition_index[1:seventyfivepercent]


train <- df[train_index,]

test <- df[-train_index,]


train_y <- train$status_group

train_x <- train %>% dplyr::select(-"status_group")

test_y <- test$status_group

test_x <- test %>% dplyr::select(-"status_group")


scaler <- function(x) { (x-min(x))/(max(x)-min(x)) }

factors <- purrr::keep(test,is.factor)
numerics <- purrr::keep(test,is.numeric)



factors$funder <- as.numeric(factors$funder)
factors$installer <- as.numeric(factors$installer)
factors$basin <- as.numeric(factors$basin)
factors$subvillage <- as.numeric(factors$subvillage)
factors$region <- as.numeric(factors$region)
factors$lga <- as.numeric(factors$lga)
factors$ward <- as.numeric(factors$ward)
factors$public_meeting <- as.numeric(factors$public_meeting)
factors$scheme_management <- as.numeric(factors$scheme_management)
factors$permit <- as.numeric(factors$permit)
factors$extraction_type <- as.numeric(factors$extraction_type)
factors$extraction_type_group <- as.numeric(factors$extraction_type_group)
factors$extraction_type_class <- as.numeric(factors$extraction_type_class)
factors$management <- as.numeric(factors$management)
factors$management_group <- as.numeric(factors$management_group)
factors$payment <- as.numeric(factors$payment)
factors$payment_type <- as.numeric(factors$payment_type)
factors$water_quality <- as.numeric(factors$water_quality)
factors$quality_group <- as.numeric(factors$quality_group)
factors$quantity <- as.numeric(factors$quantity)
factors$quantity_group <- as.numeric(factors$quantity_group)
factors$source <- as.numeric(factors$source)
factors$source_type <- as.numeric(factors$source_type)
factors$source_class <- as.numeric(factors$source_class)
factors$waterpoint_type <- as.numeric(factors$waterpoint_type)
factors$waterpoint_type_group <- as.numeric(factors$waterpoint_type_group)
factors$status_group <- as.numeric(factors$status_group)

numerizedtrain <- cbind(factors,numerics)

scaledtrain <- numerizedtrain %>% apply(2,scaler) %>% data.frame()

scaledtrain$status_group <- scaledtrain$status_group %>% as.factor()

scaledtrainx <- scaledtrain %>% select(-"status_group")

scaledtrainy <- scaledtrain$status_group





numerizedtest <- cbind(factors,numerics)

scaledtest <- numerizedtest %>% apply(2,scaler) %>% data.frame()

scaledtest$status_group <- scaledtest$status_group %>% as.factor()

scaledtestx <- scaledtest %>% select(-"status_group")

scaledtesty <- scaledtest$status_group


###### Neural Network Model

nnetmodel <- nnet(status_group~.,data =scaledtrain,
                  size=4,rang=.1,decay=3,maxit=400)

save(nnetmodel,file = "1st_submission_model.rda")

pred <- predict(nnetmodel,scaledtrainx,type = "class") %>% as.factor()

confusionMatrix(pred,scaledtrainy)

#### %72 train acc.


##### test accuracy

testpred <- predict(nnetmodel,scaledtestx,type="class") %>% as.factor()

confusionMatrix(testpred,scaledtesty)

####### %71 test acc.



