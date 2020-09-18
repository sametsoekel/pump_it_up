df <- read.csv("~/Desktop/pump_it_up/Imputed Train Set/PredictiveImputedNoMode50k.csv", stringsAsFactors=TRUE)

scaler <- function(x){(x-min(x))/(max(x)-min(x))}

df_num <- as.data.frame(lapply(df, as.numeric))
## 1 functional 3 non funtional 2 functional needs repair

df_num$trgt <- as.factor(df_num$trgt)

scaled <- df_num %>% select(-c("id","trgt")) %>% apply(2,scaler) %>% data.frame() %>% cbind(status_group=df_num$trgt)



index <- sample(1:nrow(scaled))

train_index <- index[1:35000]


trainset <- scaled[train_index,]
testset <- scaled[-train_index,]

train_x <- trainset %>% select(-"status_group")
train_y <- trainset$status_group

test_x <- testset %>% select(-"status_group")
test_y <- testset$status_group




################### First model


nnet1 <- nnet(status_group~.,data=trainset,
              size=20,decay=0.37,maxit=697)


pred <- predict(nnet1,test_x,type="class") %>% as.factor()
obs <- test_y

hata <- data.frame(pred,obs)

defaultSummary(hata)


######## %77 train, %76 test hatası size=20 decay=0.37, maxit = 697

#### Giving submission 5th by nnet1 #### Giving submission 5th by nnet1
#### Giving submission 5th by nnet1 #### Giving submission 5th by nnet1

sub_num <- as.data.frame(lapply(targetsub, as.numeric))
## 1 functional 3 non funtional 2 functional needs repair


subnum_scaled <- sub_num %>% select(-c("id")) %>% apply(2,scaler) %>% data.frame()

submission5pred <- predict(nnet1,subnum_scaled,type="class")

textpred <- ifelse(submission5pred==1,"functional",
                   ifelse(submission5pred==2,"functional needs repair","non functional"))

SubmissionFormat$status_group <- textpred
write_csv(SubmissionFormat,"5thSubByNnet.csv")













############ Building lightgbm model

df_num$trgt <- as.numeric(df_num$trgt)-1
df_num$trgt <- as.factor(df_num$trgt)

trainset <- df_num[train_index,]
testset <- df_num[-train_index,]

train_x <- trainset %>% select(-c("trgt","id"))
train_y <- trainset$trgt %>% as.numeric()
train_y <- train_y-1



test_x <- testset %>% select(-c("trgt","id"))
test_y <- testset$trgt %>% as.numeric()
test_y <- test_y-1



dtrain <- lgb.Dataset(as.matrix(train_x),label=train_y)
dtest <- lgb.Dataset(as.matrix(test_x),label=test_y)
valids <- list(test = dtest)

params <- list(objective = "multiclass", metric = "multi_logloss",num_class=3)
lgbmodel <- lgb.train(
  params = params
  , data = dtrain
  , nrounds = 1000
  , min_data = 1L
  , valids = valids
  , learning_rate = 0.1
  , early_stopping_rounds = 20
)

predsss <- predict(lgbmodel,as.matrix(train_x),reshape = T)

tahmin <- predsss %>% data.frame()

myf <- function(x){
  match(max(x),x)
}
predsss <- apply(tahmin,1,myf)-1
predsss <- as.factor(predsss)


confusionMatrix(predsss,as.factor(train_y))


################# Lightgbm %90 train, %81 test hatası <3



###### Bismillahirrahmanirrahim
########### Giving submission BY LIGHTGBM



x <-sub_num %>% select(-"id")

predsss <- predict(lgbmodel,as.matrix(x),reshape = T)

tahmin <- predsss %>% data.frame()

myf <- function(x){
  match(max(x),x)
}
predsss <- apply(tahmin,1,myf)-1
predsss <- as.factor(predsss)
length(predsss)


textpred <- ifelse(predsss==0,"functional",
                   ifelse(predsss==1,"functional needs repair","non functional"))


SubmissionFormat$status_group <- textpred


write_csv(SubmissionFormat,"6thSubByLightGBM.csv")


