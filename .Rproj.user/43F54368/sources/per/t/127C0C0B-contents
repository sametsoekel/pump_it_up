train_x <- read.csv("Data/train_x.csv", stringsAsFactors=TRUE)

train_y <- read_csv("Data/train_y.csv")


###### SOME VARIABLES I FOUND INSIGNIFICANT GETS OUT -- 14/09/2020 #######

train_xx <- train_x %>% select(-c("id","wpt_name","num_private","date_recorded","recorded_by","scheme_name"))

plasteR::na.outline(train_xx,plot_show = T) ### checked current missing values

Freq(as.factor(train_xx$construction_year)) ### checked faulty frequencies



#### Filling zero dates with NA

train_xx$construction_year <- ifelse(train_xx$construction_year==0,NA,train_xx$construction_year)
