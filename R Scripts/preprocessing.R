train_x <- read_csv("Data/train_x.csv")

train_y <- read_csv("Data/train_y.csv")


###### SOME VARIABLES I FOUND INSIGNIFICANT GETS OUT -- 14/09/2020 #######

train_xx <- train_x %>% select(-c("wpt_name","num_private","date_recorded","recorded_by"))


