maintest <-  read.csv("~/Desktop/pump_it_up/Data/test_x.csv", na.strings=c("NA",""), stringsAsFactors=TRUE)

maintest <- maintest %>% select(-c("id","wpt_name","num_private","date_recorded","recorded_by","scheme_name",))

maintest$construction_year <- ifelse(maintest$construction_year==0,NA,maintest$construction_year)

plasteR::na.outline(numerizedmain,plot_show = T)

setwd("/home/samet/Desktop/pump_it_up/Predictors")
load("16 Eyl. LGBM CV (9.21).rda")

#### basic imputation

maintest$construction_year <- ifelse(is.na(maintest$construction_year),1998,maintest$construction_year)


scaler <- function(x) { (x-min(x))/(max(x)-min(x)) }

factors <- purrr::keep(maintest,is.factor)
numerics <- purrr::keep(maintest,is.numeric)



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

numerizedmain <- cbind(factors,numerics)

############ imputing funder

val <- unique(numerizedmain$funder[!is.na(numerizedmain$funder)])
mode <- val[which.max(tabulate(match(numerizedmain$funder, val)))]

### funder en sık tekrar eden 251


numerizedmain$funder <- ifelse(is.na(numerizedmain$funder),mode,numerizedmain$funder)


############ imputing installer

val <- unique(numerizedmain$installer[!is.na(numerizedmain$installer)])
mode <- val[which.max(tabulate(match(numerizedmain$installer, val)))]

### installer en sık tekrar eden 248

numerizedmain$installer <- ifelse(is.na(numerizedmain$installer),mode,numerizedmain$installer)



############ imputing subvillage

val <- unique(numerizedmain$subvillage[!is.na(numerizedmain$subvillage)])
mode <- val[which.max(tabulate(match(numerizedmain$subvillage, val)))]

### installer en sık tekrar eden 7716

numerizedmain$subvillage <- ifelse(is.na(numerizedmain$subvillage),mode,numerizedmain$subvillage)




############ imputing public_meeting

val <- unique(numerizedmain$public_meeting[!is.na(numerizedmain$public_meeting)])
mode <- val[which.max(tabulate(match(numerizedmain$public_meeting, val)))]

### public_meeting en sık tekrar eden 2

numerizedmain$public_meeting <- ifelse(is.na(numerizedmain$public_meeting),mode,numerizedmain$public_meeting)





############ imputing scheme_management

val <- unique(numerizedmain$scheme_management[!is.na(numerizedmain$scheme_management)])
mode <- val[which.max(tabulate(match(numerizedmain$scheme_management, val)))]

### scheme management en sık tekrar eden 7

numerizedmain$scheme_management <- ifelse(is.na(numerizedmain$scheme_management),mode,numerizedmain$scheme_management)







########### imputing permit

val <- unique(numerizedmain$permit[!is.na(numerizedmain$permit)])
mode <- val[which.max(tabulate(match(numerizedmain$permit, val)))]

### permit en sık tekrar eden 2

numerizedmain$permit <- ifelse(is.na(numerizedmain$permit),mode,numerizedmain$permit)









############# double check

anyNA(numerizedmain)


###### no NA left


scaledmain <- numerizedmain %>% apply(2,scaler) %>% data.frame()





########################## First Submission ########################


################### BİSMİLLAHİRRAHMANİRRAHİM ######################

#######  in the name of Allah, the beneficent, the merciful #######


predictions <- predict(nnetmodel,scaledmain,type="class") %>% as.factor()

View(predictions)

summary(test$status_group)
##### 0 functional, 0.5 functional needs repair, 1 non functional
textpred <- ifelse(predictions==0,"functional",
                   ifelse(predictions==0.5,"functional needs repair","non functional"))


#### giving submission

submitter <- read_csv("~/Desktop/pump_it_up/SubmissionFormat.csv")


submitter$status_group <- textpred

write_csv(submitter,"1st_submit.csv")


