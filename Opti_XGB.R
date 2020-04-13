setwd("C:/Final year/Competitions/analytics vidhya/comp2")
#Libraries
library(data.table)
library(ggplot2)
library(plyr)
library(xgboost)
library(rattle)
library(rpart)
library(RColorBrewer)
library(e1071)
library(clusterSim)
#Seed & Read
set.seed(100)
train <- fread("train.csv")
test <- fread("test.csv")
first <- fread("first.csv")
second <- fread("second.csv")
third <- fread("third.csv")
patient <- fread("patient.csv")
camp <- fread("camp.csv")

#Dates
train$Registration_Date <- as.Date(train$Registration_Date,format = "%d-%b-%y")
test$Registration_Date <- as.Date(test$Registration_Date,format = "%d-%b-%y")
x<-unclass(strptime(train$Registration_Date, format = "%Y-%m-%d"))
y<-unclass(strptime(test$Registration_Date, format = "%Y-%m-%d"))
# train$mon <- x$mon
train$wday <- x$wday
train$year <- x$year
train$yday <- x$yday
train$mday <- x$mday
# test$mon <- y$mon
test$wday <- y$wday
test$year <- y$year
test$yday <- y$yday
test$mday <- y$mday
train[,Registration_Date:=NULL]
test[,Registration_Date:=NULL]
train$id <- paste0(train$Health_Camp_ID,".",train$Patient_ID)
test$id <- paste0(test$Health_Camp_ID,".",test$Patient_ID)
first$id <- paste0(first$Health_Camp_ID,".",first$Patient_ID)
second$id <- paste0(second$Health_Camp_ID,".",second$Patient_ID)
third$id <- paste0(third$Health_Camp_ID,".",third$Patient_ID)

#Making Data
camp_all <- levels(as.factor(train$Health_Camp_ID))
camp1 <- levels(as.factor(first$Health_Camp_ID))
camp2 <- levels(as.factor(second$Health_Camp_ID))
camp3 <- levels(as.factor(third$Health_Camp_ID))
train1 <- train[train$Health_Camp_ID %in% camp1]
train2 <- train[train$Health_Camp_ID %in% c(camp1,camp2)]
train3 <- train[train$Health_Camp_ID %in% camp3]
test_camp1 <- camp$Health_Camp_ID[camp$Category1 == "First"]
test_camp2 <- camp$Health_Camp_ID[camp$Category1 == "Second"]
test_camp3 <- camp$Health_Camp_ID[camp$Category1 == "Third"]
test1 <- test[test$Health_Camp_ID %in% test_camp1]
test2 <- test[test$Health_Camp_ID %in% c(test_camp1,test_camp2)]
test3 <- test[test$Health_Camp_ID %in% test_camp3]
train2$target <- 0
train2$target[which(train2$id %in% second$id) ] <- 1
train2 <- na.omit(train2)
train3$target <- 0
stall_id <- third$id[third$Number_of_stall_visited > 0] 
train3$target[which(train3$id %in% stall_id) ] <- 1
patient2 <- copy(patient)
patient2[patient2 == "None"] <- NA
patient2$First_Interaction <- as.Date(patient2$First_Interaction,format = "%d-%b-%y")
x2<-unclass(strptime(patient2$First_Interaction, format = "%Y-%m-%d"))
patient2$join_year <- x2$year
patient2$join_yday <- x2$yday
patient2[,First_Interaction:=NULL]
patient2$tsp <- 365*(patient2$join_year-102)+x2$yday
patient2[,join_year:=NULL]
patient2$Employer_Category[patient2$Employer_Category == "."] <- NA
patient2$Employer_Category <- as.factor(patient2$Employer_Category)
patient2$City_Type[patient2$City_Type == "."] <- NA
patient2$City_Type <- as.factor(patient2$City_Type)
patient2 <- sapply(patient2,as.numeric)
patient2 <- as.data.table(patient2)
#Merging Patient Data
train2$Patient_ID <- as.numeric(train2$Patient_ID)
setkey(patient2,Patient_ID)
setkey(train2,Patient_ID)
setkey(test2,Patient_ID)
setkey(train3,Patient_ID)
setkey(test3,Patient_ID)
train2 <- merge(train2,patient2)
test2 <- merge(test2,patient2)
train3 <- merge(train3,patient2)
test3 <- merge(test3,patient2)

#Modelling
target2 <- train2$target
train2[,c(1,2,9,10):=NULL]
test2_copy <- copy(test2) 
test2[,c(1,2,9):=NULL]
train2 <- as.data.table(sapply(train2,as.numeric))
test2 <- as.data.table(sapply(test2,as.numeric))
train2 <- as.matrix(train2)
test2 <- as.matrix(test2)

train2$City_Type1 <- 0
train2$City_Type2 <- 0
train2$City_Type3 <- 0
train2$City_Type4 <- 0
train2$City_Type5 <- 0
train2$City_Type6 <- 0
train2$City_Type7 <- 0
train2$City_Type8 <- 0
train2$City_Type9 <- 0
train2$City_Type10 <- 0
train2$City_Type1[train2$City_Type == 1] <- 1
train2$City_Type2[train2$City_Type == 2] <- 1
train2$City_Type3[train2$City_Type == 3] <- 1
train2$City_Type4[train2$City_Type == 4] <- 1
train2$City_Type5[train2$City_Type == 5] <- 1
train2$City_Type6[train2$City_Type == 6] <- 1
train2$City_Type7[train2$City_Type == 7] <- 1
train2$City_Type8[train2$City_Type == 8] <- 1
train2$City_Type9[train2$City_Type == 9] <- 1
train2$City_Type10[train2$City_Type == 10] <- 1
train2[,City_Type:=NULL]

test2$City_Type1 <- 0
test2$City_Type2 <- 0
test2$City_Type3 <- 0
test2$City_Type4 <- 0
test2$City_Type5 <- 0
test2$City_Type6 <- 0
test2$City_Type7 <- 0
test2$City_Type8 <- 0
test2$City_Type9 <- 0
test2$City_Type10 <- 0
test2$City_Type1[test2$City_Type == 1] <- 1
test2$City_Type2[test2$City_Type == 2] <- 1
test2$City_Type3[test2$City_Type == 3] <- 1
test2$City_Type4[test2$City_Type == 4] <- 1
test2$City_Type5[test2$City_Type == 5] <- 1
test2$City_Type6[test2$City_Type == 6] <- 1
test2$City_Type7[test2$City_Type == 7] <- 1
test2$City_Type8[test2$City_Type == 8] <- 1
test2$City_Type9[test2$City_Type == 9] <- 1
test2$City_Type10[test2$City_Type == 10] <- 1
test2[,City_Type:=NULL]


#Modelling2
target3 <- train3$target
train3[,c(1,2,9,10):=NULL]
test3_copy <- copy(test3) 
test3[,c(1,2,9):=NULL]
train3 <- as.data.table(sapply(train3,as.numeric))
test3 <- as.data.table(sapply(test3,as.numeric))
train3 <- as.matrix(train3)
test3 <- as.matrix(test3)

#XGBOOST1

train.xg = xgb.DMatrix(train2, label=target2, missing=NA)
test.xg = xgb.DMatrix(test2, missing=NA)
param <- list(max_depth = 8,
              eta = 0.08,
              objective="binary:logistic",
              #num_class=5,
              eval_metric = "auc",
              subsample = 0.5,
              min_child_weight = 800,
              colsample_bytree = 1,
              base_score =0.2
)
set.seed(123)
start_time <- Sys.time()
model_xgb2 <- xgb.train(param, train.xg, nthread = 16, nround = 1600,verbose = 1)
end_time <- Sys.time()
time_taken <- end_time - start_time

#Predictions2
prediction2<-predict(model_xgb2,test2, missing=NA)
test2_copy$Outcome <- prediction2
test2_copy[,c(3:20):=NULL]

#XGBOOST2
train.xg = xgb.DMatrix(train3, label=target3, missing=NA)
test.xg = xgb.DMatrix(test3, missing=NA)
param <- list(max_depth = 8,
              eta = 0.08,
              objective="binary:logistic",
              #num_class=5,
              eval_metric = "auc",
              subsample = 0.75,
              min_child_weight = 800,
              colsample_bytree = 1,
              base_score =0.2
)
set.seed(123)
start_time <- Sys.time()

#Bayesian Optimized tuned Hyperparameter
model_xgb3 <- xgb.train(param, train.xg, nthread = 16, nround = 1600,verbose = 1)

end_time <- Sys.time()
time_taken <- end_time - start_time


#Predictions3
prediction3<-predict(model_xgb3,test3, missing=NA)
test3_copy$Outcome <- prediction3
test3_copy[,c(3:20):=NULL]


write.csv(test2_copy,"ans_reg2.csv",row.names = F)
write.csv(test3_copy,"ans_reg3.csv",row.names = F)
