setwd("C:/Final year/Competitions/analytics vidhya")


#Libraries
library(data.table)
library(ggplot2)
library(plyr)
library(xgboost)

#Seed & Read
set.seed(100)
train <- fread("train.csv")
test <- fread("test.csv")
sam <- fread("sam.csv")

train2 <- copy(train)
test2 <- copy(test)

# train$Date <- as.Date(train$Date)
# test$Date <- test$Date
#both trainand test have NA values

#remove outliers

train$Date <- as.Date(train$Date,format = "%d-%m-%y")
test$Date <- as.Date(test$Date,format = "%d-%m-%y")

x<-unclass(strptime(train$Date, format = "%Y-%m-%d"))
y<-unclass(strptime(test$Date, format = "%Y-%m-%d"))

train$mon <- x$mon
train$wday <- x$wday
train$year <- x$year
train$yday <- x$yday
train$mday <- x$mday

test$mon <- y$mon
test$wday <- y$wday
test$year <- y$year
test$yday <- y$yday
test$mday <- y$mday

train[,Date:=NULL]
test[,Date:=NULL]

#remove outliers

train$Average_Breeze_Speed[train$Average_Breeze_Speed < 7] <- 7
train$Average_Breeze_Speed[train$Average_Breeze_Speed > 140] <- 140

train$Max_Breeze_Speed[train$Max_Breeze_Speed < 11] <- 11
train$Max_Breeze_Speed[train$Max_Breeze_Speed > 192] <- 192

#train$Min_Breeze_Speed[train$Min_Breeze_Speed < 5] <- 5
train$Min_Breeze_Speed[train$Min_Breeze_Speed > 120] <- 120

train$Var1[train$Var1 > 300] <- 300



train$Average_Atmospheric_Pressure[train$Average_Atmospheric_Pressure < 7988] <- 7988
train$Average_Atmospheric_Pressure[train$Average_Atmospheric_Pressure > 8578] <- 8578



train$Max_Atmospheric_Pressure[train$Max_Atmospheric_Pressure < 8045] <- 8045
train$Max_Atmospheric_Pressure[train$Max_Atmospheric_Pressure > 8596] <- 8596


train$Min_Atmospheric_Pressure[train$Min_Atmospheric_Pressure < 7896] <- 7896
train$Min_Atmospheric_Pressure[train$Min_Atmospheric_Pressure > 8562] <- 8562


train$Min_Ambient_Pollution[train$Min_Ambient_Pollution < 9] <- 9
train$Min_Ambient_Pollution[train$Min_Ambient_Pollution > 330] <- 330

train$Max_Ambient_Pollution[train$Max_Ambient_Pollution < 15] <- 15
train$Max_Ambient_Pollution[train$Max_Ambient_Pollution > 342] <- 342




train$Average_Moisture_In_Park[train$Average_Moisture_In_Park < 106] <- 106
train$Average_Moisture_In_Park[train$Average_Moisture_In_Park > 292] <- 292



train$Max_Moisture_In_Park[train$Max_Moisture_In_Park < 148] <- 148
train$Max_Moisture_In_Park[train$Max_Moisture_In_Park > 292] <- 292


train$Min_Moisture_In_Park[train$Min_Moisture_In_Park < 60] <- 60
train$Min_Moisture_In_Park[train$Min_Moisture_In_Park > 292] <- 292



target<-train$Footfall








#Filling up NA values
#DIRECTION OF WIND
#mon_avgdist <- train[!is.na(Var1),lapply(.SD,mean),by=c("mon","Park_ID"),
.SDcols=c("Var1")]
# park_avgdist <- train[!is.na(Var1),lapply(.SD,mean),by=c("Park_ID"),
#                                     .SDcols=c("Var1")]
#train[,matchingIndex:=match(train$mon,mon_avgdist$mon)]
#train[is.na(Var1),Var1:=mon_avgdist$Var1[train$matchingIndex[is.na(train$Var1)]]]

#train[,matchingIndex:=match(train$Park_ID,park_avgdist$Park_ID)]
#train[is.na(Var1),Var1:=park_avgdist$Var1[train$matchingIndex[is.na(train$Var1)]]]

#mon_avgdist <- train[!is.na(Average_Breeze_Speed),lapply(.SD,mean),by=c("mon","Park_ID"),
.SDcols=c("Average_Breeze_Speed")]
# park_avgdist <- train[!is.na(Var1),lapply(.SD,mean),by=c("Park_ID"),
#                                     .SDcols=c("Var1")]
#train[,matchingIndex:=match(train$mon,mon_avgdist$mon)]
#train[is.na(Average_Breeze_Speed),Average_Breeze_Speed:=mon_avgdist$Average_Breeze_Speed[train$matchingIndex[is.na(train$Average_Breeze_Speed)]]]



train[,matchingIndex :=NULL]


train[,Footfall :=NULL]
# train[is.na(train)] <- 0
# test[is.na(test)] <- 0

train <-train[,colnames(train)[-c(2:4,6,7,9,11:14,16,17,21)]:= NULL,with=F]

train_mat = data.matrix(as.data.frame(train))

train.xg = xgb.DMatrix(train_mat, label=target, missing=NA)




param <- list(max_depth = 10,
              eta = 0.015,
              silent = 0,
              objective="reg:linear",
              eval_metric="rmse",
              # subsample = 0.75,
              min_child_weight = 50,
              colsample_bytree = 1,
              base_score =0
)

set.seed(123)

start_time <- Sys.time()

model_xgb2 <- xgb.train(param, train.xg, nthread = 16, nround = 2600, verbose = 1)

end_time <- Sys.time()
time_taken <- end_time - start_time

test <-test[,colnames(test)[-c(2:4,6,7,9,11:14,16,17,21)]:= NULL,with=F]

test_mat = data.matrix(as.data.frame(test))
#test.xg = xgb.DMatrix(test, missing=NA)




prediction<-predict(model_xgb2,test_mat, missing=NA)
test$Footfall <- prediction
test <- fread("test.csv")

ml_ans <- as.data.table(test$ID)
ml_ans$ID <- ml_ans$V1
ml_ans[,V1:=NULL]
ml_ans$Footfall <- prediction


write.csv(ml_ans,"m1NAf_only_date2.csv",row.names = FALSE)









#Random Forest






