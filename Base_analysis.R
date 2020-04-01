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

train$Date <- as.Date(train$Date,format = "%d-%m-%Y")
test$Date <- as.Date(test$Date,format = "%d-%m-%Y")

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

#remove 2 years

#remove outliers

train$Average_Breeze_Speed[train$Average_Breeze_Speed < 7] <- NA
train$Average_Breeze_Speed[train$Average_Breeze_Speed > 140] <- NA

train$Max_Breeze_Speed[train$Max_Breeze_Speed < 11] <- NA
train$Max_Breeze_Speed[train$Max_Breeze_Speed > 192] <- NA

#train$Min_Breeze_Speed[train$Min_Breeze_Speed < 5] <- 5
train$Min_Breeze_Speed[train$Min_Breeze_Speed > 120] <- NA

train$Var1[train$Var1 > 300] <- NA



train$Average_Atmospheric_Pressure[train$Average_Atmospheric_Pressure < 7988] <- NA
train$Average_Atmospheric_Pressure[train$Average_Atmospheric_Pressure > 8578] <- NA



train$Max_Atmospheric_Pressure[train$Max_Atmospheric_Pressure < 8045] <- NA
train$Max_Atmospheric_Pressure[train$Max_Atmospheric_Pressure > 8596] <- NA


train$Min_Atmospheric_Pressure[train$Min_Atmospheric_Pressure < 7896] <- NA
train$Min_Atmospheric_Pressure[train$Min_Atmospheric_Pressure > 8562] <- NA


train$Min_Ambient_Pollution[train$Min_Ambient_Pollution < 9] <- NA
train$Min_Ambient_Pollution[train$Min_Ambient_Pollution > 330] <- NA

train$Max_Ambient_Pollution[train$Max_Ambient_Pollution < 15] <- NA
train$Max_Ambient_Pollution[train$Max_Ambient_Pollution > 342] <- NA




train$Average_Moisture_In_Park[train$Average_Moisture_In_Park < 106] <- NA
train$Average_Moisture_In_Park[train$Average_Moisture_In_Park > 292] <- NA



train$Max_Moisture_In_Park[train$Max_Moisture_In_Park < 148] <- NA
train$Max_Moisture_In_Park[train$Max_Moisture_In_Park > 292] <- NA


train$Min_Moisture_In_Park[train$Min_Moisture_In_Park < 60] <- NA
train$Min_Moisture_In_Park[train$Min_Moisture_In_Park > 292] <- NA



target<-train$Footfall






#Treating the NA values

#train$Var2 <- train$Var1


#Filling up NA values
#DIRECTION OF WIND
#mon_avgdist <- train[!is.na(Var1),lapply(.SD,mean),by=c("Min_Moisture_In_Park"),
 #                                 .SDcols=c("Var1")]
# park_avgdist <- train[!is.na(Var1),lapply(.SD,mean),by=c("Park_ID"),
#                                     .SDcols=c("Var1")]
#train[,matchingIndex:=match(train$Min_Moisture_In_Park,mon_avgdist$Min_Moisture_In_Park)]
#train[is.na(Var1),Var1:=mon_avgdist$Var1[train$matchingIndex[is.na(train$Var1)]]]

#train[,matchingIndex:=match(train$Park_ID,park_avgdist$Park_ID)]
#train[is.na(Var1),Var1:=park_avgdist$Var1[train$matchingIndex[is.na(train$Var1)]]]






#train <- na.omit(train)

#Feature

#trying new feature
train$weekno <- as.numeric(as.integer(train$yday/14))
train$pol <- as.numeric(train$Max_Ambient_Pollution - train$Min_Ambient_Pollution)/train$Average_Breeze_Speed
train$new <- 1/1+log(train$pol)

train$pol2 <- log(as.numeric(train$Max_Atmospheric_Pressure - train$Min_Atmospheric_Pressure/train$Average_Moisture_In_Park))

train$pol3 <- (as.numeric(train$Max_Moisture_In_Park - train$Min_Moisture_In_Park/1+train$Var1))


train$f1 <- 10^(as.numeric(train$Average_Breeze_Speed/train$Average_Atmospheric_Pressure))
train$f2 <- 2.2^(as.numeric(train$Min_Ambient_Pollution/train$Max_Ambient_Pollution))
train$f3 <- -0.04*as.numeric((train$Average_Moisture_In_Park-train$Min_Moisture_In_Park)/train$Max_Moisture_In_Park-train$Average_Moisture_In_Park)




test$weekno <- as.numeric(as.integer(test$yday/14))
test$pol <- as.numeric(test$Max_Ambient_Pollution - test$Min_Ambient_Pollution)/test$Average_Breeze_Speed
test$new <- 1/1+log(test$pol)

test$pol2 <- log(as.numeric(test$Max_Atmospheric_Pressure - test$Min_Atmospheric_Pressure/test$Average_Moisture_In_Park))

test$pol3 <-(as.numeric(test$Max_Moisture_In_Park - test$Min_Moisture_In_Park/1+test$Var1))


test$f1 <- 10^(as.numeric(test$Average_Breeze_Speed/test$Average_Atmospheric_Pressure))
test$f2 <- 2.2^(as.numeric((test$Min_Ambient_Pollution)/test$Max_Ambient_Pollution))
test$f3 <- -0.04*as.numeric((test$Average_Moisture_In_Park-test$Min_Moisture_In_Park)/test$Max_Moisture_In_Park-test$Average_Moisture_In_Park)




# 
# 
train$new[train$new < -0.5] <- NA
train$new[train$new > 4.6] <- NA


train$pol2[train$pol2 < 8.954] <- NA
train$pol2[train$pol2 > 9.05] <- NA


train$pol3[train$pol3 < 0] <- NA
train$pol3[train$pol3 > 350] <- NA


train$Var1[train$Var1==0] <- NA

# 
# train$f1[train$f1 > 1.029] <- NA
# train$f3[train$f2 < 1.6] <- NA


# 
# 
# 
# 
# 
# #MEANING STUFF
# 
# by park and yday 

train$setkey <- paste0(train$yday,".",train$Park_ID)
train$setkey <- as.numeric(train$setkey)




mon_avgdist <- train[!is.na(Average_Breeze_Speed),lapply(.SD,mean),by=c("setkey"),
                     .SDcols=c("Average_Breeze_Speed")]
mean1 <- mean(mon_avgdist$Average_Breeze_Speed)
train[,matchingIndex:=match(train$setkey,mon_avgdist$setkey)]
train[is.na(Average_Breeze_Speed),Average_Breeze_Speed:=mon_avgdist$Average_Breeze_Speed[train$matchingIndex[is.na(train$Average_Breeze_Speed)]]]
train[,matchingIndex :=NULL]

train[is.na(Average_Breeze_Speed),Average_Breeze_Speed:=mean1]



#########



train$setkey <- paste0(train$yday,".",train$Park_ID)
train$setkey <- as.numeric(train$setkey)




mon_avgdist <- train[!is.na(Direction_Of_Wind),lapply(.SD,mean),by=c("setkey"),
                     .SDcols=c("Direction_Of_Wind")]
mean1 <- mean(mon_avgdist$Direction_Of_Wind)
train[,matchingIndex:=match(train$setkey,mon_avgdist$setkey)]
train[is.na(Direction_Of_Wind),Direction_Of_Wind:=mon_avgdist$Direction_Of_Wind[train$matchingIndex[is.na(train$Direction_Of_Wind)]]]
train[,matchingIndex :=NULL]

train[is.na(Direction_Of_Wind),Direction_Of_Wind:=mean1]







train$setkey <- paste0(train$yday,".",train$Park_ID)
train$setkey <- as.numeric(train$setkey)




mon_avgdist <- train[!is.na(Min_Breeze_Speed),lapply(.SD,mean),by=c("setkey"),
                     .SDcols=c("Min_Breeze_Speed")]
mean1 <- mean(mon_avgdist$Min_Breeze_Speed)
train[,matchingIndex:=match(train$setkey,mon_avgdist$setkey)]
train[is.na(Min_Breeze_Speed),Min_Breeze_Speed:=mon_avgdist$Min_Breeze_Speed[train$matchingIndex[is.na(train$Min_Breeze_Speed)]]]
train[,matchingIndex :=NULL]

train[is.na(Min_Breeze_Speed),Min_Breeze_Speed:=mean1]






train$setkey <- paste0(train$yday,".",train$Park_ID)
train$setkey <- as.numeric(train$setkey)




mon_avgdist <- train[!is.na(Max_Breeze_Speed),lapply(.SD,mean),by=c("setkey"),
                     .SDcols=c("Max_Breeze_Speed")]
mean1 <- mean(mon_avgdist$Max_Breeze_Speed)
train[,matchingIndex:=match(train$setkey,mon_avgdist$setkey)]
train[is.na(Max_Breeze_Speed),Max_Breeze_Speed:=mon_avgdist$Max_Breeze_Speed[train$matchingIndex[is.na(train$Max_Breeze_Speed)]]]
train[,matchingIndex :=NULL]

train[is.na(Max_Breeze_Speed),Max_Breeze_Speed:=mean1]




##############################################################



train$setkey <- paste0(train$yday,".",train$Park_ID)
train$setkey <- as.numeric(train$setkey)



mon_avgdist <- train[!is.na(Average_Atmospheric_Pressure),lapply(.SD,mean),by=c("setkey"),
                     .SDcols=c("Average_Atmospheric_Pressure")]
mean1 <- mean(mon_avgdist$Average_Atmospheric_Pressure)

train[,matchingIndex:=match(train$setkey,mon_avgdist$setkey)]
train[is.na(Average_Atmospheric_Pressure),Average_Atmospheric_Pressure:=mon_avgdist$Average_Atmospheric_Pressure[train$matchingIndex[is.na(train$Average_Atmospheric_Pressure)]]]
train[,matchingIndex :=NULL]

train[is.na(Average_Atmospheric_Pressure),Average_Atmospheric_Pressure:=mean1]







train$setkey <- paste0(train$yday,".",train$Park_ID)
train$setkey <- as.numeric(train$setkey)




mon_avgdist <- train[!is.na(Max_Ambient_Pollution),lapply(.SD,mean),by=c("setkey"),
                     .SDcols=c("Max_Ambient_Pollution")]
mean1 <- mean(mon_avgdist$Max_Ambient_Pollution)
train[,matchingIndex:=match(train$setkey,mon_avgdist$setkey)]
train[is.na(Max_Ambient_Pollution),Max_Ambient_Pollution:=mon_avgdist$Max_Ambient_Pollution[train$matchingIndex[is.na(train$Max_Ambient_Pollution)]]]
train[,matchingIndex :=NULL]

train[is.na(Max_Ambient_Pollution),Max_Ambient_Pollution:=mean1]





train$setkey <- paste0(train$yday,".",train$Park_ID)
train$setkey <- as.numeric(train$setkey)




mon_avgdist <- train[!is.na(Min_Ambient_Pollution),lapply(.SD,mean),by=c("setkey"),
                     .SDcols=c("Min_Ambient_Pollution")]
mean1 <- mean(mon_avgdist$Min_Ambient_Pollution)
train[,matchingIndex:=match(train$setkey,mon_avgdist$setkey)]
train[is.na(Min_Ambient_Pollution),Min_Ambient_Pollution:=mon_avgdist$Min_Ambient_Pollution[train$matchingIndex[is.na(train$Min_Ambient_Pollution)]]]
train[,matchingIndex :=NULL]

train[is.na(Min_Ambient_Pollution),Min_Ambient_Pollution:=mean1]
# 
# 
# 
# 
# 
# 
# 
# train$Average_Moisture_In_Park[is.na(train$Average_Moisture_In_Park)] <- mean(train$Average_Moisture_In_Park)
# train$pol2[is.na(train$pol2)] <- mean(train$pol2)
# train$yday[is.na(train$yday)] <- mean(train$yday)
# 
# train$Max_Moisture_In_Park[is.na(train$Max_Moisture_In_Park)] <- mean(train$Max_Moisture_In_Park)
# 
# 
# 
# train[is.na(train)] <- 1
# 
# 
# 
# 











#test$Var2 <- test$Var1












mon_avgdist <- train[!is.na(Var1),lapply(.SD,mean),by=c("Min_Moisture_In_Park"),
                     .SDcols=c("Var1")]
# park_avgdist <- train[!is.na(Var1),lapply(.SD,mean),by=c("Park_ID"),
#                                     .SDcols=c("Var1")]
test[,matchingIndex:=match(test$Min_Moisture_In_Park,mon_avgdist$Min_Moisture_In_Park)]
test[is.na(Var1),Var1:=mon_avgdist$Var1[test$matchingIndex[is.na(test$Var1)]]]

test[,matchingIndex :=NULL]








mon_avgdist <- train[!is.na(Average_Breeze_Speed),lapply(.SD,mean),by=c("Min_Moisture_In_Park"),
                     .SDcols=c("Average_Breeze_Speed")]
# park_avgdist <- train[!is.na(Var1),lapply(.SD,mean),by=c("Park_ID"),
#                                     .SDcols=c("Var1")]
test[,matchingIndex:=match(test$Min_Moisture_In_Park,mon_avgdist$Min_Moisture_In_Park)]
test[is.na(Average_Breeze_Speed),Average_Breeze_Speed:=mon_avgdist$Average_Breeze_Speed[test$matchingIndex[is.na(test$Average_Breeze_Speed)]]]

test[,matchingIndex :=NULL]


# 
# 
# 
# 
# 
# 
# target <-train$Footfall
# train[,Footfall :=NULL]
# 
# 
# #Neural Net
# 
# library(neuralnet)
# train <-train[,colnames(train)[-c(2:4,5,6,7,9,11:14,16,17,21,24,25,26)]:= NULL,with=F]
# test <-test[,colnames(test)[-c(2:4,5,6,7,9,11:14,16,17,21,24,25,26)]:= NULL,with=F]
# train$Footfall <- target
# 
# 
# set.seed(123)
# 
# start_time <- Sys.time()
# train[is.na(train)] <- 1
# train<-as.data.frame(train)
# 
# model_net <- neuralnet(formula = Footfall~Var1+new+pol2,data = train)
# 
# 
# end_time <- Sys.time()
# time_taken <- end_time - start_time
# 









#Random Forest
# 

# 
# 
# train[,ID:=NULL]
# train$Direction_Of_Wind <- as.integer(train$Direction_Of_Wind/7)
# train$Average_Breeze_Speed <- as.integer(train$Average_Breeze_Speed/3)
# train$Var1 <- as.integer(train$Var1/6)
# train$Max_Breeze_Speed <- as.integer(train$Max_Breeze_Speed)
# train$Min_Breeze_Speed <- as.integer(train$Min_Breeze_Speed)
# 
# 
# train$Average_Atmospheric_Pressure <- as.integer(train$Average_Atmospheric_Pressure/12)
# train$Max_Atmospheric_Pressure <- as.integer(train$Max_Atmospheric_Pressure/12) 
# train$Min_Atmospheric_Pressure <- as.integer(train$Min_Atmospheric_Pressure/13)
# 
# train$Min_Ambient_Pollution <- as.integer(train$Min_Ambient_Pollution/10)
# train$Max_Ambient_Pollution <- as.integer(train$Min_Ambient_Pollution/1.2)
# 
# train$Max_Moisture_In_Park <- as.integer(train$Max_Moisture_In_Park/1.1)
# 
# 
# train$pol <- as.integer(train$pol)
# train$pol2 <- as.integer(100*train$pol2)
# train$new <- as.integer(8*train$new)
# train$pol3 <- as.integer(train$pol3/10)
# 
# 
# 
# 
# 
# a<-test$ID
# 
# test[,ID:=NULL]
# test$Direction_Of_Wind <- as.integer(test$Direction_Of_Wind/7)
# test$Average_Breeze_Speed <- as.integer(test$Average_Breeze_Speed/3)
# test$Var1 <- as.integer(test$Var1/6)
# test$Max_Breeze_Speed <- as.integer(test$Max_Breeze_Speed)
# test$Min_Breeze_Speed <- as.integer(test$Min_Breeze_Speed)
# 
# 
# test$Average_Atmospheric_Pressure <- as.integer(test$Average_Atmospheric_Pressure/12)
# test$Max_Atmospheric_Pressure <- as.integer(test$Max_Atmospheric_Pressure/12) 
# test$Min_Atmospheric_Pressure <- as.integer(test$Min_Atmospheric_Pressure/13)
# 
# test$Min_Ambient_Pollution <- as.integer(test$Min_Ambient_Pollution/10)
# test$Max_Ambient_Pollution <- as.integer(test$Min_Ambient_Pollution/1.2)
# 
# test$Max_Moisture_In_Park <- as.integer(test$Max_Moisture_In_Park/1.1)
# 
# 
# test$pol <- as.integer(test$pol)
# test$pol2 <- as.integer(100*test$pol2)
# test$new <- as.integer(8*test$new)
# test$pol3 <- as.integer(test$pol3/10)
# 
# library("randomForest")
# 
# 
# 
# 
# 
# target <- train$Footfall
# train[,Footfall:=NULL]
# 
# train <-train[,colnames(train)[-c(1:13,16:18,20,23:25)]:= NULL,with=F]
# 
# 
# 
# test <-test[,colnames(test)[-c(1:13,16:18,20,23:25)]:= NULL,with=F]
# 
# #c(2:4,5,6,7,9,11:14,16,17,21,24,25,26)  -1
# 
# 
# train$Footfall <- target
# train11 <- na.omit(train)
# model_rf <- randomForest(Footfall ~  Park_ID + Direction_Of_Wind + Average_Breeze_Speed + Max_Breeze_Speed + Min_Breeze_Speed + Var1 + Average_Atmospheric_Pressure 
#                          + Max_Ambient_Pollution + Average_Moisture_In_Park +
#                            Max_Moisture_In_Park +  mon + mday + new + pol2 + pol3 , data=train11, ntree=1600, mtry=4, importance=TRUE)
# 
# 
# test<-as.matrix(as.data.frame(test))
# pred <- predict(model_rf,test)
# ans<-as.data.table(test$ID)
# ans$ID <- ans$V1
# ans[,V1:=NULL]
# ans$Footfall <- as.integer(pred)
# 
# 
# write.csv(ans,"rfans.csv",row.names = FALSE)
# 
# #xgboost model






train$f4 <- train$f1*train$f2*train$f3



train$f4[train$f4 < 7] <- NA
train$f4[train$f4 > 24.1] <- NA






train[,Footfall :=NULL]
# train[is.na(train)] <- 0
# test[is.na(test)] <- 0

train <-train[,colnames(train)[-c(2:4,5,6,7,9,11:14,16,17,21,24,25,26,30)]:= NULL,with=F]

train_mat = data.matrix(as.data.frame(train))

train.xg = xgb.DMatrix(train_mat, label=target, missing=NA)




param <- list(max_depth = 10,
              eta = 0.015,
              silent = 0,
              objective="reg:linear",
              eval_metric="rmse",
              # subsample = 0.75,
              min_child_weight = 800,
              colsample_bytree = 1,
              base_score =0
)



start_time <- Sys.time()

set.seed(123)
model_xgb2 <- xgb.train(param, train.xg, nthread = 16, nround = 1600, verbose = 1)

end_time <- Sys.time()
time_taken <- end_time - start_time

test <-test[,colnames(test)[-c(2:4,5,6,7,9,11:14,16,17,21,24,25,26,30)]:= NULL,with=F]

test_mat = data.matrix(as.data.frame(test))
#test.xg = xgb.DMatrix(test, missing=NA)




prediction<-predict(model_xgb2,test_mat, missing=NA)
test$Footfall <- prediction
test <- fread("test.csv")

ml_ans <- as.data.table(test$ID)
ml_ans$ID <- ml_ans$V1
ml_ans[,V1:=NULL]
ml_ans$Footfall <- prediction


write.csv(ml_ans,"highgh.csv",row.names = FALSE)

ml_ans$Footfall<- ml_ans$Footfall - 11.5

write.csv(ml_ans,"highgher.csv",row.names = FALSE)




#feature 0010 11.5


