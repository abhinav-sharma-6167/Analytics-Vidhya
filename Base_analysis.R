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
#Feature Engineering
test[,matchingIndex:=match(test$Min_Moisture_In_Park,mon_avgdist$Min_Moisture_In_Park)]
test[is.na(Average_Breeze_Speed),Average_Breeze_Speed:=mon_avgdist$Average_Breeze_Speed[test$matchingIndex[is.na(test$Average_Breeze_Speed)]]]

test[,matchingIndex :=NULL]



train$f4 <- train$f1*train$f2*train$f3

train$f4[train$f4 < 7] <- NA
train$f4[train$f4 > 24.1] <- NA



train[,Footfall :=NULL]
# train[is.na(train)] <- 0
# test[is.na(test)] <- 0

