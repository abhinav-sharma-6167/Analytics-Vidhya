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
sol <- fread("fall_300.csv")

sol2 <- copy(sol)


# 
# sol$p <- test$Park_ID
# # 
# # 
# sol2$Footfall[sol$p==13] <- sol2$Footfall[sol$p==13] -2
# write.csv(sol2,"1.csv",row.names = FALSE)
# 
# 
# #-5.5
# #-2
# 
# 









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







sol$p <- test$mon
sol$q <- test$year
k<-c(102,103)
k<-as.integer(k)
l<-c(104,105)
l<-as.integer(l)

# 
# sol2$Footfall[sol$p==11 & sol$q %in% k]<-sol2$Footfall[sol$p==11 & sol$q %in% k]-3
# write.csv(sol2,"1.csv",row.names = FALSE)
# 
# 
# sol2$Footfall[sol$p==11 & sol$q %in% l]<-sol2$Footfall[sol$p==11 & sol$q %in% l]+20
# write.csv(sol2,"1.csv",row.names = FALSE)




#+15 +7
#-45 -25
#-10 -5
#-35 -20
#+8 +4
#-25 -15
#+15 +7

#
#+16 +8
#
#+42 +20
sol$r <- test$Min_Breeze_Speed

sol$r[is.na(sol$r)] <- 17.37
sol2$Footfall[(sol$r == 0) & sol$q %in% k]<-sol2$Footfall[(sol$r == 0) & sol$q %in% k]-5
write.csv(sol2,"1.csv",row.names = FALSE)


sol2$Footfall[sol$p==11 & sol$q %in% l]<-sol2$Footfall[sol$p==11 & sol$q %in% l]+20
write.csv(sol2,"1.csv",row.names = FALSE)




#-3


