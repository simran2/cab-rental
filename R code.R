rm(list=ls())
#set working directory
setwd('C:/Users/simran.chhabra1/Desktop/extras/personal/Edwisor projects/Cab rental')

#check current working directory
getwd()

#read train and test data
train = read.csv('train_cab/train_cab.csv')
test = read.csv('test/test.csv')
str(train)
summary(train)
head(train)

#Type of fare is factor. It needs to be converted to numeric for further usage
train$fare_amount = as.numeric(levels(train$fare_amount))[train$fare_amount]
summary(train$fare_amount)

#Negative values of fare are not possible thus were converted to NA
train$fare_amount[train$fare_amount <0] = NA

#Less than 1 and greater than 6 passengers cannot travel at a time thus converted such passenger_count to NA
train$passenger_count[(train$passenger_count<1) | (train$passenger_count>6)] = NA
summary(train$passenger_count)

summary(train$pickup_latitude)
#There're few values with latitude greater than 90.Assigning such values to mean of the latitude.
train$pickup_latitude[(train$pickup_latitude > 90)] = mean(train$pickup_latitude)

#Fare depends on the distance travelled rather than latitude and longitude values.Below, I am creating a new variable distance_travelled in kms.
library(geosphere)
for(i in 1:nrow(train)){
  train$distance_travelled[i] = distm(c(train$pickup_longitude[i], train$pickup_latitude[i]), c(train$dropoff_longitude[i], train$dropoff_latitude[i]), fun = distHaversine)/1000
}

#Extracting the year, month, day and time values from pickup_datetime
for(i in 1:nrow(train)){
  train$Year[i] = format(as.POSIXlt(train$pickup_datetime[i], format = "%Y-%m-%d %H:%M:%S UTC"), "%Y")
  train$Month[i] = format(as.POSIXlt(train$pickup_datetime[i], format = "%Y-%m-%d %H:%M:%S UTC"), "%m")
  train$Day[i] = format(as.POSIXlt(train$pickup_datetime[i], format = "%Y-%m-%d %H:%M:%S UTC"), "%d")
  train$Hour[i] = format(as.POSIXlt(train$pickup_datetime[i], format = "%Y-%m-%d %H:%M:%S UTC"), "%H")
}

#Remove no more needed columns
drop_cols = c("pickup_datetime","pickup_longitude","pickup_latitude","dropoff_longitude","dropoff_latitude")
train = train[,!names(train) %in% drop_cols]
head(train)

#filling missing values in passenger count using mode since it will give us the frequent number of travellers.
pcount_freq = table(train$passenger_count)
mode_pcount = which.max(pcount_freq)
train$passenger_count[is.na(train$passenger_count)] = mode_pcount

#Since KNN is a time consuming process and there is less amount of missing data, we could use mean/median to compute null fare_amount. Median is popular with whole numbers and here, amount is float therefore using mean to compute the missing values.
train$fare_amount[is.na(train$fare_amount)] = mean(train$fare_amount, na.rm = TRUE)

boxplot(x=train$fare_amount)
boxplot(x=train$distance_travelled)

#Detect outliers
val = train$fare_amount[train$fare_amount%in%boxplot.stats(train$fare_amount)$out]
train = train[which(!train$fare_amount%in%val),]
val = train$distance_travelled[train$distance_travelled%in%boxplot.stats(train$distance_travelled)$out]
train = train[which(!train$distance_travelled%in%val),]

summary(train)
hist(as.numeric(train$Hour), breaks = 25, col='cornsilk', xlab='Hour', main='No. of cabs booked every hour starting midnight')
#Finding: Peek hours of cab booking is evening between 6-8

hist(as.numeric(train$Month), breaks = 13, col='cornsilk', xlab='Month', main='No. of cabs booked every month')
#Finding: More cabs are booked in the months 1-6 than in 7-12

hist(as.numeric(train$Day), breaks = 31, col='cornsilk', xlab='Day', main='No. of cabs booked on different days')
#Finding: Cab booking does not depend on what day of the month it is

scatter.smooth(y = train$fare_amount, x = train$Hour)
#Finding: Fares are almost independent of the time of travel

scatter.smooth(y = train$fare_amount, x = train$distance_travelled)
#Fares are linearly dependent on distance travelled

scatter.smooth(y = train$fare_amount, x = train$Year)
#Finding: Fares have increased over the past years

#ggplot2::ggplot(train, ggplot2::aes_string(train$distance_travelled, train$fare_amount))+ggplot2::geom_point()

train = train[which(!is.na(train$Year)),]
train[which(is.na(train)),]

train$Year = as.numeric(train$Year)
train$Month = as.numeric(train$Month)
train$Day = as.numeric(train$Day)
train$Hour = as.numeric(train$Hour)

require(caTools)
sample = sample.split(train, SplitRatio = 0.7)
train1 = subset(train, sample==TRUE)
test1 = subset(train, sample==FALSE)

train1_scaled = scale(train1)
test1_scaled = scale(test1)

#1) Linear Regression
#check for normality
hist(train$distance_travelled)
#check correlation
cor(train$distance_travelled, train$fare_amount)

lm_model = lm(data = data.frame(train1), fare_amount~.)
pred = predict(lm_model, data.frame(test1)[,2:7])
summary(lm_model)

library(DMwR)
regr.eval(test1[,1], pred, stats = c('mae','rmse'))
#mae: 1.65, rmse: 2.36,R2: 0.63

#2) Random Forest regression
library(randomForest)
fare.rf <- randomForest(fare_amount ~ ., data=train1, mtry=1,importance=TRUE, na.action=na.omit, ntree=100)
pred = predict(fare.rf, test1)

regr.eval(test1[,1], pred, stats = c('mae','rmse'))
y = test1[,1]
r2 = 1 - sum((y-pred)^2)/sum((y-mean(y))^2)
r2
#mae: 1.74, rmse: 2.36, R2: 0.58

#3) KNeighbors Regression
library(class)
library(FNN)

pred = knn.reg(train= train1[,2:7], test= test1[,2:7], y=train1[,1])
regr.eval(test1[,1], pred$pred, stats = c('mae','rmse'))
y = test1[,1]
r2 = 1 - sum((y-pred$pred)^2)/sum((y-mean(y))^2)
r2
#mae: 2.04, rmse: 2.77, R2: 0.42

# Linear regeressor is preferred as it gives minimum error and maximum R-squared

