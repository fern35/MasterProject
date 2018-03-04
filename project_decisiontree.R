rm(list=objects())
###############packages
library(lubridate)
library(rpart)
library(tree)
library(plotmo)
library(rpart.plot)
library(caret)
library(party)
library(randomForest)
library(Rborist)
library(magrittr)
rmse<-function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=0))
}

mape<-function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=10))
}


###############Import data
#setwd("D:/m2/projet datamining")
#C:\Enseignement\2015-2016\Projet Data Mining\TP\Regression
data0<-read.csv(file="D:/m2/projet datamining/ad_viz_plotval_data_no2_texas_austin2015.csv", header=TRUE, sep=",")
data1<-read.csv(file="D:/m2/projet datamining/ad_viz_plotval_data_no2_texas_austin2016.csv", header=TRUE, sep=",")
data_train <- rbind(data0, data1)

data_test<-read.csv(file="D:/m2/projet datamining/ad_viz_plotval_data_no2_texas_austin2017.csv", header=TRUE, sep=",")

data_train$Date <- as.Date(data_train$Date, "%m/%d/%Y")
data_test$Date <- as.Date(data_test$Date, "%m/%d/%Y")

data_train$Weekday <- wday(data_train$Date)
data_test$Weekday <- wday(data_test$Date)

data_train$VIS_HIGH = as.numeric(as.character(data_train$VIS_HIGH))
data_train$VIS_AVG = as.numeric(as.character(data_train$VIS_AVG))
data_train$VIS_LOW = as.numeric(as.character(data_train$VIS_LOW))
data_test$VIS_HIGH = as.numeric(as.character(data_test$VIS_HIGH))
data_test$VIS_AVG = as.numeric(as.character(data_test$VIS_AVG))
data_test$VIS_LOW = as.numeric(as.character(data_test$VIS_LOW))

data_train$PRECIP<-as.numeric(levels(data_train$PRECIP))[data_train$PRECIP]
data_test$PRECIP<-as.numeric(levels(data_test$PRECIP))[data_test$PRECIP]

eq<-Daily.Max.1.hour.NO2.Concentration.ppb. ~ TEM_HIGH + TEM_AVG +  TEM_LOW + DEW_HIGH + DEW_AVG + DEW_LOW + HUM_HIGH + HUM_AVG + HUM_LOW + SLP_HIGH + SLP_AVG + SLP_LOW + VIS_HIGH + VIS_AVG + VIS_LOW + WIN_HIGH + WIN_HIGH + WIN_AVG + PRECIP + MUSIC + Weekday

data_train_impute <- rfImpute(eq,ntree=500,data=data_train, importance=TRUE)
data_test_impute <- rfImpute(eq,ntree=500,data=data_test, importance=TRUE)
####rpart
data_app<-data_train_impute
rpart0<- rpart(eq, data = data_app,
               control=c(maxsurrogate=6))
rpart0.forecast<-predict(rpart0,newdata=data_test_impute)
mape(data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb.,rpart0.forecast)
rmse(data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb.-rpart0.forecast)

plot(data_test$Date, data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb., type='l',col='red')
lines(data_test$Date, rpart0.forecast, type = 'l', col='blue')
