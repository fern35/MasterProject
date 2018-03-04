######### library
library(dygraphs)
library(xts)
library(MASS)
library(caret)
library(lubridate)
library(glmnet)
######### criterion definition
rmse<-function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=0))
}

mape<-function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=2))
}

##########load data
data2015<-read.csv(file="/Users/zhangyuan/Documents/Workspace/RWorkspace/Project/data/ad_viz_plotval_data_no2_texas_austin2015.csv", header=TRUE, sep=",")
names(data2015)
plot(data2015$Daily.Max.1.hour.NO2.Concentration.ppb.,type='l')

data2016<-read.csv(file="/Users/zhangyuan/Documents/Workspace/RWorkspace/Project/data/ad_viz_plotval_data_no2_texas_austin2016.csv", header=TRUE, sep=",")
data_train <- rbind(data2015, data2016)

data_test<-read.csv(file="/Users/zhangyuan/Documents/Workspace/RWorkspace/Project/data/ad_viz_plotval_data_no2_texas_austin2017.csv", header=TRUE, sep=",")

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

#remove AQI
data_train=data_train[,-c(3,4,5)]
data_test=data_test[,-c(3,4,5)]
#remove the rows with NA
data_train.rvNA=data_train[rowSums(is.na(data_train)) == 0 ,]
data_test.rvNA=data_test[rowSums(is.na(data_test)) == 0 ,]
#remove Date
data_test.rvDate=data_test.rvNA[c(2:ncol(data_test.rvNA))]
data_train.rvDate=data_train.rvNA[c(2:ncol(data_train.rvNA))]

#lasso
data_train.matrix = data.matrix(data_train.rvDate[,c(2:ncol(data_train.rvDate))])
data_test.matrix = data.matrix(data_test.rvDate[,c(2:ncol(data_test.rvDate))])
lasso = glmnet(data_train.matrix, data_train.rvDate[,1], alpha = 1)
y_predict_lasso=predict(lasso, data_test.matrix)
error_lasso=y_predict_lasso-data_test.rvDate[,1]
rmse_lasso=apply(error_lasso,2,rmse)
mape_lasso=apply(y_predict_lasso,2,mape,ychap=data_test.rvDate[,1])
which.min(mape_lasso)
min(rmse_lasso)
plot(y_predict_lasso[,15],col='red',type = 'l')
lines(data_test.rvDate[,1],col='blue')


#ridge
ridge = glmnet(data_train.matrix, data_train.rvDate[,1], alpha = 0)
y_predict_ridge=predict(ridge, data_test.matrix)
error_ridge=y_predict_ridge-data_test.rvDate[,1]
rmse_ridge=apply(error_ridge,2,rmse)
mape_ridge=apply(y_predict_lasso,2,mape,ychap=data_test.rvDate[,1])
which.min(mape_ridge)
min(rmse_ridge)
plot(data_test.rvDate[,1],col='blue',type = 'l')
lines(y_predict_ridge[,15],col='red')




