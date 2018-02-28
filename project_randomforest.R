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

################random forest####
data_train_impute <- rfImpute(eq,ntree=500,data=data_train, importance=TRUE)
data_test_impute <- rfImpute(eq,ntree=500,data=data_test, importance=TRUE)
rf0 <- randomForest(eq,ntree=500,data=data_train_impute, importance=TRUE)
rf0.fitted <- predict(rf0,newdata=data_train_impute)
rf0.forecast <- predict(rf0,newdata=data_test_impute)
mape(data_train_impute$Daily.Max.1.hour.NO2.Concentration.ppb.,rf0.fitted)
mape(data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb.,rf0.forecast)

names(rf0)
plot(rf0$mse)

rf0$ntree
plot(rf0)  ###oob error

#########param¨¨tres propres de RF
#ntree: nb d'arbres
#mtry: nb de variables tir¨¦es au hasard de chaque split, par d¨¦faut: sqrt(p)
#replace: dans le bagging, avec ou sans replacement?
#sampsize: nb d'observations dans chaque sous tirage du bagging
#nodesize: taille min d'un noeud terminal
#maxnodes: nb max de noeuds terminaux de chaque arbre
#importance: calcul de l'importance des variables ou pas
#localImp?
#nPerm: pour le calcul de l'importance, nb de permuttation
#proximity: pour l'approche Knn
#...
mape.fitted <- list()
mape.forecast <- list()
rmse.fitted <- list()
rmse.forecast <- list()
rmse.oob <- list()
#ntree
ntest<-seq(1,1000,by=100)
rfTest<-lapply(ntest, function(n){randomForest(eq, ntree = n, data=data_train_impute)})

rfTest.fitted<-lapply(rfTest, predict, newdata=data_train_impute)
rfTest.forecast<-lapply(rfTest, predict, newdata=data_test_impute)

rmse.fitted$ntree<-lapply(rfTest.fitted, function(x){rmse(data_train_impute$Daily.Max.1.hour.NO2.Concentration.ppb.-x)})%>%unlist()
rmse.forecast$ntree<-lapply(rfTest.forecast, function(x){rmse(data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb.-x)})%>%unlist()%>%unlist()
rmse.oob$ntree <-  lapply(rfTest, function(x){sqrt(x$mse) %>% tail(, n=1) })%>%unlist()

plot(ntest, rmse.fitted$ntree,type='l', col='royalblue2', ylim=range(rmse.fitted,rmse.forecast))
lines(ntest, rmse.forecast$ntree, col='orangered2')
lines(ntest, rmse.oob$ntree, col='dark green')

mape.fitted$ntree<-lapply(rfTest.fitted, function(x){mape(data_train_impute$Daily.Max.1.hour.NO2.Concentration.ppb.,x)})%>%unlist()
mape.forecast$ntree<-lapply(rfTest.forecast, function(x){mape(data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb.,x)})%>%unlist()%>%unlist()

plot(ntest, mape.fitted$ntree,type='l', col='royalblue2', ylim=range(mape.fitted,mape.forecast))
lines(ntest, mape.forecast$ntree, col='orangered2')


#mtry
mtry<-c(1:8)
rfTest<-lapply(mtry, function(n){randomForest(eq, mtry = n, ntree=200, data=data_train_impute)})

rfTest.fitted<-lapply(rfTest, predict, newdata=data_train_impute)
rfTest.forecast<-lapply(rfTest, predict, newdata=data_test_impute)

rmse.fitted$mtry<-lapply(rfTest.fitted, function(x){rmse(data_train_impute$Daily.Max.1.hour.NO2.Concentration.ppb.-x)})%>%unlist()
rmse.forecast$mtry<-lapply(rfTest.forecast, function(x){rmse(data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb.-x)})%>%unlist()%>%unlist()
rmse.oob$mtry <-  lapply(rfTest, function(x){sqrt(x$mse) %>% tail(, n=1) })%>%unlist()

plot(mtry, rmse.fitted$mtry,type='l', col='royalblue2', ylim=range(rmse.fitted,rmse.forecast))
lines(mtry, rmse.forecast$mtry, col='orangered2')
lines(mtry, rmse.oob$mtry, col='dark green')

mape.fitted$mtry<-lapply(rfTest.fitted, function(x){mape(data_train_impute$Daily.Max.1.hour.NO2.Concentration.ppb.,x)})%>%unlist()
mape.forecast$mtry<-lapply(rfTest.forecast, function(x){mape(data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb.,x)})%>%unlist()
plot(mtry, mape.fitted$mtry,type='l', col='royalblue2', ylim=range(mape.fitted,mape.forecast))
lines(mtry, mape.forecast$mtry, col='orangered2')

#sampsize: nb d'observations dans chaque sous tirage du bagging
sampsize<-floor(seq(0.1,1,length=10)*nrow(data_train_impute))
rfTest<-lapply(sampsize, function(n){randomForest(eq, sampsize = n, ntree=200, mtry =6,data=data_train_impute)})

rfTest.fitted <-lapply(rfTest, predict, newdata=data_train_impute)
rfTest.forecast <-lapply(rfTest, predict, newdata=data_test_impute)


rmse.fitted$sampsize<-lapply(rfTest.fitted, function(x){rmse(data_train_impute$Daily.Max.1.hour.NO2.Concentration.ppb.-x)})%>%unlist()
rmse.forecast$sampsize<-lapply(rfTest.forecast, function(x){rmse(data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb.-x)})%>%unlist()%>%unlist()
rmse.oob$sampsize <-  lapply(rfTest, function(x){sqrt(x$mse) %>% tail(, n=1) })%>%unlist()

plot(sampsize, rmse.fitted$sampsize,type='l', col='royalblue2', ylim=range(rmse.fitted,rmse.forecast))
lines(sampsize, rmse.forecast$sampsize, col='orangered2')
lines(sampsize, rmse.oob$sampsize, col='dark green')

mape.fitted$sampsize<-lapply(rfTest.fitted, function(x){mape(data_train_impute$Daily.Max.1.hour.NO2.Concentration.ppb.,x)})%>%unlist()
mape.forecast$sampsize<-lapply(rfTest.forecast, function(x){mape(data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb.,x)})%>%unlist()
plot(sampsize, mape.fitted$sampsize,type='l', col='royalblue2', ylim=range(mape.fitted,mape.forecast))
lines(sampsize, mape.forecast$sampsize, col='orangered2')

#nodesize: taille min d'un noeud terminal
nodesize<-c(1:20)
rfTest<-lapply(nodesize, function(n){randomForest(eq, nodesize = n, ntree=200, mtry =6, sampsize=500, data=data_train_impute)})

rfTest.fitted<-lapply(rfTest, predict, newdata=data_train_impute)
rfTest.forecast<-lapply(rfTest, predict, newdata=data_test_impute)


rmse.fitted$nodesize<-lapply(rfTest.fitted, function(x){rmse(data_train_impute$Daily.Max.1.hour.NO2.Concentration.ppb.-x)})%>%unlist()
rmse.forecast$nodesize<-lapply(rfTest.forecast, function(x){rmse(data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb.-x)})%>%unlist()%>%unlist()
rmse.oob$nodesize <-  lapply(rfTest, function(x){sqrt(x$mse) %>% tail(, n=1) })%>%unlist()

plot(nodesize, rmse.fitted$nodesize,type='l', col='royalblue2', ylim=range(rmse.fitted,rmse.forecast))
lines(nodesize, rmse.forecast$nodesize, col='orangered2')
lines(nodesize, rmse.oob$nodesize, col='dark green')

mape.fitted$nodesize<-lapply(rfTest.fitted, function(x){mape(data_train_impute$Daily.Max.1.hour.NO2.Concentration.ppb.,x)})%>%unlist()
mape.forecast$nodesize<-lapply(rfTest.forecast, function(x){mape(data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb.,x)})%>%unlist()
plot(nodesize, mape.fitted$nodesize,type='l', col='royalblue2', ylim=range(mape.fitted,mape.forecast))
lines(nodesize, mape.forecast$nodesize, col='orangered2')

boxplot(rmse.forecast)

boxplot(mape.forecast)

##################importance
set.seed(100)
rf0<-randomForest(eq,  data=data_train_impute, sampsize=700, mtry=8, ntree=300, nodesize=1, importance=TRUE)
imp<-varImpPlot(rf0, type=1)

o<-order(imp,decreasing=TRUE)
plot(imp[o],type='b',pch=20, axes=F, xlab="", ylab='importance')
axis(1,c(1:length(imp)), rownames(imp)[o], las=2, cex=0.5)
axis(2)


###############################some graphs
o<-order(imp,decreasing=TRUE)
rownames(imp)[o]

par(mfrow=c(3,2))
partialPlot(rf0,pred.data=data_train_impute,x.var="WIN_AVG")
partialPlot(rf0,pred.data=data_train_impute,x.var="DEW_LOW")
partialPlot(rf0,pred.data=data_train_impute,x.var="HUM_LOW")
partialPlot(rf0,pred.data=data_train_impute,x.var="DEW_AVG")
partialPlot(rf0,pred.data=data_train_impute,x.var="TEM_LOW")
partialPlot(rf0,pred.data=data_train_impute,x.var="TEM_HIGH")

######
rmse(forecast-data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb.)
mape(forecast,data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb.)

par(mfrow=c(1,1))
forecast<-predict(rf0, data_test_impute)
plot(data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb., type='l',col='red')
lines(forecast, type = 'l', col='blue')