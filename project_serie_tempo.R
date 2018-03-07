rm(list=objects())
###############packages
library(forecast)
library(RColorBrewer)
library(magrittr)
library(lubridate)


rmse<-function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=0))
}

mape<-function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=10))
}


###############Import data
data0<-read.csv(file="D:/m2/projet datamining/ad_viz_plotval_data_no2_texas_austin2015.csv", header=TRUE, sep=",")
data1<-read.csv(file="D:/m2/projet datamining/ad_viz_plotval_data_no2_texas_austin2016.csv", header=TRUE, sep=",")
data_train <- rbind(data0, data1)
data_test<-read.csv(file="D:/m2/projet datamining/ad_viz_plotval_data_no2_texas_austin2017.csv", header=TRUE, sep=",")

data_train$Date <- as.Date(data_train$Date, "%m/%d/%Y")
data_test$Date <- as.Date(data_test$Date, "%m/%d/%Y")

data_train$Weekday <- wday(data_train$Date)
data_test$Weekday <- wday(data_test$Date)

# Date <- paste(data0$Day, data0$Month,data0$Year, sep='-')
# as.POSIXct(strptime(Date, "%d-%m-%Y"))
# 
# data0$Date<-as.POSIXct(strptime(data0$Date, "%Y-%m-%d"))
# data1$Date<-as.POSIXct(strptime(data1$Date, "%Y-%m-%d"))
# data0$BH1<-as.factor(data0$BH1)
# data1$BH1<-as.factor(data1$BH1)


rollArima<-function (arima.model, ynew, horizon = 1) 
{
  prevARIMA <- array(0, dim = length(ynew))
  prevARIMA[1] <- forecast(arima.model, h = horizon)$mean[horizon]
  for (i in 1:(length(ynew) - 1)) {
    ts2 <- c(arima.model$x, ynew[1:i])
    refit <- Arima(ts2, model = arima.model)
    prevARIMA[i + 1] <- forecast(refit, h = horizon)$mean[horizon]
  }
  return(prevARIMA)
}


rollHW <- function (hw.model, ynew, horizon = 1) 
{
  prevHW <- array(0, dim = length(ynew))
  prevHW[1] <- forecast(hw.model, h = horizon)$mean[horizon]
  refit <- hw.model
  for (i in 1:(length(ynew) - 1)) {
    if(hw.model$gamma==T)
    {
      ts2 <- ts(c(hw.model$x, ynew[1:i]), frequency = frequency(fit.hw$x))
      refit <- HoltWinters(ts2, seasonal=hw.model$seasonal,l.start=refit$coefficients["a"], b.start=refit$coefficients["b"]
                           , s.start=refit$coefficients[grep("s",names(fit.hw$coefficients))]
                           , optim.start = c(alpha = refit$alpha, beta = refit$beta, gamma = refit$gamma))
    }
    
    else{
      ts2 <- ts(c(hw.model$x, ynew[1:i]), frequency = frequency(fit.hw$x))
      refit <- HoltWinters(ts2,l.start=refit$coefficients["a"], b.start=refit$coefficients["b"]
                           , optim.start = c(alpha = refit$alpha, beta = refit$beta, gamma = F))
    }
    prevHW[i + 1] <- forecast(refit, h = horizon)$mean[horizon]
  }
  return(prevHW)
}



rollETS <- function (ets.model, ynew, horizon = 1) 
{
  prevETS <- array(0, dim = length(ynew))
  prevETS[1] <- forecast(ets.model, h = horizon)$mean[horizon]
  for (i in 1:(length(ynew) - 1)) {
    ts2 <- ts(c(ets.model$x, ynew[1:i]), frequency=frequency(ets.model$x))
    refit <- ets(ts2, model = ets.model)
    prevETS[i + 1] <- forecast(refit, h = horizon)$mean[horizon]
  }
  return(prevETS)
}


#####################################################################
#######################time series: SARIMA model
#####################################################################

ts<-ts(data_train$Daily.Max.1.hour.NO2.Concentration.ppb., frequency=7) 
plot(data_train$Date, ts, type = 'l')

par(mfrow=c(1,2))
acf(ts)
pacf(ts)

par(mfrow=c(1,2))
acf(diff(ts), lag.max=7*3)
pacf(diff(ts), lag.max=7*3)

par(mfrow=c(1,2))
acf(diff(diff(ts), lag=7), lag.max=7*3)
pacf(diff(diff(ts), lag=7), lag.max=7*3)

#Pmax=2
#Qmax=1

par(mfrow=c(1,2))
acf(diff(diff(ts), lag=7), lag.max=20)
pacf(diff(diff(ts), lag=7), lag.max=20)

###pmax= 4 ou 11
###qmax=3
fit1 <- Arima(ts, order=c(4,1,3), seasonal=c(2,1,1), method=c("CSS"))
prevARIMA1<-rollArima(fit1,ynew=data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,horizon=1)
mape(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,prevARIMA1)

fit.arima <- auto.arima(ts,max.p=11,max.q=3, max.P=2, max.Q=1, trace=T,start.p=7,start.q=2,ic="aic")
prevARIMA<-rollArima(fit.arima,ynew=data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,horizon=1)

mape(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,prevARIMA)

fit.arimaBIC<- auto.arima(ts,max.p=7,max.q=7, max.P=8, max.Q=8, trace=T,start.p=7,start.q=2,ic="bic")
prevARIMABIC<-rollArima(fit.arimaBIC,ynew=data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,horizon=1)
mape(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,prevARIMABIC)



#####################################################################
#######################exponential smoothing
#####################################################################

####################################
####simple, non saisonnier
####################################
ts<-ts(data_train$Daily.Max.1.hour.NO2.Concentration.ppb.) 
fit.ets<-ets(ts)
prevETS<-rollETS(fit.ets,ynew=data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,horizon=1)
mape(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,prevETS)

####################################
####double, non saisonnier
####################################
ts<-ts(data_train$Daily.Max.1.hour.NO2.Concentration.ppb., frequency=7)##frequency is necessary 
fit.hw<-HoltWinters(ts, gamma=FALSE)                                   ##to make prevHW run
fit.hw$gamma
prevHW<-rollHW(fit.hw,ynew=data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,horizon=1)
mape(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,prevHW)


fit.hw_mu<-HoltWinters(ts,seasonal="multiplicative")
fit.hw_mu$gamma
prevHW_mu<-rollHW(fit.hw_mu,ynew=data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,horizon=1)
mape(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,prevHW_mu)



par(ask=F)
par(mfrow=c(1,1))
col.pal<-brewer.pal(4, "Spectral")
plot(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,type='l')
lines(prevARIMA,col=col.pal[1])
lines(prevETS,col=col.pal[2])
lines(prevHW,col=col.pal[3])
lines(prevHW_mu,col=col.pal[4])
legend("topright",col=col.pal,legend=c("ARIMA","ETS","HW","HW_mu"),lty=1, cex =0.75)



# plot(cumsum(data1$Load-prevARIMA),type='l',col=col.pal[1])
# lines(cumsum(data1$Load-prevETS),col=col.pal[2])
# lines(cumsum(data1$Load-prevHW),col=col.pal[3])
# lines(cumsum(data1$Load-prevHW_mu),col=col.pal[4])
# legend("topleft",col=col.pal,legend=c("ARIMA","ETS","HW","HW_mu"),lty=1)



mape(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,prevARIMA)
mape(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,prevARIMABIC)
mape(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,prevETS)
mape(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,prevHW)
mape(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,prevHW_mu)

rmse(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.-prevARIMA)
rmse(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.-prevARIMABIC)
rmse(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.-prevETS)
rmse(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.-prevHW)
rmse(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.-prevHW_mu)
# time_series<-list()
# time_series$prevARIMA<-prevARIMA
# time_series$prevHW<-prevHW
# time_series$prevETS<-prevETS
# saveRDS(time_series,"C:\\Enseignement\\2016-2017\\Results\\time_series_models.RDS")
# 

###############################################################
#########randomforest +arima correction models
###############################################################

# noel = which(abs(data0$Day - 24) <= 3 & data0$Month == 12)
# consoNoel = vector("numeric", length(data0$Time))
# consoNoel[noel] = 1
# data0 <- data.frame(data0, consoNoel)
# noel = which(abs(data1$Day - 24) <= 3 & data1$Month == 12)
# consoNoel = vector("numeric", length(data1$Time))
# consoNoel[noel] = 1
# data1 <- data.frame(data1, consoNoel)
library(randomForest)
eq<-Daily.Max.1.hour.NO2.Concentration.ppb. ~ TEM_HIGH + TEM_AVG +  TEM_LOW + DEW_HIGH + DEW_AVG + DEW_LOW + HUM_HIGH + HUM_AVG + HUM_LOW + SLP_HIGH + SLP_AVG + SLP_LOW + VIS_HIGH + VIS_AVG + VIS_LOW + WIN_HIGH + WIN_HIGH + WIN_AVG + PRECIP + MUSIC + Weekday

data_train$VIS_HIGH = as.numeric(as.character(data_train$VIS_HIGH))
data_train$VIS_AVG = as.numeric(as.character(data_train$VIS_AVG))
data_train$VIS_LOW = as.numeric(as.character(data_train$VIS_LOW))
data_test$VIS_HIGH = as.numeric(as.character(data_test$VIS_HIGH))
data_test$VIS_AVG = as.numeric(as.character(data_test$VIS_AVG))
data_test$VIS_LOW = as.numeric(as.character(data_test$VIS_LOW))

data_train$PRECIP<-as.numeric(levels(data_train$PRECIP))[data_train$PRECIP]
data_test$PRECIP<-as.numeric(levels(data_test$PRECIP))[data_test$PRECIP]

data_train_impute <- rfImpute(eq,ntree=500,data=data_train, importance=TRUE)
data_test_impute <- rfImpute(eq,ntree=500,data=data_test, importance=TRUE)
rf0<-randomForest(eq,  data=data_train_impute, sampsize=700, mtry=8, ntree=300, nodesize=1, importance=TRUE)
rf0.fitted <- predict(rf0,newdata=data_train_impute)
rf0.forecast <- predict(rf0,newdata=data_test_impute)
mape(data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb.,rf0.forecast)
rmse(data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb.-rf0.forecast)

rf0$residuals <- data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb.- rf0.forecast
par(mfrow=c(2,1))
acf(rf0$residuals)
pacf(rf0$residuals)

Nblock<-10
borne_block<-seq(1, nrow(data0), length=Nblock+1)%>%floor
block_list<-list()
l<-length(borne_block)
for(i in c(2:(l-1)))
{
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))


res.bloc<-function(block, formula)
{
  
  rf<- randomForest(formula, data=data_train_impute[-block,])
  forecast<-predict(rf, data=data_train_impute[block,])
  return(data_train_impute[block,]$Daily.Max.1.hour.NO2.Concentration.ppb.-forecast)
}

block= block_list[[1]]
res.cv <- lapply(block_list, res.bloc, formula=eq)%>%unlist

par(mfrow=c(2,1))
acf(res.cv)
pacf(res.cv)

fit.arima.res <- auto.arima(res.cv,max.p=4,max.q=12, max.P=0, max.Q=0, trace=T,ic="aic")
#res<-data_test$Daily.Max.1.hour.NO2.Concentration.ppb.-rf0.forecast
prevARIMA.res<-rollArima(fit.arima.res,ynew=data_test$Daily.Max.1.hour.NO2.Concentration.ppb.-rf0.forecast,horizon=1)

prevRF_ARIMA <- rf0.forecast+prevARIMA.res
mape(data_test$Daily.Max.1.hour.NO2.Concentration.ppb., prevRF_ARIMA)
rmse(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.-prevRF_ARIMA)


####################################################
##########################RF+ETS
####################################################
ts<-ts(res.cv) 
fit.ets<-ets(ts)
prevETS<-rollETS(fit.ets,ynew=data_test_impute$Daily.Max.1.hour.NO2.Concentration.ppb.-rf0.forecast,horizon=1)
prevRF_ETS <- rf0.forecast+prevETS
mape(data_test$Daily.Max.1.hour.NO2.Concentration.ppb., prevRF_ETS)
rmse(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.-prevRF_ETS)



####




 

df0 <- data.frame(ds=data_train$Date, y= data_train$Daily.Max.1.hour.NO2.Concentration.ppb.)
df1 <- data.frame(ds=data_test$Date, y= data_test$Daily.Max.1.hour.NO2.Concentration.ppb.)

proph <- prophet(df = df0)
proph$forecast <- predict(proph, df1)$yhat
mape(data_test$Daily.Max.1.hour.NO2.Concentration.ppb., proph$forecast)

plot(data_test$Daily.Max.1.hour.NO2.Concentration.ppb., type='l')
lines(proph$forecast, col='red')


proph$weekly.seasonality
names(proph)

#####

par(ask=F)
par(mfrow=c(1,1))
col.pal<-brewer.pal(4, "Spectral")
plot(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,type='l')
lines(prevARIMA,col=col.pal[1])
lines(prevHW_mu,col=col.pal[2])
lines(prevRF_ARIMA,col=col.pal[3])
lines(prevRF_ETS,col=col.pal[4])
legend("topright",col=col.pal,legend=c("ARIMA", "HW_mu", "RF+ARIMA", "RF_ETS"),lty=1, cex =0.75)
##

par(ask=F)
par(mfrow=c(1,1))
plot(data_test$Date, abs(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.-prevRF_ARIMA)/data_test$Daily.Max.1.hour.NO2.Concentration.ppb., type = 'l')
lines(data_test$Date, abs(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.-prevARIMA)/data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,col='red')
lines(data_test$Date, abs(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.-prevHW_mu)/data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,col='blue')
lines(data_test$Date, abs(data_test$Daily.Max.1.hour.NO2.Concentration.ppb.-prevRF_ETS)/data_test$Daily.Max.1.hour.NO2.Concentration.ppb.,col='yellow')
legend("topright",col=c('red', 'blue', 'yellow'),legend=c("ARIMA", "HW_mu", "RF_ETS"),lty=1, cex =0.75)









