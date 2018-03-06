rm(list=objects())
###############packages
library(nlme)
library(mgcv)
library(mgcViz)
library(gridExtra)

###############evaluation criteria
rmse<-function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=0))
}

mape<-function(y,ychap)#mean absolute percentage error (MAPE)#measure of prediction accuracy
{
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=2))
}


cor(data_train.rvDate[,c(2:ncol(data_train.rvDate))])

############################################################
############block Cross Validation for choosing k
############################################################
attach(data_train.rvDate)
names(data_train.rvDate)

univ<-function(k, block, bs)
{
  g<- gam(Daily.Max.1.hour.NO2.Concentration.ppb.~s(WIN_HIGH, k=k, bs=bs), data=data_train.rvDate[-block,])
  #g<- gam(Load~s(Temp, k=k, bs="cr") + s(Time, k=3) + s(NumWeek, k=20), data=data0[-block,])
  forecast<-predict(g, newdata=data_train.rvDate[block,])
  return(data_train.rvDate[block,]$Daily.Max.1.hour.NO2.Concentration.ppb.-forecast)# return the difference between the real values and the predicted ones
}
Nblock<-10
borne_block<-seq(1, nrow(data_train.rvDate), length=Nblock+1)%>%floor
block_list<-list()
l<-length(borne_block)
for(i in c(2:(l-1)))
{
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))

K<-c(3:20)
rmseK<-lapply(K, function(k){lapply(block_list, univ,k=k,bs='tp')%>%unlist%>%rmse} )
plot(K, rmseK, type='b', pch=20)

#It is not appropriate for gam
equation <- Daily.Max.1.hour.NO2.Concentration.ppb.~s(X0EM_HIGH,k=3)+s(X0EM_LOW,k=3)+s(HUM_HIGH,k=3)+VIS_HIGH+VIS_AVG+VIS_LOW+WIN_HIGH+WIN_AVG+PRECIP+MUSIC
bam_reg<- bam(equation, data=data_train.rvDate)
model=bam_reg
ychap.bam <- predict(model, newdata=data_train.rvDate[1,])
for(i in c(1:(nrow(data_train.rvDate)-1)))
{
  model <- bam.update(model, data=data_train.rvDate[i,])
  ychap.bam <- c(ychap.bam, predict(model, newdata=data_train.rvDate[i+1,]))
}

plot(ychap.bam[c(1:100)],type='l')
lines(data_train.rvDate$Daily.Max.1.hour.NO2.Concentration.ppb.[c(1:100)], col='red')
rmse(data_train.rvDate$Daily.Max.1.hour.NO2.Concentration.ppb.-ychap.bam)
mape(data_train.rvDate$Daily.Max.1.hour.NO2.Concentration.ppb.,ychap.bam)

