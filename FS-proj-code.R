# before

data_before = read.csv("air_daily_return_2017_2019.CSV",header = F)
data_before =ts(data_before$V2)
data_before=data_before/100
data_before=log(data_before+1)

plot.ts(data_before,ylab="daily return",xlab="")
plot.ts(data_before^2,ylab="(daily return)^2",xlab="")

plot.ts(data_before,ylab="daily return",xlab="")
plot.ts(data_before^2,ylab="(daily return)^2",xlab="")

library(tseries)
adf.test(data_before)

library(TSA)
eacf=eacf(data_before,7,7)
eacf

library(forecast)
mod2=arima(data_before,order=c(0,0,3))
summary(mod2)
checkresiduals(mod2)

library(forecast)
automod2=auto.arima(data_before,ic="aic")
summary(automod2)
checkresiduals(automod2)

library(FinTS)
for (i in 1:5) print(ArchTest(data_before,lag=i))

fit=garch(data_before,order=c(0,1))
summary(fit)

library(TSA)
eacf(data_before^2,7,7)

fit1_1=garch(data_before,order=c(1,1))
summary(fit1_1)
checkresiduals(fit1_1)

pred=predict(fit1_1)
pred
plot(pred,xlab="")
plot(data_before,pch=8,,ylab = "daily return",xlab="")
lines(pred[,1],col=2)
lines(pred[,2],col=2)
abline(h=1.96*sd(data_before),col=4,lty=2)
abline(h=-1.96*sd(data_before),col=4,lty=2)

# after

data = read.csv("air_daily_return_2020_2021.CSV",header = F)
data=data/100
data=ts(data$V2)
data=log(data+1)

plot.ts(data,ylab="daily return",xlab="")
plot.ts(data^2,ylab="(daily return)^2",xlab="")

acf(data,main=" ",lag=100)
pacf(data,main=" ",lag=100)

acf(data^2,main=" ",lag=50)
pacf(data^2,main=" ",lag=50)

library(tseries)
adf.test(data)
adf.test(data^2)

library(TSA)
eacf(data,7,7)

library(forecast)
automod1=auto.arima(data,ic="aic")
summary(automod1)
checkresiduals(automod1)

fit_=Arima(data,order = c(0,0,1))
summary(fit_)
pred = forecast(fit_,h=60)
pred
plot(pred)
#plot(data,pch=8)
#lines(pred[,1],col=2)
#lines(pred[,2],col=2)
abline(h=1.96*sd(data),col=4,lty=2)
abline(h=-1.96*sd(data),col=4,lty=2)

library(FinTS)
for (i in 1:5) print(ArchTest(data,lag=i))















