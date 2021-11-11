#EX_1
X<-arima.sim(n=200, list(order=c(1,0,0), ar=0.7))
ts.plot(X)

#모수추정
library(forecast)
Arima(X,order=c(1,0,0))
Arima(X,order=c(1,0,0), include.mean=F)
Arima(X,order=c(1,0,0), fixed=c(NA,0))

#잔차분석
X.fit<-Arima(X,order=c(1,0,0), include.mean=F)
r<-X.fit$res 
ts.plot(r)

acf(r)
pacf(r)
Box.test(r, lag=12, type="Ljung-Box")

#참고
Box.test(r, lag=12, type="Ljung-Box", fitdf=1)

#최종모형
X.fit
ts.plot(X)
hat.X<-X.fit$fitted
lines(hat.X, col="red")

#예측
forecast(X.fit, h=5)
X.fore<-forecast(X.fit, h=30)
plot(X.fore)
