#EX_4
X<-arima.sim(n=200, list(order=c(1,1,0), ar=0.3))[1:200]
ts.plot(X)
Y<-1000+X
ts.plot(Y)

library(forecast)
Arima(X,order=c(1,1,0))
Arima(Y,order=c(1,1,0))

#Y_t에 대한 잔차분석
Y.fit<-Arima(Y,order=c(1,1,0))
r<-Y.fit$res
ts.plot(r)
acf(r)
pacf(r)
Box.test(r, lag=12, type="Ljung-Box")

#예측
Y.fore<-forecast(Y.fit, h=30)
plot(Y.fore)
