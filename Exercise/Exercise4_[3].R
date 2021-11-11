#EX_3

tt<-1:200
X<-1+0.2*tt+arima.sim(n=200, list(order=c(1,0,1), ar=0.3, ma=0.5))
ts.plot(X)

#모수추정
Arima(X, include.drift=T, order=c(1,0,1))

#잔차분석
X.fit<-Arima(X, order=c(1,0,1), include.drift=T)
hat.eta<-X.fit$res
ts.plot(hat.eta)

acf(hat.eta)
pacf(hat.eta)

Box.test(hat.eta, lag=12, type="Ljung-Box")

#예측
X.fore<-forecast(X.fit, h=30)
plot(X.fore)