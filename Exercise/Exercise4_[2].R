#EX_2
X<-2+arima.sim(n=200, list(order=c(1,0,1), ar=0.3, ma=0.5))
ts.plot(X)

Arima(X, order=c(1,0,1))

plot(forecast(Arima(X, order=c(1,0,1)), h=30))
