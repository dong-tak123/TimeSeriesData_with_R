#Ex_5
tt<-1:200
Y<-100+0.2*tt+X
ts.plot(Y)

Y.fit<-Arima(Y,include.drift=T, order=c(1,1,0))
Y.fit

#cf
Arima(diff(Y), order=c(1,0,0))

#�����м�
r<-Y.fit$res
ts.plot(r)
acf(r)
pacf(r)
Box.test(r, lag=12, type="Ljung-Box")

#����
plot(forecast(Y.fit,h=30))