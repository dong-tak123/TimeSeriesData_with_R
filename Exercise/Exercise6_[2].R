#실습 6-2
data("AirPassengers")
AP<-AirPassengers
ts.plot(AP)
ts.plot(lAP<-log(AP))

#모형 식별
acf(lAP,lag.max=48)
ts.plot(D.lAP<-diff(lAP, 12))

layout(t(1:2))
acf(D.lAP )
pacf(D.lAP)

ts.plot(dD.lAP<-diff(D.lAP))

layout(t(1:2))
acf(dD.lAP, lag.max = 36); pacf(dD.lAP, lag.max = 36)

library(forecast)
aic<-1000
for(p in 0:2) for(q in 0:2)for(P in 0:2) for(Q in 0:2){
       try({aic.tmp<-AIC(Arima(lAP, order=c(p,1,q), seasonal = list(order = c(P, 1,Q), period = 12)))}, TRUE)
       if(aic.tmp<aic){
             aic<-aic.tmp
             cat("p=",p, "q=",q, "P=",P,"Q=",Q," AIC=",aic ,"\n")
            }
     } 

#잔차분석
library(forecast)
lAP.fit<-Arima(lAP, order=c(0,1,1), seasonal = list(order = c(0, 1, 1), period = 12))
r<-lAP.fit$res
layout(matrix(c(1,2,1,3),2,2))
ts.plot(r); acf(r); pacf(r)

# 잔차의 자기상관계수가 12시차까지 0이라고 할 수 있는가? 
Box.test(r, lag=12, type="Ljung") 

#모수 추정 결과
lAP.fit

hat.lAP<-lAP.fit$fitted # 적합값(추정된 모형에 의해서 계산된 값)
ts.plot(AP)
lines(exp(hat.lAP), col="red", lty=2) 

#예측
plot(forecast(lAP.fit, h=24))

lAP.fore<-forecast(lAP.fit, h=24)
AP.mean<-ts(exp(lAP.fore$mean), start=1961, freq=12)
AP.U95<-ts(exp(lAP.fore$upper[,2]), start=1961, freq=12)
AP.L95<-ts(exp(lAP.fore$lower[,2]), start=1961, freq=12)
ts.plot(AP, xlim=c(1949,1960+3), ylim=c(80,max(AP.U95)+50))
lines(AP.mean, col="blue")
lines(AP.U95, col="blue", lty=2)
lines(AP.L95, col="blue", lty=2)

#참고 : auto.arima
auto.arima(lAP)

plot(forecast(auto.arima(lAP), h=24))

lAP.fore<-forecast(auto.arima(lAP),h=24)
AP.mean<-ts(exp(lAP.fore$mean), start=1961, freq=12)
AP.U95<-ts(exp(lAP.fore$upper[,2]), start=1961, freq=12)
AP.L95<-ts(exp(lAP.fore$lower[,2]), start=1961, freq=12)
ts.plot(AP, xlim=c(1949,1960+3), ylim=c(80,max(AP.U95)+50))
lines(AP.mean, col="blue")
lines(AP.U95, col="blue", lty=2)
lines(AP.L95, col="blue", lty=2)
