#실습 6-1
library(TSA)
data(co2)
co2<-ts(co2)

#모형 식별
acf(co2,lag.max=48)

ts.plot(Dc<-diff(co2, 12))

layout(t(1:2))
acf(Dc); pacf(Dc)

ts.plot(dDc<-diff(Dc))

layout(t(1:2))
acf(dDc, lag.max = 36); pacf(dDc, lag.max = 36)

library(forecast)
aic<-1000
for(p in 0:2) for(q in 0:2)for(P in 0:1) for(Q in 0:1){
       aic.tmp<-AIC(Arima(co2, order=c(p,1,q), seasonal = list(order = c(P, 1,Q), period = 12)))
       if(aic.tmp<aic){
             aic<-aic.tmp
             cat("p=",p, "q=",q, "P=",P,"Q=",Q," AIC=",aic ,"\n")
           }
    } 

#잔차분석   
co2.fit<-Arima(co2, order=c(0,1,1), seasonal = list(order = c(0, 1, 1), period = 12))
r<-co2.fit$res
layout(matrix(c(1,2,1,3),2,2))
ts.plot(r); acf(r); pacf(r)

# 잔차의 자기상관계수가 12시차까지 0이라고 할 수 있는가? 
Box.test(r, lag=12, type="Ljung") 
# 잔차가 이 모형에서의 잔차라고 할 수 있는가?
Box.test(r, lag=12, type="Ljung", fitdf=1+1)

#모수 추정결과
co2.fit
hat.co2<-co2.fit$fitted # 적합값(추정된 모형에 의해서 계산된 값)
ts.plot(co2)
lines(hat.co2, col="red", lty=2) # 적합값 그래프

#예측
forecast(co2.fit, h=12)