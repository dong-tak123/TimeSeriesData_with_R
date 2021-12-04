setwd("C:/Users/liver/Downloads/20212시계열(송준모)/Exercise/실습8")
exchange<-scan("exchange.txt")
ts.plot(exchange)

#log 차분 변환
ts.plot(log.r<-diff(log(exchange))*100)
layout(t(1:2))
acf(log.r); pacf(log.r)

layout(t(1:2))
acf(log.r^2); pacf(log.r^2)

#모형 식별
library(forecast)
get.arma.pq<-function(X, p.max=3,q.max=3){
   aic<-aic.tmp<-10000
   for(p in 0:p.max)for(q in q.max){
     aic.tmp<-AIC(Arima(X,order=c(p,0,q)))
     if(aic.tmp<aic){
       aic<-aic.tmp
       cat("p=",p,"q=",q,"AIC=",aic,"\n")
     }
   }
 }
get.arma.pq(log.r)

Arima(log.r, order=c(2,0,3))

#e_t에 대한 GARCH 모형 식별
fit.ARMA<-Arima(log.r, order=c(2,0,3))
e<-fit.ARMA$res
layout(matrix(c(1,1,2,3,4,5),3,2,byrow=T))
ts.plot(e)
acf(e);pacf(e)
acf(e^2); pacf(e^2)

#e_t는 WN와 같은 형태인데, e_t의 제곱이 종속관계있음..
#e_t를 GARCH(p,q)로 적합..
library(tseries)
get.garch.pq<-function(X, p.max=2, q.max=2){
   options(warn=-1)
   aic<-aic.tmp<-10000
   for( p in 0:p.max)for( q in 0:q.max){
    try(aic.tmp<-AIC(garch(X, order=c(q,p), trace=F)), T)
     if(aic.tmp<aic){
       aic<-aic.tmp
       cat("p=",p,"q=",q,"AIC=",aic,"\n")
     }
   }
   options(warn=0)
}
 
get.garch.pq(e)
#AIC기준으로 잠정모형 선택
fit.GARCH<-garch(e, order=c(2,1), trace=F)
summary(fit.GARCH)

fit.GARCH<-garch(e, order=c(1,1), trace=F)
summary(fit.GARCH)

#잔차분석
eta<-fit.GARCH$residuals
eta<-eta[!is.na(eta)] 
layout(matrix(c(1,1,2,3,4,5),3,2,byrow=T))
ts.plot(eta)
acf(eta);pacf(eta)
acf(eta^2); pacf(eta^2)

#최종모형 확정
fit.ARMA
fit.GARCH

#예측
library(forecast)
plot(forecast(Arima(log.r, order=c(2,0,3)),h=50))

fore.arma<-forecast(Arima(log.r,order=c(2,0,3)),h=50)$mean

get.con.var<-function(X,h){
   fit<-garch(X, order=c(1,1), trace=F)
   hat.w<-fit$coef[1]
   hat.a<-fit$coef[2]
   hat.b<-fit$coef[3]
   hat.sigma2<-fit$fitted.values[,1]^2
   n<-length(hat.sigma2)
   
   con.var<-rep(0,h)
   hat.sig2_1<-hat.w+hat.a*X[n]^2+hat.b*hat.sigma2[n]
   con.var[1]<-hat.sig2_1
   
   for(i in 2:h)con.var[i]<-hat.w+(hat.a+hat.b)*con.var[i-1]
   con.var
 }
 
hat.sigma<-get.con.var(e,50)

ts.plot(log.r, xlim=c(1,1484+50))
fore<-ts(fore.arma, st=1485)
U95<-fore+1.96*hat.sigma
L95<-fore-1.96*hat.sigma
lines(fore, col="blue")
lines(U95, col="blue",lty=2)
lines(L95, col="blue",lty=2)

