setwd("C:/Users/liver/Downloads/20212시계열(송준모)/시계열 팀프")
Y<- read.csv("포항_월합강수량.csv")

layout(1)
#데이터 준비
Y<- Y[,-c(1,2)]
Y
ts.plot(Y)
t<-Y[,1]
Y<-Y[,2]


#전처리..
f<-Y>1
Y_b <- Y[f]
min(Y_b)


Y[!f] <- min(Y_b)
min(Y)

Y_1 <- Y

#전처리 완료
Y<-log(Y)
ts.plot(Y)

acf(Y)

"""
#DC : 12번 차분값
ts.plot(Dc <- diff(Y,12))

layout(t(1:2))
acf(Dc);pacf(Dc)
"""


#dDc : Dc에서 한번 더 차분
layout(1)
ts.plot(dDc<-diff(Y))
acf(dDc, lag.max = 36); pacf(dDc, lag.max = 36)
layout(t(1:2))



#예측한번 해보자..
library(forecast)
aic<-2000
#aci.tmp<- AIC(Arima(Y, order=c(1,1,1),
#                    seasonal=list(order=c(1,1,1), period=12)))

for(p in 1:1) for(q in 1:1)for(P in 2:2) for(Q in 1:1){
    aic.tmp<-AIC(Arima(Y, order=c(p,1,q), 
                       seasonal = list(order = c(P, 0, Q), period = 12)))
    #if(aic.tmp<aic){
    aic<-aic.tmp
    cat("p=",p, "q=",q, "P=",P,"Q=",Q," AIC=",aic ,"\n")
#    }
}


#잔차분석
Y.fit<-Arima(Y, order=c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))
r<-Y.fit$res
layout(matrix(c(1,2,1,3),2,2))
ts.plot(r); acf(r); pacf(r)


#Boxtest
Box.test(r, lag=12, type="Ljung")

#모형의 적합도 검정
Box.test(r, lag=12, type="Ljung", fitdf=1+1)


#모수 추정
Y.fit

hat.Y<-Y.fit$fitted # 적합값(추정된 모형에 의해서 계산된 값)
layout(1)
ts.plot(Y)
lines(hat.Y, col="red", lty=2) # 적합값 그래프

#예측
fore <- forecast(Y.fit, h=12)

real_fore <- exp(fore$mean)
real_fore
ts.plot(real_fore)

ts.plot(Y_1, xlim=c(1,144))
lines(real_fore,col="red")
real_hat.Y <- exp(hat.Y)
lines(real_hat.Y, col="red", lty=2)


auto.arima(Y)

