#확률적 추세를 가진 데이터..
#데이터의 형태를 보고 차분하면 0을 중심으로한
#정상시계열이라고 생각하면 베스트..

setwd("C:/Users/liver/Downloads/20212시계열(송준모)/Exercise/실습2")
X<-read.table("ex2-2.txt", header = T)[,1]
X
ts.plot(X)

#acf 느리게(직선으로) 감소..
acf(X)


#확률적 추세를 가지면 차분한 데이터 활용..
dX<-diff(X)#차분..
dX
#차분하니까 0을 중심으로 한 정상시계열 같다..
ts.plot(dX)
acf(dX)
pacf(dX)

#최소 AIC값 찾기..
aic <- 1000
for (p in 0:3) for (q in 0:3){
    #해당 arma(p,q)모형의 aic점수 확인..
    aic.tmp<-AIC(arima(dX, order=c(p,0,q)))
    if (aic.tmp<aic){#원래거보다 작으면 바꾸고 출력..
        aic<-aic.tmp
        cat("p=", p, "q=", q, " AIC=", aic, "\n")
    }
}

#dX에 대한 모수추정..
arima(dX, order=c(1,0,0))
#intercept가 -0.34로 추정되었는데, se의 2배수 안에 들어가기 때문에
#0이라고 봐도 무방하다

#잔차 : 오차항에 대한 추정치..

#기댓값(mu)을 포함시키는지 결정하는 include.mean매개변수..
#차분된 데이터에서 얻은 예측./
dX.fit<- arima(dX, order=c(1,0,0), include.mean=F)

#잔차분석
r <-dX.fit$residuals
ts.plot(r)
acf(r)
pacf(r)

#<예측>
fit<-arima(X, order=c(1,1,0), include.mean=F)
fit
predict(fit, n.ahead=8)


#추정치 그리기..
X.fore<-predict(fit, n.ahead=20)
X.P<-X.fore$pred
X.SE<-X.fore$se
X.U<-X.P + 2*X.SE
X.L<-X.P - 2*X.SE

#확률적 추세를 보이기 때문에 오차 범위가 더 넓다..
ts.plot(X, xlim=c(1,140), ylim=c(-30, 30))
lines(X.P, col="red")
lines(X.U, col="red", lty=3)
lines(X.L, col="red", lty=3)