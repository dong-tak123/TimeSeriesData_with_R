#예제2 : 차분된 데이터에 대해 ARMA모형 적합..
#선형추세 때문에 기댓값이 0이아닌 ARMA모형임..

setwd("C:/Users/liver/Downloads/20212시계열(송준모)/Exercise/실습3")
X <- read.table("ex3-2.txt", header=T)[,1]

X
ts.plot(X)

adf.test(X)
#얘도 아까랑 똑같이 3번째가 중요하다..
# -> 주어진 형태가 선형추세 + 확률적 추세인 형태로 예측..

#확률적 추세니까 차분..
dX <- diff(X)
ts.plot(dX)

#정상시계열인지 판단하려면 acf, pacf..
acf(dX)
pacf(dX)

#차분된거에서 arma.. 즉, 원래거에 arima..

#bic점수 확인..
#bic기준으로는 AR(1)..??
bic<-1000
for(p in 0:3) for(q in 0:3){
   bic.tmp<-BIC(arima(dX, order=c(p,0,q)))
   if(bic.tmp<bic){
        bic<-bic.tmp
        cat("p=",p, "q=",q, " BIC=",bic ,"\n")
   }
} 
#arima(1,0,0)으로 예측..
dX.fit <- arima(dX, order=c(1,0,0))
dX.fit
#원래 intercept값을 보면 p.value가 2이하기 때문에 H0에 의해
#mu를 0으로 추정해야하는데, 데이터가 이미 선형추세를 가지고
#있는 것을 알기 때문에 0으로 바꾸지 않는다..

r <- dX.fit$residuals
ts.plot(r)
acf(r)
pacf(r)

#이제 차분된데이터가 기댓값이 mu인 AR(1)인 모델이라고 예측..

#<예측>
#차분된 자료에 대한 기댓값을 추정해야지 기댓값이 살아있음..
#arima모형은 기본적으로 차분한 자료가 기댓값이 0이라고 생각함..
hat.mu <- arima(dX, order=c(1,0,0))$coef[2]

#zero-mean ARIMA모형으로 적합합
X.fit<- arima(X, order=c(1,0,0))

#순수한 확률적 추세에 대한 예측..
X.fore <- predict(X.fit, n.ahead=12)

#hat.mu * (1:12)는 선형 추세에 의한 예측..
#직접 mu를 쌓아주어야함..
X.P<-X.fore$pred + hat.mu*(1:12)

X.SE <- X.fore$se
X.U<-X.P+2*X.SE
X.L<-X.P-2*X.SE

cbind(X.L, X.P, X.U)

#이제 선형추세가 포함된 것 같아 보인다..
ts.plot(X,  xlim=c(1,132), ylim=c(0,70))
lines(X.P,col="red") 
lines(X.L,col="red", lty=3)
lines(X.U,col="red", lty=3)

#얘는 선형추세인 y = mu*t 그래프를 그린 것이다..
abline(0, hat.mu, col="blue")


#예제 1과 예제 2의 차이
#1은 선형 + 정상 -> 에러바운드가 작다..
#2는 선형 + 확률 -> 에러바운드가 크다..