setwd("C:/Users/liver/Downloads/20212시계열(송준모)/Exercise/실습3")
Y <- read.table("ex3-1.txt", header=T)[,1]

Y[1:10]
ts.plot(Y)

#선형추세에 대한 확신이 있는 경우 주어진 자료가
#"선형추세 + 확률적 추세" 인지 -> arima
#"선형추세 + 정상과정"인지 -> arma
#확인을 하기위해 ADF 검정을 수행..

library(aTSA)
adf.test(Y)
#p.value가 크면 H0를 채택함.. 작으면 H1 채택..


#3번째 모형이 중요함.. H_1이 (선형추세 + 정상과정..)
#3번째 p.value가 모두 1%미만.. -> H1이 선택..
#주어진 데이터는 3번째 결과에 의해 (선형추세 + 정상과정)이다..

#시계열 : 시간에 대한 부분(f(t)) + 오차항에 대한 모형화(e_t)

#시간에 대한 회귀 분석

#설명변수 생성
x<- 1:length(Y)
x
#회귀 분석하는 lm변수
#lm(관심대상 ~ 종속변수(설명변수))
#추정결과가 fit.lm에 저장됨..
fit.lm <- lm(Y~x)
summary(fit.lm)

#회귀모형에도 잔차분석..
fit.lm <- lm(Y~0+x) #상수항이 없을때 0+..
r <- fit.lm$residuals#잔차 가져오기..
#잔차가 white noise와 비슷한 모양인지 확인..
ts.plot(r, ylim=c(-5,5))
#acf도 확인..
acf(r); pacf(r)
#확인결과 잔차 r이 어떠한 종속관계가 있는것으로 보임..
#그럼 모형화를 할 수 있음..

#Y = f(t) + e_t
#근데 e_t를 관찰불가능 하기때문에 대안으로 잔차로 모형화 하는 것임..


#<오차항의 모형화>
#아까 잔차의 acf와 pacf를 보니까 AR(2)정도로 보인다..
#오차항의 오차항도 모형화 해야함..
arima(r, order=c(2,0,0))
#intercept : 평균에 대한 예측값.. -> 0으로 가정해도 될거같다

#잔차에 대해 모델링을 해서, 리얼 오차(e_t)를 추정하고싶다..
fit.e <- arima(r, order=c(2,0,0), include.mean=F)
fit.e
fit.e.res<-fit.e$residuals#잔차부분의 오차의 잔차..에 대한 추정치
plot(fit.e.res, main=expression(paste("Plot of ", hat(eta)[t])),
     ylab=expression(hat(eta)[t]), col="red")
acf(fit.e.res)
pacf(fit.e.res, ylim=c(-0.5,0.5))
#오차항의 오차부분(eta_t)이 이제 WN이니까 리얼 오차도 AR(2)라고 하자..

#선형회귀..
fit.lm
#잔차를 가지고 오차항에 대한 추정한것..
fit.e


#<예측>
#선형추세에 의한 예측 + 정상과정에 의한 예측으로 따로 해야함
#예측구간은 정상과정(오차항)에 의한 예측에 의해 만들어짐..
x
x.new <- data.frame(x=length(Y)+1:12)#x는 lm(Y~x)에서 사용된 설명변수

#선형과정에 대한 예측..
Y.lm.fore<-predict(fit.lm, x.new)
#오차항에 대한 예측..
e.fore<-predict(fit.e, n.ahead=12)

#오차범위 지정..
e.P <- e.fore$pred
e.se <- e.fore$se
e.L<-e.P-2*e.se
e.U<-e.P+2*e.se

#직선 부분에다가 더함..
Y.P<- Y.lm.fore + e.P
Y.L<- Y.lm.fore + e.L
Y.U<- Y.lm.fore + e.U

#그래프 그리기..
ts.plot(Y, xlim=c(1,132), ylim=c(-1,25))
lines(Y.P, col="red")
lines(Y.L, col="red", lty=3)
lines(Y.U, col="red", lty=3)

#추정된 직선..
#예측인 빨간색과 거의 비슷하다.. -> 차이가 0을 기준으로 왓다갓다..
abline(fit.lm, col="blue")
