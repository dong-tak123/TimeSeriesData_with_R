#첫번째 데이터.. (정상적 추세..)

?LakeHuron
data("LakeHuron")


LH <- LakeHuron
LH

plot(LH)
plot(LH, ylim=c(570, 590))  #580기준으로 웃도는거 같다..
acf(LH)     #acf빠르게 감소하는 정상시계열.. -> ARMA모형 적합..

#sacf와 spacf를 확인한다..
layout(t(1:2))
acf(LH); pacf(LH)

#p.q 쌍에대해서 AIC, BIC점수 확인..
#arma(1,1)에서 최소..
options(digits=5) 
for(p in 0:3) for (q in 0:3){
    est<-arima(LH, order=c(p,0,q))  #p,q에 해당하는 arima 모형으로 적합하라..
    #AIC, BIC점수를 확인..
    #cat은 출력해달라는 말..
    cat("p=", p, "q=", q, " AIC=", AIC(est), "BIC=", BIC(est), "\n")
}

#얘는 돌아가면서 최솟값 찾는..
aic <- 1000
for (p in 0:3) for (q in 0:3){
    #해당 arma(p,q)모형의 aic점수 확인..
    aic.tmp<-AIC(arima(LH, order=c(p,0,q)))
    if (aic.tmp<aic){#원래거보다 작으면 바꾸고 출력..
        aic<-aic.tmp
        cat("p=", p, "q=", q, " AIC=", aic, "\n")
    }
}

#잠정 모형은 arma(1,1)로 선택할 수 있다..
#데이터가 어떤 값(mu)을 기준으로 나타날 수 있으니까
#(X - mu)로 잠정모형을 선택해야한다..


#<모형진단>
#잠정모형이 적절한지 판단하는 단계... (잔차분석..)
#1. 잔차가 0을 중심으로 하느냐(by 잔차 플롯)
#2, 잔차의 분산이 시간과 관계없이 일정한가..?
#3. 잔차의 선형관계가 어떤가..?(by 잔차의 SACF, SPACF or Ljung-Box검정)

#잔차 끄집어내기..

#교재와 ma모델의 세타의 부호가 바뀌어있다..
fit <- arima(LH, order=c(1,0,1))
fit
fit$residuals#잔차계산을 하는 변수명명
r<-fit$residuals

#잔차 plot
layout(t(1))
ts.plot(r)

#파란색 점선안에 있으면 acf값을 0으로 봐도 무방하다..
#잔차를 끄집어내서 종속관계가 있으면 또 모델링, 다시 잔차분석..
#종속관계가 없어질때 까지..
acf(r)
pacf(r)


#<예측하기>
#predict(arima적합 결과, 몇 시차까지 예측할지..)
#$pred : 예측, $se : 예측오차..
predict(fit, n.ahead=8)

#$pred예측치..
LH.f<-predict(fit, n.ahead=20)
LH.f$pred

LH.P<-LH.f$pred
LH.U<-LH.P + 2*LH.f$se
LH.L<-LH.P - 2*LH.f$se
cbind(LH.L, LH.P, LH.U)

ts.plot(LH,xlim=c(1875,1992))
#기존의 그림에다가 덧붙혀서 그리는 lines..
lines(LH.P, col="red")
lines(LH.U, col="red", lty=3)
lines(LH.L, col="red", lty=3)
#결과.. -> 정상시계열이기 때문에 막 튀지 않는다..
#예측 구간이 상대적으로 안정적이다..