#출석과제 10_2

#자료 가져오기
setwd("C:/Users/liver/Downloads/20212시계열(송준모)/Exercise/실습3")
Y<- read.table("ex3-2.txt", header=T)[,1]
ts.plot(Y)

#확률적 추세부분에 대한 모형 식별
AIC.table<-matrix(0,4,4)
for(p in 0:3)for(q in 0:3){
   AIC.table[p+1,q+1]<- AIC(Arima(diff(Y), order=c(p,0,q)))
}
rownames(AIC.table)<-paste("p=",0:3,sep="")
colnames(AIC.table)<-paste("q=",0:3,sep="")
AIC.table

#잔차분석
Y.fit<-Arima(Y, include.drift=T, order=c(3,1,2))
r<-Y.fit$res
ts.plot(r)
acf(r)
pacf(r)
Box.test(r, lag=12, type="Ljung")
Y.fit

#예측
plot(forecast(Y.fit, h=30))
