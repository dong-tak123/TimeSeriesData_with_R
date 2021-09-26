#5.1

AR <- function(phi, n){
    
    X <- rep(0,n)
    e <- rnorm(n)
    
    for (i in 2:n) X[i] <- phi*X[i-1] + e[i]
    X
    
}

plot(AR(1, 200), type="l", ylim=c(-20, 20))
for (i in 2:3) lines(AR(1,2000), col=i)
for (i in 2:200) lines(AR(1,200), col=i)

RW2 <- function(n){
    
    X <- rep(0,n)
    #e를 (-1, 1) 중에서 각각 0.5의 확률로 n번 뽑아라
    e <- sample(c(-1,1), n, replace=T, prob=c(0.5, 0.5))
    
    for (i in 2:n) X[i] <- X[i-1] + e[i]
    X
}

plot(RW2(10), type="l", ylim=c(-10, 10))
for (i in 2:3) lines(RW2(10), col=i)
for (i in 2:500) lines(RW2(10), col=i)


#phi값만 바꿔서..
AR2 <- function(phi, n){
    
    X<-rep(0,n)
    e<-sample(c(-1,1), n, replace=T, prob=c(0.5,0.5))
    
    for (i in 2:n) X[i] <- p hi*X[i-1] + e[i]
    X
}

#일정한 범위안에서 논다..
plot(AR2(0.5, 10), type="l", ylim=c(-10, 10))
for (i in 2:3) lines(AR2(0.5, 10), col=i)
for (i in 2:500) lines(AR2(0.5, 10), col=i)


#5.2
RW.mu <- function(mu, n){
    
    X<-rep(0,n)
    e<-rnorm(n)
    for (i in 2:n) X[i] <- X[i-1] +mu+e[i]
    X
}
#화면을 2행2열로 나눠라..
layout(matrix(1:4,2,2))
for(i in 1:4) ts.plot(RW.mu(0.2, 200))

layout(c(1))
plot(RW.mu(0.2, 200), type="l", ylim=c(-20, 90))
for (i in 2:200) lines(RW.mu(0.2, 200), col=i)
abline(0,0.2, lwd=3, col="blue")


#5.3
data("AirPassengers")
library(forecast)

BoxCox.lambda(AirPassengers)

lambda<-BoxCox.lambda(AirPassengers)        #적절한 lambda 구하기..
g_lambda<-BoxCox(AirPassengers, lambda)     #Box-Cox 변환시행..

#우측이 분산이 더 안정화 됨을 알 수 있다..
layout(t(1:2))
plot(AirPassengers)
plot(g_lambda)


layout(1:2)
ts.plot(diff(g_lambda)) #차분한 결과를 그리겠다..
acf(diff(g_lambda), lag=48, main="")    #acf그래프를 그리자!
#acf가 1,2,3,4에서 엄청 크다.. => 주기성때문!!
