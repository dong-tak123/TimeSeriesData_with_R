#CH02
#기본 컨셉

#확률과정 _VS_ 확률분포
#확률과정은 X들의 집합이고, 안의 X들 사이에 관계가 존재한다..
#확률분포는 모분포가 중요하고, 각 X들 사이에 관계가 없다..(iid)

#즉, 확률과정 : 관계(종속성) + 각지점에서의 분포(marginal) 둘다 중요
#joint 분포를 아는게 베스트인데, 실제 데이터는 부족하다..
#TS 데이터는 확률과정에 속한다..

#각 시점의 확률분포의 기댓값, 분산 : 확률분포의 marginal 특징을 표현
#각 확률분포끼리의 공분산, 자기상관계수 : 두 확률분포의 관계를 표현

#이제 할일은..
#1. 주어진 TS모형에서, 각시점의 기댓값, 분산, Corr를 구한다
#2. 구한 값들에서 알아낼 수 있는 것을 알아낸다..

library(TSA)

#1) Random Walk
#Y_t = Y_(t-1) + e_t
#기댓값 : 0, 분산은 t에 비례해서 커짐..
#-> 0을 중심으로 점점 퍼지는 과정..

win.graph(width=4.875, height=2.5, pointsize=8)
data(rwalk)
plot(rwalk, type='o', ylab='Random Walk')

#길리가 n인 sample path 하나 생성..
RW <- function(n){
    X<-rep(0,n)     #길이가 n인 0벡터..
    e<-rnorm(n, 0, 1)
    
    #Random Walk를 만드는 점화식
    for (i in 2:n) X[i]<-X[i-1]+e[i]
    X
}

plot(RW(100), type='l', ylim=c(-30,30))
for (i in 2:10) lines(RW(100),col = i)  #10개까지 더 그리자

#더 그리자..
plot(RW(100), type="l", ylim=c(-30,30))
for (i in 2:500) lines(RW(100), col=i)


#2)
# Y_t = 0.5e_(t-1) + 0.5e_(t-2)
# 기댓값 : 0, 분산은 t와 관계없이 일정..
# -> 0을 중심으로 일정범위 안에 분포..

ex2<-function(n){
    
    X<-rep(0, n)
    e<-rnorm(n,0,1)
    
    #예제 과정을 만드는 점화식
    for (i in 3:n) X[i]<-0.5*e[i-1]+0.5*e[i-2]
    X
}
plot(ex2(100), type='l', ylim=c(-2,2))
#2개만..
for (i in 2:3) lines(ex2(100), col=i)
#더많이..
for (i in 2:100) lines(ex2(100), col=i)


#Stationary(정상성)
# 1) 모든 시점에 대해서 기댓값이 같다
# 2) 모든 시점에 대해서 분산이 같다..
# 3) 각 시점의 분포끼리의 상관계수가 시차에만 관련이 있다
# sample path만 보면 1,2번은 얼추 확인 가능한데 3번은 안된다..
# -> ACF를 확인해야한다!!

# 강정상성 _vs_ 약정상성
# 강정상성 : 시점을 어떻게 뽑더라도 joint분포가 같음..
# 약정상성 : 기댓값, 분산등 모먼트를 가지고 이야기함..

#모델링 시에 Stationary 하지 않으면 Stationary하도록 변환 후 Stationary 모델을 찾는다
