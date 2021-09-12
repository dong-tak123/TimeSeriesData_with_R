#CH01
#TS 데이터에 대한 전반적 소개..

#라이브러리 불러오기
library(TSA)

#LA 강수량 데이터 그리기
win.graph(width=4.875, height=2.5, pointsize=8)
data(larain)
plot(larain, ylab='Inches', xlab='Year', type='o')

#색깔 특성 데이터 그리기
win.graph(width=4.875, height=2.5, pointsize=8)
data(color)
plot(color, ylab='Color Property',xlab='Batch', type='o')

#캐나다 고용률 데이터 그리기
win.graph(width=4.875, height=2.5, pointsize=8)
data(hare)
plot(hare, ylab='Abundance', xlab='Year', type='o')

#월별 평균 기온.. 주기성 있는 TS데이터
win.graph(width=4.875, height=2.5, pointsize=8)
data(tempdub)
plot(tempdub, ylab='Temperature', type='o')

#월별 오일 필터 가격.. 주기성 있는 TS데이터
win.graph(width=4.875, height=2.5, pointsize=8)
data(oilfilters)
plot(oilfilters, type='o', ylab='Sales')


# 시계열 데이터는 시간에 흐름에 따라 얻어지는 자료이다
# 값과 시간정보가 함께 있어야한다
# 실습시에는 누락된 시간정보 혹은 값을 따로 채워야한다

#앞선 TS데이터..
data(larain)
class(larain)

larain

#얘는 그냥 정규분포.. 시간정보 없음
X<- rnorm(100)
class(X)
X

# TS데이터에서 중요한 것은 각 시점의 분포와 각 분포사이의 관계도 중요하다
# 즉, TS데이터는 자료가 모두 종속이다..
#  -> short range, long range dependence..
# iid 에서는 모분포만 알아내면 끝이었다


#zlag 확인 : 젤 앞을 비우고 한칸씩 미룸
A<-c(1,5,2,3,6)
zlag(A)

#TS데이터는 이전 시점의 데이터와 관계가 있다..
win.graph(width=3, height=3, pointsize=8)
plot(y=larain, x=zlag(larain), ylab='Inches',
     xlab='Previous Year Inches')


win.graph(width=4.875, height=2.5, pointsize=8)
data(color)
plot(color, ylab='Color Property',xlab='Batch', type='o')

win.graph(width=3, height=3, pointsize=8)
plot(y=color, x=zlag(color), ylab='Color Property',
     xlab='Previous Batch Color Property')

#iid는 관계가 없다..
X<-rnorm(500)
Z.X<-zlag(X)
plot(x=Z.X, y=X, pch=20)


##모델링을 잘해야한다..
#1. 데이터를 잘 관찰한다
#2. 내가 아는 모델의 특징을 잘 파악한다
#4. 최대한 잘 맞는거 찾는다..

#모든 모델이 다 맞추지는 못하지만, 중요한 특징은 표현가능하다