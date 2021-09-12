#CH04
#Stationary Models

#Review
#정상성 : 기댓값과 분산이 일정, Corr이 시차에만 의존..
#Sample Path와 ACF를 동시에 봐야한다

library(TSA)

#4.1 General Linear Processes
#Y_t = 현재의 WN + 이전의 모든 WH의 가중치합..
# -> WN의 무한합.. (발산할 수도..) 고로 가중치의 제곱의 합이 유한하다고 가정..
#계산 결과 정상성 만족..
# -> 가중치가 점점 줄어든다.. 즉, 먼 시점의 noise의 영향이 줄어든다..
# -> ACF도 시간에 따라 빠르게 0으로 감소한다..


#4.2 Moving Average Processes
# Y_t = 현재의 WN - 이전의 가중치합.. (q까지의..)
# MA(q) 모델은 무조건 Stationary하다..
# 기댓값은 0이고, 분산도 계산하면 일정하다..
# Corr는 시차가 주어진 q보다 크면 0이다..(관계 없다..)
# 모형의 ACF는 q차 이후에는 0이다..
# -> SACF를 보고 이런 모양이면 MA모델이라고 추정할 수 있다..

# MA(1)모델에서, 같은 모델에서 두개의 세타 값이 나올 수 있다..
# -> 그럼 '가역성 조건'을 만족하는 세타를 찾는다..

win.graph(width=3, height=3, pointsize=8)
plot(y=ma1.2.s, x=zlag(ma1.2.s), ylab=expression(Y[t]),
     xlab=expression(Y[t-1]), type='p')
