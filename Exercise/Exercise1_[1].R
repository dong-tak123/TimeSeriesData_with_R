#시간 정보를 추가하여 TS데이터로 만들기!!


data("AirPassengers")
AP <- AirPassengers
X <- rnorm(15)

#TS데이터라서 시간 정보가 드러난다..
AP
#얘는 시간 정보 없다..
X

class(AP)

#주기성이 보이고, 점점 폭도 커지고 있다..
#시간정보가 x축..
plot(AP)

start(AP)   #첫 시간 정보
end(AP)     #마지막 시간 정보
frequency(AP)   #1년에 몇번 관측하는지..



#근데 보통의 시간정보가 없는 데이터에 우리가 시간 정보를 넣어야한다..
#workin directory 지정
setwd("C:/Users/liver/Downloads/20212시계열(송준모)/실습1")

#주어진 데이터가 head가 있어서 다르게 불러와야한다
dat<-read.table("Maine.txt")
dat[1:10,]

#불러와서 10줄 보기
dat<-read.table("Maine.txt", header=T)
head(dat, n=10)

#plot해보기..
plot(dat$unemploy)
plot(dat$unemploy, type="l")


#1) ts 함수..
# 시간정보를 넣어보자..
# ts 함수를 사용.. -> 시계열 데이터로 바꿔줌.. 
# 시작 시점과 빈도 설정..
dat.ts <- ts(dat, start=c(1996,1), frequency=12)
dat.ts
class(dat.ts)
#그림 그리면 이제 시계열 그래프가 그려진다..
plot(dat.ts)


#2) window 함수..
# 원하는 시점의 데이터를 볼 수 있다..
window(dat.ts, st=1997, end=1998)
window(dat.ts, st=1997, end=1999)
window(dat.ts, st=2000)
window(dat.ts, st=c(1997,3), end=c(1998,12))


# 원하는 시점의 빈도도 지정 가능..
# 1996.0이 1월이고, 1을 12로 나눠서 계속 간다..
window(dat.ts, st=c(1996,5), freq=1)    #1년에 한번씩 확인..




#다른 데이터도 시계열로 만들고 그래프..
CBE <- read.table("cbe.txt", header=T)
head(CBE, n=10)
#각 컬럼을 시계열 데이터로 만든다..
Elec.ts <- ts(CBE[,3], start=1958, freq=12)
Beer.ts <- ts(CBE[,2], start=1958, freq=12)
Choc.ts <- ts(CBE[,1], start=1958, freq=12)

#부분 그래프..
layout(1:3)
plot(Elec.ts); plot(Beer.ts); plot(Choc.ts)