#시간을 잘 파악 못하는 것을 포매팅으로 시간 정보 전달!

#working directory 설정
setwd("C:/Users/liver/Downloads/20212시계열(송준모)/실습1")

#얘가 시간으로 잘 파악을 못한다..
rvk <- read.csv("oxford-kernel.csv", header=TRUE)
head(rvk, n=10)

plot(rvk[,1], rvk[,2])


#시간정보 바꾸기.. strptime 함수!!
#format 옵션 사용..

#시간나타내는 데이터를
rvk[1:10, 1]
#형식지정해서 제대로된 시간정보로 바꾼다..
strptime(rvk[1:10,1], format="%Y%m%d")

#바꾼거 저장..
#데이터에는 시간 정보만 들어가있음..
data <- strptime(rvk[,1], format="%Y%m%d")

#결측치 관리..
rvk[1:20,]

#결측치 확인..
# 결측치 일때만 TRUE
is.na(rvk[1:20, 2])

#결측치인 것만 FASLE
id <- !is.na(rvk[,2])

#인덱싱 다시.. 결측치 제거..
data <- data[id]
Z<-rvk[id,2]


#제거 후 그림그리기..
#x축은 시간, y축은 값..
plot(data, Z, type="l")