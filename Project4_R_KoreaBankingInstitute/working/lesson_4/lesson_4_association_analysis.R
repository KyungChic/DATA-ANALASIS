#데이터 살피기
str(travel)
head(travel)

#데이터 슬라이싱
travel
travel[1:20,]

#통합 구문을 통한 집계 - 도시 개수
aggregate(. ~ destination, data=travel, sum)

#데이터 추리기
travel1<-travel[,c("cust_id", "destination")]
travel1

#데이터를 트랜잭션으로 변환

##id별 도시 및 전체 도시
travel2 <- split(travel$destination, travel$cust_id)
travel2

#트랜잭션 전환
library(arules)
travel3 <- as(travel2, "transactions")
travel3

summary(travel3)
#----> frequency가 나오고, element length(도시 개수별 분포 : 한명이 몇 도시까지 갔는지)

#트랜잭션 구조 살피기 : 트랜잭션을 역으로 DF로 전환
as(travel3, "data.frame")
#----> 장바구니 식으로 묶여있음을 알 수 있다.

#트랜잭션 구조 살피기 : tabel 이용
table(travel1)


#연관성 분석
rules <- apriori(travel3, parameter = list(supp=0.15, conf=0.8))
summary(rules)
inspect(rules)
#---> inspect 부분에서 연관도를 알 수 있음. / confidence 수치를 통해 확인가능.



#더 큰 사이즈 데이터를 통한 응용

