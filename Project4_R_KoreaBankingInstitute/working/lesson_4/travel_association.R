## 여행지 연관성분석 수행 

# 1) 데이터 불러오기
# 우측 Environment에서 Import Dataset > From Local File을
# 선택해서 travel.csv 데이터를 불러주시면 됩니다.

# 2) 데이터 탐색
str(travel)
head(travel)
aggregate(.~destination,data=travel1,sum)
tail(travel)

# 3) 상위 20개 케이스 확인
travel[1:20,]

# 4) 필요 변수 추출
travel1<-travel[,c("cust_id","destination")]
travel1


# 5) travel 데이터를 transaction 형태로 변환
library(datasets)

travel2<-split(travel1$destination, travel1$cust_id)
travel2

# transaction 형태로 변환
travel3<-as(travel2, "transactions")
travel3

# 변환된 transaction 형태의 데이터 확인
summary(travel3)
as(travel3,"data.frame")

# 다른 형태의 확인
table(travel1)


# 6) 연관성분석을 수행하고 확인
library(arules)
rules <- apriori(travel3, parameter = list(supp=0.15, conf = 0.8))
summary(rules)
inspect(rules)


