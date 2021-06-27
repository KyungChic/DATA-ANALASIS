## 데이터 다루기 실습

# 1) 고객 데이터 불러오기
str(customer)
head(customer)

# 2) 고객 id와 금액만 출력
customer[,c(1,3)]
customer[,-c(2,4,5)]
customer$gender<-NULL

# 3) 금액 기준으로 상위고객, 하위고객을 정의 
customer$level<-ifelse(customer$cost>570000,"우수","일반")
customer

# 4) 월별 구매 금액 총액 계산
customer$register<-as.character(customer$register)
customer$month<-substr(customer$register,6,7)
aggregate(cost~month,data=customer,sum)
