## 데이터 다루기 실습

# 1) 벡터로 데이터 프레임 생성


# 2) 고객 데이터 불러오기
str(customer)
head(customer)

# 3) 5명의 고객을 임의로 추출
customer[sample(nrow(customer),5),]

# 4) 서울에 거주하는 남성 고객들의 id, 성별 가입일자 정보를 출력 
customer_man<-subset(customer,gender=="남자" & area=="서울")
customer_man[,c(1,2,5)]

# 5) 전체 고객을 구매 금액이 높은 순으로 나열 
customer[order(-customer$cost),]

# 6) 구매 금액이 가장 높은 남성 고객과 여성 고객의 id 확인
customer[order(customer$gender,-customer$cost),]

# 7) 서울 지역에 거주하는 고객중 가장 구매 금액이 낮은 고객의 id 확인
customer_seoul<-customer[customer$area=="서울",]
customer_seoul[order(customer_seoul$cost),]

