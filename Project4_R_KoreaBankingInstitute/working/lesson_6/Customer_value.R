## 고객가치 분류

# 1) 데이터 불러오기
customer_purchase

# 2) 데이터 탐색
str(customer_purchase)
head(customer_purchase)
summary(customer_purchase)

# 3) 군집분석 수행
km <- kmeans(customer_purchase, 3)
km

# 4) 군집분석의 군집정보 추출
km$cluster

# 5) vip 정보 불러오기
customer_vip

# 6) 고객금액 데이터, 군집분석 결과, VIP 정보 합치기
result<-cbind(customer_purchase, cluster=km$cluster,cust_level=customer_vip$vip)
head(result)

# 7) VIP 등급 확인
summary(result)
result$cluster<-as.factor(result$cluster)

aggregate(monetary~cluster,data=result,mean)
aggregate(monetary~cust_level,data=result,mean)

subset(result,cust_level!="일반")


