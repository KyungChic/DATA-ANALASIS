## 데이터 통합

# 1) 데이터 불러오기 및 탐색
travel
head(travel)
str(travel)

# 2) 가장 높은 총액을 소비한 상위 10 고객
cust_sum<-aggregate(cost~cust_id,data=travel,sum)
cust_sum[order(-cust_sum$cost),][1:10,]

# 3) 가장 높은 평균 금액을 소비한 상위 10 고객
cust_mean<-aggregate(cost~cust_id,data=travel,mean)
cust_mean[order(-cust_mean$cost),][1:10,]

# 4) 가장 낮은 총액을 보이는 세 도시
city_sum<-aggregate(cost~destination,data=travel,sum)
city_sum[order(city_sum$cost),][1:3,]

# 5) 일본에 방문한 총 고객 수
japan_visit<-travel[travel$country=="일본",]
aggregate(cost~cust_id,data=japan_visit,sum)

# 6) 가장 많이 여행한 고객  
travel$visit<-1
visit_sum<-aggregate(visit~cust_id,data=travel,sum)
visit_sum[order(-visit_sum$visit),]

# 7) 가장 여행을 많이 다닌 상위 3 고객이 방문한 도시
travel_most<-subset(travel,cust_id==1|cust_id==2|cust_id==3)
aggregate(visit~destination,data=travel_most,sum)