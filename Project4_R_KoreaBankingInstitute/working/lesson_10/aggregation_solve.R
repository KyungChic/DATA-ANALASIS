## 데이터통합

# 1) 데이터 불러오기 및 탐색
travel
head(travel)
str(travel)

# 2) 가장 높은 총액을 소비한 상위 10 고객
aggregate(cost~cust_id, data=travel, sum)
cost_sum <- aggregate(cost~cust_id, data=travel, sum)

cost_sum[order(-cost_sum$cost),][1:10,]

# 3) 가장 높은 평균 금액을 소비한 상위 10 고객
aggregate(cost~cust_id, data=travel, mean)
cost_mean <- aggregate(cost~cust_id, data=travel, mean)

cost_mean[order(-cost_mean$cost),][1:10,]

# 4) 가장 낮은 총액을 보이는 세 도시
aggregate(cost~destination, data=travel, sum)
cost_sum_destination <- aggregate(cost~destination, data=travel, sum)

cost_sum_destination[order(cost_sum_destination$cost),][1:3,]


# 5) 일본에 방문한 총 고객 수
japan_visit <- travel[travel$country=="일본",]
aggregate(cost~cust_id, data=japan_visit, sum)
  #sum 수치는 필요없지만 count 하기 위해 작성


# 6) 가장 많이 여행한 고객

travel$visit <- 1

visit_sum<-aggregate(visit~cust_id, data=travel, sum)
visit_sum[(order(-visit_sum$visit)),][1,]

# 7) 가장 여행을 많이 다닌 상위 3고객이 방문한 도시
top3_city <- subset(travel, travel$cust_id==1 | travel$cust_id==2 | travel$cust_id==3)
aggregate(visit~destination, data=top3_city, sum)
