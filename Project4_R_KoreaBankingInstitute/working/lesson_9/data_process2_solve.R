## 데이터다루기실습

# 1) 고객데이터 불러오기
str(customer_dat)
head(customer_dat)

# 2) 고객 id와 금액만 출력
customer_dat[,c("id","cost")]
customer_dat[,c(1,3)]
customer_dat[,-c(2,4,5)]

# 3) 금액 기준으로 상위고객, 하위고객을 정의
customer_dat$level <- ifelse(customer_dat$cost>570000, "HIGH", "LOW")
customer_dat

# 4) 월별 구매 금액 총액 계산

customer_dat$register <- as.character(customer_dat$register)
customer_dat$month <- substr(customer_dat$register,6,7)
aggregate(cost~month, data=customer_dat, sum)
