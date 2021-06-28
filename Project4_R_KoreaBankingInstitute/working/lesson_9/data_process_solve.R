## 데이터 다루기 실습

# 1) 벡터로 데이터 프레임 생성

x<-c(1,2,3)
y<-c("남","여","남")

df<-data.frame(x,y)

df

# 2) 고객 데이터 불러오기
str(customer_dat)
head(customer_dat)

# 3) 5명의 고객을 임의로 추출
customer_dat[sample(nrow(customer_dat),5),]

# 4) 서울에 거주하는 남성 고객들의 id, 성별, 가입일자 정보를 출력 
customer_dat[customer_dat$area=="서울", c("id", "gender", "register")]

subset(customer_dat, gender=='남자' & area=='서울', c('id', "gender", 'register'))

# 5) 전체 고객을 구매 금액이 높은 순으로 나열
customer_dat[order(-customer_dat$cost),]


# 6) 구매 금액이 가장 높은 남성 고객과 여성 고객의 id 확인
customer_dat[order(customer_dat$gender, -customer_dat$cost),]


# 7) 서울 지역에 거주하는 고객 중 가장 구매 금액이 낮은 고객의 id 확인.
customer_dat
seoul<-customer_dat[customer_dat$area=="서울",]
seoul
seoul[order(seoul$cost),]
