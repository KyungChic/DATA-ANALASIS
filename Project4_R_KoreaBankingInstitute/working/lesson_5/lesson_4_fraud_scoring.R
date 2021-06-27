#[벡터유형 살펴보기]
## c는 괄호안의 요소들을 묶어주는 combine 함수
### 숫자유형의 벡터이므로 numeric 으로 type 잡힘

v <- c(1,2,3,4,5)
v

v1 <- c(1:5)
v1

v2<-c(1:5, 10, 100, 101:105, -10)
v2


#[벡터 연산해보기]

v+1
v+v1

#[논리연산 알아보기]

v>3
v==1

#[생성된 벡터를 다른 벡터와 묶기]
## NA는 결측치(값이 비어있음을 의미)

c(v,6)
c(v,NA)

#[벡터 유형 파악]
## 벡터의 유형은 단 하나임 : 벡터안의 값들이 숫자 + 문자로 섞일 수 없다.

is.numeric(v)
is.factor(v)

c(v,"남자")
#실행하는 순간 모두 문자형으로 변화함.

#[벡터값의 유형 각각 확인하기]

is.na(c(v,NA))





#[실습]

head(iris)
str(iris)

#factor를 문자형으로 고치기

iris$Species<-as.character(iris$Species)
head(iris)
str(iris)

#데이터 합치기
##rbind 함수는 변수명과 변수 개수가 동일할 때 사용 가능(Row bind)
###cbind는 변수나 변수개수가 달라도 되지만, case 수가 동일해야함.(column bind)

iris_1<-iris[1:10,]
iris_2<-iris[11:20,]

rbind(iris_1,iris_2)


BOD
cbind(BOD,iris[1:6,])



#[credit_data 실습]

str(credit_data)
head(credit_data)

  #모두 숫자 변수임이 확인됨
  #fraud : 이산형 변수 / bank : 주거래은행 / credit_card : 카드 개수 / monetary : 거래금액

summary(credit_data)
  #기술통계 산출

credit_data$bank <- as.factor(credit_data$bank)
  #bank를 factor변수로 대체

str(credit_data)
head(credit_data)

summary(credit_data)
  #factor로 변환해서 bank부분에 빈도수로 통계값이 산출됨을 알 수 있다.


aggregate(monetary~bank, data=credit_data, mean)
  #거래 은행에 따흔 거래금액 평균 산출


# 4) 로지스틱회귀분석 수행
model <- glm(fraud~bank+credit_card+monetary,data=credit_data,family="binomial")
summary(model)

# 5) 로지스틱 회귀분석으로부터 부정사용 의심 점수 계산(100점 만점 환산)
score<-fitted(model)*100

  #score는 400명의 부정사용 점수를 담고있는 numeric 자료

head(score)



result <- cbind(credit_data, score)
head(result)




# 부정고객 점수 시각화


# 패키지 불러오기
library(ggplot2)

# 주거래 은행에 따른 고객의심 부정사용 의심 점수
ggplot(result,aes(x=score,fill=bank))+
  geom_histogram(binwidth=0.6,alpha=0.5,position="identity")+
  ggtitle("주거래 은행에 따른 고객의심 부정사용 의심 점수")+
  theme_bw()
