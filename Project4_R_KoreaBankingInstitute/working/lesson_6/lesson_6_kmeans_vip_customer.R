
#[변수추출방법들]

iris[,1:4]
  # <1> 변수(칼럼)4개 추출
1:4
  #벡터값임을 알 수 있음

iris[,c("Sepal.Length", "Sepal.Width")]
  # <2> c(combine) 방식으로도 추출가능'

names(iris)
  #iris 데이터의 변수 명을 벡터로 추출가능
names(iris)[1:4]
  #1차원 데이터(벡터)이므로, 대괄호 안에 콤마 없이 추출됨.
iris_name <- names(iris)[1:4]
iris[,iris_name]
  # <3> names 함수를 이용한 추출


#[CASE 추출방법]

iris[1:10,]
  #원하는 row 추출

iris[iris$Sepal.Length > 7,]
  #대괄호 기준으로 콤마 왼쪽에 논리형 연산을 넣어주면 조건에 맞는 CASE가 추출된다.
iris$Sepal.Length > 7
  #연산문만 추출해보면 논리형을 충족하는 CASE에 대해 벡터가 형성됨.

iris[iris$Species=="virginica",]

iris[iris$Sepal.Length>7 & iris$Species=="virginica",]
  #두 조건을 동시에 충족하는 행 출력(끝에 "," 넣어줘야함)

subset(iris, Sepal.Length>7 & Species=="virginica")
  #subset 함수를 이용하여 따로 참조없이 추출도 가능함.



#[변수와 cASE 동시에 slicing]

iris[iris$Species=="virginica" & iris$Petal.Width > 2.3, 4:5]
  #<1> 참조방식이용

subset(iris[,4:5], Species=="virginica" & Petal.Width > 2.3)
  #<2> subset 방식 이용



#[실습 k-means 군집분석]

#데이터 살펴보기
str(customer_purchase)
head(customer_purchase)
summary(customer_purchase)


#군집분석

km <- kmeans(customer_purchase,3)
  #kmeans(data_set, 분류할 그룹 수)

km
  #첫줄 cluster size를 잘 봐야함 : group당 할당된 고객 수


#통계 기준과 비즈니스 기준 비교 : customer_vip와 비교

result <- cbind(customer_purchase,cluster=km$cluster,cust_level=customer_vip$vip)
  #kmeans 분류 결과는 km객체의 cluster에 저장됨.
  #변수(칼럼)이 지정되지 않은 변수에 대해서는 칼럼명을 지정해주어야함.

summary(result)
str(result)
  #cluster 정보가 factor형으로 정의되어야 할 필요가 있음.(군집명이 숫자로 되어있음)

result$cluster <- as.factor(result$cluster)
  #cluster를 factor형으로 변환

summary(result)


##상세비교

aggregate(monetary~cust_level, data=result, mean)
aggregate(monetary~cluster, data=result, mean)

  #비즈니스적인 구매금액과 통계적인 구매금액의 차이가 남을 확인할 수 있음.

subset(result, cust_level != "일반")
  #비즈니스 적인 vip는 단순한 고객 구매 금액이 아님.