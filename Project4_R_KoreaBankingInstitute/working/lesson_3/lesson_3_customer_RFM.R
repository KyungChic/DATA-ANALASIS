
#특정 데이터 값 추출
iris$Sepal.Length

#조건에 맞는 데이터 추출
y <- ifelse(iris$Sepal.Length>5, "more than 5", "less than 5")
y

#DF상에 없던 자료 추가하기
iris$Sepal.Length_level <- y
head(iris)

#데이터 값들의 통계치 구하기(통합)
aggregate(Sepal.Length ~ Species, data = iris, mean)
aggregate(Sepal.Length ~ Species, data = iris, sum)
aggregate(. ~ Species, data = iris, mean)


#고객군 데이터 확인
str(customer_rfm)
head(customer_rfm)

aggregate(monetary ~ monetary_level, data = customer_rfm, mean)
aggregate(recency ~ recency_level, data = customer_rfm, mean)
aggregate(frequency ~ frequency_level, data = customer_rfm, mean)

#프로파일링 데이터 제작
rfm_level <- aggregate(. ~ monetary_level + frequency_level + recency_level, data = customer_rfm, mean)
rfm_level

#고객세분화 데이터 시각화해보기
#rgl라ㅇ브러리 불러오기
library(rgl)

#3d plot을 이용한 RFM 지수 시각화
plot3d(customer_rfm$monetary, customer_rfm$frequency, customer_rfm$recency,
       xlab = 'Monetarym', ylab = 'Frequency', zlab = 'Recency', col = "blue", size = 6)
