# [R의 기초통계 계산]

# 1) 데이터 불러오기

iris
mtcars

head(iris)

# 2) 다양한 통계값 산출

m1<-mean(iris$Sepal.Length)
m2<-median(iris$Sepal.Length)
s1<-sd(iris$Sepal.Length)


# 3) 표준화 수행 : 표준정규분포

iris$Sepal.Length_z<-(iris$Sepal.Length - m1)/s1
head(iris)


# 4) 상관분석

cor(iris, method = c("pearson"))
  # Species 값이 범주형이기 때문에 오류가 남 (모든 값이 수치형이어야함)

cor(iris[,c(1:4)], method = c("pearson"))
  #첫 네 변수를 바탕으로 상관분석 실행

library(PerformanceAnalytics)
chart.Correlation(iris[,c(1:4)],pch=19)


# 5) t 검정 : 두 그룹 간 평균 비교시 사용

iris_test <- subset(iris, Species=="setosa" | Species=="virginica")
boxplot(Sepal.Length~Species, data=iris_test)
  # 차트 상 중간 굵은 선은 median 임

##<가설> setosa / virginica의 sepel length는 유의미한 차이가 있다.

t.test(iris_test$Sepal.Length~iris_test$Species, var.equal=T)

##p-value < 0.05 이므로 유의미한 가설이라고 할 수 있다.


# 6) 카이제곱 검정 : 두 범주형 변수의 관계가 독립/종속인지 확인

##교차표 분석

table(mtcars$vs, mtcars$cyl)


##카이제곱 검정
chisq.test(mtcars$vs, mtcars$cyl)
  #p-value 값이 현저하게 작으므로 두 변수간 유의미한 관계가 있음을 알 수 있음.




#[시계열 분석]

# 1) 데이터 불러오기

AirPassengers
  #global data로서, ts형식임을 알 수 있음.
  #월별 데이터임을 파악


# 2) 시계열 데이터 시각화
ts.plot(AirPassengers)
title("1949~1960년 월별 탑승 승객")
  #계속해서 승객 늘어나고 있음을 확인


# 3) 시계열 분쇄

ts<-decompose(AirPassengers)
ts
plot(ts)


# 4) 시계열 분석 수행
fit <- arima(AirPassengers, order=c(1,0,0), list(order=c(2,1,0), period=12))
  #학습모형 저장


# 시계열 예측 수행 
predict <- predict(fit, n.ahead=24)


# 6)  visual
ts.plot(AirPassengers, predict$pred, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("실제", "예측"), col=c(1,2), lty=c(1,1))
