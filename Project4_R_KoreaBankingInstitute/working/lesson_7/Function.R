## R의 기초통계 계산

# 1) 데이터 불러오기기
iris
mtcars

# 2) 다양한 통계값 산출
m1<-mean(iris$Sepal.Length)
m2<-median(iris$Sepal.Length)
s1<-sd(iris$Sepal.Length)


# 3) 표준화 수행
iris$Sepal.Length_z<-(iris$Sepal.Length-m1)/s1
head(iris)


# 4) 상관분석
cor(iris[,c(1:4)],method=c("pearson"))

library(PerformanceAnalytics)
chart.Correlation(iris[,c(1:4)],pch=19)

# 5) t 검정 
iris_test<-subset(iris,Species=="setosa" | Species =="virginica")
boxplot(Sepal.Length~Species,data=iris_test)
t.test(iris_test$Sepal.Length~iris_test$Species,var.equal=T)


# 6) 카이제곱 검정
table(mtcars$vs,mtcars$cyl)
chisq.test(mtcars$vs,mtcars$cyl)



