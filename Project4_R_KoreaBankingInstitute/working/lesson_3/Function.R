## iris 데이터에서 변수 추출하기

#1) iris 데이터불러오기
iris <- read.csv("C:/Users/skim178/Desktop/working/lesson_3/iris.txt", sep="")
head(iris)

#2) 데이터에서 컬럼을 추출하는 방법
iris$Sepal.Length

#3) x 벡터에 ifelse 함수를 적용해 y 벡터 생
y<-ifelse(iris$Sepal.Length>5,"꽃잎길이 5이상","꽃잎길이 5 미만")
y

#4) iris 데이터에 Sepal.Length_level 이라는 새로운 변수 추가
iris$Sepal.Length_level<-y



## 함수 학습

#1) 통합 함수
aggregate(Sepal.Length ~ Species, data = iris, mean)
aggregate(Sepal.Length ~ Species, data = iris, sum)

#2) 통합 작업 컬럼 전체의 통계값
aggregate(. ~ Species, data = iris, mean)
