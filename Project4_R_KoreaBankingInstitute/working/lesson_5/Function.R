## 벡터 학습

# 1) 벡터 정의
v <- c(1,2,3,4,5)
v1 <- c(1:5)
v2 <- c(1:5,10,100,100:105,-10)

# 2) 벤터 연산

v+1
v+v1

# 3) 논리 연산

v>3
v==1

# 4) 벡터 묶기

c(v,6)
c(v,NA)

# 5) 벡터의 유형 확인

is.numeric(v)
is.factor(v)
is.na(c(v,NA))


## 데이터 유형 변환

# 1) 데이터 불러와 구조 확인
iris
str(iris)

# 2) Species 유형의 변환
iris$Species<-as.character(iris$Species)


## rbind, cbind 함수 학습

# 1) 데이터 불러오기
iris

# 2) iris 데이터를 두세트로 구분
iris_1<-iris[1:10,]
iris_2<-iris[11:20,]

# 3) 두 세트의 행결합
rbind(iris_1,iris_2)

# 4) 두 세트의 열결합
cbind(iris_1[1:6,],BOD)



