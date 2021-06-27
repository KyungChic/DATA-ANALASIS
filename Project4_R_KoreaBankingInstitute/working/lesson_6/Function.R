
## 조건부 변수 선택

# 1) 데이터 불러오기
iris

# 2) iris 숫자형 데이터 선ㅌ
iris[,1:4]

iris_names<-names(iris)[1:4]
iris_names
iris[,iris_names]


## 조건부 케이스 선택 

# 1) 조건부 선택
subset(iris,Sepal.Length >7)
subset(iris,Species=="virginica")

# 2) 다중 조건

subset(iris,Species=="virginica" & Sepal.Length >7)
subset(iris,Species=="virginica" & Sepal.Length >7 & Sepal.Width <= 2.9)


## 조건부 변수와케 케이스 선택 

# 1) iris 데이터의 4-5번째 위치한 변수만 선택하고 그 변수중 
subset(iris[,4:5],Species=="virginica" & Petal.Width < 1.8)


