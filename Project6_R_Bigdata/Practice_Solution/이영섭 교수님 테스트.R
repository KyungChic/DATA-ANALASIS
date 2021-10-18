<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
#이영섭 교수님 2021-1기 R을 활용한 빅데이터 분석 시험

#엄경식(우리은행)


####이론문제####

#1."A" 명목형

#2."D" 의사결정나무 모형보다 설명력이 뛰어나지 않다.

#3."C" SVM

#4."D" 정확도가 떨어짐.

#5 "A" ID3




###실습문제####


#0. 전처리

hmeq = read.csv("C:/Users/Gargantua/Desktop/data_analysis/Project6_R_Bigdata/DataSet/hmeq.txt", header=T, sep='\t')

str(hmeq)


  ##missing value 제거

hmeq2 = na.omit(hmeq)

any(is.na(hmeq2))


  ## 훈련용데이터 60% 검증용데이터 40% 분리

set.seed(1234)
train = sample(nrow(hmeq2), nrow(hmeq2) * 0.6)

hmeq2_train = hmeq2[train,]
hmeq2_test = hmeq2[-train,]


nrow(hmeq2)
nrow(hmeq2_train)
nrow(hmeq2_test)

#1. Y변수인 BAD를 범주형 변수로 변환하시오

hmeq2$BAD = as.factor(hmeq2$BAD)

class(hmeq2$BAD)


#2. 

library(rpart)

  ##모델 구축

model = rpart(BAD ~ VALUE + DELINQ + CLNO + DEBTINC, data = hmeq2_train)

  ##결과 프린트
print(model)

  ##모형 그리기
plot(model)
text(model)

#3.

  ## test데이터 통한 예측
pred = predict(model, newdata = hmeq2_test, type = "class")


  ##오류율 구하기

table = table(hmeq2_test$BAD, pred)

=======
#이영섭 교수님 2021-1기 R을 활용한 빅데이터 분석 시험

#엄경식(우리은행)


####이론문제####

#1."A" 명목형

#2."D" 의사결정나무 모형보다 설명력이 뛰어나지 않다.

#3."C" SVM

#4."D" 정확도가 떨어짐.

#5 "A" ID3




###실습문제####


#0. 전처리

hmeq = read.csv("C:/Users/Gargantua/Desktop/data_analysis/Project6_R_Bigdata/DataSet/hmeq.txt", header=T, sep='\t')

str(hmeq)


  ##missing value 제거

hmeq2 = na.omit(hmeq)

any(is.na(hmeq2))


  ## 훈련용데이터 60% 검증용데이터 40% 분리

set.seed(1234)
train = sample(nrow(hmeq2), nrow(hmeq2) * 0.6)

hmeq2_train = hmeq2[train,]
hmeq2_test = hmeq2[-train,]


nrow(hmeq2)
nrow(hmeq2_train)
nrow(hmeq2_test)

#1. Y변수인 BAD를 범주형 변수로 변환하시오

hmeq2$BAD = as.factor(hmeq2$BAD)

class(hmeq2$BAD)


#2. 

library(rpart)

  ##모델 구축

model = rpart(BAD ~ VALUE + DELINQ + CLNO + DEBTINC, data = hmeq2_train)

  ##결과 프린트
print(model)

  ##모형 그리기
plot(model)
text(model)

#3.

  ## test데이터 통한 예측
pred = predict(model, newdata = hmeq2_test, type = "class")


  ##오류율 구하기

table = table(hmeq2_test$BAD, pred)

>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======
#이영섭 교수님 2021-1기 R을 활용한 빅데이터 분석 시험

#엄경식(우리은행)


####이론문제####

#1."A" 명목형

#2."D" 의사결정나무 모형보다 설명력이 뛰어나지 않다.

#3."C" SVM

#4."D" 정확도가 떨어짐.

#5 "A" ID3




###실습문제####


#0. 전처리

hmeq = read.csv("C:/Users/Gargantua/Desktop/data_analysis/Project6_R_Bigdata/DataSet/hmeq.txt", header=T, sep='\t')

str(hmeq)


  ##missing value 제거

hmeq2 = na.omit(hmeq)

any(is.na(hmeq2))


  ## 훈련용데이터 60% 검증용데이터 40% 분리

set.seed(1234)
train = sample(nrow(hmeq2), nrow(hmeq2) * 0.6)

hmeq2_train = hmeq2[train,]
hmeq2_test = hmeq2[-train,]


nrow(hmeq2)
nrow(hmeq2_train)
nrow(hmeq2_test)

#1. Y변수인 BAD를 범주형 변수로 변환하시오

hmeq2$BAD = as.factor(hmeq2$BAD)

class(hmeq2$BAD)


#2. 

library(rpart)

  ##모델 구축

model = rpart(BAD ~ VALUE + DELINQ + CLNO + DEBTINC, data = hmeq2_train)

  ##결과 프린트
print(model)

  ##모형 그리기
plot(model)
text(model)

#3.

  ## test데이터 통한 예측
pred = predict(model, newdata = hmeq2_test, type = "class")


  ##오류율 구하기

table = table(hmeq2_test$BAD, pred)

>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======
#이영섭 교수님 2021-1기 R을 활용한 빅데이터 분석 시험

#엄경식(우리은행)


####이론문제####

#1."A" 명목형

#2."D" 의사결정나무 모형보다 설명력이 뛰어나지 않다.

#3."C" SVM

#4."D" 정확도가 떨어짐.

#5 "A" ID3




###실습문제####


#0. 전처리

hmeq = read.csv("C:/Users/Gargantua/Desktop/data_analysis/Project6_R_Bigdata/DataSet/hmeq.txt", header=T, sep='\t')

str(hmeq)


  ##missing value 제거

hmeq2 = na.omit(hmeq)

any(is.na(hmeq2))


  ## 훈련용데이터 60% 검증용데이터 40% 분리

set.seed(1234)
train = sample(nrow(hmeq2), nrow(hmeq2) * 0.6)

hmeq2_train = hmeq2[train,]
hmeq2_test = hmeq2[-train,]


nrow(hmeq2)
nrow(hmeq2_train)
nrow(hmeq2_test)

#1. Y변수인 BAD를 범주형 변수로 변환하시오

hmeq2$BAD = as.factor(hmeq2$BAD)

class(hmeq2$BAD)


#2. 

library(rpart)

  ##모델 구축

model = rpart(BAD ~ VALUE + DELINQ + CLNO + DEBTINC, data = hmeq2_train)

  ##결과 프린트
print(model)

  ##모형 그리기
plot(model)
text(model)

#3.

  ## test데이터 통한 예측
pred = predict(model, newdata = hmeq2_test, type = "class")


  ##오류율 구하기

table = table(hmeq2_test$BAD, pred)

>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======
#이영섭 교수님 2021-1기 R을 활용한 빅데이터 분석 시험

#엄경식(우리은행)


####이론문제####

#1."A" 명목형

#2."D" 의사결정나무 모형보다 설명력이 뛰어나지 않다.

#3."C" SVM

#4."D" 정확도가 떨어짐.

#5 "A" ID3




###실습문제####


#0. 전처리

hmeq = read.csv("C:/Users/Gargantua/Desktop/data_analysis/Project6_R_Bigdata/DataSet/hmeq.txt", header=T, sep='\t')

str(hmeq)


  ##missing value 제거

hmeq2 = na.omit(hmeq)

any(is.na(hmeq2))


  ## 훈련용데이터 60% 검증용데이터 40% 분리

set.seed(1234)
train = sample(nrow(hmeq2), nrow(hmeq2) * 0.6)

hmeq2_train = hmeq2[train,]
hmeq2_test = hmeq2[-train,]


nrow(hmeq2)
nrow(hmeq2_train)
nrow(hmeq2_test)

#1. Y변수인 BAD를 범주형 변수로 변환하시오

hmeq2$BAD = as.factor(hmeq2$BAD)

class(hmeq2$BAD)


#2. 

library(rpart)

  ##모델 구축

model = rpart(BAD ~ VALUE + DELINQ + CLNO + DEBTINC, data = hmeq2_train)

  ##결과 프린트
print(model)

  ##모형 그리기
plot(model)
text(model)

#3.

  ## test데이터 통한 예측
pred = predict(model, newdata = hmeq2_test, type = "class")


  ##오류율 구하기

table = table(hmeq2_test$BAD, pred)

>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
    ### 오류율 = (11+83) / (1270+11+83+42) = 0.06685633