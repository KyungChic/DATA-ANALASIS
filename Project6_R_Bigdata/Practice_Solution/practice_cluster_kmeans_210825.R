<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
#클러스터 예제

#248~

#데이터 전처리

sns0<-read.csv("DataSet/snsdata.csv")

str(sns0)

  #gender변수 factor화 하기

sns0$gender = as.factor(sns0$gender)

summary(sns0)


  #gender 결측치 제거

sum(is.na(sns0$gender))
#na개수

sns1 = sns0[!is.na(sns0$gender),]

sns1 = sns0 %>% filter(!is.na(gender))


sum(is.na(sns1$gender))
#any(is.na(sns1$gender))


  #age 13이상 20이하 결측치 제거

sum(is.na(sns1$age))


  #방법1
sns2 = sns1 %>% 
  mutate(age_new = ifelse( age < 13 | age > 20, NA, age)) %>% 
  mutate(age_new2 = ifelse( is.na(age_new), mean(age_new, na.rm=T), age_new))

sns2$age_new2
sum(is.na(sns2$age_new2))

  #방법2

sns1$newage = ifelse(sns1$age < 13 | sns1$age >20, NA, sns1$age)

m = mean(sns1$newage, na.rm=T)
sns1$newage[is.na(sns1$newage)] = m


# sns2 = sns1 %>% 
#   filter(age >= 13, age <= 20)
# 
# any(is.na(sns2$age))



  #데이터 표준화(scaling)

scaled_sns = as.data.frame(scale(sns2[,5:40]))
#그냥하면 matrix 형태로 결과 도출됨.

class(scaled_sns)

sapply(scaled_sns, mean)
sapply(scaled_sns, sd)

summary(scaled_sns)

mean(scaled_sns[,"swimming"])
sd(scaled_sns[,"swimming"])

#K-means 군집분석하기

result = kmeans(scaled_sns,5)
result

result$size











=======
#클러스터 예제

#248~

#데이터 전처리

sns0<-read.csv("DataSet/snsdata.csv")

str(sns0)

  #gender변수 factor화 하기

sns0$gender = as.factor(sns0$gender)

summary(sns0)


  #gender 결측치 제거

sum(is.na(sns0$gender))
#na개수

sns1 = sns0[!is.na(sns0$gender),]

sns1 = sns0 %>% filter(!is.na(gender))


sum(is.na(sns1$gender))
#any(is.na(sns1$gender))


  #age 13이상 20이하 결측치 제거

sum(is.na(sns1$age))


  #방법1
sns2 = sns1 %>% 
  mutate(age_new = ifelse( age < 13 | age > 20, NA, age)) %>% 
  mutate(age_new2 = ifelse( is.na(age_new), mean(age_new, na.rm=T), age_new))

sns2$age_new2
sum(is.na(sns2$age_new2))

  #방법2

sns1$newage = ifelse(sns1$age < 13 | sns1$age >20, NA, sns1$age)

m = mean(sns1$newage, na.rm=T)
sns1$newage[is.na(sns1$newage)] = m


# sns2 = sns1 %>% 
#   filter(age >= 13, age <= 20)
# 
# any(is.na(sns2$age))



  #데이터 표준화(scaling)

scaled_sns = as.data.frame(scale(sns2[,5:40]))
#그냥하면 matrix 형태로 결과 도출됨.

class(scaled_sns)

sapply(scaled_sns, mean)
sapply(scaled_sns, sd)

summary(scaled_sns)

mean(scaled_sns[,"swimming"])
sd(scaled_sns[,"swimming"])

#K-means 군집분석하기

result = kmeans(scaled_sns,5)
result

result$size











>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======
#클러스터 예제

#248~

#데이터 전처리

sns0<-read.csv("DataSet/snsdata.csv")

str(sns0)

  #gender변수 factor화 하기

sns0$gender = as.factor(sns0$gender)

summary(sns0)


  #gender 결측치 제거

sum(is.na(sns0$gender))
#na개수

sns1 = sns0[!is.na(sns0$gender),]

sns1 = sns0 %>% filter(!is.na(gender))


sum(is.na(sns1$gender))
#any(is.na(sns1$gender))


  #age 13이상 20이하 결측치 제거

sum(is.na(sns1$age))


  #방법1
sns2 = sns1 %>% 
  mutate(age_new = ifelse( age < 13 | age > 20, NA, age)) %>% 
  mutate(age_new2 = ifelse( is.na(age_new), mean(age_new, na.rm=T), age_new))

sns2$age_new2
sum(is.na(sns2$age_new2))

  #방법2

sns1$newage = ifelse(sns1$age < 13 | sns1$age >20, NA, sns1$age)

m = mean(sns1$newage, na.rm=T)
sns1$newage[is.na(sns1$newage)] = m


# sns2 = sns1 %>% 
#   filter(age >= 13, age <= 20)
# 
# any(is.na(sns2$age))



  #데이터 표준화(scaling)

scaled_sns = as.data.frame(scale(sns2[,5:40]))
#그냥하면 matrix 형태로 결과 도출됨.

class(scaled_sns)

sapply(scaled_sns, mean)
sapply(scaled_sns, sd)

summary(scaled_sns)

mean(scaled_sns[,"swimming"])
sd(scaled_sns[,"swimming"])

#K-means 군집분석하기

result = kmeans(scaled_sns,5)
result

result$size











>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======
#클러스터 예제

#248~

#데이터 전처리

sns0<-read.csv("DataSet/snsdata.csv")

str(sns0)

  #gender변수 factor화 하기

sns0$gender = as.factor(sns0$gender)

summary(sns0)


  #gender 결측치 제거

sum(is.na(sns0$gender))
#na개수

sns1 = sns0[!is.na(sns0$gender),]

sns1 = sns0 %>% filter(!is.na(gender))


sum(is.na(sns1$gender))
#any(is.na(sns1$gender))


  #age 13이상 20이하 결측치 제거

sum(is.na(sns1$age))


  #방법1
sns2 = sns1 %>% 
  mutate(age_new = ifelse( age < 13 | age > 20, NA, age)) %>% 
  mutate(age_new2 = ifelse( is.na(age_new), mean(age_new, na.rm=T), age_new))

sns2$age_new2
sum(is.na(sns2$age_new2))

  #방법2

sns1$newage = ifelse(sns1$age < 13 | sns1$age >20, NA, sns1$age)

m = mean(sns1$newage, na.rm=T)
sns1$newage[is.na(sns1$newage)] = m


# sns2 = sns1 %>% 
#   filter(age >= 13, age <= 20)
# 
# any(is.na(sns2$age))



  #데이터 표준화(scaling)

scaled_sns = as.data.frame(scale(sns2[,5:40]))
#그냥하면 matrix 형태로 결과 도출됨.

class(scaled_sns)

sapply(scaled_sns, mean)
sapply(scaled_sns, sd)

summary(scaled_sns)

mean(scaled_sns[,"swimming"])
sd(scaled_sns[,"swimming"])

#K-means 군집분석하기

result = kmeans(scaled_sns,5)
result

result$size











>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======
#클러스터 예제

#248~

#데이터 전처리

sns0<-read.csv("DataSet/snsdata.csv")

str(sns0)

  #gender변수 factor화 하기

sns0$gender = as.factor(sns0$gender)

summary(sns0)


  #gender 결측치 제거

sum(is.na(sns0$gender))
#na개수

sns1 = sns0[!is.na(sns0$gender),]

sns1 = sns0 %>% filter(!is.na(gender))


sum(is.na(sns1$gender))
#any(is.na(sns1$gender))


  #age 13이상 20이하 결측치 제거

sum(is.na(sns1$age))


  #방법1
sns2 = sns1 %>% 
  mutate(age_new = ifelse( age < 13 | age > 20, NA, age)) %>% 
  mutate(age_new2 = ifelse( is.na(age_new), mean(age_new, na.rm=T), age_new))

sns2$age_new2
sum(is.na(sns2$age_new2))

  #방법2

sns1$newage = ifelse(sns1$age < 13 | sns1$age >20, NA, sns1$age)

m = mean(sns1$newage, na.rm=T)
sns1$newage[is.na(sns1$newage)] = m


# sns2 = sns1 %>% 
#   filter(age >= 13, age <= 20)
# 
# any(is.na(sns2$age))



  #데이터 표준화(scaling)

scaled_sns = as.data.frame(scale(sns2[,5:40]))
#그냥하면 matrix 형태로 결과 도출됨.

class(scaled_sns)

sapply(scaled_sns, mean)
sapply(scaled_sns, sd)

summary(scaled_sns)

mean(scaled_sns[,"swimming"])
sd(scaled_sns[,"swimming"])

#K-means 군집분석하기

result = kmeans(scaled_sns,5)
result

result$size











>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
