<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
#연습문제1
##datasets package에 있는 데이터셋 <state.x77>을 활용하여 다음의 문제를 풀어보세요

state.x77

#(1) Income이 4500보다 큰 주들은 어떤 주들인가? 몇 개의 주인가?

x = state.x77

x[,"Income">4500]

nrow(x[,"Income">4500])


#풀이

#조건판별
x[,"Income"] > 4500

#주들의 이름 풀이
x[x[,"Income"] > 4500, 0]

state.name[state.x77[,"Income"]>4500]

rownames(state.x77)[x[,"Income"] > 4500]
##rownames는 입력/출력 둘 다 가능하다.

#주 개수 풀이
nrow(x[x[,"Income"] > 4500, 0])
sum(x[,"Income"] > 4500)




#(2) Income이 4500보다 큰 주들의 Illiteracy 평균을 구해보세요

mean(x[,"Income">4500][,"Illiteracy"])

#(2)풀이

mean(x[x[,"Income"] > 4500, "Illiteracy"])


v1 = state.x77[,"Illiteracy"]
v2 = v1[state.x77[,"Income"]>4500]
mean(v1)
mean(v2)

v2



# 
# state.x77[ , "Income"] > 4500
# 
# sum(state.x77[ , "Income"] > 4500) # 26개
# state.name[state.x77[ , "Income"] > 4500]
# 
# s_name<-rownames(state.x77)
# s_name[state.x77[ , "Income"] > 4500]
# colnames(state.x77)
# 
# #(2)
# mean(state.x77[ , "Illiteracy"])
# 
# a<-state.x77[ , "Illiteracy"]
# mean(v)
# a2<-v[state.x77[ , "Income"] > 4500]
# mean(v2)






#연습문제2 (62p.)

#(1)

class(state.x77)

#(2)
apply(state.x77, 2, mean)

#(3)
df_state = data.frame(state.x77)

lapply(df_state, mean)

sapply(df_state, mean)


#해설

#(3)

colnames(as.data.frame(state.x77))
colnames(data.frame(state.x77))

#as를 사용하면 변수명에 있는 공백이 그대로 들어가서 불편하다.





#연습문제3(p.64)


#(1)

state.region

table(state.region)


#(2)

state_df = data.frame(state.x77, state.name, state.region)
state_df

#숫자 + 문자형으로 구성되어야 하므로, DF형만 가능하다.

#(3)

tapply(state_df$Income, state_df$state.region, mean )

aggregate(Income ~ state.region, state_df, mean )





#연습문제4(p.65)

#(1)(2)

df = read.csv("DataSet/titanic.csv")

str(df)


#(3)

condition = df$pclass == '1st'

mean(df[condition,'survived'])


#(4)

aggregate(survived ~ pclass, df, mean)
tapply(df$survived, df$pclass, mean)



#연습문제5(p.66)

#(1)

df$sex_num_male = df$sex=="male"
df$sex_num_female = df$sex=="female"

aggregate(cbind(sex_num_male,sex_num_female) ~ pclass, df, sum)


condition1 = df$pclass == '1st'

condition1

condition2 = df[condition1, 'age']>=20

df_20 = df[condition2, age]



#연습문제 4, 5(p. 65 66) 해설

#(1)(2)
titanic = read.csv(file="DataSet/titanic.csv")
str(titanic)


#(3)-1

table(titanic$pclass)

sub = subset(titanic, pclass == '1st')

nrow(sub)

mean(sub$survived)


#(3)-2

mean(titanic[titanic$pclass == '1st', 'survived'])


#(4)-1
tapply(titanic$survived, titanic$pclass, mean)

#(4)-2
aggregate(survived ~ pclass, titanic, mean)


#(5)

aggregate(sex ~ pclass, titanic, table)

#(5)-2
tapply(titanic$sex, titanic$pclass, table)

#(5)-3
table(titanic$sex, titanic$pclass)


#(6)
#NA 값을 고려해야함.
##고려하는 방법은 매우 많지만 일단 제거하는 걸로 하자.

length(titanic$age)
sum(is.na(titanic$age))


sub1 = subset(titanic, age>=20)
sub2 = subset(titanic, !is.na(age))

any(is.na(sub1$age))

nrow(sub2)

nrow(sub1)/nrow(sub2)
  #전체 성인의 비율

nrow(sub1[sub1$pclass=='1st',]) / nrow(sub2[sub1$pclass=='1st',])
  #1등석 성인 비율


#(7)

titanic$AgeGroup = ifelse(titanic$age>18, 'adult', 'child')
View(titanic)
  #missing 값은 그대로 들어감 확인.


aggregate(survived ~ AgeGroup + pclass + sex, titanic, mean)



#titanic 문제 dplyr로 풀어보기


titanic = read.csv("DataSet/titanic.csv")

#1 #2

str(titanic)


#3

titanic %>% 
  filter(pclass == "1st") %>% 
  summarise(survived_ratio = mean(survived))


#4

titanic %>% 
  group_by(pclass) %>% 
  summarise(survived_mean = mean(survived)) %>% 
  select(pclass, survived_mean)


#5

titanic %>% 
  group_by(pclass,sex) %>% 
  summarise(table(sex))

#6

num = titanic %>% 
  filter(!is.na(age), pclass == "1st") %>% 
  summarise(n = n())

num_adult = titanic %>% 
  filter(age>=20, pclass == '1st') %>% 
  summarise(n=n())

num_adult/num

#7

titanic %>% 
  mutate(age_group = ifelse(age>=18, "adult", "child")) %>%
  filter(!is.na(age)) %>% 
  group_by(age_group, pclass,  sex ) %>% 
  summarise(sur_mean = mean(survived))




#병원자료 실습

#100p.

#1
admissions = read.csv("DataSet/admissions.csv")

str(admissions)

admissions$ADMDATE2 = as.Date(admissions$ADMDATE, "%Y-%m-%d")

admissions$DISDATE2 = as.Date(admissions$DISDATE, "%Y-%m-%d")

head(admissions)

#2
admissions %>% 
  filter(HOSP==3) %>% 
  summarise(freq = n())

#101p.

#1
admissions['PRIMDX']

admissions %>%
  mutate(p_group = ifelse(substr(PRIMDX, 1, 3)=="410", "MI", 
                          ifelse(substr(PRIMDX,1,3)=="428", "CHF", "Others"))) %>% 
  group_by(p_group) %>% 
  summarise(n = n())

         
#2
admissions %>% 
  mutate(Period = DISDATE2-ADMDATE2+1) %>% 
  filter(Period>=14) %>% 
  select(PT_ID, ADMDATE, DISDATE, Period, HOSP) %>% 
  head(7)


admissions %>% 
  mutate(Period = as.Date(DISDATE,'%Y-%m-%d')-as.Date(ADMDATE,'%Y-%m-%d')+1) %>% 
  filter(Period>=14) %>% 
  select(PT_ID, ADMDATE, DISDATE, Period, HOSP)
head(7)

  
#102p.

#1

admissions %>% 
  group_by(PT_ID) %>% 
  mutate(days = DISDATE2-ADMDATE2+1) %>% 
  summarise(nstay = n(), min_stay = min(days), max_stay = max(days))


#2
hospital = read.csv("DataSet/hospital.csv")

names(hospital)

hosp_list = hospital %>% 
  filter(hospname %in% c("Our Lady of Charity", "Community Hospital")) %>% 
  select(hosp_id)

hosp_list

admissions %>% 
  filter(HOSP %in% hosp_list$hosp_id) %>% 
  select(PT_ID, ADMDATE, DISDATE, HOSP)

#103p.

head(hospital)

tmp = admissions %>% 
  rename(hosp_id = HOSP) %>% 
  group_by(hosp_id) %>% 
  summarise(Freq = n())

left_join(hospital, tmp, by="hosp_id")
=======
#연습문제1
##datasets package에 있는 데이터셋 <state.x77>을 활용하여 다음의 문제를 풀어보세요

state.x77

#(1) Income이 4500보다 큰 주들은 어떤 주들인가? 몇 개의 주인가?

x = state.x77

x[,"Income">4500]

nrow(x[,"Income">4500])


#풀이

#조건판별
x[,"Income"] > 4500

#주들의 이름 풀이
x[x[,"Income"] > 4500, 0]

state.name[state.x77[,"Income"]>4500]

rownames(state.x77)[x[,"Income"] > 4500]
##rownames는 입력/출력 둘 다 가능하다.

#주 개수 풀이
nrow(x[x[,"Income"] > 4500, 0])
sum(x[,"Income"] > 4500)




#(2) Income이 4500보다 큰 주들의 Illiteracy 평균을 구해보세요

mean(x[,"Income">4500][,"Illiteracy"])

#(2)풀이

mean(x[x[,"Income"] > 4500, "Illiteracy"])


v1 = state.x77[,"Illiteracy"]
v2 = v1[state.x77[,"Income"]>4500]
mean(v1)
mean(v2)

v2



# 
# state.x77[ , "Income"] > 4500
# 
# sum(state.x77[ , "Income"] > 4500) # 26개
# state.name[state.x77[ , "Income"] > 4500]
# 
# s_name<-rownames(state.x77)
# s_name[state.x77[ , "Income"] > 4500]
# colnames(state.x77)
# 
# #(2)
# mean(state.x77[ , "Illiteracy"])
# 
# a<-state.x77[ , "Illiteracy"]
# mean(v)
# a2<-v[state.x77[ , "Income"] > 4500]
# mean(v2)






#연습문제2 (62p.)

#(1)

class(state.x77)

#(2)
apply(state.x77, 2, mean)

#(3)
df_state = data.frame(state.x77)

lapply(df_state, mean)

sapply(df_state, mean)


#해설

#(3)

colnames(as.data.frame(state.x77))
colnames(data.frame(state.x77))

#as를 사용하면 변수명에 있는 공백이 그대로 들어가서 불편하다.





#연습문제3(p.64)


#(1)

state.region

table(state.region)


#(2)

state_df = data.frame(state.x77, state.name, state.region)
state_df

#숫자 + 문자형으로 구성되어야 하므로, DF형만 가능하다.

#(3)

tapply(state_df$Income, state_df$state.region, mean )

aggregate(Income ~ state.region, state_df, mean )





#연습문제4(p.65)

#(1)(2)

df = read.csv("DataSet/titanic.csv")

str(df)


#(3)

condition = df$pclass == '1st'

mean(df[condition,'survived'])


#(4)

aggregate(survived ~ pclass, df, mean)
tapply(df$survived, df$pclass, mean)



#연습문제5(p.66)

#(1)

df$sex_num_male = df$sex=="male"
df$sex_num_female = df$sex=="female"

aggregate(cbind(sex_num_male,sex_num_female) ~ pclass, df, sum)


condition1 = df$pclass == '1st'

condition1

condition2 = df[condition1, 'age']>=20

df_20 = df[condition2, age]



#연습문제 4, 5(p. 65 66) 해설

#(1)(2)
titanic = read.csv(file="DataSet/titanic.csv")
str(titanic)


#(3)-1

table(titanic$pclass)

sub = subset(titanic, pclass == '1st')

nrow(sub)

mean(sub$survived)


#(3)-2

mean(titanic[titanic$pclass == '1st', 'survived'])


#(4)-1
tapply(titanic$survived, titanic$pclass, mean)

#(4)-2
aggregate(survived ~ pclass, titanic, mean)


#(5)

aggregate(sex ~ pclass, titanic, table)

#(5)-2
tapply(titanic$sex, titanic$pclass, table)

#(5)-3
table(titanic$sex, titanic$pclass)


#(6)
#NA 값을 고려해야함.
##고려하는 방법은 매우 많지만 일단 제거하는 걸로 하자.

length(titanic$age)
sum(is.na(titanic$age))


sub1 = subset(titanic, age>=20)
sub2 = subset(titanic, !is.na(age))

any(is.na(sub1$age))

nrow(sub2)

nrow(sub1)/nrow(sub2)
  #전체 성인의 비율

nrow(sub1[sub1$pclass=='1st',]) / nrow(sub2[sub1$pclass=='1st',])
  #1등석 성인 비율


#(7)

titanic$AgeGroup = ifelse(titanic$age>18, 'adult', 'child')
View(titanic)
  #missing 값은 그대로 들어감 확인.


aggregate(survived ~ AgeGroup + pclass + sex, titanic, mean)



#titanic 문제 dplyr로 풀어보기


titanic = read.csv("DataSet/titanic.csv")

#1 #2

str(titanic)


#3

titanic %>% 
  filter(pclass == "1st") %>% 
  summarise(survived_ratio = mean(survived))


#4

titanic %>% 
  group_by(pclass) %>% 
  summarise(survived_mean = mean(survived)) %>% 
  select(pclass, survived_mean)


#5

titanic %>% 
  group_by(pclass,sex) %>% 
  summarise(table(sex))

#6

num = titanic %>% 
  filter(!is.na(age), pclass == "1st") %>% 
  summarise(n = n())

num_adult = titanic %>% 
  filter(age>=20, pclass == '1st') %>% 
  summarise(n=n())

num_adult/num

#7

titanic %>% 
  mutate(age_group = ifelse(age>=18, "adult", "child")) %>%
  filter(!is.na(age)) %>% 
  group_by(age_group, pclass,  sex ) %>% 
  summarise(sur_mean = mean(survived))




#병원자료 실습

#100p.

#1
admissions = read.csv("DataSet/admissions.csv")

str(admissions)

admissions$ADMDATE2 = as.Date(admissions$ADMDATE, "%Y-%m-%d")

admissions$DISDATE2 = as.Date(admissions$DISDATE, "%Y-%m-%d")

head(admissions)

#2
admissions %>% 
  filter(HOSP==3) %>% 
  summarise(freq = n())

#101p.

#1
admissions['PRIMDX']

admissions %>%
  mutate(p_group = ifelse(substr(PRIMDX, 1, 3)=="410", "MI", 
                          ifelse(substr(PRIMDX,1,3)=="428", "CHF", "Others"))) %>% 
  group_by(p_group) %>% 
  summarise(n = n())

         
#2
admissions %>% 
  mutate(Period = DISDATE2-ADMDATE2+1) %>% 
  filter(Period>=14) %>% 
  select(PT_ID, ADMDATE, DISDATE, Period, HOSP) %>% 
  head(7)


admissions %>% 
  mutate(Period = as.Date(DISDATE,'%Y-%m-%d')-as.Date(ADMDATE,'%Y-%m-%d')+1) %>% 
  filter(Period>=14) %>% 
  select(PT_ID, ADMDATE, DISDATE, Period, HOSP)
head(7)

  
#102p.

#1

admissions %>% 
  group_by(PT_ID) %>% 
  mutate(days = DISDATE2-ADMDATE2+1) %>% 
  summarise(nstay = n(), min_stay = min(days), max_stay = max(days))


#2
hospital = read.csv("DataSet/hospital.csv")

names(hospital)

hosp_list = hospital %>% 
  filter(hospname %in% c("Our Lady of Charity", "Community Hospital")) %>% 
  select(hosp_id)

hosp_list

admissions %>% 
  filter(HOSP %in% hosp_list$hosp_id) %>% 
  select(PT_ID, ADMDATE, DISDATE, HOSP)

#103p.

head(hospital)

tmp = admissions %>% 
  rename(hosp_id = HOSP) %>% 
  group_by(hosp_id) %>% 
  summarise(Freq = n())

left_join(hospital, tmp, by="hosp_id")
>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======
#연습문제1
##datasets package에 있는 데이터셋 <state.x77>을 활용하여 다음의 문제를 풀어보세요

state.x77

#(1) Income이 4500보다 큰 주들은 어떤 주들인가? 몇 개의 주인가?

x = state.x77

x[,"Income">4500]

nrow(x[,"Income">4500])


#풀이

#조건판별
x[,"Income"] > 4500

#주들의 이름 풀이
x[x[,"Income"] > 4500, 0]

state.name[state.x77[,"Income"]>4500]

rownames(state.x77)[x[,"Income"] > 4500]
##rownames는 입력/출력 둘 다 가능하다.

#주 개수 풀이
nrow(x[x[,"Income"] > 4500, 0])
sum(x[,"Income"] > 4500)




#(2) Income이 4500보다 큰 주들의 Illiteracy 평균을 구해보세요

mean(x[,"Income">4500][,"Illiteracy"])

#(2)풀이

mean(x[x[,"Income"] > 4500, "Illiteracy"])


v1 = state.x77[,"Illiteracy"]
v2 = v1[state.x77[,"Income"]>4500]
mean(v1)
mean(v2)

v2



# 
# state.x77[ , "Income"] > 4500
# 
# sum(state.x77[ , "Income"] > 4500) # 26개
# state.name[state.x77[ , "Income"] > 4500]
# 
# s_name<-rownames(state.x77)
# s_name[state.x77[ , "Income"] > 4500]
# colnames(state.x77)
# 
# #(2)
# mean(state.x77[ , "Illiteracy"])
# 
# a<-state.x77[ , "Illiteracy"]
# mean(v)
# a2<-v[state.x77[ , "Income"] > 4500]
# mean(v2)






#연습문제2 (62p.)

#(1)

class(state.x77)

#(2)
apply(state.x77, 2, mean)

#(3)
df_state = data.frame(state.x77)

lapply(df_state, mean)

sapply(df_state, mean)


#해설

#(3)

colnames(as.data.frame(state.x77))
colnames(data.frame(state.x77))

#as를 사용하면 변수명에 있는 공백이 그대로 들어가서 불편하다.





#연습문제3(p.64)


#(1)

state.region

table(state.region)


#(2)

state_df = data.frame(state.x77, state.name, state.region)
state_df

#숫자 + 문자형으로 구성되어야 하므로, DF형만 가능하다.

#(3)

tapply(state_df$Income, state_df$state.region, mean )

aggregate(Income ~ state.region, state_df, mean )





#연습문제4(p.65)

#(1)(2)

df = read.csv("DataSet/titanic.csv")

str(df)


#(3)

condition = df$pclass == '1st'

mean(df[condition,'survived'])


#(4)

aggregate(survived ~ pclass, df, mean)
tapply(df$survived, df$pclass, mean)



#연습문제5(p.66)

#(1)

df$sex_num_male = df$sex=="male"
df$sex_num_female = df$sex=="female"

aggregate(cbind(sex_num_male,sex_num_female) ~ pclass, df, sum)


condition1 = df$pclass == '1st'

condition1

condition2 = df[condition1, 'age']>=20

df_20 = df[condition2, age]



#연습문제 4, 5(p. 65 66) 해설

#(1)(2)
titanic = read.csv(file="DataSet/titanic.csv")
str(titanic)


#(3)-1

table(titanic$pclass)

sub = subset(titanic, pclass == '1st')

nrow(sub)

mean(sub$survived)


#(3)-2

mean(titanic[titanic$pclass == '1st', 'survived'])


#(4)-1
tapply(titanic$survived, titanic$pclass, mean)

#(4)-2
aggregate(survived ~ pclass, titanic, mean)


#(5)

aggregate(sex ~ pclass, titanic, table)

#(5)-2
tapply(titanic$sex, titanic$pclass, table)

#(5)-3
table(titanic$sex, titanic$pclass)


#(6)
#NA 값을 고려해야함.
##고려하는 방법은 매우 많지만 일단 제거하는 걸로 하자.

length(titanic$age)
sum(is.na(titanic$age))


sub1 = subset(titanic, age>=20)
sub2 = subset(titanic, !is.na(age))

any(is.na(sub1$age))

nrow(sub2)

nrow(sub1)/nrow(sub2)
  #전체 성인의 비율

nrow(sub1[sub1$pclass=='1st',]) / nrow(sub2[sub1$pclass=='1st',])
  #1등석 성인 비율


#(7)

titanic$AgeGroup = ifelse(titanic$age>18, 'adult', 'child')
View(titanic)
  #missing 값은 그대로 들어감 확인.


aggregate(survived ~ AgeGroup + pclass + sex, titanic, mean)



#titanic 문제 dplyr로 풀어보기


titanic = read.csv("DataSet/titanic.csv")

#1 #2

str(titanic)


#3

titanic %>% 
  filter(pclass == "1st") %>% 
  summarise(survived_ratio = mean(survived))


#4

titanic %>% 
  group_by(pclass) %>% 
  summarise(survived_mean = mean(survived)) %>% 
  select(pclass, survived_mean)


#5

titanic %>% 
  group_by(pclass,sex) %>% 
  summarise(table(sex))

#6

num = titanic %>% 
  filter(!is.na(age), pclass == "1st") %>% 
  summarise(n = n())

num_adult = titanic %>% 
  filter(age>=20, pclass == '1st') %>% 
  summarise(n=n())

num_adult/num

#7

titanic %>% 
  mutate(age_group = ifelse(age>=18, "adult", "child")) %>%
  filter(!is.na(age)) %>% 
  group_by(age_group, pclass,  sex ) %>% 
  summarise(sur_mean = mean(survived))




#병원자료 실습

#100p.

#1
admissions = read.csv("DataSet/admissions.csv")

str(admissions)

admissions$ADMDATE2 = as.Date(admissions$ADMDATE, "%Y-%m-%d")

admissions$DISDATE2 = as.Date(admissions$DISDATE, "%Y-%m-%d")

head(admissions)

#2
admissions %>% 
  filter(HOSP==3) %>% 
  summarise(freq = n())

#101p.

#1
admissions['PRIMDX']

admissions %>%
  mutate(p_group = ifelse(substr(PRIMDX, 1, 3)=="410", "MI", 
                          ifelse(substr(PRIMDX,1,3)=="428", "CHF", "Others"))) %>% 
  group_by(p_group) %>% 
  summarise(n = n())

         
#2
admissions %>% 
  mutate(Period = DISDATE2-ADMDATE2+1) %>% 
  filter(Period>=14) %>% 
  select(PT_ID, ADMDATE, DISDATE, Period, HOSP) %>% 
  head(7)


admissions %>% 
  mutate(Period = as.Date(DISDATE,'%Y-%m-%d')-as.Date(ADMDATE,'%Y-%m-%d')+1) %>% 
  filter(Period>=14) %>% 
  select(PT_ID, ADMDATE, DISDATE, Period, HOSP)
head(7)

  
#102p.

#1

admissions %>% 
  group_by(PT_ID) %>% 
  mutate(days = DISDATE2-ADMDATE2+1) %>% 
  summarise(nstay = n(), min_stay = min(days), max_stay = max(days))


#2
hospital = read.csv("DataSet/hospital.csv")

names(hospital)

hosp_list = hospital %>% 
  filter(hospname %in% c("Our Lady of Charity", "Community Hospital")) %>% 
  select(hosp_id)

hosp_list

admissions %>% 
  filter(HOSP %in% hosp_list$hosp_id) %>% 
  select(PT_ID, ADMDATE, DISDATE, HOSP)

#103p.

head(hospital)

tmp = admissions %>% 
  rename(hosp_id = HOSP) %>% 
  group_by(hosp_id) %>% 
  summarise(Freq = n())

left_join(hospital, tmp, by="hosp_id")
>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======
#연습문제1
##datasets package에 있는 데이터셋 <state.x77>을 활용하여 다음의 문제를 풀어보세요

state.x77

#(1) Income이 4500보다 큰 주들은 어떤 주들인가? 몇 개의 주인가?

x = state.x77

x[,"Income">4500]

nrow(x[,"Income">4500])


#풀이

#조건판별
x[,"Income"] > 4500

#주들의 이름 풀이
x[x[,"Income"] > 4500, 0]

state.name[state.x77[,"Income"]>4500]

rownames(state.x77)[x[,"Income"] > 4500]
##rownames는 입력/출력 둘 다 가능하다.

#주 개수 풀이
nrow(x[x[,"Income"] > 4500, 0])
sum(x[,"Income"] > 4500)




#(2) Income이 4500보다 큰 주들의 Illiteracy 평균을 구해보세요

mean(x[,"Income">4500][,"Illiteracy"])

#(2)풀이

mean(x[x[,"Income"] > 4500, "Illiteracy"])


v1 = state.x77[,"Illiteracy"]
v2 = v1[state.x77[,"Income"]>4500]
mean(v1)
mean(v2)

v2



# 
# state.x77[ , "Income"] > 4500
# 
# sum(state.x77[ , "Income"] > 4500) # 26개
# state.name[state.x77[ , "Income"] > 4500]
# 
# s_name<-rownames(state.x77)
# s_name[state.x77[ , "Income"] > 4500]
# colnames(state.x77)
# 
# #(2)
# mean(state.x77[ , "Illiteracy"])
# 
# a<-state.x77[ , "Illiteracy"]
# mean(v)
# a2<-v[state.x77[ , "Income"] > 4500]
# mean(v2)






#연습문제2 (62p.)

#(1)

class(state.x77)

#(2)
apply(state.x77, 2, mean)

#(3)
df_state = data.frame(state.x77)

lapply(df_state, mean)

sapply(df_state, mean)


#해설

#(3)

colnames(as.data.frame(state.x77))
colnames(data.frame(state.x77))

#as를 사용하면 변수명에 있는 공백이 그대로 들어가서 불편하다.





#연습문제3(p.64)


#(1)

state.region

table(state.region)


#(2)

state_df = data.frame(state.x77, state.name, state.region)
state_df

#숫자 + 문자형으로 구성되어야 하므로, DF형만 가능하다.

#(3)

tapply(state_df$Income, state_df$state.region, mean )

aggregate(Income ~ state.region, state_df, mean )





#연습문제4(p.65)

#(1)(2)

df = read.csv("DataSet/titanic.csv")

str(df)


#(3)

condition = df$pclass == '1st'

mean(df[condition,'survived'])


#(4)

aggregate(survived ~ pclass, df, mean)
tapply(df$survived, df$pclass, mean)



#연습문제5(p.66)

#(1)

df$sex_num_male = df$sex=="male"
df$sex_num_female = df$sex=="female"

aggregate(cbind(sex_num_male,sex_num_female) ~ pclass, df, sum)


condition1 = df$pclass == '1st'

condition1

condition2 = df[condition1, 'age']>=20

df_20 = df[condition2, age]



#연습문제 4, 5(p. 65 66) 해설

#(1)(2)
titanic = read.csv(file="DataSet/titanic.csv")
str(titanic)


#(3)-1

table(titanic$pclass)

sub = subset(titanic, pclass == '1st')

nrow(sub)

mean(sub$survived)


#(3)-2

mean(titanic[titanic$pclass == '1st', 'survived'])


#(4)-1
tapply(titanic$survived, titanic$pclass, mean)

#(4)-2
aggregate(survived ~ pclass, titanic, mean)


#(5)

aggregate(sex ~ pclass, titanic, table)

#(5)-2
tapply(titanic$sex, titanic$pclass, table)

#(5)-3
table(titanic$sex, titanic$pclass)


#(6)
#NA 값을 고려해야함.
##고려하는 방법은 매우 많지만 일단 제거하는 걸로 하자.

length(titanic$age)
sum(is.na(titanic$age))


sub1 = subset(titanic, age>=20)
sub2 = subset(titanic, !is.na(age))

any(is.na(sub1$age))

nrow(sub2)

nrow(sub1)/nrow(sub2)
  #전체 성인의 비율

nrow(sub1[sub1$pclass=='1st',]) / nrow(sub2[sub1$pclass=='1st',])
  #1등석 성인 비율


#(7)

titanic$AgeGroup = ifelse(titanic$age>18, 'adult', 'child')
View(titanic)
  #missing 값은 그대로 들어감 확인.


aggregate(survived ~ AgeGroup + pclass + sex, titanic, mean)



#titanic 문제 dplyr로 풀어보기


titanic = read.csv("DataSet/titanic.csv")

#1 #2

str(titanic)


#3

titanic %>% 
  filter(pclass == "1st") %>% 
  summarise(survived_ratio = mean(survived))


#4

titanic %>% 
  group_by(pclass) %>% 
  summarise(survived_mean = mean(survived)) %>% 
  select(pclass, survived_mean)


#5

titanic %>% 
  group_by(pclass,sex) %>% 
  summarise(table(sex))

#6

num = titanic %>% 
  filter(!is.na(age), pclass == "1st") %>% 
  summarise(n = n())

num_adult = titanic %>% 
  filter(age>=20, pclass == '1st') %>% 
  summarise(n=n())

num_adult/num

#7

titanic %>% 
  mutate(age_group = ifelse(age>=18, "adult", "child")) %>%
  filter(!is.na(age)) %>% 
  group_by(age_group, pclass,  sex ) %>% 
  summarise(sur_mean = mean(survived))




#병원자료 실습

#100p.

#1
admissions = read.csv("DataSet/admissions.csv")

str(admissions)

admissions$ADMDATE2 = as.Date(admissions$ADMDATE, "%Y-%m-%d")

admissions$DISDATE2 = as.Date(admissions$DISDATE, "%Y-%m-%d")

head(admissions)

#2
admissions %>% 
  filter(HOSP==3) %>% 
  summarise(freq = n())

#101p.

#1
admissions['PRIMDX']

admissions %>%
  mutate(p_group = ifelse(substr(PRIMDX, 1, 3)=="410", "MI", 
                          ifelse(substr(PRIMDX,1,3)=="428", "CHF", "Others"))) %>% 
  group_by(p_group) %>% 
  summarise(n = n())

         
#2
admissions %>% 
  mutate(Period = DISDATE2-ADMDATE2+1) %>% 
  filter(Period>=14) %>% 
  select(PT_ID, ADMDATE, DISDATE, Period, HOSP) %>% 
  head(7)


admissions %>% 
  mutate(Period = as.Date(DISDATE,'%Y-%m-%d')-as.Date(ADMDATE,'%Y-%m-%d')+1) %>% 
  filter(Period>=14) %>% 
  select(PT_ID, ADMDATE, DISDATE, Period, HOSP)
head(7)

  
#102p.

#1

admissions %>% 
  group_by(PT_ID) %>% 
  mutate(days = DISDATE2-ADMDATE2+1) %>% 
  summarise(nstay = n(), min_stay = min(days), max_stay = max(days))


#2
hospital = read.csv("DataSet/hospital.csv")

names(hospital)

hosp_list = hospital %>% 
  filter(hospname %in% c("Our Lady of Charity", "Community Hospital")) %>% 
  select(hosp_id)

hosp_list

admissions %>% 
  filter(HOSP %in% hosp_list$hosp_id) %>% 
  select(PT_ID, ADMDATE, DISDATE, HOSP)

#103p.

head(hospital)

tmp = admissions %>% 
  rename(hosp_id = HOSP) %>% 
  group_by(hosp_id) %>% 
  summarise(Freq = n())

left_join(hospital, tmp, by="hosp_id")
>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======
#연습문제1
##datasets package에 있는 데이터셋 <state.x77>을 활용하여 다음의 문제를 풀어보세요

state.x77

#(1) Income이 4500보다 큰 주들은 어떤 주들인가? 몇 개의 주인가?

x = state.x77

x[,"Income">4500]

nrow(x[,"Income">4500])


#풀이

#조건판별
x[,"Income"] > 4500

#주들의 이름 풀이
x[x[,"Income"] > 4500, 0]

state.name[state.x77[,"Income"]>4500]

rownames(state.x77)[x[,"Income"] > 4500]
##rownames는 입력/출력 둘 다 가능하다.

#주 개수 풀이
nrow(x[x[,"Income"] > 4500, 0])
sum(x[,"Income"] > 4500)




#(2) Income이 4500보다 큰 주들의 Illiteracy 평균을 구해보세요

mean(x[,"Income">4500][,"Illiteracy"])

#(2)풀이

mean(x[x[,"Income"] > 4500, "Illiteracy"])


v1 = state.x77[,"Illiteracy"]
v2 = v1[state.x77[,"Income"]>4500]
mean(v1)
mean(v2)

v2



# 
# state.x77[ , "Income"] > 4500
# 
# sum(state.x77[ , "Income"] > 4500) # 26개
# state.name[state.x77[ , "Income"] > 4500]
# 
# s_name<-rownames(state.x77)
# s_name[state.x77[ , "Income"] > 4500]
# colnames(state.x77)
# 
# #(2)
# mean(state.x77[ , "Illiteracy"])
# 
# a<-state.x77[ , "Illiteracy"]
# mean(v)
# a2<-v[state.x77[ , "Income"] > 4500]
# mean(v2)






#연습문제2 (62p.)

#(1)

class(state.x77)

#(2)
apply(state.x77, 2, mean)

#(3)
df_state = data.frame(state.x77)

lapply(df_state, mean)

sapply(df_state, mean)


#해설

#(3)

colnames(as.data.frame(state.x77))
colnames(data.frame(state.x77))

#as를 사용하면 변수명에 있는 공백이 그대로 들어가서 불편하다.





#연습문제3(p.64)


#(1)

state.region

table(state.region)


#(2)

state_df = data.frame(state.x77, state.name, state.region)
state_df

#숫자 + 문자형으로 구성되어야 하므로, DF형만 가능하다.

#(3)

tapply(state_df$Income, state_df$state.region, mean )

aggregate(Income ~ state.region, state_df, mean )





#연습문제4(p.65)

#(1)(2)

df = read.csv("DataSet/titanic.csv")

str(df)


#(3)

condition = df$pclass == '1st'

mean(df[condition,'survived'])


#(4)

aggregate(survived ~ pclass, df, mean)
tapply(df$survived, df$pclass, mean)



#연습문제5(p.66)

#(1)

df$sex_num_male = df$sex=="male"
df$sex_num_female = df$sex=="female"

aggregate(cbind(sex_num_male,sex_num_female) ~ pclass, df, sum)


condition1 = df$pclass == '1st'

condition1

condition2 = df[condition1, 'age']>=20

df_20 = df[condition2, age]



#연습문제 4, 5(p. 65 66) 해설

#(1)(2)
titanic = read.csv(file="DataSet/titanic.csv")
str(titanic)


#(3)-1

table(titanic$pclass)

sub = subset(titanic, pclass == '1st')

nrow(sub)

mean(sub$survived)


#(3)-2

mean(titanic[titanic$pclass == '1st', 'survived'])


#(4)-1
tapply(titanic$survived, titanic$pclass, mean)

#(4)-2
aggregate(survived ~ pclass, titanic, mean)


#(5)

aggregate(sex ~ pclass, titanic, table)

#(5)-2
tapply(titanic$sex, titanic$pclass, table)

#(5)-3
table(titanic$sex, titanic$pclass)


#(6)
#NA 값을 고려해야함.
##고려하는 방법은 매우 많지만 일단 제거하는 걸로 하자.

length(titanic$age)
sum(is.na(titanic$age))


sub1 = subset(titanic, age>=20)
sub2 = subset(titanic, !is.na(age))

any(is.na(sub1$age))

nrow(sub2)

nrow(sub1)/nrow(sub2)
  #전체 성인의 비율

nrow(sub1[sub1$pclass=='1st',]) / nrow(sub2[sub1$pclass=='1st',])
  #1등석 성인 비율


#(7)

titanic$AgeGroup = ifelse(titanic$age>18, 'adult', 'child')
View(titanic)
  #missing 값은 그대로 들어감 확인.


aggregate(survived ~ AgeGroup + pclass + sex, titanic, mean)



#titanic 문제 dplyr로 풀어보기


titanic = read.csv("DataSet/titanic.csv")

#1 #2

str(titanic)


#3

titanic %>% 
  filter(pclass == "1st") %>% 
  summarise(survived_ratio = mean(survived))


#4

titanic %>% 
  group_by(pclass) %>% 
  summarise(survived_mean = mean(survived)) %>% 
  select(pclass, survived_mean)


#5

titanic %>% 
  group_by(pclass,sex) %>% 
  summarise(table(sex))

#6

num = titanic %>% 
  filter(!is.na(age), pclass == "1st") %>% 
  summarise(n = n())

num_adult = titanic %>% 
  filter(age>=20, pclass == '1st') %>% 
  summarise(n=n())

num_adult/num

#7

titanic %>% 
  mutate(age_group = ifelse(age>=18, "adult", "child")) %>%
  filter(!is.na(age)) %>% 
  group_by(age_group, pclass,  sex ) %>% 
  summarise(sur_mean = mean(survived))




#병원자료 실습

#100p.

#1
admissions = read.csv("DataSet/admissions.csv")

str(admissions)

admissions$ADMDATE2 = as.Date(admissions$ADMDATE, "%Y-%m-%d")

admissions$DISDATE2 = as.Date(admissions$DISDATE, "%Y-%m-%d")

head(admissions)

#2
admissions %>% 
  filter(HOSP==3) %>% 
  summarise(freq = n())

#101p.

#1
admissions['PRIMDX']

admissions %>%
  mutate(p_group = ifelse(substr(PRIMDX, 1, 3)=="410", "MI", 
                          ifelse(substr(PRIMDX,1,3)=="428", "CHF", "Others"))) %>% 
  group_by(p_group) %>% 
  summarise(n = n())

         
#2
admissions %>% 
  mutate(Period = DISDATE2-ADMDATE2+1) %>% 
  filter(Period>=14) %>% 
  select(PT_ID, ADMDATE, DISDATE, Period, HOSP) %>% 
  head(7)


admissions %>% 
  mutate(Period = as.Date(DISDATE,'%Y-%m-%d')-as.Date(ADMDATE,'%Y-%m-%d')+1) %>% 
  filter(Period>=14) %>% 
  select(PT_ID, ADMDATE, DISDATE, Period, HOSP)
head(7)

  
#102p.

#1

admissions %>% 
  group_by(PT_ID) %>% 
  mutate(days = DISDATE2-ADMDATE2+1) %>% 
  summarise(nstay = n(), min_stay = min(days), max_stay = max(days))


#2
hospital = read.csv("DataSet/hospital.csv")

names(hospital)

hosp_list = hospital %>% 
  filter(hospname %in% c("Our Lady of Charity", "Community Hospital")) %>% 
  select(hosp_id)

hosp_list

admissions %>% 
  filter(HOSP %in% hosp_list$hosp_id) %>% 
  select(PT_ID, ADMDATE, DISDATE, HOSP)

#103p.

head(hospital)

tmp = admissions %>% 
  rename(hosp_id = HOSP) %>% 
  group_by(hosp_id) %>% 
  summarise(Freq = n())

left_join(hospital, tmp, by="hosp_id")
>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======
#연습문제1
##datasets package에 있는 데이터셋 <state.x77>을 활용하여 다음의 문제를 풀어보세요

state.x77

#(1) Income이 4500보다 큰 주들은 어떤 주들인가? 몇 개의 주인가?

x = state.x77

x[,"Income">4500]

nrow(x[,"Income">4500])


#풀이

#조건판별
x[,"Income"] > 4500

#주들의 이름 풀이
x[x[,"Income"] > 4500, 0]

state.name[state.x77[,"Income"]>4500]

rownames(state.x77)[x[,"Income"] > 4500]
##rownames는 입력/출력 둘 다 가능하다.

#주 개수 풀이
nrow(x[x[,"Income"] > 4500, 0])
sum(x[,"Income"] > 4500)




#(2) Income이 4500보다 큰 주들의 Illiteracy 평균을 구해보세요

mean(x[,"Income">4500][,"Illiteracy"])

#(2)풀이

mean(x[x[,"Income"] > 4500, "Illiteracy"])


v1 = state.x77[,"Illiteracy"]
v2 = v1[state.x77[,"Income"]>4500]
mean(v1)
mean(v2)

v2



# 
# state.x77[ , "Income"] > 4500
# 
# sum(state.x77[ , "Income"] > 4500) # 26개
# state.name[state.x77[ , "Income"] > 4500]
# 
# s_name<-rownames(state.x77)
# s_name[state.x77[ , "Income"] > 4500]
# colnames(state.x77)
# 
# #(2)
# mean(state.x77[ , "Illiteracy"])
# 
# a<-state.x77[ , "Illiteracy"]
# mean(v)
# a2<-v[state.x77[ , "Income"] > 4500]
# mean(v2)






#연습문제2 (62p.)

#(1)

class(state.x77)

#(2)
apply(state.x77, 2, mean)

#(3)
df_state = data.frame(state.x77)

lapply(df_state, mean)

sapply(df_state, mean)


#해설

#(3)

colnames(as.data.frame(state.x77))
colnames(data.frame(state.x77))

#as를 사용하면 변수명에 있는 공백이 그대로 들어가서 불편하다.





#연습문제3(p.64)


#(1)

state.region

table(state.region)


#(2)

state_df = data.frame(state.x77, state.name, state.region)
state_df

#숫자 + 문자형으로 구성되어야 하므로, DF형만 가능하다.

#(3)

tapply(state_df$Income, state_df$state.region, mean )

aggregate(Income ~ state.region, state_df, mean )





#연습문제4(p.65)

#(1)(2)

df = read.csv("DataSet/titanic.csv")

str(df)


#(3)

condition = df$pclass == '1st'

mean(df[condition,'survived'])


#(4)

aggregate(survived ~ pclass, df, mean)
tapply(df$survived, df$pclass, mean)



#연습문제5(p.66)

#(1)

df$sex_num_male = df$sex=="male"
df$sex_num_female = df$sex=="female"

aggregate(cbind(sex_num_male,sex_num_female) ~ pclass, df, sum)


condition1 = df$pclass == '1st'

condition1

condition2 = df[condition1, 'age']>=20

df_20 = df[condition2, age]



#연습문제 4, 5(p. 65 66) 해설

#(1)(2)
titanic = read.csv(file="DataSet/titanic.csv")
str(titanic)


#(3)-1

table(titanic$pclass)

sub = subset(titanic, pclass == '1st')

nrow(sub)

mean(sub$survived)


#(3)-2

mean(titanic[titanic$pclass == '1st', 'survived'])


#(4)-1
tapply(titanic$survived, titanic$pclass, mean)

#(4)-2
aggregate(survived ~ pclass, titanic, mean)


#(5)

aggregate(sex ~ pclass, titanic, table)

#(5)-2
tapply(titanic$sex, titanic$pclass, table)

#(5)-3
table(titanic$sex, titanic$pclass)


#(6)
#NA 값을 고려해야함.
##고려하는 방법은 매우 많지만 일단 제거하는 걸로 하자.

length(titanic$age)
sum(is.na(titanic$age))


sub1 = subset(titanic, age>=20)
sub2 = subset(titanic, !is.na(age))

any(is.na(sub1$age))

nrow(sub2)

nrow(sub1)/nrow(sub2)
  #전체 성인의 비율

nrow(sub1[sub1$pclass=='1st',]) / nrow(sub2[sub1$pclass=='1st',])
  #1등석 성인 비율


#(7)

titanic$AgeGroup = ifelse(titanic$age>18, 'adult', 'child')
View(titanic)
  #missing 값은 그대로 들어감 확인.


aggregate(survived ~ AgeGroup + pclass + sex, titanic, mean)



#titanic 문제 dplyr로 풀어보기


titanic = read.csv("DataSet/titanic.csv")

#1 #2

str(titanic)


#3

titanic %>% 
  filter(pclass == "1st") %>% 
  summarise(survived_ratio = mean(survived))


#4

titanic %>% 
  group_by(pclass) %>% 
  summarise(survived_mean = mean(survived)) %>% 
  select(pclass, survived_mean)


#5

titanic %>% 
  group_by(pclass,sex) %>% 
  summarise(table(sex))

#6

num = titanic %>% 
  filter(!is.na(age), pclass == "1st") %>% 
  summarise(n = n())

num_adult = titanic %>% 
  filter(age>=20, pclass == '1st') %>% 
  summarise(n=n())

num_adult/num

#7

titanic %>% 
  mutate(age_group = ifelse(age>=18, "adult", "child")) %>%
  filter(!is.na(age)) %>% 
  group_by(age_group, pclass,  sex ) %>% 
  summarise(sur_mean = mean(survived))




#병원자료 실습

#100p.

#1
admissions = read.csv("DataSet/admissions.csv")

str(admissions)

admissions$ADMDATE2 = as.Date(admissions$ADMDATE, "%Y-%m-%d")

admissions$DISDATE2 = as.Date(admissions$DISDATE, "%Y-%m-%d")

head(admissions)

#2
admissions %>% 
  filter(HOSP==3) %>% 
  summarise(freq = n())

#101p.

#1
admissions['PRIMDX']

admissions %>%
  mutate(p_group = ifelse(substr(PRIMDX, 1, 3)=="410", "MI", 
                          ifelse(substr(PRIMDX,1,3)=="428", "CHF", "Others"))) %>% 
  group_by(p_group) %>% 
  summarise(n = n())

         
#2
admissions %>% 
  mutate(Period = DISDATE2-ADMDATE2+1) %>% 
  filter(Period>=14) %>% 
  select(PT_ID, ADMDATE, DISDATE, Period, HOSP) %>% 
  head(7)


admissions %>% 
  mutate(Period = as.Date(DISDATE,'%Y-%m-%d')-as.Date(ADMDATE,'%Y-%m-%d')+1) %>% 
  filter(Period>=14) %>% 
  select(PT_ID, ADMDATE, DISDATE, Period, HOSP)
head(7)

  
#102p.

#1

admissions %>% 
  group_by(PT_ID) %>% 
  mutate(days = DISDATE2-ADMDATE2+1) %>% 
  summarise(nstay = n(), min_stay = min(days), max_stay = max(days))


#2
hospital = read.csv("DataSet/hospital.csv")

names(hospital)

hosp_list = hospital %>% 
  filter(hospname %in% c("Our Lady of Charity", "Community Hospital")) %>% 
  select(hosp_id)

hosp_list

admissions %>% 
  filter(HOSP %in% hosp_list$hosp_id) %>% 
  select(PT_ID, ADMDATE, DISDATE, HOSP)

#103p.

head(hospital)

tmp = admissions %>% 
  rename(hosp_id = HOSP) %>% 
  group_by(hosp_id) %>% 
  summarise(Freq = n())

left_join(hospital, tmp, by="hosp_id")
>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
