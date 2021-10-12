
help(package="MASS")

help(mean)
?mean

help(cars)
?cars

help(quine, package="MASS")
?quine
#quine 은 MASS 패키지 안에 있어서 따로 검색 안됨.

## ----------------------------------------------------------------

installed.packages()
installed.packages()[, 1]
##설치된 패키지 확인


install.packages("ggplot2")
library(ggplot2)
#library 함수실행시 설치되어 있으면 아무 메시지가 나오지 않음.

if(!require(ggplot2)) install.packages('ggplot2')
#팁 : 설치가 안되어있으면 설치함

installed.packages()[, 1]

## ----------------------------------------------------------------
(a<-3)
(.a<-4)
#Environment에 숨기고 싶으면 변수명 앞에 .을 붙임
##단, 변수명은 '.a'가 됨.
.a
(A<-5)


## ----------------------------------------------------------------
# 1.a<-6 # error
##숫자로 시작하는 변수 안됨.

# 여러 줄 comment 하기
# - 하이라이트 하고 shift + ctrl + c

## ----------------------------------------------------------------
a<-3; b<-4
a+b


## ----------------------------------------------------------------
x<-10; y <-5; z<-3;
x+y
x-y
x*y + z
x/y
y^2
y%/%z # 몫
y%% z # 나머지


## ----------------------------------------------------------------
(c<-"Hello")

#()안에 넣으면 입력과 동시에 출력이 된다.

# '' / "" 둘다 가능하지만 ' " / " ' 이런 건 안된다.

## ----------------------------------------------------------------
c1<-"South Africa"
nchar(c1)
#글자 수 세기 : 띄어쓰기도 포함됨.


## ----------------------------------------------------------------
substr(c1,1,5)
substr(c1,7,12)

?substr

## ----------------------------------------------------------------
(x<-NA)
is.na(x)


## ----------------------------------------------------------------
10/0


## ----------------------------------------------------------------
0/0
# 0/0은 정의되지 않는다

Inf-Inf

is.nan(0/0)
is.nan(3) # 비교 


## ----------------------------------------------------------------
v1<-c(1,2,NA)
v1

v2<-c("a", "b", "c")
v2

v3<-c("d", "e")

v4<-c(v2,v3)
v4

(v5<-c(1,2,"a")) # 숫자 원소들을 문자로 형식 자동 변환

#형태를 섞을 수는 없다. : 숫자 + 문자 --> 문자로 변환됨.


## ----------------------------------------------------------------
c(v2, v3, c("apple", "pie"))


## ----------------------------------------------------------------
10:15
4:-2
seq(from = 1, to = 10, by = 2)
seq(from=1, to=2, length.out=5)
rep(1, times = 5)
rep(c(1,2), times = 2)
rep(c(1,2), each=3)

## ----------------------------------------------------------------
v<-c(3, pi, 4)
v[3] # 3번째 원소 선택
v[2]

## ----------------------------------------------------------------
v<-c(3, pi, 4)
names(v)<-c("num1", "num2", "num3")
v["num1"]
v

## ----------------------------------------------------------------
v[-1] # 1번째 원소 제외
v[2:3] # 2, 3번째 원소 선택
v[c(1,3)] # 1, 3 번째 원소 선택
v[c("num1", "num3")]


## ----------------------------------------------------------------
v<-c(3, pi, 4)
w<-c(pi, pi, pi) #rep(pi, 3)

v == w # 비교연산자 유의


## ----------------------------------------------------------------
v[v==w]
v[v<=w]

v[v>=3]

## ----------------------------------------------------------------
any(v==pi)
#'하나라도' 만족하는지

all(v==pi)
#'모두' 만족하는지


## ----------------------------------------------------------------
new.v<-c(v, NA)
new.v

is.na(new.v)
#is.na 는 벡터에도 적용가능.

## ----------------------------------------------------------------
new.v[!is.na(new.v)]  
new.v[is.na(new.v)]

## ----------------------------------------------------------------
length(w)
NROW(w)

#대문자랑 소문자 역할이 다르다.
# 그냥 length 쓰자


## ----------------------------------------------------------------
nrow(w)
#소문자 nrow 는 matrix 나 DF 전용함수임.(행과 열이 존재해야함.)


## ----------------------------------------------------------------
LETTERS
length(LETTERS)
LETTERS[1:3]

#LETTERS 는 빌트인 함수임.

## ----------------------------------------------------------------
#붙이기

paste("Today is", date())

paste("1st", "2nd", "3rd", sep = ", ")

paste("1st", "2nd", "3rd", sep = "")
c("1st", "2nd", "3rd")
## -> paste는 벡터를 만드는 함수가 아니다.



## ----------------------------------------------------------------

##FACTOR VS VECTOR

(blood.v<-rep(c("O", "AB", "A"), length.out=10))
(blood.f1<-factor(blood.v))
#factor는 Levels 가 뜬다.

table(blood.f1)
#B형이 없네?

(blood.f2<-factor(blood.v, levels=c("A", "B", "AB", "O")))
table(blood.f2)
#factor는 보이지 않는 값을 level로 추가할 수 있다.

(blood.f3<-factor(blood.f2)) #벡터에서 사용되지 않는 값은 레벨에서 삭제함  





## ----------------------------------------------------------------
fac1<-factor(LETTERS[1:5])
fac2<-factor(LETTERS[1:5], levels=c("C", "D", "E", "B", "A"))
table(fac1)
table(fac2)

#순서도 컨트롤 할 수 있다.


## ----------------------------------------------------------------
fac3<-factor(1:3, labels=c("A", "B", "C"))
table(fac3)
#데이터가 들어올 때 숫자화 되어있을 때 사용함.


## ----------------------------------------------------------------
matrix(1:12, nrow = 3)
matrix(1:12, nrow = 3, byrow=TRUE)
matrix(1:12, nrow = 3, byrow=T)

#벡터와 마찬가지로 성분의 형태가 동일해야함.


## ----------------------------------------------------------------
x<-matrix(1:12, ncol = 3)
x
nrow(x)
ncol(x)
dim(x)
#matrix 모양 파악하기


## ----------------------------------------------------------------
v1<-1:4
v2<-5:8
cbind(v1,v2) # column bind
rbind(v1,v2) # row bind


## ----------------------------------------------------------------
x<-matrix(1:8, ncol = 4)
x
x[1,2]
x[2,3]
x[, 1:2]
x[, c(1,4)]
x[-1,]


## ----------------------------------------------------------------
rownames(x)<-c("r1", "r2")
x

colnames(x)<-c("c1", "c2", "c3", "c4")
x

x["r1",]
x[, c("c1", "c3")]



## ----------------------------------------------------------------
x <- c(100, 75, 80)
y <- c("10452", "10455", "10565")
z <- data.frame(score=x, ID=y)
z


## ----------------------------------------------------------------
z$score
z$ID


## ----------------------------------------------------------------
z$name<-c("Mike", "Tony", "Evan")
z

str(z)
#성격을 알 수 있음.

## ----------------------------------------------------------------
colnames(z)


## ----------------------------------------------------------------
str(z)


## ----------------------------------------------------------------
z[1,2]
z[1,]
z[, c("name", "ID")]

#행렬함수는 DF에 먹히는데, DF함수들은 행렬에는 안먹힘.

## ----------------------------------------------------------------
library(MASS) # 내장된 MASS library를 loading
View(Cars93)
head(Cars93, n=3)


## ----------------------------------------------------------------
str(Cars93)


## ----------------------------------------------------------------
#리스트

Hong <- list(kor.name="길동", eng.name="Gil-dong", age=40, married=T, no.child=2, child.ages=c(3, 7))
Hong

#서로의 길이가 달라서 출력이 독특함


## ----------------------------------------------------------------
Hong$age
Hong$kor.name
Hong$child.ages

Hong[[3]] # 두 겹 괄호로 리스트의 부분 추출 가능
Hong[3] #한 겹 괄호하면 인덱스가 살아있음. & 그에 따라 연산이 안됨.

Hong[c(1,2)] # 여러 개를 추출할 때는 한겹 괄호



## ----------------------------------------------------------------
is.list(Hong)

class(MASS::Cars93)

x<-matrix(1:9, ncol = 3)
class(x)
x

class(state.x77)
is.matrix(state.x77)

newx<-as.data.frame(x)
newx
#x에 변수 이름이 없기 때문에 v1, v2, v3가 자동으로 들어감.


class(newx)
is.matrix(newx)


## ----------------------------------------------------------------
is.data.frame(newx)
names(newx)
newx$V1


## ----------------------------------------------------------------
data1<-read.table(file="C:/Users/Gargantua/Desktop/data_analysis/Project6_R_Bigdata/DataSet/KOSPI.csv", 
                  header=T, sep=",")

View(data1)

## ----------------------------------------------------------------
colnames(data1)
dim(data1)
str(data1)
head(data1, n=3)
tail(data1, n=3)


## ----------------------------------------------------------------
#날짜가 현재 char로 되어있음 -> 연산 불가 -> 형태변환 필요


data1$newDate<-as.Date(data1$Date)
str(data1)


## ----------------------------------------------------------------
data1$Date[10] - data1$Date[1]
data1$newDate[10] - data1$newDate[1]
#연산이 가능해짐.


data1$newDate[10]
data1$newDate[1]
data1$newDate[1]+11


## ----------------------------------------------------------------
strDates <- c("01/05/2010", "08/16/1995")
as.Date(strDates) # 제대로 인식 못함 
as.Date(strDates, "%m/%d/%Y") # 형식 적용 



## ----------------------------------------------------------------
(today <- Sys.Date())
#오늘 컴퓨터 날짜

format(today, "%B %d %Y")
format(Sys.Date(), "%a %b %d")

#정해진 형식으로 출력하기


## ----------------------------------------------------------------
getwd()


## ----------------------------------------------------------------
setwd("C:\\Users\\Gargantua\\Desktop\\data_analysis\\Project6_R_Bigdata")
getwd()

data1new<-read.table(file="DataSet/KOSPI.csv", header=T, sep=",")
#첫 row에 변수 명 있으면 header = T 놓아주면 된다.

colnames(data1new)

#wd 설정하면 그 뒤 경로만 입력하면 됨.

## ----------------------------------------------------------------
data2<-read.csv(file="DataSet/KOSPI.csv") 
all(data1new==data2)

str(data2)

#read.csv 가능.


## ----------------------------------------------------------------
tab_dat<-read.table("DataSet/hsb2.txt", header=T, sep="\t")
tab_dat[1:3, ]

#csv 구분이 탭으로 되어 있으면 sep 이하 위와 같이 되어야함.

## ----------------------------------------------------------------
if (!require('readxl')) install.packages('readxl') 
library('readxl')

data5<-read_excel("DataSet/excel_exam.xlsx") 
data5

## ----------------------------------------------------------------
data6<-read_excel("DataSet/excel_exam_sheet.xlsx", sheet=3) 

#sheet가 여러개이면 불러올 sheet 지정해주어야함.

data6
data6$id

## ----------------------------------------------------------------
save(data1, tab_dat, file="Output/newR.Rdata")

#Environment 상 변수까지 저장하고 싶을때 -> 저장 후 load함수 해주면 됨.


## ----------------------------------------------------------------
setwd("C:\\Users\\Gargantua\\Desktop\\data_analysis\\Project6_R_Bigdata")

load(file="Output/newR.Rdata")


## ----------------------------------------------------------------
write.csv(data1, file="Output/data1_1.csv", row.names=T)
write.csv(data1, file="Output/data1_2.csv", row.names=F)

#저장형식 지정할 수 있다.(R로 통한다면 굳이 쓸필요 없다)

write.table(data1, file="Output/data1_3.txt")


## ----------------------------------------------------------------
dat1 = subset(Cars93, select=c(Model, MPG.city, AirBags), subset=(MPG.city>30))
dat1

Cars93[Cars93$MPG.city > 30, c("Model", "MPG.city", "AirBags")]

#subset함수의 장점 : 따옴표 필요없음.


sub<-subset(Cars93, select=c(Model, MPG.city, AirBags), subset=(MPG.city>30 & AirBags !="None"))
sub
subset(sub, select=-AirBags)

?subset

## ----------------------------------------------------------------
attach(MASS::Cars93)
table(Type, DriveTrain)

#Model만 떼어내고 싶을 때
Model


## ----------------------------------------------------------------
detach(MASS::Cars93)
Model

## ----------------------------------------------------------------
library(MASS)
table(Cars93$Type, Cars93$DriveTrain)
with(Cars93, table(Type, DriveTrain))


## ----------------------------------------------------------------
table(Cars93$AirBags)
Cars93[Cars93$AirBags == "Driver & Passenger", 1:2]


## ----------------------------------------------------------------
(idx<-which(Cars93$AirBags == "Driver & Passenger"))
Cars93[idx, 1:2]


## ----------------------------------------------------------------
for(i in 1:3){
  print(paste("i=", i))
}


## ----------------------------------------------------------------
for(i in 1:5){
  if(i %% 2 == 0){
    print('짝수')
  }
  else{
    print('홀수')
  }
}


## ----------------------------------------------------------------
w1<-1:5
ifelse(w1 %% 2 == 0, "짝수", "홀수")


## ----------------------------------------------------------------
x <- list(a = 1:10, beta = exp(-3:3))
(y<-lapply(x, mean))
(z<-sapply(x, mean))
y$a
z[1]

#자료 형태가 고르지 않아도 적용할 수 있다.


## ----------------------------------------------------------------
(mat <- cbind(x1 = 3, x2 = c(3:1, 2:4)))
rownames(mat)<-letters[1:6]


mat
apply(mat, 1, mean)
apply(mat, 2, mean)

#apply는 사각형 형태의 자료에만 가능.


## ----------------------------------------------------------------

table(Cars93$Type)

tapply(Cars93$Price, Cars93$Type, mean)
#with(Cars93, tapply(Price, Type, mean))

#그룹별 통계치 계산이 가능.

## ----------------------------------------------------------------
aggregate(Price ~ Type, Cars93, mean)

#결과 모양이 더 깔끔.

## ----------------------------------------------------------------
## ----------------------------------------------------------------

library(dplyr)

## ----------------------------------------------------------------
exam<-read.csv("DataSet/csv_exam.csv")

exam %>% filter(class == 1)

exam %>% filter(class == 2)

exam %>% filter(class == 3)


filter(exam, class==1)

filter(exam, class==2)

filter(exam, class==3)

## ----------------------------------------------------------------
exam %>% filter(class %in% c(1,3))

exam %>% filter(class ==1 | class==3)

#일일이 class 안써도댐

## ----------------------------------------------------------------
exam %>% filter(math >= 90 | english >= 90)


## ----------------------------------------------------------------
exam %>% filter(class == 2 & english >= 80)

exam %>% filter(class == 2, english >= 80) 

#결과는 &이나 , 나 동일함.


## ----------------------------------------------------------------
class1<-exam %>% filter(class == 1)
mean(class1$english)


## ----------------------------------------------------------------
df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA))
df

is.na(df)               # 결측치 확인
table(is.na(df))        # 결측치 빈도 출력
table(is.na(df$sex))  # sex 결측치 빈도 출력


## ----------------------------------------------------------------
df[is.na(df$sex), ]

df %>% filter(is.na(sex))


## ----------------------------------------------------------------
df[!is.na(df$sex), ]


df %>% filter(!is.na(sex))


## ----------------------------------------------------------------
df[!is.na(df$sex)&!is.na(df$score), ]

df %>% filter(!is.na(sex)&!is.na(score))


## ----------------------------------------------------------------
na.omit(df)

#일일이 할필요 없이 NA 값 포함된 데이터 모두 제거함.


## ----------------------------------------------------------------
mean(c(1,2,3, NA))
mean(c(1,2,3, NA), na.rm=T)

#NA값이 포함되면 작동안함. 
##na.rm=T 붙여주면 NA제거하고 계산함.


## ----------------------------------------------------------------

select<- dplyr::select

#일반적인 select가 아니라 dplyr 안에 있는 select를 덮어 씀

exam %>% select(class, math, english) %>% head(3)


## ----------------------------------------------------------------
exam %>% select(classroom=class, math, english) %>% head(3)

#일부 변수를 선택하되 해당 변수 이름만 바꾸어 가져옴

## ----------------------------------------------------------------
exam %>% rename(classroom=class) %>% head(3)

#rename : 모든 변수를 선택하되 해당 변수 이름만 변경


## ----------------------------------------------------------------
exam %>% select(-math) %>% head(5)


## ----------------------------------------------------------------
exam %>% select(-math, -english) %>% head(5)


## ----------------------------------------------------------------
exam %>% select(-(math:science)) %>% head(5)


## ----------------------------------------------------------------
exam %>% arrange(math)
exam %>% arrange(desc(math))

#math 기준 정렬.

## ----------------------------------------------------------------
exam %>% arrange(desc(math)) %>% head(10)


## ----------------------------------------------------------------
exam %>% arrange(english, math) %>% head(10)

#engligh -> math  순으로 정렬

## ----------------------------------------------------------------
exam %>% top_n(5, math)

#상위 5명. / 정렬이 되지 않은 채 보여줌.

## ----------------------------------------------------------------
exam %>% arrange(desc(math)) %>% head(5)

#순서대로 보고 싶다면

## ----------------------------------------------------------------
exam %>%
  mutate(average = (math + english + science)/3) %>%
  head

#mutate : 새로운 변수 추가
#exam$average = (math+english+science)/3

## ----------------------------------------------------------------
exam %>%
  mutate(average = (math + english + science)/3) %>%
  arrange(average) %>%
  head


## ----------------------------------------------------------------
head(exam, n=10)

new_exam<-exam %>% mutate(test=ifelse(science >=70, "pass", "fail"))

head(new_exam, n=10)


## ----------------------------------------------------------------
exam %>% summarise(mean_sc = mean(science))


## ----------------------------------------------------------------
exam %>% summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math),
            n = n())


## ----------------------------------------------------------------
exam %>% summarise(class=n_distinct(class))

#class는 몇개인지?

table(exam$class)


Cars93 %>% summarise(Type=n_distinct(Type))


## ----------------------------------------------------------------
exam %>% group_by(class) %>%
  summarise(mean_sc = mean(science))


## ----------------------------------------------------------------
exam %>% group_by(class) %>%
  summarise(mean_math = mean(math),
            median_math = median(math),
            n = n())


## ----------------------------------------------------------------
exam %>% sample_n(7)

#무작위 추출 -> 샘플테스트


set.seed(123)
exam %>% sample_n(7)

#이렇게 쓰면 무작위 추출한 샘플을 재생산할 수 있다.


## ----------------------------------------------------------------
exam %>% sample_frac(0.2)

#20% 추출.

set.seed(123)
exam %>% sample_frac(0.2)

#마찬가지로 이렇게 할 수 있음.


## ----------------------------------------------------------------
library(nycflights13)


## ----eval=F------------------------------------------------------
## ?flights # 도움말


## ----------------------------------------------------------------
str(flights)
dim(flights)

head(flights)
#티블은 데이터 넘어가면 안 보여줌

names(flights)

## ----------------------------------------------------------------
flights %>% filter(month == 1, day == 1) %>% head(5)


## ----------------------------------------------------------------
df <- flights %>% filter(month == 1 & day %in% c(3, 4))
nrow(df)

flights %>% filter(month == 1 & day %in% c(3, 4)) %>%  summarize(n=n())


## ----------------------------------------------------------------
arrange(flights, year, month, day) %>% head(5)

flights %>% arrange(year, month, day) %>% head(5)


#두 코드는 같음.


## ----------------------------------------------------------------
arrange(flights, desc(dep_delay)) %>% head(5)


## ----------------------------------------------------------------
flights  %>%  select(year, month, day) %>% head(3)

flights %>% select(year:day) %>% head(3)


## ----------------------------------------------------------------
select(flights, -(year:day)) %>% head(3)


## ----------------------------------------------------------------
flights %>%  select(tail_num = tailnum) %>% head(5)


## ----------------------------------------------------------------
flights %>%  rename(tail_num = tailnum) %>% head(5)


## ----------------------------------------------------------------
flights %>% mutate(gain = arr_delay - dep_delay, speed = distance * 60 / air_time) %>%  head(3)


## ----------------------------------------------------------------
f1<-mutate(flights, gain = arr_delay - dep_delay, speed = distance * 60 / air_time)
colnames(f1)


## ----------------------------------------------------------------
flights %>%  summarise(delay = mean(dep_delay, na.rm = TRUE))

summarise(flights, delay = mean(dep_delay)) 
# NA


## ----------------------------------------------------------------
flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay, year, month, day) %>%
  summarise(arr = mean(arr_delay, na.rm = TRUE), dep = mean(dep_delay, na.rm = TRUE)) %>%
  filter(arr > 50 & dep > 50)


## ----------------------------------------------------------------
name<-data.frame(class = c(1, 2, 3, 4, 5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
name

exam_new<-left_join(exam, name, by = "class")
head(exam_new, n=10)


## ----------------------------------------------------------------
group_a <- data.frame(id = c(1, 2, 3, 4, 5), test = c(60, 80, 70, 90, 85))

group_b <- data.frame(id = c(5, 7, 8), test = c(65, 80, 90))

group_a
group_b

group_all <- bind_rows(group_a, group_b)
group_all


## ----------------------------------------------------------------
bind_rows(data.frame(x = 1:3), data.frame(y = 1:4))


## ----------------------------------------------------------------
payroll<-read.csv("DataSet/payroll.csv")
str(payroll)
#날짜가 char 형태로 들어가있음

head(payroll, n=7)


## ----------------------------------------------------------------
payroll2<-read.csv("DataSet/payroll2.csv")

str(payroll2)
head(payroll2, n=7)


## ----------------------------------------------------------------
payroll$BirthD<-as.Date(payroll$Birth, '%Y/%m/%d')
payroll$BirthD[1:5]
str(payroll)


## ----------------------------------------------------------------
min_b<-payroll %>% group_by(Gender) %>% summarize(BirthD = min(BirthD))
min_b


## ----------------------------------------------------------------
left_join(min_b, payroll, by="BirthD")


## ----------------------------------------------------------------
payroll$Age<-floor((Sys.Date()-payroll$BirthD)/ 365.25)
payroll %>% group_by(Jobcode) %>% summarize(avg_age = round(mean(Age), digits=0), avg_salary = mean(Salary))



payroll %>% 
  mutate(Age = floor((Sys.Date()-payroll$BirthD)/ 365.25) ) %>% 
  group_by(Jobcode) %>% 
  summarize(avg_age = round(mean(Age), digits=0), avg_salary = mean(Salary)) %>% 
  arrange(avg_salary)


## ----------------------------------------------------------------
names(payroll)
names(payroll2)


names(payroll2)[1]<-names(payroll)[1]

names(payroll)
names(payroll2)


##변수 이름을 맞추어줌


updated<-left_join(payroll, payroll2, by="IdNumber")

head(updated)
View(updated)

##붙여진 자료 확인


updated$salary2<-ifelse(is.na(updated$salary), updated$Salary, updated$salary)

#업데이트가 실시간으로 이루어지는 점 확인 가능함.


updated %>% select(IdNumber, Gender, Jobcode, Salary, salary, salary2) %>% head(15)


payroll<-read.csv("DataSet/payroll.csv")
payroll2<-read.csv("DataSet/payroll2.csv")

names(payroll2)

names(payroll2)[1]<-names(payroll)[1]


payroll %>% left_join(payroll2,  by="IdNumber") %>% 
  mutate(salary2 = ifelse(is.na(updated$salary), updated$Salary, updated$salary)) %>% 
  select(IdNumber, Gender, Jobcode, Salary, salary, salary2) %>% head(15)





payroll<-read.csv("DataSet/payroll.csv")
payroll2<-read.csv("DataSet/payroll2.csv")




payroll2 %>% 
  rename(IdNumber = idnum) %>% 
  right_join(payroll, by = "IdNumber")%>% 
  mutate(salary2 = ifelse(is.na(payroll2$salary), payroll$Salary, payroll2$salary)) %>% 
  select(IdNumber, Gender, Jobcode, Salary, salary, salary2) %>% head(15)

payroll2$salary


