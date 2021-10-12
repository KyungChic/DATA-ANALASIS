#1.3.3 예제 : 인구에 따른 살인 비율의 위치 추정

setwd('C:\\Users\\Gargantua\\Desktop\\data_analysis\\Project7_Practical_Statistics_for_Data_Scientists\\data')

state <- read.csv('state.csv')

head(state)


#평균
mean(state$Population)

#절사평균
mean(state$Population, trim=0.1)

#중간값
median(state$Population)



#인구 수에 가중을 둔 가중평균 구하기
weighted.mean(state$Murder.Rate, w=state$Population)

#인구 수에 가중을 두어 중간값 구하기

install.packages('matrixStats')
library('matrixStats')

weightedMedian(state$Murder.Rate, state$Population)




#1.4.3 예제 : 주별 인구의 변이(variability) 추정

head(state, 8)


#표준편차

sd(state$Population)


#사분위범위(IQR)

IQR(state$Population)


#중위절대편차(MAD)
## MAD(Median Absolute Deviation) : 중간값 기준의 편차의 절대값의 중간값
### MAD = Median(|X1-m| + |X2-m| + .... + |Xn-m|)

mad(state$Population)





#1.5.1 백분위수와 상자그림

quantile(state$Murder.Rate, p=c(.05, .25, .5, .75, .95))

boxplot(state$Population/1000000, ylab='Population (millions)')





#1.5.2 도수분포표와 히스토그램


#도수분포표

breaks = seq(from=min(state$Population),
             to=max(state$Population), length=11)
pop_freq = cut(state$Population, breaks=breaks, right=TRUE, include.lowest = TRUE)
table(pop_freq)


#히스토그램

hist(state$Population, breaks = breaks)




#1.5.3 밀도 그림과 추정
##밀도그림은 좀 더 부드러운 히스토그램 형태
###히스토그램과 가장 큰 차이는 y축의 값. 히스토그램의 축 값은 도수를 나타내지만 밀도 그림은 '비중'을 나타낸다 : freq=FALSE 값을 통하여 비율로 값을 나타냄.


hist(state$Murder.Rate, freq=FALSE)
lines(density(state$Murder.Rate), lwd=3, col='blue')




#1.6 이진 데이터와 범주 데이터 선택하기

##막대 도표는 범주형자료를 표현. : x축위에 범주 / y축 위에 횟수나 비율.


dfw = read.csv("dfw_airline.csv")
barplot(as.matrix(dfw) / 6, cex.axis = 0.8, cex.names = 0.7,
        xlab = 'Cause of delay', ylab='Count')




#1.7 상관관계

sp500_px = read.csv("sp500_px.csv", header = T, row.names = 1)
  ##read 함수에서 row.names = 1 을 통해 첫 column을 row name으로 정할 수 있음.


head(sp500_px)
row.names(sp500_px)
View(sp500_px)

sp500_sym = read.csv("sp500_sectors.csv")
head(sp500_sym)


etfs = sp500_px[row.names(sp500_px) > "2012-07-01",
                sp500_sym[sp500_sym$sector=="etf", 'symbol']]
  ##row는 날짜 : 2012-07-01 이후 날짜 / col은 섹터가 etf인 것의 티커들에 대해 sp500_px의 주가 값 슬라이싱

library(corrplot)
corrplot(cor(etfs), method="ellipse")


View(sp500_sym)




#1.7.1 산점도
## 두 변수의 관계를 시각화 하는 가장 기본적인 방법

sp500_px = read.csv("sp500_px.csv", row.names = 1)
sp500_sym = read.csv("sp500_sectors.csv")
telecom = sp500_px[,sp500_sym[sp500_sym$sector_label == "Telecom", 'symbol']]

View(sp500_sym)
View(sp500_px)
View(telecom)

plot(telecom$T, telecom$VZ, xlab = 'ATT (T)', ylab = 'Verizon (VZ)')




#1.8 두 개 이상의 변수 탐색하기

## 데이터 특성에 따라 분석 형태가 달라진다. (수치형 / 범주형)

#1.8.1 수치형 - 수치형 데이터 분석1(육각형 구간 그래프 그리기)

## 수십 수백만의 레코드를 산점도로 나타내기는 어려움. -> 육각형 구간과 등고선 활용

kc_tax= read.csv("kc_tax.csv.gz")

View(kc_tax)

colnames(kc_tax)


#subset함수를 이용한 이상치 제거

kc_tax0 = subset(kc_tax, TaxAssessedValue < 750000 &
                   SqFtTotLiving > 100 &
                   SqFtTotLiving <3500)

nrow(kc_tax0)


#ggplot2를 이용한 육각형구간그림

##집들의 과세 평가액과 크기 사이의 관계

library(ggplot2)
library(hexbin)

ggplot(kc_tax0, (aes(x=SqFtTotLiving, y=TaxAssessedValue))) +
  stat_binhex(color = 'white') +
  theme_bw() +
  scale_fill_gradient(low='white', high='black') +
  labs(x='Finished Square Feet', y='Tax-Assessed Value')



# 수치형 - 수치형 데이터 분석2(등고선 그래프 그리기)

ggplot(kc_tax0, aes(SqFtTotLiving, TaxAssessedValue)) +
  theme_bw() +
  geom_point(alpha=0.1) +
  geom_density2d(color='white') +
  labs(x='Finished Square Feet', y='Tax-Assessed Value')





#1.8.2 범주형 변수 대 범주형 변수
## 두 범주형 변수의 요약에는 <분할표>를 사용하여 범주별 빈도수를 기록한다.


lc_loans = read.csv("lc_loans.csv")
View(lc_loans)

install.packages("descr")
library(descr)

x_tab = CrossTable(lc_loans$grade, lc_loans$status,
                   prop.c = FALSE, prop.chisq = FALSE, prop.t = FALSE)
#column에 대한 비중 / 카이스퀘어 값 / 테이블에 대한 비중 제거

x_tab





#1.8.3 범주형 변수 대 수치형 변수

#박스플롯 도표 그리기

airline_stats = read.csv("airline_stats.csv")
View(airline_stats)

names(airline_stats)

boxplot(pct_carrier_delay ~ airline, data=airline_stats, ylim=c(0,50))


#바이올린 도표 그리기
##박스플롯에서는 보이지 않는 값을 볼 수 있다 : alsaka / Delta / United 항공사가 상대적으로 0 값의 비중이 큼을 알 수 있다.

library(ggplot2)

ggplot(data = airline_stats, aes(airline, pct_carrier_delay)) +
  ylim(0,50) +
  geom_violin() +
  labs(x='', y='Daily % of Delayed Flights')





#1.8.4 다변수 시각화하기
## 다변량 분석은 <조건화(conditioning)> 개념을 통해이변량 분석 툴을 좀 더 고차원화 하여 살펴볼 수 있다.

#일례로, 육각형구간 그래프 분석에서 우편번호를 추가하여 지역변수까지 추가해볼 수 있다.

kc_tax= read.csv("kc_tax.csv.gz")
kc_tax0 = subset(kc_tax, TaxAssessedValue < 750000 &
                   SqFtTotLiving > 100 &
                   SqFtTotLiving <3500)

ggplot(subset(kc_tax0, ZipCode %in% c(98188, 98105, 98108, 98126)),
       aes(x = SqFtTotLiving, y = TaxAssessedValue)) +
  stat_binhex(color='white') +
  theme_bw() +
  scale_fill_gradient(low='white', high='blue') +
  labs(x = 'Finished Square Feet', y = 'Tax-Assessed Value') +
  facet_wrap('ZipCode')



#


