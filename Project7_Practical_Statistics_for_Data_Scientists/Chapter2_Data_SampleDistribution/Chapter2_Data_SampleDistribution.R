setwd("C:\\Users\\Gargantua\\Desktop\\data_set\\Project7_Practical_Statistics_for_Data_Scientists_data_set")

#2.3 통계학에서의 데이터 분포

##히스토그램

library(ggplot2)
loans_income = read.csv("loans_income.csv")[,1]

View(loans_income)

#단순임의표본 추출
samp_data = data.frame(income=sample(loans_income, 1000),
                       type = 'data_dist')

# 5개 값의 평균으로 이루어진 표본을 하나 취한다.
samp_mean_05 = data.frame(income = tapply(sample(loans_income, 1000*5),
                                          rep(1:1000, rep(5,1000)), FUN=mean),
                          type = 'mean_of_5')


# 20개 값의 평균으로 이루어진 표본을 하나 취한다.
samp_mean_20 = data.frame(income = tapply(sample(loans_income, 1000*20),
                                          rep(1:1000, rep(20,1000)), FUN=mean),
                          type = 'mean_of_20')

#data.frame 바인딩 후 factor형으로 변환
income = rbind(samp_data, samp_mean_05, samp_mean_20)
income$type = factor(income$type,
                     levels = c('data_dist', 'mean_of_5', 'mean_of_20'),
                     labels = c('Data', 'Mean of 5', 'Mean of 20'))

#히스토그램 그리기
ggplot(income, aes(x=income)) +
  geom_histogram(bins=40) +
  facet_grid(type ~ .)





#2.4 부트스트랩

library(boot)
stat_fun = function(x, idx) median(x[idx])
boot_obj = boot(loans_income, R=1000, statistic = stat_fun)

View(boot_obj)

boot_obj




#2.6.1 정규분포와 QQ그림

norm_samp = rnorm(100)
qqnorm(norm_samp)
abline(a = 0, b = 1, col = 'grey')





#2.7 긴꼬리분포

sp500_px = read.csv("sp500_px.csv")

nflx = sp500_px[,'NFLX']
nflx = diff(log(nflx[nflx>0]))
qqnorm(nflx)
abline(a = 0, b = 1, col = 'grey')





#2.9 이항분포

## 한 번의 클릭이 판매로 이어질 확률이 0.02 일 때, 200회 클릭으로 '0회 매출'을 관찰할 확률은 얼마인가?

dbinom(x = 0, size = 200, p =  0.02)

## 한 번의 클릭이 판매로 이어질 확률이 0.02 일 때, 200회 클릭으로 '2회 이하의 매출'을 관찰할 확률은 얼마인가?

pbinom(2, 200, 0.02)





#2.12.1 푸아송 분포

rpois(100, lambda = 2)

  # 람다가 2일 때, 100에 대한 횟수
    # ex) 고객 서비스 센터에 1분당 평균 2회로 문의전화가 접수될 때, 100분당 문의 전화 횟수





#2.12.2 지수분포

rexp(n = 100, rate = 0.2)
  # 주기별 평균 사건의 수가 0.2인 지수분포에서 난수 100개를 생성.
    # ex) 분당 평균 0.2회 서비스 문의전화가 걸려오는 경우, 100분 동안의 서비스 센터 문의 전화를 시뮬레이션





#