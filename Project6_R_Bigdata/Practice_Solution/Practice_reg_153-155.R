hsb2 = read.csv("DataSet/hsb2.txt", header = T, sep="\t")

#다중선형회귀분석

#변수타입 정렬

str(hsb2)


hsb2$female_fac = factor(hsb2$female, labels=c("male","female" ))
hsb2$race_fac = factor(hsb2$race, labels=c('african american', 'asian', 'hispanic', 'white' ))
hsb2$ses_fac = factor(hsb2$ses, labels=c('low', 'middle', 'high'))
hsb2$schtyp_fac = factor(hsb2$schtyp, labels=c('public', 'private'))
hsb2$prog_fac = factor(hsb2$prog, labels=c('general', 'academic', 'vocational'))


hsb2$female = as.factor(hsb2$female)
hsb2$race = as.factor(hsb2$race)
hsb2$ses = as.factor(hsb2$ses)
hsb2$schtyp = as.factor(hsb2$schtyp)
hsb2$prog = as.factor(hsb2$prog)

head(hsb2)


#도수분포표 파악

hsb2_freq = hsb2 %>% 
  select("female_fac", 'ses_fac', 'prog_fac', 'race_fac')

lapply(hsb2_freq, table)


#science 점수를 다른 점수들 + 성별 + 인종을 독립변수로 하여 예측하여고 함

  #1 다중공선성 파악 : 분석 전 수치형 변수들 간의 상관분석
    #하는 이유 : 회귀분석 전제(변수들은 서로 독립관계) 위반 파악
      #방법 1. 변수들 간의 상관관계 파악 2. VIF(Variance Inflaction Factors) >= 10 파악

  #(사전) 변수들 간 상관계수 파악(연속형 변수만 가능)

library(ggcorrplot)

str(hsb2)

hsb2_cont = hsb2 %>% 
  select(read:socst)

corrl_hsb2 = cor(hsb2_cont)

ggcorrplot::ggcorrplot(corrl_hsb2, hc.order = T, type='lower', lab = T)

GGally::ggpairs(hsb2_cont)


hsb2new2<-hsb2 %>% dplyr::select(read:socst) # 수치형 변수만 포함 

cor(hsb2new2)

cor.test(hsb2$science, hsb2$math)

# H0 : rho = 0(상관관계가 유의하지 않다) / H1 : not H0 (상관관계가 유의하다, statistically significant)

# p-value < alpha(0.05) => H0 기각, H1 채택





  #


  #2. 회귀분석하기 - 수동

names(hsb2)
head(hsb2)

# hsb2_reg = lm(science ~ read:math + socst + female_fac + race_fac, data = hsb2)
# 결과값에 read:math가 묶여 회귀분석 되어버림.

      #수정
hsb2_new = hsb2 %>% select(read:socst, female_fac, race_fac)

reg1 = lm(science ~ ., data = hsb2_new)

f =formula(reg1)


summary(reg1)

library(car)
vif(reg1)

  #2-1. 표준화 잔차의 절대값이 2.5가 넘는 관측치를 제거 : 결정계수 증가

remove = which(abs(stdres(reg1)) >= 2.5)

hsb2_new_rm = hsb2_new[-remove,]
#hsb2_new_rm = hsb2_new[!remove,] 하면 안된다. (값이 NA로 대체되고 행은 그대로 남아버림) )

hsb2_new_rm[15,]

reg1_rm = lm(f, data = hsb2_new_rm)

summary(reg1_rm)



  #stepwise를 이용한 변수 선택
hsb2_new = hsb2 %>% select(read:socst, female_fac, race_fac)

reg2 = lm(science ~ ., data = hsb2_new)

reg2.step = step(reg2, direction = "both")
  #AIC(Akaike Information Criterion)가 작을 수록 더 좋은 모델 (변수를 통해 제어함)

f2 = formula(reg2.step)
  #socst 변수 삭제됨.

summary(reg2.step)

  #다중공선성 확인
vif(reg2.step)

  #이상값 확인
idx = which(abs(stdres(reg2.step)) >= 2.5)

idx

hsb2_new_rm2 = hsb2_new[-idx,]

reg2.step_new = lm(f2, data = hsb2_new_rm2)

summary(reg2.step_new)

plot(reg2.step_new, which=4)

View(hsb2_new_rm2)
model.matrix(reg2.step_new)

#결정계수 59% -> y의 변이를 회귀식이 59% 설명한다.

# predicted science = 9.66 + read * 0.24 + ....... + white * 3.19
  ## read 1점이 science 점수에 미치는 영향력은 0.24
  ## 여자인 경우는 3.49점 감소함.


#범주형 변수가 어떻게 범주화 되었는지는 꼭 확인해야 한다.

# 결정계수 59% => y의 변이를 회귀식이 59% 설명한다 
# predicted science = 9.66298 + 0.23875 read + 0.32738 write + 0.23115 math 
#                       -3.48944 genderfemale - 0.29034 race2Asian -1.44513 race2Hispanic + 3.18825 race2White


# genderfemale = 0 (male) / 1 (female)
# (race2Asian, race2Hispanic, race2White) = (0,0,1) : white
# (race2Asian, race2Hispanic, race2White) = (0,1,0) : hispanic
# (race2Asian, race2Hispanic, race2White) = (0,0,0) : african american (base)
# (race2Asian, race2Hispanic, race2White) = (1,0,0) : asian 

# (예) read = 50, write = 60, math = 40, 여자, asian 
# => predicted science = 9.66298 + 0.23875 x 50 + 0.32738 x 60 + 0.23115 x 40
#                       -3.48944 x1 - 0.29034 x 1 


# (예) read = 50, write = 60, math = 40, 여자, white
# => predicted science = 9.66298 + 0.23875 x 50 + 0.32738 x 60 + 0.23115 x 40
#                       -3.48944 x1 + 3.18825 x 1







#------------------------------------------------------------------

#참고 : 인종을 (백인, 그외) 로 나눈 경우
hsb2<-read.table("DataSet/hsb2.txt", header=T, sep="\t")
hsb2$gender<-factor(hsb2$female, labels=c('male', 'female'))
hsb2$ses2<-factor(hsb2$ses, labels=c('low', 'middle', 'high'))
hsb2$prog2<-factor(hsb2$prog, labels=c('general', 'academic', 'vocational'))
hsb2$race2<-factor(hsb2$race, labels=c('AfricanAmerican', 'Asian', 'Hispanic', 'White'))
hsb2$schtyp2<-factor(hsb2$schtyp, labels=c('public', 'private'))
head(hsb2)

dat<-hsb2
names(dat)
dat$white<-as.factor(ifelse(dat$race == 4, 1, 0))
str(dat)
names(dat)
dat2<-dat %>% select(gender, white, read:science)
names(dat2)
newreg2<-lm(science ~ ., data=dat2)
summary(newreg2)

model.matrix(newreg2)
