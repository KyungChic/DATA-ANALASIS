
## ----------------------------------------------------------------

?anscombe
View(anscombe)
class(anscombe)

sapply(anscombe, mean)
sapply(anscombe, sd)
diag(cor(anscombe[,1:4], anscombe[, 5:8]))


op <- par(las=1, mfrow=c(2,2), mar=c(4,4,2,2), oma=c(0,0,0,0),
          lab=c(6,6,7), cex.axis=0.8, mgp=c(3,1,0))
ff <- y ~ x
for(i in 1:4) {
  ff[[2]] <- as.name(paste("y", i, sep=""))
  ff[[3]] <- as.name(paste("x", i, sep=""))
  lmi <- lm(ff, data= anscombe)
  xl <- substitute(expression(x[i]), list(i=i))
  yl <- substitute(expression(y[i]), list(i=i))
  plot(ff, data=anscombe, col="black", pch=16, cex=1.1,
       xlim=c(3,19), ylim=c(3,13), xlab=eval(xl), ylab=yl)
  abline(lmi, col="blue")
}
par(op)

#상관계수만으로 결과평가 불가능. 그림 그려봐야함 무조건.

## ----------------------------------------------------------------
plot(cars$speed, cars$dist)


## ----------------------------------------------------------------
(m<-lm(dist ~ speed, data=cars)) 
#y~x / y ~ x1 + x2


## ----------------------------------------------------------------
plot(cars$speed, cars$dist)
abline(coef(m), col="blue")


## ----------------------------------------------------------------
library(ggplot2)
ggplot(data = cars, aes(x = speed, y =dist)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue")


## ----------------------------------------------------------------
coef(m)


## ----------------------------------------------------------------
fitted(m)[1:5]

#회귀식에 실제 값 대입

## ----------------------------------------------------------------
residuals(m)[1:5]

#잔차값. y-y햇


## ----------------------------------------------------------------
summary(m)

#회귀계수 유의성 검정
#intercept는 중요 X
#speed와 관련된 내용 보면 되는데, 

#p-value는 보통 유의수준 0.05와 ㅂ교함
# P-value < 0.05 -> H0 기각 -> 회귀계수가 통계적으로 유의하다(significant) -> speed가 dist를 설명한다.


## ----------------------------------------------------------------
summary(cars$speed)
predict(m, newdata = data.frame(speed=c(5,10,15)))

#5, 10 15에 대해 예측값 산정

cars[cars$speed<=15,c("dist","speed")]
## ----------------------------------------------------------------
ex1<-read.table("DataSet\\influence1.txt", header=T, sep="\t")
ex2<-read.table("DataSet\\influence2.txt", header=T, sep="\t")
ex3<-read.table("DataSet\\influence3.txt", header=T, sep="\t")
ex4<-read.table("DataSet\\influence4.txt", header=T, sep="\t")

par(mfrow=c(2,2), mar=rep(2.5, 4))
with(ex1, plot(x,y, pch=19, main="reg1", cex.main=0.9))
with(ex2, plot(x,y, pch=19, main="reg2: outlier", cex.main=0.9))
with(ex2, points(x[21], y[21], col=rgb(0.8, 0.2, 0.2, 0.5),pch=16, cex=5))
with(ex3, plot(x,y, pch=19, main="reg3: high leverage point", cex.main=0.9))
with(ex3, points(x[21], y[21], col=rgb(0.8, 0.2, 0.2, 0.5),pch=16, cex=5))
with(ex4, plot(x,y, pch=19, main="reg4: outlier, high leverage point, influential", cex.main=0.9))
with(ex4, points(x[21], y[21], col=rgb(0.8, 0.2, 0.2, 0.5),pch=16, cex=5))


## ----------------------------------------------------------------
library(MASS)



reg2<-lm(y ~ x, data=ex2)
which(abs(stdres(reg2))>2.5)

reg3<-lm(y ~ x, data=ex3)
which(abs(stdres(reg3))>2.5)

reg4<-lm(y ~ x, data=ex4)
which(abs(stdres(reg4))>2.5)


## ----------------------------------------------------------------
hatvalues(reg3)
dffits(reg3)
dfbetas(reg3)[15:21, ]



## ----------------------------------------------------------------
cooks.distance(reg3)
rstudent(reg3)



## ----------------------------------------------------------------
plot(reg3, which=4) 


## ----------------------------------------------------------------
library(car)
influencePlot(reg3)


## ----------------------------------------------------------------
hatvalues(reg4)
dffits(reg4)
dfbetas(reg4)[15:21, ]



## ----------------------------------------------------------------
cooks.distance(reg4)
rstudent(reg4)


##------------------------------------------------------------------

reg4<-lm(y ~ x, data=ex4)
idx<-which(abs(stdres(reg4))>2.5)

summary(reg4)

# 이상치, 영향치 제거 
ex4_new<-ex4[-idx, ]
reg4_new<-lm(y ~ x, data=ex4_new)

summary(reg4_new)


## ----------------------------------------------------------------
plot(reg4, which=4) 


## ----------------------------------------------------------------
influencePlot(reg4)


## ----------------------------------------------------------------
par(mfrow=c(1,2))
plot(reg4, which=1:2)


## ----------------------------------------------------------------
set.seed(1234)
n<-1000; normal_dat<-rnorm(n)
par(mfrow=c(1,2))
hist(normal_dat, main="Normal distribution")
qqnorm(normal_dat, pch = 1, frame = FALSE)
qqline(normal_dat, col = "steelblue", lwd = 2)


## ----------------------------------------------------------------
chisq_dat<-rchisq(n, df=5); 
par(mfrow=c(1,2))
hist(chisq_dat, main="Positive skew")
qqnorm(chisq_dat, pch = 1, frame = FALSE)
qqline(chisq_dat, col = "steelblue", lwd = 2)


## ----------------------------------------------------------------
new_dat<-10-chisq_dat
par(mfrow=c(1,2))
hist(new_dat, main="Negative skew")
qqnorm(new_dat, pch = 1, frame = FALSE)
qqline(new_dat, col = "steelblue", lwd = 2)


## ----------------------------------------------------------------
t_dat<-rt(n, df=2); 
par(mfrow=c(1,2))
hist(t_dat, main="Fat-tails")
qqnorm(t_dat, pch = 1, frame = FALSE)
qqline(t_dat, col = "steelblue", lwd = 2)


## ----------------------------------------------------------------
x<-seq(from = -5, to = 5, by = 0.1)
y<-1+ x^2 + rnorm(length(x), mean=0, sd=0.5)
par(mfrow=c(1,2), mar=c(3,5,3,2))
plot(x,y, pch = 16, cex = 0.5, ylab = expression(y == x^2), main="X-Y scatterplot", font.main=1)
reg1<-lm(y~x)
plot(reg1, which=2, pch = 16, cex = 0.5)


## ----------------------------------------------------------------
x<-seq(from=1, to = 50, by=0.5)
y<-3*sqrt(x)+rnorm(length(x), mean=0, sd = 0.1)
par(mfrow=c(1,2), mar=c(3,5,3,2))
plot(x,y, pch = 16, cex = 0.5, ylab = expression(y == 3 %*% sqrt(x)), main="X-Y scatterplot", font.main=1)
reg2<-lm(y~x)
plot(reg2, which=2, pch = 16, cex = 0.5)


## ----------------------------------------------------------------
x<-seq(from = 2, to = 6, by = 0.05)
y<-exp(2*x+1+rnorm(length(x), mean = 0, sd = 0.1))
par(mfrow=c(1,2), mar=c(3,5,3,2))
plot(x,y, pch = 16, cex = 0.5, ylab = expression(y == exp( 2 %*%  x + 1)), main="X-Y scatterplot", font.main=1)
reg3<-lm(y~x)
plot(reg3, which=2, pch = 16, cex = 0.5)


## ----------------------------------------------------------------
if(!require(mlbench)) install.packages('mlbench')
library(mlbench)
data("BostonHousing")
str(BostonHousing)

summary(BostonHousing)
#연속형 자료는 통계치를 / 팩터형 자료는 수를 나타내줌.

## ----------------------------------------------------------------
sub<-subset(BostonHousing, select = -chas)
corr1<-cor(sub)


library(ggcorrplot)
ggcorrplot::ggcorrplot(corr1, hc.order=T, type='lower', lab=T)


## ----------------------------------------------------------------
library(GGally)
GGally::ggpairs(sub)


## ----------------------------------------------------------------
GGally::ggpairs(sub, columns = c(1:6, 13))


## ----------------------------------------------------------------
GGally::ggpairs(sub, columns = c(7:12, 13))


## ----------------------------------------------------------------
h.reg<-lm(medv~.,data=BostonHousing)

#medv~. : medv를 제외한 모든 자료를 x값으로 둠
summary(h.reg)

h.reg.step<-step(h.reg, direction = "both")

#변수선택법을 맡김.
##both / forward / backward


## ----------------------------------------------------------------
formula(h.reg.step)

#선택된 공식.

## ----------------------------------------------------------------
summary(h.reg.step)


## ----------------------------------------------------------------
install.packages("car")

library(car)
vif(h.reg.step)

#다중공선성 확인
#10초과 여부를 기준으로 봄
#초과하면? 대상 데이터를 하나씩 빼면서 유의성이나 결정계수를 확인함.

summary(h.reg.step)

## ----------------------------------------------------------------
salaryDat<-read.csv(file="DataSet/salary.csv", header=T) 

salaryDat
str(salaryDat)

## ----------------------------------------------------------------
salaryDat$edu<-as.factor(salaryDat$edu)
salaryDat$gender<-as.factor(salaryDat$gender)

str(salaryDat)


## ----------------------------------------------------------------
salaryReg<-lm(salary ~ gender + edu + year, data=salaryDat)

model.matrix(salaryReg)

#1은 여자 / 0은 남자

#edu = 1 => newedu2=0, newedu3=0 (고졸)
#edu = 2 => newedu2=1, newedu3=0 (대졸)
#edu = 3 => newedu2=0, newedu3=1 (대학원졸)



## ----------------------------------------------------------------
summary(salaryReg)

# predicted salary = 154.162 + 13.318 gender+ 
#                      22.781 newedu2 + 38.396 newedu3 + 6.592 year

# 모든 다른 변수값이 동일하다는 가정하에, 여자의 샐러리가 남자보다 13.318만큼 높다
# 모든 다른 변수값이 동일하다는 가정하에, 고졸 = 0, 대졸 = 22.781, 대학원졸 = 38.396

predict(salaryReg, newdata = data.frame(gender=as.factor(0), edu=as.factor(2), year=10))

data.frame(year=10, gender=0, edu=2)

# (예측) 10년, 남성, 대졸 => 154.162 + 6.592 * 10 + 22.781
# (예측) 10년, 여성, 대졸 => 154.162 + 6.592 * 10 + 22.781 + 13.318
# (예측) 10년, 여성, 고졸 => 154.162 + 13.318 + 6.592 * 10



## ----------------------------------------------------------------
## ----------------------------------------------------------------



hsb2<-read.table("DataSet/hsb2.txt", header=T, sep="\t")

hsb2$gender<-factor(hsb2$female, labels=c('male', 'female'))

hsb2$ses2<-factor(hsb2$ses, labels=c('low', 'middle', 'high'))

hsb2$prog2<-factor(hsb2$prog, labels=c('general', 'academic', 'vocational'))

hsb2$race2<-factor(hsb2$race, labels=c('AfricanAmerican', 'Asian', 'Hispanic', 'White'))

head(hsb2)


## ----------------------------------------------------------------
## ----------------------------------------------------------------
set.seed(123)
(rb_reg<-lqs(y ~ x, data=ex4))

rb_betas<-rb_reg$coefficients 
rb_betas

## ----------------------------------------------------------------
out.reg<-lm(y ~ x, data=ex4)
reg_betas<-out.reg$coefficients
summary(out.reg)


## ----------------------------------------------------------------

par(mfrow=c(1,3))
plot(out.reg, which=c(1, 2, 4))


## ----------------------------------------------------------------
library(ggplot2)
ggplot(data =ex4, aes(x, y)) + 
  geom_point() +
  geom_abline(intercept = reg_betas[1], slope = reg_betas[2], color='blue')+
  geom_abline(intercept = rb_betas[1], slope = rb_betas[2], color='red')

#빨간선이 로버스트 / 파랑색은 일반 선형회귀
  ## 이상치가 제외된 후 구해짐.


## ----------------------------------------------------------------
mpg_loess<-loess(hwy~displ, data=mpg)
summary(mpg_loess)


## ----------------------------------------------------------------
ggplot(data=mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_smooth(color='blue')


## ----------------------------------------------------------------
ldata<-read.csv("DataSet/binary.csv")

str(ldata)

## ----------------------------------------------------------------
ldata$rank<-factor(ldata$rank)

#factor화 시켜서 더미변수 형성

str(ldata)


## ----------------------------------------------------------------
model<-glm(admit ~ gre + gpa + rank, data=ldata, family="binomial")

#Genearlize Linear Model : 매우 어려운 모델이지만 간단하게 해보는 것임.


## ----------------------------------------------------------------
summary(model)


## ----------------------------------------------------------------
exp(coef(model))

#odds ratio로 바꾸어야 해석이 가능함.
## 그냥 beta를 가지고 해석하지 말것!

## ----------------------------------------------------------------
install.packages("aod")
library(aod)

wald.test(b=coef(model), Sigma=vcov(model), Terms=4:6)

#4:6 : rank2~rank4
#유의성 판당.


## ----------------------------------------------------------------
lvec<-cbind(0,0,0,1,-1,0) # testing linear combination 
wald.test(b=coef(model), Sigma=vcov(model), L=lvec)


## ----------------------------------------------------------------
ldata1<-with(ldata, data.frame(gre=mean(gre), gpa=mean(gpa), rank=factor(1:4)))
ldata1$rankP<-predict(model, newdata=ldata1, type='response')


## ----------------------------------------------------------------
ldata1

#rankP 는 admit이 1일 확률.(합격확률)


## ----------------------------------------------------------------
ldata2<-with(ldata, data.frame(gre=rep(seq(from = 200, to = 800, length.out=100),4), 
                               gpa=mean(gpa), rank=factor(rep(1:4, each=100)))) 

#GRE와 RANK 값만을 변경한 값을 바탕으로 값을 추정해봄.



  #with(DF, new dataset) : ldata$gpa 이런 식으로 반복하지 않게
  
  #예시
  
  # table(Cars93$Type, Cars93$AirBags)
  # with(Cars93, table(Type, AirBags))


ldata2$PredictedProb<-predict(model, newdata=ldata2, type='response')
head(ldata2)


## ----------------------------------------------------------------
ggplot(ldata2, aes(x=gre, y = PredictedProb)) +
  geom_line(aes(colour=rank))


## ----------------------------------------------------------------
#위와 동일한 결과값을 갖는 다른 방법.


ldata3<-with(ldata, data.frame(gre=rep(seq(from = 200, to = 800, length.out=100),4), 
                               gpa=mean(gpa), rank=factor(rep(1:4, each=100)))) 

ldata3$PredictedProb2<-plogis(predict(model, newdata=ldata3, type="link"))
head(ldata3)


## ----------------------------------------------------------------
ggplot(ldata3, aes(x=gre, y = PredictedProb2)) +
  geom_line(aes(colour=rank))


## ----------------------------------------------------------------
credit.df<-read.csv("DataSet/credit_dataset_final.csv", header = T, sep = ",")
dim(credit.df)
str(credit.df)

## ----------------------------------------------------------------
summary(credit.df[1:10])


## ----------------------------------------------------------------
summary(credit.df[11:21])


## ----------------------------------------------------------------

#feature는 variance와 동일한 의미로 쓰임
##scale은 평균을 0으로 바꾸어줌.
###범위가 너무 다를때 바꾸어줌.

scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center=T, scale=T)
  }
  return(df)
}

# normalize variables
numeric.vars <- c("credit.duration.months", "age", "credit.amount")
credit.df <- scale.features(credit.df, numeric.vars)


## ----------------------------------------------------------------
# 함수 
to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

# 변수지정 
categorical.vars <- c('credit.rating', 'account.balance', 'previous.credit.payment.status',
                      'credit.purpose', 'savings', 'employment.duration', 'installment.rate',
                      'marital.status', 'guarantor', 'residence.duration', 'current.assets',
                      'other.credits', 'apartment.type', 'bank.credits', 'occupation', 
                      'dependents', 'telephone', 'foreign.worker')

credit.df <- to.factors(df=credit.df, variables=categorical.vars)


## ----------------------------------------------------------------
summary(credit.df[1:10])


## ----------------------------------------------------------------
summary(credit.df[11:21])


## ----------------------------------------------------------------
set.seed(10)
indexes <- sample(1:nrow(credit.df), size=0.6*nrow(credit.df))

#indexes에 행의 60%를 무작위 추출

train.data <- credit.df[indexes,]
#훈련데이터에 60%가 추출되어 들어감

test.data <- credit.df[-indexes,]
#테스트 데이터에 40%가 추출되어 들어감


## ----------------------------------------------------------------
test.feature.vars <- test.data[,-1]
#x에 해당하는 아이들

test.class.var <- test.data[,1]
#y에 해당하는 아이들



lr.model <- glm(credit.rating ~ ., data=train.data, family="binomial")
#train 데이터로 학습함.

## ----------------------------------------------------------------
summary(lr.model)


## ----------------------------------------------------------------
lr.predictions<-predict(lr.model, test.data, type="response")
#예측된 확률값

lr.predictions[1:10]
#어느 표본의 종속변수가 1일 확률임.
## -> 이 자체로 모델 평가가 불가함.

lr.predictions<-factor(round(lr.predictions))
#반올림(0.5초과시 1)하여 값을 가져감.
##어느 값을 기준으로 분류할지도 잘 정해야함.

lr.predictions[1:10]
#확률값을 0과 1로 분류함.



install.packages("caret")
install.packages("e1071")

library(caret); library(e1071)

caret::confusionMatrix(data=lr.predictions, reference=test.class.var, positive='1')

#confusionMatrix(데이터값, 예측된 모형, 실제 값, )
#테스트 데이터를 예측 모형에 넣어 정확도 테스트하는 원리.


##--------------------------------------------------

## ----fig=TRUE, out.height='0.7\\textheight',out.extra='keepaspectratio'----

#trainControl : 반복적으로 변수를 선택하여 변수 중요도를 선별함


control <- trainControl(method="repeatedcv", number=10, repeats=2)

model <- train(credit.rating ~ ., data=train.data, method="glm", trControl=control)

importance <- varImp(model, scale=FALSE)

plot(importance)

#변수 선택은 실험자 본인의 선택임

## ----------------------------------------------------------------
lr.model.new <- glm(credit.rating ~ account.balance + previous.credit.payment.status + 
                      savings + residence.duration + credit.purpose, data=train.data, family="binomial")

#상위 5개 변수만을 통한 모델 구축


## ----------------------------------------------------------------
summary(lr.model.new)


## ----------------------------------------------------------------
lr.predictions.new<-predict(lr.model.new, test.data, type="response")

lr.predictions.new<-factor(round(lr.predictions.new))

confusionMatrix(data=lr.predictions.new, reference=test.class.var, positive='1')


## ----------------------------------------------------------------
install.packages("ROCR")
library(ROCR)
library(ggplot2)

lr.model.best <- lr.model

lr.prediction.values <- predict(lr.model.best, test.feature.vars, type="response")

predictions<-prediction(lr.prediction.values, test.class.var)


## ----------------------------------------------------------------
plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf,col="black",lty=1, lwd=2, main=title.text, cex.main=0.6, cex.lab=0.8,xaxs="i", yaxs="i")
  abline(0,1, col="red")
  auc <- performance(predictions,"auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4,legend=c(paste0("AUC: ",auc)),cex=0.6,bty = "n",box.col = "white")
}

plot.pr.curve <- function(predictions, title.text){
  perf <- performance(predictions, "prec", "rec")
  plot(perf,col="black",lty=1, lwd=2, main=title.text, cex.main=0.6, cex.lab=0.8, xaxs="i", yaxs="i")
}


## ----------------------------------------------------------------
par(mfrow=c(1,2))
plot.roc.curve(predictions, "LR ROC Curve")
plot.pr.curve(predictions, "LR Precision/Recall Curve")


## ----------------------------------------------------------------
## ----------------------------------------------------------------

library(arules)
data("AdultUCI")
# ?AdultUCI # 데이터 설명 


## ----------------------------------------------------------------
str(AdultUCI)


## ----------------------------------------------------------------
var_names<-names(AdultUCI)
var_names
adult0<-AdultUCI # 자료 복사 
names(adult0)<-gsub("-", "_", var_names)
adult0$education_num[1:5]
