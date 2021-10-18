<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD

## ----------------------------------------------------------------
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


## ----------------------------------------------------------------
plot(cars$speed, cars$dist)


## ----------------------------------------------------------------
(m<-lm(dist ~ speed, data=cars))


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


## ----------------------------------------------------------------
residuals(m)[1:5]


## ----------------------------------------------------------------
summary(m)


## ----------------------------------------------------------------
summary(cars$speed)
predict(m, newdata = data.frame(speed=c(5,10,15)))


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


## ----------------------------------------------------------------
sub<-subset(BostonHousing, select = -chas)
corr1<-cor(sub)
ggcorrplot::ggcorrplot(corr1, hc.order=T, type='lower', lab=T)


## ----------------------------------------------------------------
GGally::ggpairs(sub)


## ----------------------------------------------------------------
GGally::ggpairs(sub, columns = c(1:6, 13))


## ----------------------------------------------------------------
GGally::ggpairs(sub, columns = c(7:12, 13))


## ----------------------------------------------------------------
h.reg<-lm(medv~.,data=BostonHousing)
h.reg.step<-step(h.reg, direction = "both")


## ----------------------------------------------------------------
formula(h.reg.step)


## ----------------------------------------------------------------
summary(h.reg.step)


## ----------------------------------------------------------------
library(car)
vif(h.reg.step)


## ----------------------------------------------------------------
salaryDat<-read.csv(file="DataSet/salary.csv", header=T) 

salaryDat
str(salaryDat)


## ----------------------------------------------------------------
salaryDat$newedu<-as.factor(salaryDat$edu)
salaryDat$newgender<-as.factor(salaryDat$gender)

str(salaryDat)


## ----------------------------------------------------------------
salaryReg<-lm(salary ~ newgender + newedu + year, data=salaryDat)
model.matrix(salaryReg)


## ----------------------------------------------------------------
summary(salaryReg)


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


## ----------------------------------------------------------------
out.reg<-lm(y ~ x, data=ex4)
reg_betas<-out.reg$coefficients
summary(out.reg)


## ----------------------------------------------------------------

par(mfrow=c(1,3))
plot(out.reg, which=c(1, 2, 4))


## ----------------------------------------------------------------
ggplot(data =ex4, aes(x, y)) + 
  geom_point() +
  geom_abline(intercept = reg_betas[1], slope = reg_betas[2], color='blue')+
  geom_abline(intercept = rb_betas[1], slope = rb_betas[2], color='red')


## ----------------------------------------------------------------
mpg_loess<-loess(hwy~displ, data=mpg)
summary(mpg_loess)


## ----------------------------------------------------------------
ggplot(data=mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_smooth(color='blue')


## ----------------------------------------------------------------
ldata<-read.csv("DataSet/binary.csv")


## ----------------------------------------------------------------
ldata$rank<-factor(ldata$rank)
str(ldata)


## ----------------------------------------------------------------
model<-glm(admit ~ gre + gpa + rank, data=ldata, family="binomial")


## ----------------------------------------------------------------
summary(model)


## ----------------------------------------------------------------
exp(coef(model))


## ----------------------------------------------------------------
library(aod)
wald.test(b=coef(model), Sigma=vcov(model), Terms=4:6)


## ----------------------------------------------------------------
lvec<-cbind(0,0,0,1,-1,0) # testing linear combination 
wald.test(b=coef(model), Sigma=vcov(model), L=lvec)


## ----------------------------------------------------------------
ldata1<-with(ldata, data.frame(gre=mean(gre), gpa=mean(gpa), rank=factor(1:4)))
ldata1$rankP<-predict(model, newdata=ldata1, type='response')


## ----------------------------------------------------------------
ldata1


## ----------------------------------------------------------------
ldata2<-with(ldata, data.frame(gre=rep(seq(from = 200, to = 800, length.out=100),4), 
                               gpa=mean(gpa), rank=factor(rep(1:4, each=100)))) 
ldata2$PredictedProb<-predict(model, newdata=ldata2, type='response')
head(ldata2)


## ----------------------------------------------------------------
ggplot(ldata2, aes(x=gre, y = PredictedProb)) +
  geom_line(aes(colour=rank))


## ----------------------------------------------------------------
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


## ----------------------------------------------------------------
summary(credit.df[1:10])


## ----------------------------------------------------------------
summary(credit.df[11:21])


## ----------------------------------------------------------------
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
train.data <- credit.df[indexes,]
test.data <- credit.df[-indexes,]


## ----------------------------------------------------------------
test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]
lr.model <- glm(credit.rating ~ ., data=train.data, family="binomial")


## ----------------------------------------------------------------
summary(lr.model)


## ----------------------------------------------------------------
lr.predictions<-predict(lr.model, test.data, type="response")
lr.predictions<-factor(round(lr.predictions))
library(caret); library(e1071)
confusionMatrix(data=lr.predictions, reference=test.class.var, positive='1')


## ----fig=TRUE, out.height='0.7\\textheight',out.extra='keepaspectratio'----
control <- trainControl(method="repeatedcv", number=10, repeats=2)
model <- train(credit.rating ~ ., data=train.data, method="glm", trControl=control)
importance <- varImp(model, scale=FALSE)
plot(importance)


## ----------------------------------------------------------------
lr.model.new <- glm(credit.rating ~ account.balance + previous.credit.payment.status + 
                      savings + residence.duration + credit.purpose, data=train.data, family="binomial")



## ----------------------------------------------------------------
summary(lr.model.new)


## ----------------------------------------------------------------
lr.predictions.new<-predict(lr.model.new, test.data, type="response") 
lr.predictions.new<-factor(round(lr.predictions.new))
confusionMatrix(data=lr.predictions.new, reference=test.class.var, positive='1')


## ----------------------------------------------------------------
library(ROCR)
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
=======

## ----------------------------------------------------------------
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


## ----------------------------------------------------------------
plot(cars$speed, cars$dist)


## ----------------------------------------------------------------
(m<-lm(dist ~ speed, data=cars))


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


## ----------------------------------------------------------------
residuals(m)[1:5]


## ----------------------------------------------------------------
summary(m)


## ----------------------------------------------------------------
summary(cars$speed)
predict(m, newdata = data.frame(speed=c(5,10,15)))


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


## ----------------------------------------------------------------
sub<-subset(BostonHousing, select = -chas)
corr1<-cor(sub)
ggcorrplot::ggcorrplot(corr1, hc.order=T, type='lower', lab=T)


## ----------------------------------------------------------------
GGally::ggpairs(sub)


## ----------------------------------------------------------------
GGally::ggpairs(sub, columns = c(1:6, 13))


## ----------------------------------------------------------------
GGally::ggpairs(sub, columns = c(7:12, 13))


## ----------------------------------------------------------------
h.reg<-lm(medv~.,data=BostonHousing)
h.reg.step<-step(h.reg, direction = "both")


## ----------------------------------------------------------------
formula(h.reg.step)


## ----------------------------------------------------------------
summary(h.reg.step)


## ----------------------------------------------------------------
library(car)
vif(h.reg.step)


## ----------------------------------------------------------------
salaryDat<-read.csv(file="DataSet/salary.csv", header=T) 

salaryDat
str(salaryDat)


## ----------------------------------------------------------------
salaryDat$newedu<-as.factor(salaryDat$edu)
salaryDat$newgender<-as.factor(salaryDat$gender)

str(salaryDat)


## ----------------------------------------------------------------
salaryReg<-lm(salary ~ newgender + newedu + year, data=salaryDat)
model.matrix(salaryReg)


## ----------------------------------------------------------------
summary(salaryReg)


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


## ----------------------------------------------------------------
out.reg<-lm(y ~ x, data=ex4)
reg_betas<-out.reg$coefficients
summary(out.reg)


## ----------------------------------------------------------------

par(mfrow=c(1,3))
plot(out.reg, which=c(1, 2, 4))


## ----------------------------------------------------------------
ggplot(data =ex4, aes(x, y)) + 
  geom_point() +
  geom_abline(intercept = reg_betas[1], slope = reg_betas[2], color='blue')+
  geom_abline(intercept = rb_betas[1], slope = rb_betas[2], color='red')


## ----------------------------------------------------------------
mpg_loess<-loess(hwy~displ, data=mpg)
summary(mpg_loess)


## ----------------------------------------------------------------
ggplot(data=mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_smooth(color='blue')


## ----------------------------------------------------------------
ldata<-read.csv("DataSet/binary.csv")


## ----------------------------------------------------------------
ldata$rank<-factor(ldata$rank)
str(ldata)


## ----------------------------------------------------------------
model<-glm(admit ~ gre + gpa + rank, data=ldata, family="binomial")


## ----------------------------------------------------------------
summary(model)


## ----------------------------------------------------------------
exp(coef(model))


## ----------------------------------------------------------------
library(aod)
wald.test(b=coef(model), Sigma=vcov(model), Terms=4:6)


## ----------------------------------------------------------------
lvec<-cbind(0,0,0,1,-1,0) # testing linear combination 
wald.test(b=coef(model), Sigma=vcov(model), L=lvec)


## ----------------------------------------------------------------
ldata1<-with(ldata, data.frame(gre=mean(gre), gpa=mean(gpa), rank=factor(1:4)))
ldata1$rankP<-predict(model, newdata=ldata1, type='response')


## ----------------------------------------------------------------
ldata1


## ----------------------------------------------------------------
ldata2<-with(ldata, data.frame(gre=rep(seq(from = 200, to = 800, length.out=100),4), 
                               gpa=mean(gpa), rank=factor(rep(1:4, each=100)))) 
ldata2$PredictedProb<-predict(model, newdata=ldata2, type='response')
head(ldata2)


## ----------------------------------------------------------------
ggplot(ldata2, aes(x=gre, y = PredictedProb)) +
  geom_line(aes(colour=rank))


## ----------------------------------------------------------------
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


## ----------------------------------------------------------------
summary(credit.df[1:10])


## ----------------------------------------------------------------
summary(credit.df[11:21])


## ----------------------------------------------------------------
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
train.data <- credit.df[indexes,]
test.data <- credit.df[-indexes,]


## ----------------------------------------------------------------
test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]
lr.model <- glm(credit.rating ~ ., data=train.data, family="binomial")


## ----------------------------------------------------------------
summary(lr.model)


## ----------------------------------------------------------------
lr.predictions<-predict(lr.model, test.data, type="response")
lr.predictions<-factor(round(lr.predictions))
library(caret); library(e1071)
confusionMatrix(data=lr.predictions, reference=test.class.var, positive='1')


## ----fig=TRUE, out.height='0.7\\textheight',out.extra='keepaspectratio'----
control <- trainControl(method="repeatedcv", number=10, repeats=2)
model <- train(credit.rating ~ ., data=train.data, method="glm", trControl=control)
importance <- varImp(model, scale=FALSE)
plot(importance)


## ----------------------------------------------------------------
lr.model.new <- glm(credit.rating ~ account.balance + previous.credit.payment.status + 
                      savings + residence.duration + credit.purpose, data=train.data, family="binomial")



## ----------------------------------------------------------------
summary(lr.model.new)


## ----------------------------------------------------------------
lr.predictions.new<-predict(lr.model.new, test.data, type="response") 
lr.predictions.new<-factor(round(lr.predictions.new))
confusionMatrix(data=lr.predictions.new, reference=test.class.var, positive='1')


## ----------------------------------------------------------------
library(ROCR)
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
>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======

## ----------------------------------------------------------------
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


## ----------------------------------------------------------------
plot(cars$speed, cars$dist)


## ----------------------------------------------------------------
(m<-lm(dist ~ speed, data=cars))


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


## ----------------------------------------------------------------
residuals(m)[1:5]


## ----------------------------------------------------------------
summary(m)


## ----------------------------------------------------------------
summary(cars$speed)
predict(m, newdata = data.frame(speed=c(5,10,15)))


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


## ----------------------------------------------------------------
sub<-subset(BostonHousing, select = -chas)
corr1<-cor(sub)
ggcorrplot::ggcorrplot(corr1, hc.order=T, type='lower', lab=T)


## ----------------------------------------------------------------
GGally::ggpairs(sub)


## ----------------------------------------------------------------
GGally::ggpairs(sub, columns = c(1:6, 13))


## ----------------------------------------------------------------
GGally::ggpairs(sub, columns = c(7:12, 13))


## ----------------------------------------------------------------
h.reg<-lm(medv~.,data=BostonHousing)
h.reg.step<-step(h.reg, direction = "both")


## ----------------------------------------------------------------
formula(h.reg.step)


## ----------------------------------------------------------------
summary(h.reg.step)


## ----------------------------------------------------------------
library(car)
vif(h.reg.step)


## ----------------------------------------------------------------
salaryDat<-read.csv(file="DataSet/salary.csv", header=T) 

salaryDat
str(salaryDat)


## ----------------------------------------------------------------
salaryDat$newedu<-as.factor(salaryDat$edu)
salaryDat$newgender<-as.factor(salaryDat$gender)

str(salaryDat)


## ----------------------------------------------------------------
salaryReg<-lm(salary ~ newgender + newedu + year, data=salaryDat)
model.matrix(salaryReg)


## ----------------------------------------------------------------
summary(salaryReg)


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


## ----------------------------------------------------------------
out.reg<-lm(y ~ x, data=ex4)
reg_betas<-out.reg$coefficients
summary(out.reg)


## ----------------------------------------------------------------

par(mfrow=c(1,3))
plot(out.reg, which=c(1, 2, 4))


## ----------------------------------------------------------------
ggplot(data =ex4, aes(x, y)) + 
  geom_point() +
  geom_abline(intercept = reg_betas[1], slope = reg_betas[2], color='blue')+
  geom_abline(intercept = rb_betas[1], slope = rb_betas[2], color='red')


## ----------------------------------------------------------------
mpg_loess<-loess(hwy~displ, data=mpg)
summary(mpg_loess)


## ----------------------------------------------------------------
ggplot(data=mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_smooth(color='blue')


## ----------------------------------------------------------------
ldata<-read.csv("DataSet/binary.csv")


## ----------------------------------------------------------------
ldata$rank<-factor(ldata$rank)
str(ldata)


## ----------------------------------------------------------------
model<-glm(admit ~ gre + gpa + rank, data=ldata, family="binomial")


## ----------------------------------------------------------------
summary(model)


## ----------------------------------------------------------------
exp(coef(model))


## ----------------------------------------------------------------
library(aod)
wald.test(b=coef(model), Sigma=vcov(model), Terms=4:6)


## ----------------------------------------------------------------
lvec<-cbind(0,0,0,1,-1,0) # testing linear combination 
wald.test(b=coef(model), Sigma=vcov(model), L=lvec)


## ----------------------------------------------------------------
ldata1<-with(ldata, data.frame(gre=mean(gre), gpa=mean(gpa), rank=factor(1:4)))
ldata1$rankP<-predict(model, newdata=ldata1, type='response')


## ----------------------------------------------------------------
ldata1


## ----------------------------------------------------------------
ldata2<-with(ldata, data.frame(gre=rep(seq(from = 200, to = 800, length.out=100),4), 
                               gpa=mean(gpa), rank=factor(rep(1:4, each=100)))) 
ldata2$PredictedProb<-predict(model, newdata=ldata2, type='response')
head(ldata2)


## ----------------------------------------------------------------
ggplot(ldata2, aes(x=gre, y = PredictedProb)) +
  geom_line(aes(colour=rank))


## ----------------------------------------------------------------
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


## ----------------------------------------------------------------
summary(credit.df[1:10])


## ----------------------------------------------------------------
summary(credit.df[11:21])


## ----------------------------------------------------------------
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
train.data <- credit.df[indexes,]
test.data <- credit.df[-indexes,]


## ----------------------------------------------------------------
test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]
lr.model <- glm(credit.rating ~ ., data=train.data, family="binomial")


## ----------------------------------------------------------------
summary(lr.model)


## ----------------------------------------------------------------
lr.predictions<-predict(lr.model, test.data, type="response")
lr.predictions<-factor(round(lr.predictions))
library(caret); library(e1071)
confusionMatrix(data=lr.predictions, reference=test.class.var, positive='1')


## ----fig=TRUE, out.height='0.7\\textheight',out.extra='keepaspectratio'----
control <- trainControl(method="repeatedcv", number=10, repeats=2)
model <- train(credit.rating ~ ., data=train.data, method="glm", trControl=control)
importance <- varImp(model, scale=FALSE)
plot(importance)


## ----------------------------------------------------------------
lr.model.new <- glm(credit.rating ~ account.balance + previous.credit.payment.status + 
                      savings + residence.duration + credit.purpose, data=train.data, family="binomial")



## ----------------------------------------------------------------
summary(lr.model.new)


## ----------------------------------------------------------------
lr.predictions.new<-predict(lr.model.new, test.data, type="response") 
lr.predictions.new<-factor(round(lr.predictions.new))
confusionMatrix(data=lr.predictions.new, reference=test.class.var, positive='1')


## ----------------------------------------------------------------
library(ROCR)
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
>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======

## ----------------------------------------------------------------
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


## ----------------------------------------------------------------
plot(cars$speed, cars$dist)


## ----------------------------------------------------------------
(m<-lm(dist ~ speed, data=cars))


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


## ----------------------------------------------------------------
residuals(m)[1:5]


## ----------------------------------------------------------------
summary(m)


## ----------------------------------------------------------------
summary(cars$speed)
predict(m, newdata = data.frame(speed=c(5,10,15)))


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


## ----------------------------------------------------------------
sub<-subset(BostonHousing, select = -chas)
corr1<-cor(sub)
ggcorrplot::ggcorrplot(corr1, hc.order=T, type='lower', lab=T)


## ----------------------------------------------------------------
GGally::ggpairs(sub)


## ----------------------------------------------------------------
GGally::ggpairs(sub, columns = c(1:6, 13))


## ----------------------------------------------------------------
GGally::ggpairs(sub, columns = c(7:12, 13))


## ----------------------------------------------------------------
h.reg<-lm(medv~.,data=BostonHousing)
h.reg.step<-step(h.reg, direction = "both")


## ----------------------------------------------------------------
formula(h.reg.step)


## ----------------------------------------------------------------
summary(h.reg.step)


## ----------------------------------------------------------------
library(car)
vif(h.reg.step)


## ----------------------------------------------------------------
salaryDat<-read.csv(file="DataSet/salary.csv", header=T) 

salaryDat
str(salaryDat)


## ----------------------------------------------------------------
salaryDat$newedu<-as.factor(salaryDat$edu)
salaryDat$newgender<-as.factor(salaryDat$gender)

str(salaryDat)


## ----------------------------------------------------------------
salaryReg<-lm(salary ~ newgender + newedu + year, data=salaryDat)
model.matrix(salaryReg)


## ----------------------------------------------------------------
summary(salaryReg)


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


## ----------------------------------------------------------------
out.reg<-lm(y ~ x, data=ex4)
reg_betas<-out.reg$coefficients
summary(out.reg)


## ----------------------------------------------------------------

par(mfrow=c(1,3))
plot(out.reg, which=c(1, 2, 4))


## ----------------------------------------------------------------
ggplot(data =ex4, aes(x, y)) + 
  geom_point() +
  geom_abline(intercept = reg_betas[1], slope = reg_betas[2], color='blue')+
  geom_abline(intercept = rb_betas[1], slope = rb_betas[2], color='red')


## ----------------------------------------------------------------
mpg_loess<-loess(hwy~displ, data=mpg)
summary(mpg_loess)


## ----------------------------------------------------------------
ggplot(data=mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_smooth(color='blue')


## ----------------------------------------------------------------
ldata<-read.csv("DataSet/binary.csv")


## ----------------------------------------------------------------
ldata$rank<-factor(ldata$rank)
str(ldata)


## ----------------------------------------------------------------
model<-glm(admit ~ gre + gpa + rank, data=ldata, family="binomial")


## ----------------------------------------------------------------
summary(model)


## ----------------------------------------------------------------
exp(coef(model))


## ----------------------------------------------------------------
library(aod)
wald.test(b=coef(model), Sigma=vcov(model), Terms=4:6)


## ----------------------------------------------------------------
lvec<-cbind(0,0,0,1,-1,0) # testing linear combination 
wald.test(b=coef(model), Sigma=vcov(model), L=lvec)


## ----------------------------------------------------------------
ldata1<-with(ldata, data.frame(gre=mean(gre), gpa=mean(gpa), rank=factor(1:4)))
ldata1$rankP<-predict(model, newdata=ldata1, type='response')


## ----------------------------------------------------------------
ldata1


## ----------------------------------------------------------------
ldata2<-with(ldata, data.frame(gre=rep(seq(from = 200, to = 800, length.out=100),4), 
                               gpa=mean(gpa), rank=factor(rep(1:4, each=100)))) 
ldata2$PredictedProb<-predict(model, newdata=ldata2, type='response')
head(ldata2)


## ----------------------------------------------------------------
ggplot(ldata2, aes(x=gre, y = PredictedProb)) +
  geom_line(aes(colour=rank))


## ----------------------------------------------------------------
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


## ----------------------------------------------------------------
summary(credit.df[1:10])


## ----------------------------------------------------------------
summary(credit.df[11:21])


## ----------------------------------------------------------------
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
train.data <- credit.df[indexes,]
test.data <- credit.df[-indexes,]


## ----------------------------------------------------------------
test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]
lr.model <- glm(credit.rating ~ ., data=train.data, family="binomial")


## ----------------------------------------------------------------
summary(lr.model)


## ----------------------------------------------------------------
lr.predictions<-predict(lr.model, test.data, type="response")
lr.predictions<-factor(round(lr.predictions))
library(caret); library(e1071)
confusionMatrix(data=lr.predictions, reference=test.class.var, positive='1')


## ----fig=TRUE, out.height='0.7\\textheight',out.extra='keepaspectratio'----
control <- trainControl(method="repeatedcv", number=10, repeats=2)
model <- train(credit.rating ~ ., data=train.data, method="glm", trControl=control)
importance <- varImp(model, scale=FALSE)
plot(importance)


## ----------------------------------------------------------------
lr.model.new <- glm(credit.rating ~ account.balance + previous.credit.payment.status + 
                      savings + residence.duration + credit.purpose, data=train.data, family="binomial")



## ----------------------------------------------------------------
summary(lr.model.new)


## ----------------------------------------------------------------
lr.predictions.new<-predict(lr.model.new, test.data, type="response") 
lr.predictions.new<-factor(round(lr.predictions.new))
confusionMatrix(data=lr.predictions.new, reference=test.class.var, positive='1')


## ----------------------------------------------------------------
library(ROCR)
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
>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======

## ----------------------------------------------------------------
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


## ----------------------------------------------------------------
plot(cars$speed, cars$dist)


## ----------------------------------------------------------------
(m<-lm(dist ~ speed, data=cars))


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


## ----------------------------------------------------------------
residuals(m)[1:5]


## ----------------------------------------------------------------
summary(m)


## ----------------------------------------------------------------
summary(cars$speed)
predict(m, newdata = data.frame(speed=c(5,10,15)))


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


## ----------------------------------------------------------------
sub<-subset(BostonHousing, select = -chas)
corr1<-cor(sub)
ggcorrplot::ggcorrplot(corr1, hc.order=T, type='lower', lab=T)


## ----------------------------------------------------------------
GGally::ggpairs(sub)


## ----------------------------------------------------------------
GGally::ggpairs(sub, columns = c(1:6, 13))


## ----------------------------------------------------------------
GGally::ggpairs(sub, columns = c(7:12, 13))


## ----------------------------------------------------------------
h.reg<-lm(medv~.,data=BostonHousing)
h.reg.step<-step(h.reg, direction = "both")


## ----------------------------------------------------------------
formula(h.reg.step)


## ----------------------------------------------------------------
summary(h.reg.step)


## ----------------------------------------------------------------
library(car)
vif(h.reg.step)


## ----------------------------------------------------------------
salaryDat<-read.csv(file="DataSet/salary.csv", header=T) 

salaryDat
str(salaryDat)


## ----------------------------------------------------------------
salaryDat$newedu<-as.factor(salaryDat$edu)
salaryDat$newgender<-as.factor(salaryDat$gender)

str(salaryDat)


## ----------------------------------------------------------------
salaryReg<-lm(salary ~ newgender + newedu + year, data=salaryDat)
model.matrix(salaryReg)


## ----------------------------------------------------------------
summary(salaryReg)


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


## ----------------------------------------------------------------
out.reg<-lm(y ~ x, data=ex4)
reg_betas<-out.reg$coefficients
summary(out.reg)


## ----------------------------------------------------------------

par(mfrow=c(1,3))
plot(out.reg, which=c(1, 2, 4))


## ----------------------------------------------------------------
ggplot(data =ex4, aes(x, y)) + 
  geom_point() +
  geom_abline(intercept = reg_betas[1], slope = reg_betas[2], color='blue')+
  geom_abline(intercept = rb_betas[1], slope = rb_betas[2], color='red')


## ----------------------------------------------------------------
mpg_loess<-loess(hwy~displ, data=mpg)
summary(mpg_loess)


## ----------------------------------------------------------------
ggplot(data=mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_smooth(color='blue')


## ----------------------------------------------------------------
ldata<-read.csv("DataSet/binary.csv")


## ----------------------------------------------------------------
ldata$rank<-factor(ldata$rank)
str(ldata)


## ----------------------------------------------------------------
model<-glm(admit ~ gre + gpa + rank, data=ldata, family="binomial")


## ----------------------------------------------------------------
summary(model)


## ----------------------------------------------------------------
exp(coef(model))


## ----------------------------------------------------------------
library(aod)
wald.test(b=coef(model), Sigma=vcov(model), Terms=4:6)


## ----------------------------------------------------------------
lvec<-cbind(0,0,0,1,-1,0) # testing linear combination 
wald.test(b=coef(model), Sigma=vcov(model), L=lvec)


## ----------------------------------------------------------------
ldata1<-with(ldata, data.frame(gre=mean(gre), gpa=mean(gpa), rank=factor(1:4)))
ldata1$rankP<-predict(model, newdata=ldata1, type='response')


## ----------------------------------------------------------------
ldata1


## ----------------------------------------------------------------
ldata2<-with(ldata, data.frame(gre=rep(seq(from = 200, to = 800, length.out=100),4), 
                               gpa=mean(gpa), rank=factor(rep(1:4, each=100)))) 
ldata2$PredictedProb<-predict(model, newdata=ldata2, type='response')
head(ldata2)


## ----------------------------------------------------------------
ggplot(ldata2, aes(x=gre, y = PredictedProb)) +
  geom_line(aes(colour=rank))


## ----------------------------------------------------------------
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


## ----------------------------------------------------------------
summary(credit.df[1:10])


## ----------------------------------------------------------------
summary(credit.df[11:21])


## ----------------------------------------------------------------
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
train.data <- credit.df[indexes,]
test.data <- credit.df[-indexes,]


## ----------------------------------------------------------------
test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]
lr.model <- glm(credit.rating ~ ., data=train.data, family="binomial")


## ----------------------------------------------------------------
summary(lr.model)


## ----------------------------------------------------------------
lr.predictions<-predict(lr.model, test.data, type="response")
lr.predictions<-factor(round(lr.predictions))
library(caret); library(e1071)
confusionMatrix(data=lr.predictions, reference=test.class.var, positive='1')


## ----fig=TRUE, out.height='0.7\\textheight',out.extra='keepaspectratio'----
control <- trainControl(method="repeatedcv", number=10, repeats=2)
model <- train(credit.rating ~ ., data=train.data, method="glm", trControl=control)
importance <- varImp(model, scale=FALSE)
plot(importance)


## ----------------------------------------------------------------
lr.model.new <- glm(credit.rating ~ account.balance + previous.credit.payment.status + 
                      savings + residence.duration + credit.purpose, data=train.data, family="binomial")



## ----------------------------------------------------------------
summary(lr.model.new)


## ----------------------------------------------------------------
lr.predictions.new<-predict(lr.model.new, test.data, type="response") 
lr.predictions.new<-factor(round(lr.predictions.new))
confusionMatrix(data=lr.predictions.new, reference=test.class.var, positive='1')


## ----------------------------------------------------------------
library(ROCR)
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
>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
