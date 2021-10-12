################################?ǻ???��???? ?ǽ?################################

#rpart package down

install.packages("rpart")
library(rpart)

###   rpart(formula, data, method,??)
###   formula : R???? ?????ϴ? ???????? ????
###   data : train ?????Ϳ? ?ش??ϴ? data frame ?̸?
###   method : default ???? ??ǻ?Ͱ? ??????��?? ?????Ͽ? ??��
###   ??anova?? = ȸ?ͳ??? ?????? ????, ��?????? ???ҿ? ???? ?з?
###   ??class?? = ?з????? ?????? ????, ????(Gini) ?????? ???? ?з? (default) 
###   entropy ?????? ?з??ϰ? ??�� ????
###        parms=list(split=??information??)�� ?߰? ?Է??ؾ? ?Ѵ?.
#####################################################################

###  1. Classification Trees ?ǽ? : iris ??????
dim(iris)
iris[1:5,]

set.seed(12321)
d<-sample(nrow(iris),nrow(iris)*0.7)
train<-iris[d,]
test<-iris[-d,]

#모델구축
model3<-rpart(Species ~ Petal.Length +Petal.Width +Sepal.Length +Sepal.Width, data=train)
print(model3)

plot(model3)
text(model3)

#test값과 predict값 비교
pred<-predict(model3,newdata=test)
pred2 <- predict(model3, newdata=test,type="class")
  #범주형으로 예측값 저장 : type="class"

pred
#확률값이 출력됨을 알 수 있음.

cbind(test, pred2)


#결과table
# table(pred2,test$Species)

(class.pred2=table(pred2,test$Species))
1-sum(diag(class.pred2))/sum(class.pred2)  
# 오류율

######################################
## training set과 test set 을 분류하는 또다른 방법

sub <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25)) 
#구간별 추출

fit <- rpart(Species ~ ., data = iris, subset = sub) 
fit

table(predict(fit, iris[-sub,], type = "class"), iris[-sub, "Species"]) 


#########################################################

#################################################
###### 2. Regression Trees  실습 :  cu.summary  데이터  ##############

#install.packages("rpart")
#library(rpart)

# 참고: cu.summary 데이타는 rpart설치해야함 
# 자동차가격(price),원산지(Country), 신뢰도(Reliability), 차타입 (Type)에 
# 따라 연료 1 갤론 당 연비 (Mileage) 예측하기

#데이터셋
head(cu.summary)

#요청구축
fit <- rpart(Mileage~Price + Country + Reliability + Type, method="anova", data=cu.summary)
print(fit)

# plot tree 
plot(fit, uniform=TRUE, main="Regression Tree for Mileage ")
text(fit)


################################앙상블 실습################################

### German Credit Data  ############


credit<-read.csv("C:/Users/Gargantua/Desktop/data_analysis/Project6_R_Bigdata/DataSet/germancredit.csv",sep=",",header=T)

# The German Credit data set contains observations on 30 variables 
# for 1000 past applicants for credit. 
# Each applicant was rated as "good credit"(700cases) or
# "bad credit"(300 cases).

#데이터 속성 맞추기
str(credit)

# lapply 명령문만 하면 if(FALSE) {  } 부분은 안해도 됨####
credit[,-c(2,5,10,21,22)]=lapply(credit[, -c(2,5,10,21,22)], factor)

str(credit)

#####  아래 if(FALSE) {  }은 위의 lapply 한줄명령문으로 대체 가능함. ###
if(FALSE){
credit$CHK_ACCT<-as.factor(credit$CHK_ACCT)
credit$HISTORY<-as.factor(credit$HISTORY)
credit$NEW_CAR<-as.factor(credit$NEW_CAR)
credit$FURNITURE<-as.factor(credit$FURNITURE)
credit$RADIO_TV<-as.factor(credit$RADIO_TV)
credit$EDUCATION<-as.factor(credit$EDUCATION)
credit$RETRAINING<-as.factor(credit$RETRAINING)
credit$SAV_ACCT<-as.factor(credit$SAV_ACCT)
credit$EMPLOYMENT<-as.factor(credit$EMPLOYMENT)
credit$INSTALL_RATE<-as.factor(credit$INSTALL_RATE)
credit$MALE_DIV<-as.factor(credit$MALE_DIV)
credit$MALE_SINGLE<-as.factor(credit$MALE_SINGLE)
credit$MALE_MAR_WID<-as.factor(credit$MALE_MAR_WID)
credit$CO_APPLICANT<-as.factor(credit$CO_APPLICANT)
credit$GUARANTOR<-as.factor(credit$GUARANTOR)
credit$PRESENT_RESIDENT<-as.factor(credit$PRESENT_RESIDENT)
credit$REAL_ESTATE<-as.factor(credit$REAL_ESTATE)
credit$OTHER_INSTALL<-as.factor(credit$OTHER_INSTALL)
credit$RENT<-as.factor(credit$RENT)
credit$OWN_RES<-as.factor(credit$OWN_RES)
credit$NUM_CREDITS<-as.factor(credit$NUM_CREDITS)
credit$JOB<-as.factor(credit$JOB)
credit$NUM_DEPENDENTS<-as.factor(credit$NUM_DEPENDENTS)
credit$TELEPHONE<-as.factor(credit$TELEPHONE)
credit$FOREIGN<-as.factor(credit$FOREIGN)
credit$RESPONSE<-as.factor(credit$RESPONSE)
}
###########

str(credit)

#데이터 확인
head(credit)

#data setting
set.seed(71)



#데이터셋
# credit<-read.csv("C:/gc.csv",sep=",",header=T)

# Partition of Data
  ##70%는 트레이닝셋 / 30%는 테스트셋

(ntr=round(nrow(credit)*0.7))

sidx=sample(nrow(credit),size=ntr, replace=F)

training.set=credit[sidx,] # Training DATA

test.set=credit[-sidx,] # Test DATA

nrow(training.set)
nrow(test.set)

#####bagging#####

#패키지 설치 및 사용
install.packages("ipred")
library(ipred)

#모형구축

ipred.bagging <- bagging(RESPONSE ~ ., nbag=25, data = training.set)
#nbag: # of bootstrap rplications
  ##bagging(y ~ x, nbag = (복원추출하여 만들 데이터셋 수), data = (데이터셋) )

ipred.bagging 

#모형 test
pred<-predict(ipred.bagging, newdata=test.set)

table(test.set$RESPONSE, pred)



#####adabag#####

#패키지 설치 및 사용
install.packages("adabag")
library(adabag)

#rpart library should be loaded
#install.packages("rpart")
#library(rpart)

#adabog통한 bagging 방법
adabag.bagging <- bagging(RESPONSE~., data=training.set, mfinal=10)

#모형 test
pred<-predict.bagging(adabag.bagging,newdata=test.set)
pred$confusion





### Bagging 이나? boosting 할때는
# ipred  package 를 권장함
## adabag package 불안정함.
####

############# boosting #############

#####gbm패키지 사용#####

#패키지 설치 및 사용
install.packages("gbm")
library(gbm)

# gbm 에서 distribution="adaboost" 는
# the AdaBoost exponential loss for 0-1 outcomes  임.
# 0-1 outcome만 가질 수 있다..
#
# n.trees	: the total number of trees to fit. 
# This is equivalent to the number of iterations and 
# the number of basis functions in the additive expansion.
#
# cv.folds : Number of cross-validation folds to perform. 
# If cv.folds>1 then gbm, in addition to the usual fit, 
# will perform a cross-validation, calculate an estimate 
# of generalization error returned in cv.error.

#모형 구축
gbm.boost <- gbm(RESPONSE ~ ., data=training.set, distribution="adaboost",
                 n.trees=10000, cv.folds=3)

#adaboost : 수업시간에 설명한 부스팅 알고리즘
#n.trees : 여러번 뽑는 횟수를 10000번 한다.

gbm.boost

#모형 test
# gbm.perf :GBM performance
# Estimates the optimal number of boosting iterations 
# for a gbm object and optionally plots various performance measures

iteration <- gbm.perf( gbm.boost, method="cv")
iteration

prediction <- predict.gbm(gbm.boost,test.set,type="response",n.trees=iteration)
# typ="response" gives the predicted probabilities

prediction <-ifelse(prediction>=0.5,1,0)
table(prediction,test.set$RESPONSE, dnn=c("Predicted", "Actual"))

  #결과가 1만 나오는 것은 1이 더 나은 결과이기 때문임.

########################
# boosting 할때 adabag package 불안정
#  ?׷??? boosting ?Ҷ? gbm package ??????.
#install.packages("adabag")
#library(adabag)

#adabag.adaboost<-boosting(RESPONSE~., data=training.set, boos=T, mfinal=10)
#adabag.adaboost$trees
#pred<-predict.boosting(adabag.adaboost,newdata=test.set)
#pred$confusion

########################################


#####  Random Forest  #####
#패키지 설치 및 사용
install.packages("randomForest")
library(randomForest)

# ntree: number of trees grown
# mtry: number of predictors sampled for spliting at each node
# Number of variables randomly sampled as candidates at each split. 
# Note that the default values are different for classification (sqrt(p) 
# where p is number of variables in x) and regression (p/3)


#모형 구축
rf<-randomForest(RESPONSE ~., data=training.set, ntree=100, mtry=6)
print(rf)

  #ntree : 추출하여 100개의 의사결정 트리를 만듬.
  #mtry : split 할 때마다 랜덤하게 6개 변수 선택
  #디폴트값은 mtrysqrt(31) = sqrt(p) 만큼 설정.
  
  ##mtry = 31 로 설정하면 bagging과 결과가 같아짐.
    # 모든 변수가 반복 추출되므로.


#모형 test
pred<-predict(rf,newdata=test.set)
table(pred,test.set$RESPONSE, dnn=c("Predicted","Actual"))

####################################################################
