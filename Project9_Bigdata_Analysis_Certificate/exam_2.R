set.seed(2021)


data = read.csv("C:\\Users\\Gargantua\\Desktop\\data_set\\Project9_Bigdata_Analysis_Certificate\\WA_Fn-UseC_-Telco-Customer-Churn.csv")

head(data)
str(data)
summary(data)

data_1 = data

data_1$ gender = as.factor(data_1$ gender)
data_1$ SeniorCitizen = as.factor(data_1$ SeniorCitizen)
data_1$ Partner = as.factor(data_1$ Partner)
data_1$ Dependents = as.factor(data_1$ Dependents)
data_1$ PhoneService = as.factor(data_1$ PhoneService) 
data_1$MultipleLines = as.factor(data_1$MultipleLines)
data_1$ InternetService = as.factor(data_1$ InternetService)
data_1$ OnlineSecurity = as.factor(data_1$ OnlineSecurity)
data_1$ OnlineBackup = as.factor(data_1$ OnlineBackup)
data_1$ DeviceProtection = as.factor(data_1$ DeviceProtection)
data_1$ TechSupport = as.factor(data_1$ TechSupport) 
data_1$ StreamingTV = as.factor(data_1$ StreamingTV) 
data_1$ StreamingMovies = as.factor(data_1$ StreamingMovies)
data_1$Contract = as.factor(data_1$Contract)
data_1$ PaperlessBilling = as.factor(data_1$ PaperlessBilling)
data_1$ PaymentMethod = as.factor(data_1$ PaymentMethod)
data_1$ Churn = as.factor(data_1$ Churn)

summary(data_1)

med_before_TotalCharges = median(data_1$TotalCharges, na.rm = T)

data_1$TotalCharges = ifelse(is.na(data_1$TotalCharges),med_before_TotalCharges , data_1$TotalCharges)

sum(is.na(data_1))


library(caret)
train_idx = createDataPartition(y = data_1$Churn, p = 0.7, list =FALSE)

library(dplyr)
train = data_1[train_idx,]
test = data_1[-train_idx,]

str(data_1)




#로지스틱

full_md = glm(Churn ~ .-customerID, data = train, family = 'binomial')
summary(full_md)

second_md = glm(Churn ~ tenure + Contract + PaperlessBilling + PaymentMethod + TotalCharges, data = train, family = 'binomial')
summary(second_md)


step_md = step(second_md, method = "both")
summary(step_md)


pchisq((5707.1-4270.9), df = 8, lower.tail = FALSE)


pred = predict(step_md, newdata = test)
head(pred)

pred = ifelse(pred>=0.5, "Yes", "No")

library(caret)
caret::confusionMatrix(as.factor(pred), reference = test$Churn, positive = "Yes")

library(ModelMetrics)
auc(test$Churn, predicted = as.factor(pred))



#랜덤포레스트

library(randomForest)

help(randomForest)


mo_rf = randomForest(Churn ~ tenure + Contract + PaperlessBilling + PaymentMethod + TotalCharges, data = train, ntree = 250, do.trace=T)
summary(mo_rf)


pred_rf = predict(mo_rf, newdata = test)

caret::confusionMatrix(pred_rf, reference = test$Churn)
auc(test$Churn, predicted = pred_rf)

pred_rf



#svm

library(e1071)

md_svm = svm(Churn ~ tenure + Contract + PaperlessBilling + PaymentMethod + TotalCharges, data = train, )

pred_svm = predict(md_svm, newdata=test)

caret::confusionMatrix(pred_svm, test$Churn)
auc(test$Churn, predicted = pred_svm)
