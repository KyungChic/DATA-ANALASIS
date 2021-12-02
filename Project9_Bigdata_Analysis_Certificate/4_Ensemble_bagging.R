
library(mlbench)
data(PimaIndiansDiabetes2)

Pima = PimaIndiansDiabetes2

head(Pima)
str(Pima)
summary(Pima)




pima = na.omit(Pima)

sum(is.na(pima))



train_idx = sample(c(1:nrow(pima)), nrow(pima)*2/3)
train = pima[train_idx,]
test = pima[test_idx,]



library(ipred)
md.bagging = bagging(diabetes ~ ., data = train, nbagg = 25)
md.bagging
summary(md.bagging)



pred = predict(md.bagging, test)
caret::confusionMatrix(as.factor(pred), reference = test$diabetes, positive = "pos")
