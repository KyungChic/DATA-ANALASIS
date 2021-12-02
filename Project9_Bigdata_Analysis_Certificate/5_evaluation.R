
library(ISLR)
hitters = na.omit(Hitters)

full_md = lm(Salary ~ ., data = hitters)

summary(full_md)

adj_md = lm(Salary ~ AtBat + Hits + Walks + CWalks + Division + PutOuts, data = hitters)

step_md = step(adj_md, direction = "both")


fit_md = lm(Salary ~ AtBat + Hits + CWalks + Division + PutOuts, data = hitters)

library(car)
vif(fit_md)

second_md = lm(Salary ~ Hits + CWalks + Division + PutOuts, data = hitters)

vif(second_md)





library(ModelMetrics)

rmse(fit_md)
mse(fit_md)

rmse(second_md)
mse(second_md)


summary(fit_md)$r.squared
summary(fit_md)$adj.r.squared

summary(second_md)$r.squared
summary(second_md)$adj.r.squared






library(mlbench)
data(PimaIndiansDiabetes2)

Pima = PimaIndiansDiabetes2

head(Pima)
str(Pima)
summary(Pima)

pima = na.omit(Pima)

sum(is.na(pima))





train_idx = sample(c(1:nrow(pima)), nrow(pima)*0.8)
train = pima[train_idx, ]
test = pima[-train_idx, ]



library(randomForest)

md = randomForest(diabetes ~ ., data = train, ntree = 300)
md

pred = predict(md, newdata = test)
pred


library(caret)
caret::confusionMatrix(as.factor(pred), test$diabetes)


library(ModelMetrics)
auc(actual = test$diabetes, predicted = as.factor(pred))
help(auc)
















