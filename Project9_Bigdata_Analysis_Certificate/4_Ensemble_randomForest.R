rm(list = ls())




library(mlbench)

data(PimaIndiansDiabetes2)

Pima = PimaIndiansDiabetes2




sum(!complete.cases(Pima))

pima = na.omit(Pima)

sum(!complete.cases(pima))


str(pima)



test_idx = sample(c(1:nrow(pima)), size = nrow(pima)*2/3)
train = pima[test_idx, ]
test = pima[-test_idx, ]



library(randomForest)

md.rf = randomForest(diabetes ~ ., data = train, ntree = 100, proximity = TRUE)
print(md.rf)


pred = predict(md.rf, newdata = test)
pred

library(caret)
caret::confusionMatrix(pred, test$diabetes, positive = "pos")
