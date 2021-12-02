
str(iris)
summary(iris)
head(iris)


sum(is.na(iris))


train_data = sample(1:150, 100)

library(e1071)
naive_model = naiveBayes(Species ~ ., data = iris, subset = train_data)
naive_model


pred = predict(naive_model, newdata = iris[-train_data,])

pred


library(caret)
caret::confusionMatrix(pred, reference = iris[-train_data, "Species"])



help(naiveBayes)
