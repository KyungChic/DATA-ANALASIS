
str(iris)
summary(iris)
head(iris)


train_idx = sample(1:nrow(iris), 0.8*nrow(iris), replace=T)
test_idx = -train_idx

train_iris = iris[train_idx,]
test_iris = iris[test_idx,]





library(e1071)
model = svm(Species ~ ., data = iris_train)
summary(model)



pred = predict(model, newdata= iris_test, type="response")
pred


library(caret)
caret::confusionMatrix(pred, reference = iris_test$Species)



plot(model, iris, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 5.8))
