
str(iris)
head(iris)
summary(iris)


idx_train = sample(1:nrow(iris), 0.8*nrow(iris), replace=F)
idx_test = -idx_train


iris_train = iris[idx_train,]
iris_test = iris[idx_test,]


library(rpart)
md = rpart(Species ~ ., data=iris_train)
md


plot(md, compress=T, margin=0.5)
text(md, cex=1)

install.packages("rpart.plot")
library(rpart.plot)
prp(md, type=2, extra=2)



ls(md)
md$cptable
plotcp(md)


pred = predict(md, newdata = iris_test, type = "class")
str(pred)
summary(pred)

library(caret)
caret::confusionMatrix(pred, reference = iris_test$Species)
