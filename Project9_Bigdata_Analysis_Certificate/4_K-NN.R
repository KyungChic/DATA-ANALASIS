
data = iris[,c("Sepal.Length", "Sepal.Width", "Species")]

head(data)
str(data)
summary(data)


set.seed(1234)

idx = sample(x = c("train", "valid", "test"), size = nrow(data), replace=TRUE, prob=c(3,1,1))
table(idx)

train = data[idx=="train",]
valid = data[idx=="valid",]
test = data[idx=="test",]

train_x = train[,-3]
valid_x = valid[,-3]
test_x = valid[,-3]

train_y = train[,3]
valid_y = valid[,3]
test_y = valid[,3]




knn_1 = knn(train = train_x, test = valid_x, cl = train_y, k=1)
knn_2 = knn(train = train_x, test = valid_x, cl = train_y, k=2)




accuracy_k = NULL

for(i in c(1:nrow(train_x))){
  set.seed(1234)
  knn_k = knn(train = train_x, test = valid_x, cl= train_y, k = i)
  accuracy_k = c(accuracy_k, 
                 sum((knn_k==valid_y)) / length(valid_y))
}

accuracy_k

which(accuracy_k==max(accuracy_k))

valid_k = data.frame(k = c(1:nrow(train_x)),
                     accuracy = accuracy_k)

plot(formula = accuracy~k,
     data = valid_k,
     type = "o",
     pch = 20,
     main = "validation = optimal k")



min(valid_k[valid_k$accuracy %in% max(accuracy_k), "k"])

max(accuracy_k)





library(caret)

knn_13 = knn(train = train_x, test = test_x, cl = train_y, k=13)
caret::confusionMatrix(knn_13, reference = test_y)
