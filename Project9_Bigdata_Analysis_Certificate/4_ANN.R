
data(iris)

iris.scaled = cbind(scale(iris[-5]), iris[5])


set.seed(1000)

index = c(sample(1:50, 35), sample(51:100, 35), sample(101:150, 35))

train = iris.scaled[index,]
test = iris.scaled[-index,]


library(nnet)
model.nnet = nnet(Species ~ ., data = train, size = 2, maxit = 200, decay = 5e-04)

summary(model.nnet)


