<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
##############################
##  http://www.di.fc.ul.pt/~jpn/r/svm/svm.html   ##
#####

## Consider a binary classification, where input vectors x i  
# xi(the input space) and labels (aka, targets, classes) y i =¡¾1 
# yi=¡¾1

x1s <- c(.5,1,1,2,3,3.5,     1,3.5,4,5,5.5,6)
x2s <- c(3.5,1,2.5,2,1,1.2,  5.8,3,4,5,4,1)
ys <- c(rep(+1,6),          rep(-1,6))
my.data <- data.frame(x1=x1s, x2=x2s, type=as.factor(ys))
my.data

win.graph()
plot(my.data[,-3],col=(ys+3)/2, pch=19); abline(h=0,v=0,lty=3)

install.packages("e1071")
library(e1071)

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear',scale=FALSE)

#gamma: parameter needed for all kernels except linear
# (default: 1/(data dimension))

# type: Depending of whether y is a factor or not, 
# the default setting for type is C-classification 
# or eps-regression, respectively.
# C-classifcation:0<= alpha_i <= C

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) 
# show the support vectors

##
svm.model$ SV # matrix of support vectors found
svm.model$index # index of the support vectors in the input data
svm.model$coefs  #The corresponding coefficients times the training labels
svm.model$rho #The negative intercept

# Unfortunately, svmfit doesn't store the equation of boundary 
# plane (or just, normal vector of it), so we must evaluate it. 
# We can evaluate such weights with following formula:

# get parameters of hiperplane
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho 
# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)


## Prediction

observations <- data.frame(x1=c(1,3.5),x2=c(4,3.5))

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)
points(observations[1,], col="green", pch=19)
points(observations[2,], col="blue", pch=19)
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

predict(svm.model, observations) # the results are right


############################################################
## Another example : iris data
##

data(iris)
svm.model <- svm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = "linear")
# the + are support vectors
plot(iris$Sepal.Length, iris$Sepal.Width, col = as.integer(iris[, 5]), 
     pch = c("o","+")[1:150 %in% svm.model$index + 1], cex = 2, 
     xlab = "Sepal length", ylab = "Sepal width")

plot(svm.model, iris, Sepal.Width ~ Sepal.Length, 
     slice = list(sepal.width = 1, sepal.length = 2))

svm.pred  <- predict(svm.model, iris[,-5]) 
table(pred = svm.pred, true = iris[,5]) # show the confusion matrix

#######################################################

##### Non linearly separable data  ################
####  Example: Polynomial kernel (0.1*x1*x2 +1)^8

# polynomial kernel :(gamma*u'*v + coef0)^degree
# degree: parameter needed for kernel of type polynomial (default: 3)

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='polynomial', degree=8, gamma=0.1, coef0=1, scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) # show the support vectors

svm.pred <- predict(svm.model, my.data[,-3])
table(pred = svm.pred, true = my.data[,3]) 
################################################

### Using the same kernel with the iris dataset:
svm.model <- svm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = 'polynomial', degree=8, gamma=0.1, coef0=1)
plot(svm.model, iris, Sepal.Width ~ Sepal.Length, 
     slice = list(Sepal.Width = 1, Sepal.Length = 2))


svm.pred  <- predict(svm.model, iris[,-5]) 
table(pred = svm.pred, true = iris[,5]) # show the confusion matrix

# not very great, but we had just used two attributes. If we use all four:
svm.model <- svm(Species ~ ., data = iris, kernel = 'polynomial', degree=8, gamma=0.1, coef0=1)
plot(svm.model, iris, Sepal.Width ~ Sepal.Length, 
     slice = list(Petal.Width = 3, Petal.Length = 2.5)) # showing a 2D slice of the 4D space

svm.pred  <- predict(svm.model, iris[,-5]) 
table(pred = svm.pred, true = iris[,5]) # show the confusion matrix

##############################

##### Soft Margins ###########

# C controls the trade-of between the slack variable penalty 
# and the size of the margin. 
# A value C much larger than 0 converges to the original SVM algorithm. 
# Unfortunately the optimal value of C must come from trial-and-error.

### Example 

set.seed(101)
x1s <- c( rnorm(20,1,0.1), 2.5, rnorm(20,3,0.1))
x2s <- c( rnorm(20,1,0.1), 2.5, rnorm(20,3,0.1))
ys  <- c( rep(-1,21), rep(1,20))
my.data <- data.frame(x1=x1s, x2=x2s, type=as.factor(ys))
plot(my.data[,-3],col=(ys+3)/2, pch=19)

################
# First with a hard-core margin (use option "cost"):

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear', cost=1e10, scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) # show the support vectors

# get parameters of hiperplane
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

###########
# Now a soft-margin:

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear', cost=1, scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) # show the support vectors

# get parameters of hiperplane
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

=======
##############################
##  http://www.di.fc.ul.pt/~jpn/r/svm/svm.html   ##
#####

## Consider a binary classification, where input vectors x i  
# xi(the input space) and labels (aka, targets, classes) y i =¡¾1 
# yi=¡¾1

x1s <- c(.5,1,1,2,3,3.5,     1,3.5,4,5,5.5,6)
x2s <- c(3.5,1,2.5,2,1,1.2,  5.8,3,4,5,4,1)
ys <- c(rep(+1,6),          rep(-1,6))
my.data <- data.frame(x1=x1s, x2=x2s, type=as.factor(ys))
my.data

win.graph()
plot(my.data[,-3],col=(ys+3)/2, pch=19); abline(h=0,v=0,lty=3)

install.packages("e1071")
library(e1071)

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear',scale=FALSE)

#gamma: parameter needed for all kernels except linear
# (default: 1/(data dimension))

# type: Depending of whether y is a factor or not, 
# the default setting for type is C-classification 
# or eps-regression, respectively.
# C-classifcation:0<= alpha_i <= C

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) 
# show the support vectors

##
svm.model$ SV # matrix of support vectors found
svm.model$index # index of the support vectors in the input data
svm.model$coefs  #The corresponding coefficients times the training labels
svm.model$rho #The negative intercept

# Unfortunately, svmfit doesn't store the equation of boundary 
# plane (or just, normal vector of it), so we must evaluate it. 
# We can evaluate such weights with following formula:

# get parameters of hiperplane
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho 
# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)


## Prediction

observations <- data.frame(x1=c(1,3.5),x2=c(4,3.5))

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)
points(observations[1,], col="green", pch=19)
points(observations[2,], col="blue", pch=19)
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

predict(svm.model, observations) # the results are right


############################################################
## Another example : iris data
##

data(iris)
svm.model <- svm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = "linear")
# the + are support vectors
plot(iris$Sepal.Length, iris$Sepal.Width, col = as.integer(iris[, 5]), 
     pch = c("o","+")[1:150 %in% svm.model$index + 1], cex = 2, 
     xlab = "Sepal length", ylab = "Sepal width")

plot(svm.model, iris, Sepal.Width ~ Sepal.Length, 
     slice = list(sepal.width = 1, sepal.length = 2))

svm.pred  <- predict(svm.model, iris[,-5]) 
table(pred = svm.pred, true = iris[,5]) # show the confusion matrix

#######################################################

##### Non linearly separable data  ################
####  Example: Polynomial kernel (0.1*x1*x2 +1)^8

# polynomial kernel :(gamma*u'*v + coef0)^degree
# degree: parameter needed for kernel of type polynomial (default: 3)

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='polynomial', degree=8, gamma=0.1, coef0=1, scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) # show the support vectors

svm.pred <- predict(svm.model, my.data[,-3])
table(pred = svm.pred, true = my.data[,3]) 
################################################

### Using the same kernel with the iris dataset:
svm.model <- svm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = 'polynomial', degree=8, gamma=0.1, coef0=1)
plot(svm.model, iris, Sepal.Width ~ Sepal.Length, 
     slice = list(Sepal.Width = 1, Sepal.Length = 2))


svm.pred  <- predict(svm.model, iris[,-5]) 
table(pred = svm.pred, true = iris[,5]) # show the confusion matrix

# not very great, but we had just used two attributes. If we use all four:
svm.model <- svm(Species ~ ., data = iris, kernel = 'polynomial', degree=8, gamma=0.1, coef0=1)
plot(svm.model, iris, Sepal.Width ~ Sepal.Length, 
     slice = list(Petal.Width = 3, Petal.Length = 2.5)) # showing a 2D slice of the 4D space

svm.pred  <- predict(svm.model, iris[,-5]) 
table(pred = svm.pred, true = iris[,5]) # show the confusion matrix

##############################

##### Soft Margins ###########

# C controls the trade-of between the slack variable penalty 
# and the size of the margin. 
# A value C much larger than 0 converges to the original SVM algorithm. 
# Unfortunately the optimal value of C must come from trial-and-error.

### Example 

set.seed(101)
x1s <- c( rnorm(20,1,0.1), 2.5, rnorm(20,3,0.1))
x2s <- c( rnorm(20,1,0.1), 2.5, rnorm(20,3,0.1))
ys  <- c( rep(-1,21), rep(1,20))
my.data <- data.frame(x1=x1s, x2=x2s, type=as.factor(ys))
plot(my.data[,-3],col=(ys+3)/2, pch=19)

################
# First with a hard-core margin (use option "cost"):

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear', cost=1e10, scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) # show the support vectors

# get parameters of hiperplane
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

###########
# Now a soft-margin:

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear', cost=1, scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) # show the support vectors

# get parameters of hiperplane
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======
##############################
##  http://www.di.fc.ul.pt/~jpn/r/svm/svm.html   ##
#####

## Consider a binary classification, where input vectors x i  
# xi(the input space) and labels (aka, targets, classes) y i =¡¾1 
# yi=¡¾1

x1s <- c(.5,1,1,2,3,3.5,     1,3.5,4,5,5.5,6)
x2s <- c(3.5,1,2.5,2,1,1.2,  5.8,3,4,5,4,1)
ys <- c(rep(+1,6),          rep(-1,6))
my.data <- data.frame(x1=x1s, x2=x2s, type=as.factor(ys))
my.data

win.graph()
plot(my.data[,-3],col=(ys+3)/2, pch=19); abline(h=0,v=0,lty=3)

install.packages("e1071")
library(e1071)

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear',scale=FALSE)

#gamma: parameter needed for all kernels except linear
# (default: 1/(data dimension))

# type: Depending of whether y is a factor or not, 
# the default setting for type is C-classification 
# or eps-regression, respectively.
# C-classifcation:0<= alpha_i <= C

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) 
# show the support vectors

##
svm.model$ SV # matrix of support vectors found
svm.model$index # index of the support vectors in the input data
svm.model$coefs  #The corresponding coefficients times the training labels
svm.model$rho #The negative intercept

# Unfortunately, svmfit doesn't store the equation of boundary 
# plane (or just, normal vector of it), so we must evaluate it. 
# We can evaluate such weights with following formula:

# get parameters of hiperplane
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho 
# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)


## Prediction

observations <- data.frame(x1=c(1,3.5),x2=c(4,3.5))

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)
points(observations[1,], col="green", pch=19)
points(observations[2,], col="blue", pch=19)
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

predict(svm.model, observations) # the results are right


############################################################
## Another example : iris data
##

data(iris)
svm.model <- svm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = "linear")
# the + are support vectors
plot(iris$Sepal.Length, iris$Sepal.Width, col = as.integer(iris[, 5]), 
     pch = c("o","+")[1:150 %in% svm.model$index + 1], cex = 2, 
     xlab = "Sepal length", ylab = "Sepal width")

plot(svm.model, iris, Sepal.Width ~ Sepal.Length, 
     slice = list(sepal.width = 1, sepal.length = 2))

svm.pred  <- predict(svm.model, iris[,-5]) 
table(pred = svm.pred, true = iris[,5]) # show the confusion matrix

#######################################################

##### Non linearly separable data  ################
####  Example: Polynomial kernel (0.1*x1*x2 +1)^8

# polynomial kernel :(gamma*u'*v + coef0)^degree
# degree: parameter needed for kernel of type polynomial (default: 3)

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='polynomial', degree=8, gamma=0.1, coef0=1, scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) # show the support vectors

svm.pred <- predict(svm.model, my.data[,-3])
table(pred = svm.pred, true = my.data[,3]) 
################################################

### Using the same kernel with the iris dataset:
svm.model <- svm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = 'polynomial', degree=8, gamma=0.1, coef0=1)
plot(svm.model, iris, Sepal.Width ~ Sepal.Length, 
     slice = list(Sepal.Width = 1, Sepal.Length = 2))


svm.pred  <- predict(svm.model, iris[,-5]) 
table(pred = svm.pred, true = iris[,5]) # show the confusion matrix

# not very great, but we had just used two attributes. If we use all four:
svm.model <- svm(Species ~ ., data = iris, kernel = 'polynomial', degree=8, gamma=0.1, coef0=1)
plot(svm.model, iris, Sepal.Width ~ Sepal.Length, 
     slice = list(Petal.Width = 3, Petal.Length = 2.5)) # showing a 2D slice of the 4D space

svm.pred  <- predict(svm.model, iris[,-5]) 
table(pred = svm.pred, true = iris[,5]) # show the confusion matrix

##############################

##### Soft Margins ###########

# C controls the trade-of between the slack variable penalty 
# and the size of the margin. 
# A value C much larger than 0 converges to the original SVM algorithm. 
# Unfortunately the optimal value of C must come from trial-and-error.

### Example 

set.seed(101)
x1s <- c( rnorm(20,1,0.1), 2.5, rnorm(20,3,0.1))
x2s <- c( rnorm(20,1,0.1), 2.5, rnorm(20,3,0.1))
ys  <- c( rep(-1,21), rep(1,20))
my.data <- data.frame(x1=x1s, x2=x2s, type=as.factor(ys))
plot(my.data[,-3],col=(ys+3)/2, pch=19)

################
# First with a hard-core margin (use option "cost"):

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear', cost=1e10, scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) # show the support vectors

# get parameters of hiperplane
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

###########
# Now a soft-margin:

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear', cost=1, scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) # show the support vectors

# get parameters of hiperplane
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======
##############################
##  http://www.di.fc.ul.pt/~jpn/r/svm/svm.html   ##
#####

## Consider a binary classification, where input vectors x i  
# xi(the input space) and labels (aka, targets, classes) y i =¡¾1 
# yi=¡¾1

x1s <- c(.5,1,1,2,3,3.5,     1,3.5,4,5,5.5,6)
x2s <- c(3.5,1,2.5,2,1,1.2,  5.8,3,4,5,4,1)
ys <- c(rep(+1,6),          rep(-1,6))
my.data <- data.frame(x1=x1s, x2=x2s, type=as.factor(ys))
my.data

win.graph()
plot(my.data[,-3],col=(ys+3)/2, pch=19); abline(h=0,v=0,lty=3)

install.packages("e1071")
library(e1071)

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear',scale=FALSE)

#gamma: parameter needed for all kernels except linear
# (default: 1/(data dimension))

# type: Depending of whether y is a factor or not, 
# the default setting for type is C-classification 
# or eps-regression, respectively.
# C-classifcation:0<= alpha_i <= C

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) 
# show the support vectors

##
svm.model$ SV # matrix of support vectors found
svm.model$index # index of the support vectors in the input data
svm.model$coefs  #The corresponding coefficients times the training labels
svm.model$rho #The negative intercept

# Unfortunately, svmfit doesn't store the equation of boundary 
# plane (or just, normal vector of it), so we must evaluate it. 
# We can evaluate such weights with following formula:

# get parameters of hiperplane
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho 
# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)


## Prediction

observations <- data.frame(x1=c(1,3.5),x2=c(4,3.5))

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)
points(observations[1,], col="green", pch=19)
points(observations[2,], col="blue", pch=19)
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

predict(svm.model, observations) # the results are right


############################################################
## Another example : iris data
##

data(iris)
svm.model <- svm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = "linear")
# the + are support vectors
plot(iris$Sepal.Length, iris$Sepal.Width, col = as.integer(iris[, 5]), 
     pch = c("o","+")[1:150 %in% svm.model$index + 1], cex = 2, 
     xlab = "Sepal length", ylab = "Sepal width")

plot(svm.model, iris, Sepal.Width ~ Sepal.Length, 
     slice = list(sepal.width = 1, sepal.length = 2))

svm.pred  <- predict(svm.model, iris[,-5]) 
table(pred = svm.pred, true = iris[,5]) # show the confusion matrix

#######################################################

##### Non linearly separable data  ################
####  Example: Polynomial kernel (0.1*x1*x2 +1)^8

# polynomial kernel :(gamma*u'*v + coef0)^degree
# degree: parameter needed for kernel of type polynomial (default: 3)

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='polynomial', degree=8, gamma=0.1, coef0=1, scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) # show the support vectors

svm.pred <- predict(svm.model, my.data[,-3])
table(pred = svm.pred, true = my.data[,3]) 
################################################

### Using the same kernel with the iris dataset:
svm.model <- svm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = 'polynomial', degree=8, gamma=0.1, coef0=1)
plot(svm.model, iris, Sepal.Width ~ Sepal.Length, 
     slice = list(Sepal.Width = 1, Sepal.Length = 2))


svm.pred  <- predict(svm.model, iris[,-5]) 
table(pred = svm.pred, true = iris[,5]) # show the confusion matrix

# not very great, but we had just used two attributes. If we use all four:
svm.model <- svm(Species ~ ., data = iris, kernel = 'polynomial', degree=8, gamma=0.1, coef0=1)
plot(svm.model, iris, Sepal.Width ~ Sepal.Length, 
     slice = list(Petal.Width = 3, Petal.Length = 2.5)) # showing a 2D slice of the 4D space

svm.pred  <- predict(svm.model, iris[,-5]) 
table(pred = svm.pred, true = iris[,5]) # show the confusion matrix

##############################

##### Soft Margins ###########

# C controls the trade-of between the slack variable penalty 
# and the size of the margin. 
# A value C much larger than 0 converges to the original SVM algorithm. 
# Unfortunately the optimal value of C must come from trial-and-error.

### Example 

set.seed(101)
x1s <- c( rnorm(20,1,0.1), 2.5, rnorm(20,3,0.1))
x2s <- c( rnorm(20,1,0.1), 2.5, rnorm(20,3,0.1))
ys  <- c( rep(-1,21), rep(1,20))
my.data <- data.frame(x1=x1s, x2=x2s, type=as.factor(ys))
plot(my.data[,-3],col=(ys+3)/2, pch=19)

################
# First with a hard-core margin (use option "cost"):

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear', cost=1e10, scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) # show the support vectors

# get parameters of hiperplane
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

###########
# Now a soft-margin:

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear', cost=1, scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) # show the support vectors

# get parameters of hiperplane
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======
##############################
##  http://www.di.fc.ul.pt/~jpn/r/svm/svm.html   ##
#####

## Consider a binary classification, where input vectors x i  
# xi(the input space) and labels (aka, targets, classes) y i =¡¾1 
# yi=¡¾1

x1s <- c(.5,1,1,2,3,3.5,     1,3.5,4,5,5.5,6)
x2s <- c(3.5,1,2.5,2,1,1.2,  5.8,3,4,5,4,1)
ys <- c(rep(+1,6),          rep(-1,6))
my.data <- data.frame(x1=x1s, x2=x2s, type=as.factor(ys))
my.data

win.graph()
plot(my.data[,-3],col=(ys+3)/2, pch=19); abline(h=0,v=0,lty=3)

install.packages("e1071")
library(e1071)

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear',scale=FALSE)

#gamma: parameter needed for all kernels except linear
# (default: 1/(data dimension))

# type: Depending of whether y is a factor or not, 
# the default setting for type is C-classification 
# or eps-regression, respectively.
# C-classifcation:0<= alpha_i <= C

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) 
# show the support vectors

##
svm.model$ SV # matrix of support vectors found
svm.model$index # index of the support vectors in the input data
svm.model$coefs  #The corresponding coefficients times the training labels
svm.model$rho #The negative intercept

# Unfortunately, svmfit doesn't store the equation of boundary 
# plane (or just, normal vector of it), so we must evaluate it. 
# We can evaluate such weights with following formula:

# get parameters of hiperplane
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho 
# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)


## Prediction

observations <- data.frame(x1=c(1,3.5),x2=c(4,3.5))

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)
points(observations[1,], col="green", pch=19)
points(observations[2,], col="blue", pch=19)
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

predict(svm.model, observations) # the results are right


############################################################
## Another example : iris data
##

data(iris)
svm.model <- svm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = "linear")
# the + are support vectors
plot(iris$Sepal.Length, iris$Sepal.Width, col = as.integer(iris[, 5]), 
     pch = c("o","+")[1:150 %in% svm.model$index + 1], cex = 2, 
     xlab = "Sepal length", ylab = "Sepal width")

plot(svm.model, iris, Sepal.Width ~ Sepal.Length, 
     slice = list(sepal.width = 1, sepal.length = 2))

svm.pred  <- predict(svm.model, iris[,-5]) 
table(pred = svm.pred, true = iris[,5]) # show the confusion matrix

#######################################################

##### Non linearly separable data  ################
####  Example: Polynomial kernel (0.1*x1*x2 +1)^8

# polynomial kernel :(gamma*u'*v + coef0)^degree
# degree: parameter needed for kernel of type polynomial (default: 3)

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='polynomial', degree=8, gamma=0.1, coef0=1, scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,6), ylim=c(-1,6)); abline(h=0,v=0,lty=3)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) # show the support vectors

svm.pred <- predict(svm.model, my.data[,-3])
table(pred = svm.pred, true = my.data[,3]) 
################################################

### Using the same kernel with the iris dataset:
svm.model <- svm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = 'polynomial', degree=8, gamma=0.1, coef0=1)
plot(svm.model, iris, Sepal.Width ~ Sepal.Length, 
     slice = list(Sepal.Width = 1, Sepal.Length = 2))


svm.pred  <- predict(svm.model, iris[,-5]) 
table(pred = svm.pred, true = iris[,5]) # show the confusion matrix

# not very great, but we had just used two attributes. If we use all four:
svm.model <- svm(Species ~ ., data = iris, kernel = 'polynomial', degree=8, gamma=0.1, coef0=1)
plot(svm.model, iris, Sepal.Width ~ Sepal.Length, 
     slice = list(Petal.Width = 3, Petal.Length = 2.5)) # showing a 2D slice of the 4D space

svm.pred  <- predict(svm.model, iris[,-5]) 
table(pred = svm.pred, true = iris[,5]) # show the confusion matrix

##############################

##### Soft Margins ###########

# C controls the trade-of between the slack variable penalty 
# and the size of the margin. 
# A value C much larger than 0 converges to the original SVM algorithm. 
# Unfortunately the optimal value of C must come from trial-and-error.

### Example 

set.seed(101)
x1s <- c( rnorm(20,1,0.1), 2.5, rnorm(20,3,0.1))
x2s <- c( rnorm(20,1,0.1), 2.5, rnorm(20,3,0.1))
ys  <- c( rep(-1,21), rep(1,20))
my.data <- data.frame(x1=x1s, x2=x2s, type=as.factor(ys))
plot(my.data[,-3],col=(ys+3)/2, pch=19)

################
# First with a hard-core margin (use option "cost"):

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear', cost=1e10, scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) # show the support vectors

# get parameters of hiperplane
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

###########
# Now a soft-margin:

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear', cost=1, scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) # show the support vectors

# get parameters of hiperplane
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
########################