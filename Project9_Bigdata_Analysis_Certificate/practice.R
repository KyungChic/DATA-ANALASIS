

typeof(iris)

library(plyr)

adply(.data = iris, .margins = 1, .fun = function(row){row$Sepal.Length >= 5.0 & row$Species == "setosa"})

adply(iris, 1, function(row){row$Sepal.Length >= 5.0 & row$Species == "setosa"})





library(plyr)

ddply(.data = iris, .variables = .(Species, Petal.Length < 1.5), .fun = function(sub){
  data.frame(
    mean_to = mean(sub$Sepal.Length), mean_so = mean(sub$Sepal.Width),
    mean_wo = mean(sub$Petal.Length), mean_jo = mean(sub$Petal.Width))
})

ddply(iris, .(Species, Petal.Length < 1.5), function(sub){
  data.frame(
    mean_to = mean(sub$Sepal.Length), mean_so = mean(sub$Sepal.Width),
    mean_wo = mean(sub$Petal.Length), mean_jo = mean(sub$Petal.Width))
  })



help(transform)




transform(iris, total.w = Sepal.Width + Petal.Width)




library(dplyr)

iris %>% select(Species)


iris %>% filter(Species == "setosa") %>% select(Sepal.Width, Petal.Width)


iris %>%
  filter(Species == 'virginica') %>%
  mutate(Len = ifelse(Sepal.Length > 6, "L", "S")) %>%
  select(Species, Len)





library(dplyr)

iris %>%
  group_by(Species) %>%
  summarise(Petal.Width = mean(Petal.Width))



iris %>%
  filter(Species == "setosa") %>%
  mutate(Len = ifelse(Sepal.Length > 5, "L", "S")) %>%
  select(Species, Len, Sepal.Length) %>%
  arrange(Sepal.Length)

iris %>%
  filter(Species == "setosa") %>%
  mutate(Len = ifelse(Sepal.Length > 5, "L", "S")) %>%
  select(Species, Len, Sepal.Length) %>%
  arrange(desc(Sepal.Length))





help(inner_join)



x = data.frame(Department = c(11,12,13,14),
               DepartmentName = c('p', 's', 'm', 'r'),
               Manager = c(1,4,5,NA))

y = data.frame(Employee = c(1,2,3,4,5,6),
               EmployeeName = c('a','b','c','d','e','f'),
               Department = c(11,11,12,12,13,21),
               Salary = c(80,60,90,100,80,70))

x
y

library(dplyr)

inner_join(x, y, by = "Department")


left_join(x, y, by = "Department")

full_join(x, y, by = "Department")



bind_rows(x,y)

bind_cols(x,y)



library(MASS)
head(Cars93)


library(reshape2)

a = melt(data = Cars93,
     id.vars = c("Type", "Origin"),
     measure.vars = c("MPG.city", "MPG.highway" ))

colnames(a)
head(a)

dcast(data = a,
      formula = Type + Origin ~ variable,
      fun.aggregate = mean)





library(data.table)

dt = data.table(x = c(1:3),
                y = c('a','b','c'))

dt



dt_iris = as.data.table(iris)

dt_iris[1,]

dt_iris[c(1:2),]

head(dt_iris[Species=='setosa',])

dt_iris[,mean(Petal.Width), by = Species]





ds_NA = head(airquality, 5)
sum(is.na(ds_NA))
sum(!complete.cases(ds_NA))


colSums(is.na(ds_NA))





library(mlbench)
data(PimaIndiansDiabetes2)

Pima = PimaIndiansDiabetes2

pima1 = Pima

colSums(is.na(pima1))

pima1$triceps = NULL
pima1$insulin = NULL

colSums(is.na(pima1))





pima2 = Pima
pima2 = na.omit(pima2)
sum(is.na(pima2))

nrow(Pima)
nrow(pima2)



pima3 = Pima

colSums(is.na(pima3))

pima3$triceps = ifelse(is.na(pima3$triceps), mean(pima3$triceps, na.rm = T), pima3$triceps)

colSums(is.na(pima3))



pima3=Pima
str(pima3)

for (i in c(1:(ncol(pima3)-1))){
  for(j in c(1:nrow(pima3))){
    pima3[j,i] = ifelse(is.na(pima3[j,i]), mean(pima3[i], na.rm = T), pima3[j,i]) 
  }
}

sum(is.na(pima3))

pima3

warnings()

colnames(pima[-9])


mean_list = NULL
for(i in c(1:(ncol(pima3)-1))){
  mean_list = c(mean_list , mean(pima3[,i], na.rm=T))
}
mean_list



c(1:(ncol(pima3)-1))

mean(pima3[,2], na.rm=T)


mean(pima[,1])

for (i in colnames(pima[-9])){
  for(j in c(1:nrow(pima3))){
    pima3[j,i] = ifelse(is.na(pima3[j,i]), mean_list(i), pima3[j,i]) 
  }
}

pima3 = Pima
colSums(is.na(pima3))


pima3






score = as.integer(c(1,1,1,1,1,1,1,1,1,1, 1000000000))


name = c('a','b','c','d','e','f','g','h','i','j','k')

df_score = data.frame(score, name)
df_score

esd = function(x){
  return(abs((x-mean(x)) / sd(x)) < 3)
}

library(dplyr)
df_score %>% filter(esd(score))

help(IQR)



df_score = data.frame(score = c(65, 60, 70, 75, 200),
         name = c('a','b','c','d','e'))

df_score

IQR = IQR(df_score$score)
median = median(df_score$score)

df_score %>% filter(score >= (median-1.5*IQR) & score <= (median+1.5*IQR))



help(scale)

a = c(1,3,5,7,9)

scale(a)

center = min(a)
scale = max(a)-min(a)

scale(a, center, scale)


a1 = scale(a)

mean(a1)
sd(a1)















sample(x=1:10,
           size = 3)

sample(x=1:10,
       size = 5)

sample(x=1:5,
       size = 5,
       replace = T)

sample(x=1:5,
       size = 5,
       replace = T,
       prob=c(1,1,100,100,100))



ds = createDataPartition(y = iris$Species, times = 1, p = 0.8, list = FALSE)
ds

table(iris[ds, "Species"])

table(iris[-ds, "Species"])





library(caret)

ds_k_fold = createFolds(y = iris$Species,
                        k = 5,
                        list = TRUE,
                        returnTrain = TRUE)

ds_k_fold
length(ds_k_fold$Fold5)


help(createFolds)





x = c(1, 1, 5, 5, 9, 7)

row_number(x)
min_rank(x)
dense_rank(x)










mtcars


aggregate(formula = mpg ~ cyl, 
          data = mtcars,
          FUN = mean)



library(mlbench)
pima = PimaIndiansDiabetes2

library(dplyr)

pima = pima %>% mutate(age_gen = cut(pima$age, c(20,40,60,100),
                                     right = FALSE, label = c("YOUNG", "MIDDLE", "OLD")))
table(pima$age_gen)







help(princomp)

str(iris)

summary(princomp(iris[-5]))

iris_pca = princomp(iris[-5])
iris_pca
summary(iris_pca)

iris_pca$loadings
iris_pca$score




lm1 = lm(Salary ~ PutOuts, data = Hitters)

lm1


summary(lm1)







hitters = Hitters

head(hitters)
str(hitters)
summary(hitters)

sum(is.na(hitters))

hitters$Salary = ifelse(is.na(hitters$Salary), mean(hitters$Salary, na.rm = TRUE), hitters$Salary)

sum(is.na(hitters))



train_idx = sample(1:nrow(hitters), nrow(hitters)*0.8)
train = hitters[train_idx,]
test = hitters[-train_idx,]



lm2 = lm(Salary ~ . , data = train)
lm2

summary(lm2)

lm2_adj = lm(Salary ~ AtBat + Hits + Walks + CRuns + CWalks + Division + PutOuts, data = train)
lm2_adj
summary(lm2_adj)

lm2_fit = step(lm2_adj, method = "both")

library(car)
vif(lm2_fit)

lm2_fit_1 = lm(Salary ~ Hits + Walks + CRuns + CWalks + Division + PutOuts, data = train)
vif(lm2_fit_1)

lm2_fit_1_1 = lm(Salary ~ Hits + Walks  + CWalks + Division + PutOuts, data = train)
lm2_fit_1_2 = lm(Salary ~ Hits + Walks + CRuns + Division + PutOuts, data = train)

summary(lm2_fit_1_1)
summary(lm2_fit_1_2)


lm2_fit_1_1_1 = lm(Salary ~ Hits + CWalks + Division + PutOuts, data = train)
lm2_fit_1_2_1 = lm(Salary ~ Hits + CRuns + Division + PutOuts, data = train)

summary(lm2_fit_1_1_1)
summary(lm2_fit_1_2_1)

vif(lm2_fit_1_1_1)
vif(lm2_fit_1_2_1)


pred1 = predict(lm2_fit_1_1_1, newdata = test)
pred2 = predict(lm2_fit_1_2_1, newdata = test)


rmse(pred1, test$Salary)
rmse(pred2, test$Salary)

write.csv(pred2, file = "C:\\Users\\Gargantua\\Desktop\\32000816.csv")






library(ISLR)
data(Default)

head(Default)
str(Default)
summary(Default)


library(caret)

train_idx = createDataPartition(y = Default$student, times=1, p=0.8)
train = Default[train_idx$Resample1, ]
test = Default[-train_idx$Resample1, ]

table(train$student)
table(test$student)



full_model = glm(default ~ ., family = "binomial", data = train)
full_model
summary(full_model)

second_model = glm(default ~ student + balance, family = "binomial", data = train)
summary(second_model)

step_model = step(second_model, method = "both")
summary(step_model)



null_deviance = 2300.1
residual_deviance = 1235.2
df = 8000-7998

model_deviance = null_deviance - residual_deviance
pchisq(model_deviance, df = df, lower.tail=FALSE)



library(car)
vif(step_model)



pred = predict(step_model, newdata = test, type = "response")

df_pred = as.data.frame(pred)
head(df_pred)

df_pred$default = ifelse(df_pred>=0.5, "Yes", "No")

df_pred$default = as.factor(df_pred$default)



library(caret)
caret::confusionMatrix(df_pred$default, as.factor(test$default))

library(ModelMetrics)
auc(actual = test$default, predicted = df_pred$default)





str(iris)
head(iris)
summary(iris)

train_idx = sample(c(1:nrow(iris)), size = nrow(iris)*0.7)
train = iris[train_idx, ]
test = iris[-train_idx, ]


library(rpart)
tree = rpart(Species ~ ., data = train)
tree
summary(tree)

ls(tree)
tree$cptable

pred = predict(tree, newdata = test, type = "class")

library(caret)
caret::confusionMatrix(as.factor(pred), test$Species )

library(ModelMetrics)
auc(actual = test$Species, predicted = as.factor(pred))




head(iris)
str(iris)
summary(iris)

library(caret)
train_idx = createDataPartition(y=iris$Species, p=0.7, list=FALSE)
train = iris[train_idx,]
test = iris[-train_idx,]

table(train$Species)
table(test$Species)


library(e1071)
md = svm(Species ~ ., data = train)
md
summary(md)

pred = predict(md, newdata = test)
caret::confusionMatrix(as.factor(pred), test$Species)





data = iris

idx = sample(x=c("train", "valid", "test"), size = nrow(data), replace = TRUE, prob = c(3,1,1))

table(idx)

train = data[idx=="train", ]
valid = data[idx=='valid',]
test = data[idx=='test',]

train_x = train[,-5]
valid_x = valid[,-5]
test_x = test[,-5]

train_y = train[,5]
valid_y = valid[,5]
test_y = test[,5]

library(class)
knn_1 = knn(train = train_x,
            test = valid_x,
            cl = train_y,
            k = 1)
sum(knn_1 == valid_y) / length(valid_y)

knn_2 = knn(train = train_x,
            test = valid_x,
            cl = train_y,
            k = 2)
sum(knn_2 == valid_y) / length(valid_y)


accuracy_k = NULL
for(i in c(1:nrow(train))){
  knn = knn(train = train_x,
              test = valid_x,
              cl = train_y,
              k = i)
  accuracy = (sum(knn == valid_y) / length(valid_y))
  
  accuracy_k = c(accuracy_k, accuracy)
}

accuracy_k

valid_k = data.frame(k = c(1:nrow(train)),
                     accuracy_k = accuracy_k)

min(valid_k[valid_k$accuracy_k %in% max(accuracy_k), "k"])


library(dplyr)
min(valid_k %>% select(k) %>% filter(accuracy_k == max(accuracy_k)))


max(accuracy_k)


knn_9 = knn(train = train_x,
             test = test_x,
             cl = train_y,
             k = 9)

library(caret)
caret::confusionMatrix(as.factor(knn_9), test_y)




train_idx = sample(c(1:nrow(iris)), size = nrow(iris)*0.7)
train = iris[train_idx,]
test = iris[-train_idx,]

library(e1071)
md = naiveBayes(Species ~ ., data = iris, subset = train_idx)

md
summary(md)


pred = predict(md, newdata = test)


library(caret)
caret::confusionMatrix(data = as.factor(pred) , reference = test$Species)





library(mlbench)
data(PimaIndiansDiabetes2)

Pima = PimaIndiansDiabetes2

head(Pima)
str(Pima)
summary(Pima)

pima = na.omit(Pima)

sum(is.na(pima))



train_idx = sample(c(1:nrow(pima)), size = nrow(pima)*0.7)
train = pima[train_idx,]
test = pima[-train_idx,]


library(ipred)
md_bagging = bagging(diabetes ~ ., data = train, nbagg = 25)
md_bagging


pred_bagging = predict(md_bagging, newdata = test)


library(caret)
caret::confusionMatrix(pred_bagging, test$diabetes, positive = "pos")

library(ModelMetrics)
ModelMetrics::auc(actual = test$diabetes, predicted = pred_bagging)









head(Pima)
str(Pima)
summary(Pima)

train_idx = sample(c(1:nrow(pima)), size = nrow(pima)*0.7)
train = pima[train_idx,]
test = pima[-train_idx,]

pima = na.omit(Pima)




library(xgboost)

train.label = as.integer(train$diabetes) - 1

mat_train.data = as.matrix(train[,-9])
mat_test.data = as.matrix(test[,-9])

xgb.train = xgb.DMatrix(data = mat_train.data, label = train.label)
xgb.test = xgb.DMatrix(data = mat_test.data)

help(xgboost)

param_list = list(
  booster = "gbtree",
  eta = 0.001,
  gamma = 5,
  max_depth = 10,
  subsample = 0.7,
  objective = "binary:logistic",
  eval_metric = "auc")


md_xgb = xgb.train(data = xgb.train,
                   params = param_list,
                   nrounds = 200,
                   early_stopping_rounds = 10,
                   watchlist = list(val1 = xgb.train),
                   verbose = 1)

xgb.pred = predict(md_xgb, newdata = xgb.test)

xgb.pred2 = ifelse(xgb.pred >= 0.5, "pos", "neg")
xgb.pred2 = as.factor(xgb.pred2)



library(caret)
caret::confusionMatrix(xgb.pred2, as.factor(test$diabetes))




Hitters

hitters = Hitters

head(hitters)
str(hitters)
summary(hitters)

data = na.omit(hitters)

sum(is.na(data))


train_idx = sample(c(1:nrow(data)), size = nrow(data)*0.7)
train = data[train_idx,]
test = data[-train_idx,]


library(xgboost)

train$League = as.integer(train$League) -1
train$Division = as.integer(train$Division)-1
train$NewLeague = as.integer(train$NewLeague)-1

test$League = as.integer(test$League) -1
test$Division = as.integer(test$Division)-1
test$NewLeague = as.integer(test$NewLeague)-1


str(train)


mat_train = as.matrix(train[,-19])
mat_test = as.matrix(test[,-19])

xgb_train = xgb.DMatrix(mat_train)
xgb_test = xgb.DMatrix(mat_test)


pram_list = list(booster = "gblinear",
                 objective = "reg:linear",
                 eval_metric = "rmse")

xgb_lin_model = xgb.train(data = xgb_train,
                          nrounds = 5,
                          early_stopping_rounds = 10,
                          watchlist = list(val1 = xgb_train),
                          verbose=1)







train_idx = sample(c(1:nrow(pima)), size = nrow(pima)*0.7)
train = pima[train_idx,]
test = pima[-train_idx,]

pima = na.omit(Pima)


library(randomForest)

rf_model = randomForest(diabetes ~ ., data = train, ntrees=100, proximity = TRUE)
rf_model
summary(rf_model)

importance(rf_model)

pred_rf = predict(rf_model, newdata = test)

library(caret)
caret::confusionMatrix(pred_rf, test$diabetes)

library(ModelMetrics)
ModelMetrics::auc(test$diabetes, predicted = pred_rf)




help(kmeans)


library(rattle)
df = scale(wine[-1])
fit.km = kmeans(df, centers = 3, nstart = 25)
fit.km$size
fit.km$centers





mx.ex = matrix(c(1,1,1,1,10,
                 1,1,0,1,0,
                 1,0,0,1,0,
                 1,1,1,0,0,
                 1,1,1,0,0
                 ), ncol = 5, byrow=TRUE)

rownames(mx.ex) = c('p1','p2','p3','p4','p5')

colnames(mx.ex) = c('a','b','c','d','e')

mx.ex

library(arules)

trx.ex = as(mx.ex, "transactions")

summary(trx.ex)

inspect(trx.ex)




data("Groceries")

head(Groceries)
str(Groceries)
summary(Groceries)

library(arules)
apr = apriori(Groceries,
              parameter = list(support = 0.01, confidence = 0.3))

inspect(sort(apr, by="lift")[1:10])
