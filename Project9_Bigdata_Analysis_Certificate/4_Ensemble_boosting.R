
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



install.packages("xgboost")
library(xgboost)




train.label = as.integer(train$diabetes)-1

mat_train.data = as.matrix(train[,-9])
mat_test.data = as.matrix(test[,-9])





xgb.train = xgb.DMatrix(data = mat_train.data , label = train.label)
xgb.test = xgb.DMatrix(data = mat_test.data)

params = list(booster = "gbtree",
                  eta = 0.01,
                  gamma = 5,
                  max_depth = 10,
                  subsample = 0.8,
                  colsample_bytree = 0.8,
                  objective = "binary:logistic",
                  eval_metric = "auc")



md.xgb = xgb.train(params = params,
                   data = xgb.train,
                   nrounds = 100,
                   early_stopping_rounds = 10,
                   watchlist = list(val1 = xgb.train),
                   verbose = 1)





xgb.pred = predict(md.xgb, mat_test.data)
xgb.pred

xgb.pred2 = ifelse(xgb.pred>=0.5, 
                   xgb.pred <- "pos", 
                   xgb.pred <- "neg")
xgb.pred2


library(caret)
caret::confusionMatrix(as.factor(xgb.pred2), test$diabetes, positive="pos")
