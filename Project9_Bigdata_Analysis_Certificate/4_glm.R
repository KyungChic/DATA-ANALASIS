library(ISLR)

head(Default)
summary(Default)
str(Default)

sum(is.na(Default))

default = Default

set.seed(202012)

train_idx = sample(1:nrow(default), size=0.8*nrow(default), replace=F)
test_idx = -train_idx

default_train = default[train_idx, ]
default_test = default[test_idx, ]

full_model = glm(default ~ ., family = "binomial", data = default_train)
summary(full_model)

step_model = step(full_model, direction = "both")
summary(step_model)


null_deviance = 2345.0
residual_deviance = 1287.4
model_deviance = null_deviance - residual_deviance
pchisq(model_deviance, df = 2, lower.tail = FALSE)


library(car)
vif(step_model)


pred = predict(step_model, newdata = default_test[,-1], type="response")
df_pred = as.data.frame(pred)

str(pred)
str(df_pred)

summary(pred)
summary(df_pred)


df_pred$default = ifelse(df_pred$pred >= 0.5, "Yes", "No")
summary(df_pred)

df_pred$default = as.factor(df_pred$default)
summary(df_pred)



library(caret)
confusionMatrix(data = df_pred$default, reference = default_test[,1])






install.packages("ModelMetrics")
library(ModelMetrics)

auc(actual = default_test[,1], predicted = df_pred$default)


as.data.frame(installed.packages())

help(pchisq)
