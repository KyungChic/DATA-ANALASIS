
install.packages("ISLR")
library(ISLR)

data(Hitters)

head(Hitters)

summary(lm(Salary~PutOuts, data=Hitters))




str(Hitters)
summary(Hitters)


hitters = na.omit(Hitters)
summary(hitters)

sum(is.na(hitters))


full_model = lm(Salary ~ . , data = hitters)
summary(full_model)



first_model = lm(Salary ~ AtBat + Hits + Walks + CWalks + Division + PutOuts, data = hitters)

fit_model = step(first_model, direction = "backward")

formula(fit_model)



library(car)
vif(fit_model)



second_model = lm(Salary ~ Hits + CWalks + Division + PutOuts, data = hitters)
vif(second_model)


summary(second_model)
