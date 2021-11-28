

data(mtcars)

m1 = lm(hp~., data=mtcars)
m2 = step(m1, direction = "both")

formula(m2)







library(mlbench)
data(PimaIndiansDiabetes)
pima = PimaIndiansDiabetes
summary(pima$age)

library(dplyr)
pima = pima %>%
  mutate(age_gen = cut(pima$age, c(20,40,60,100),
                       right = FALSE, label = c("Young", "Middle", "Old")))

table(pima$age_gen)