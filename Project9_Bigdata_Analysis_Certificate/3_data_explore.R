
head(mtcars)

table(mtcars$cyl)



length(mtcars$cyl)



cnt = table(mtcars$cyl)
total = length(mtcars$cyl)

cnt / total

prop.table(cnt)


barplot(cnt)

pie(cnt)







summary(mtcars$wt)

head(mtcars$wt)
tail(mtcars$wt)

str(mtcars$wt)




hist(mtcars$wt, breaks=5)

boxplot(mtcars$wt)







table(mtcars$am, mtcars$cyl)

mtcars$label_am = factor(mtcars$am, labels=c("automatic", "manual"))

table(mtcars$label_am, mtcars$cyl)





prop_table = prop.table(table(mtcars$am, mtcars$cyl))*100
prop_table





addmargins(round(prop_table, digits = 1))





barplot(table(mtcars$am, mtcars$cyl))










cor_mpg_wt = cor(mtcars$mpg, mtcars$wt)
cor_mpg_wt


plot(mtcars$mpg, mtcars$wt)









aggregate(mpg ~ cyl, 
          data = mtcars, 
          FUN="mean")

aggregate(mpg ~ cyl, 
          data = mtcars, 
          FUN="median")


aggregate(mpg ~ cyl, 
          data = mtcars, 
          FUN="sd")




boxplot(mpg ~ cyl, data = mtcars, main="기통별 연비")










library(mlbench)
data(PimaIndiansDiabetes)

df_pima = PimaIndiansDiabetes[c(3:5, 8)]
df_pima
str(df_pima)
str(PimaIndiansDiabetes)

summary(df_pima)



cor(x = df_pima, method = "pearson")
cor(x = df_pima, method = "spearman")
cor(x = df_pima, method = "kendall")






windows(width=12, height=10)
pairs(df_pima)

library(corrplot)
corrplot(cor(df_pima), method = "circle", type="lower")







shapiro.test(df_pima$triceps)
shapiro.test(df_pima$insulin)



cor.test(df_pima$triceps, df_pima$insulin, method = "kendall")

cor.test(df_pima$triceps, df_pima$insulin, method = "spearman")










