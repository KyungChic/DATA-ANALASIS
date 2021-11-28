airquality



ds_NA = head(airquality, 5)

ds_NA

is.na(ds_NA)

complete.cases(ds_NA)

library(mlbench)
data(PimaIndiansDiabetes2)
Pima2 = PimaIndiansDiabetes2


str(Pima2)
summary(Pima2)


complete.cases(Pima2)

is.na(Pima2)


sum(is.na(Pima2))    #결측치의 수
sum(!complete.cases(Pima2))    #행의 개수

colSums(is.na(Pima2))

Pima2_col_del = Pima2

Pima2_col_del$triceps = NULL
Pima2_col_del$insulin = NULL

colSums(is.na(Pima2_col_del))
sum(!complete.cases(Pima2_col_del))
sum(is.na(Pima2_col_del))




library(dplyr)

colSums(is.na(Pima2_col_del))

tmp = Pima2_col_del[!is.na(Pima2_col_del),]
head(tmp)

colSums(is.na(tmp))
summary(tmp)

Pima3 = Pima2_col_del%>%filter(!is.na(glucose) & !is.na(mass))

colSums(is.na(Pima3))

dim(Pima3)

Pima4 = na.omit(Pima3)
colSums(is.na(Pima4))
dim(Pima4)

dim(Pima3[!is.na(Pima3)])







head(airquality)

ds_NA = head(airquality, 5)

head(ds_NA$Ozone)

ds_NA$Ozone = ifelse(
  is.na(ds_NA$Ozone),
  mean(ds_NA$Ozone, na.rm=TRUE),
  ds_NA$Ozone
)

table(is.na(ds_NA$Ozone))





ds_NA2 = head(airquality, 5)

ds_NA2[
  is.na(ds_NA2$Ozone), "Ozone"] = mean(ds_NA2$Ozone, na.rm = TRUE)

table(is.na(ds_NA2$Ozone))







summary(Pima3)

mean_press = mean(Pima3$pressure, na.rm=TRUE)
mean_press

std_before = sd(Pima3$pressure, na.rm=TRUE)
std_before

Pima3$pressure = ifelse(is.na(Pima3$pressure), mean_press, Pima3$pressure)

std_after = sd(Pima3$pressure)
std_after

std_diff = std_after - std_before
print(std_diff)






score = c(1,1,1,1,1,1,1,1,1,1,1,1000000000)

name = c('a','b','c','d','e','f','g','h','i','j','k','l')

df_score = data.frame(name, score)
df_score

esd(score)


esd = function(x){
  return(abs(x-mean(x))/sd(x) < 3)
}

library("dplyr")
df_score%>%filter(esd(score))






score = c(1,1,1,1,1,1,1,1,1,1,1,1000000000)

name = c('a','b','c','d','e','f','g','h','i','j','k','l')

df_score = data.frame(name, score)



min_score = median(df_score$score) - 2*IQR(df_score$score)
max_score = median(df_score$score) + 2*IQR(df_score$score)

library(dplyr)
df_score%>%filter(df_score$score >= min_score & df_score$score <= max_score)






Insurance = read.csv("C:\\Users\\Gargantua\\Desktop\\data_set\\Project9_Bigdata_Analysis_Certificate\\insurance.csv")

head(Insurance)
summary(Insurance)

esd = function(x){
  abs(x-mean(x))/sd(x) >= 1.5
}

library(dplyr)
Insurance%>%filter(esd(Insurance$charges))%>%summarise(sum=sum(charges))
sum(Insurance%>%filter(esd(Insurance$charges))%>%select(charges))






