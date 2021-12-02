

head(airquality)
str(airquality)
summary(airquality)

air = airquality

air_rm = air[!is.na(air$Solar.R),]

colSums(is.na(air_rm))


before_median = median(air_rm$Ozone, na.rm = T)
before_median

before_sd = sd(air_rm$Ozone, na.rm = T)
before_sd

air_rm$Ozone = ifelse(is.na(air_rm$Ozone), before_median, air_rm$Ozone)
colSums(is.na(air_rm))

after_sd = sd(air_rm$Ozone)

print(before_sd - after_sd)




library(dplyr)

library(ISLR)

hitters = Hitters

colSums(is.na(hitters))

med = median(hitters$Salary, na.rm=T)
med

iqr = IQR(hitters$Salary, na.rm=T)
iqr

result = sum(hitters %>% select(Salary) %>% filter(Salary > med + 2*iqr | Salary < med - 2*iqr))
print(result)









library(ggplot2)
data(diamonds)

head(diamonds)
str(diamonds)
summary(diamonds)


library(dplyr)
dia = diamonds %>% arrange(desc(price))
head(dia)

dia_df = as.data.frame(dia)

result = mean(dia_df[1:100, 'price'])
print(result)





library(ggplot2)
data("diamonds")

dia = diamonds

nrow(dia)

train = dia[c(1:nrow(dia)),]

srt = train %>% arrange(desc(price))

head(srt)

top_100 = srt[1:100,]

result = mean(top_100$price)
result





data(airquality)

air = airquality

train = air[c(1:nrow(air)*0.9), ]

before_med = median(train$Ozone, na.rm = T)
before_med

before_mean = mean(train$Ozone, na.rm = T)
before_mean


train$Ozone = ifelse(is.na(train$Ozone), before_mean, train$Ozone)
colSums(is.na(train))

after_med = median(train$Ozone)

result = after_med - before_med
print(result)




music = read.csv("C:\\Users\\Gargantua\\Desktop\\data_set\\Project9_Bigdata_Analysis_Certificate\\music_data.csv")

head(music)

n = nrow(music)
n



library(dplyr)

music_srt = music %>% arrange(tempo)

music_srt[c(1:(n*0.25+1)), "tempo"] = 0

music_srt = music_srt %>% arrange(desc(tempo))

View(music_srt)

music_srt[c(1:(n*(0.25)+1)), "tempo"] = 0

mean(music_srt$tempo) + sd(music_srt$tempo)




quantile(music$tempo)


music_mod = music
music_mod$tempo = ifelse(music_mod$tempo <= 99.38401 | music_mod$tempo >= 135.99918, 0, music_mod$tempo)

mean(music_mod$tempo) + sd(music_mod$tempo)





telco = read.csv("C:\\Users\\Gargantua\\Desktop\\data_set\\Project9_Bigdata_Analysis_Certificate\\telco-customer-chum.csv")

head(telco)
str(telco)
summary(telco)


mean_total = mean(telco$TotalCharges, na.rm = T)
mean_total
sd_total = sd(telco$TotalCharges, na.rm = T)
sd_total


telco_adj = telco %>% filter(TotalCharges < (mean_total + 1.5*sd_total) & TotalCharges > (mean_total - 1.5*sd_total))

mean(telco_adj$TotalCharges)



library(MASS)

data(cats)

head(cats)
str(cats)
summary(cats)



mean_hwt = mean(cats$Hwt)
sd_hwt = sd(cats$Hwt)

upper = mean_hwt + 1.5*sd_hwt
under = mean_hwt - 1.5*sd_hwt

cats_adj = cats %>% filter(Hwt > upper | Hwt < under)

mean(cats_adj$Hwt)



install.packages("faraway")
library(faraway)

data(orings)


head(orings)
str(orings)
summary(orings)

library(dplyr)
orings_damaged = orings %>% filter(damage >= 1)

cor(orings_damaged$temp, orings_damaged$damage)






head(mtcars)
str(mtcars)

library(dplyr)
manual = mtcars %>% filter(am == 1)
auto = mtcars %>% filter(am == 0)


manual_srt = manual %>% arrange(wt)
auto_srt = auto %>% arrange(wt)

1:10

down10_manual = mean(manual_srt[1:10, 'mpg'])
down10_auto = mean(auto_srt[1:10, 'mpg'])

print(abs(down10_manual - down10_auto))



str(diamonds)

library(ggplot2)
data(diamonds)

dia = diamonds
dia_rm = dia[-c(1:nrow(dia)*0.8),]

library(dplyr)
dia_rm_filter = dia_rm %>% filter(cut == 'Fair' & carat >= 1)

max(dia_rm_filter$price)



nrow(dia)
nrow(dia_rm)



library(ggplot2)
data(diamonds)

n = nrow(diamonds)*0.8

nrow(diamonds) * 0.8
n

dia_rm = dia[-c(1:n),]

nrow(dia_rm)

dia_rm_filter = dia_rm %>% filter(cut == 'Fair'& carat >= 1)

max(dia_rm_filter$price)



head(airquality)
str(airquality)

library(dplyr)
air_filter = airquality %>% filter(Month == 8 & Day == 21)
air_filter[,'Ozone']

colnames(airquality)
a = airquality[airquality$Month == 8 & airquality$Day == 21,] %>% 
  select(Ozone)




mean(iris$Sepal.Length) + mean(iris$Sepal.Width)


head(mtcars)


mtcarts_cyl4 = mtcars %>% filter(cyl ==4)

nrow(mtcarts_cyl4) / nrow(mtcars) * 100

a = table(mtcars$cyl) / length(mtcars$cyl)
a[1]





car_cyl4_man = mtcars %>% filter(gear ==4, am == 1)

mean(car_cyl4_man$mpg) + sd(car_cyl4_man$hp)




library(MASS)
a = Boston %>% filter(crim <= 1) %>% summarise(medv_mean = mean(medv))
a



der = iris %>% filter(Species == "virginica") %>% mutate(Len = ifelse(Sepal.Length > 6, 1 , 0))

sum(der$Len)




air_med = airquality
air_med

median_bef = mean(air_med$Ozone, na.rm=T)
air_med$Ozone = ifelse(is.na(air_med$Ozone), median_bef, air_med$Ozone)

iqr = IQR(air_med$Ozone)
median_af = median(air_med$Ozone)

upper = median_af + 2*iqr
under = median_af - 2*iqr


a = air_med %>% filter(Ozone < upper & Ozone > under) %>% summarise(Ozone_sum = sum(Ozone))
print(a)





data = read.csv("C:\\Users\\Gargantua\\Desktop\\data_set\\Project9_Bigdata_Analysis_Certificate\\marvel-wikia-data.csv")

head(data)
str(data)


train = data %>% filter(HAIR == "Brown Hair" & EYE == "Brown Eyes")

upper = mean(train$APPEARANCES, na.rm = T) + 1.5*sd(train$APPEARANCES,na.rm = T)
under = mean(train$APPEARANCES,na.rm = T) - 1.5*sd(train$APPEARANCES,na.rm = T)

mean = train %>% filter(APPEARANCES <= upper & APPEARANCES >= under) %>% summarise(mean = mean(APPEARANCES,na.rm = T))
mean






library(MASS)

data(ChickWeight)

df = ChickWeight

head(df)
str(df)
summary(df)


train = df %>% filter(Time == 10) %>% arrange(desc(weight))

head(train)


mean_before = mean(train$weight)

train[1:30,'weight'] = mean_before

mean_after = mean(train$weight)

abs(mean_before - mean_after)






fifa = read.csv("C:\\Users\\Gargantua\\Desktop\\data_set\\Project9_Bigdata_Analysis_Certificate\\fifa_ranking.csv")

head(fifa)

str(fifa)

summary(fifa)

filter = fifa %>% arrange(desc(total_points)) %>% dplyr::select(country_abrv)
head(filter,15)

filter_country = fifa %>% filter(country_abrv %in% c("GER", "BRA", "ESP")) %>% dplyr::select(total_points)

mean(filter_country$total_points)





a = fifa %>% dplyr::select(total_points) %>% arrange(desc(total_points))
a[3,]

b = fifa %>% filter(a >= 1765.05) %>% dplyr::select(country_abrv)
b






sales_train = read.csv("C:\\Users\\Gargantua\\Desktop\\data_set\\Project9_Bigdata_Analysis_Certificate\\sales_train_v2.csv")

head(sales_train)
str(sales_train)
summary(sales_train)


filter = sales_train %>% arrange(desc(item_cnt_day))
head(filter)

most = filter[1:3, 'item_id']
most

sd_3 = sales_train %>% filter(item_id %in% most) %>% summarise(sd = sd(item_price))
sd_3

sd(sales_train[, 'item_price']) - sd_3




sales_train = read.csv("C:\\Users\\Gargantua\\Desktop\\data_set\\Project9_Bigdata_Analysis_Certificate\\sales_train_v2.csv")


exam = sales_train %>% group_by(item_id) %>% summarise(n=n()) %>% arrange(desc(n))
exam

top_3 = exam[1:3, 'item_id']
top_3
top_3[,1]

sd = sales_train %>% filter(item_id %in% c(top_3[1,1],top_3[2,1],top_3[3,1])) %>% summarise(sd_3 = sd(item_price))
sd

sd_total = sd(sales_train$item_price)
sd_total

abs(sd-sd_total)
