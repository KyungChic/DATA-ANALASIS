

a= 0:9
a

str(a)

a = as.character(a)
a
str(a)


a = as.numeric(a)
a
str(a)
typeof(a)

a = as.integer(a)
a
str(a)

a = as.double(a)
a
str(a)
typeof(a)

a= as.logical(a)
a
typeof(a)




a = 0:4
str(a)

a = as.data.frame(a)
a
str(a)





a = 0:9
a = as.list(a)
a





a = 0:4
a = as.matrix(a)
a




a = 0:9
a = as.vectoa(a)
a
str(a)



a = 0:9
a = as.factor(a)
a
str(a)







data = c(1, 3, 5, 7, 9)

data_minmax = scale(data, center = min(data), scale = (max(data)-min(data)))

data_minmax

mode(data)
mode(data_minmax)

class(data)
class(data_minmax)
l





minmax = function(x){
  return(scale(x, center = min(x), scale = max(x)-min(x)))
}

minmax2 = function(x){
  return((x-min(x))/(max(x)-min(x)))
}

a = 1:10

a_minmax = minmax(a)

a_minmax

minmax2(a)

a_minmax = as.vector(a_minmax)
a_minmax






data = c(1,3,5,7,9)

data_zscore = scale(data)

mean(data_zscore)
sd(data_zscore)

data_zscore






s = sample(x=1:10, size=5, replace=FALSE)
s




d = c(1:10)
s2 = sample(x=1:10, size=5, replace=TRUE)
s2



s3 = sample(x=1:10, size=5, replace=TRUE, prob=1:10)
s3






library(caret)

ds = createDataPartition(
  iris$Species, times=2, p=0.7
)
ds
typeof(ds)
ds = as.vector(ds)

table(iris[ds$Resample2, "Species"])
table(iris[-ds$Resample2, "Species"])

idx = as.vector(ds$Resample1)
ds_train=iris[idx,]
ds_test=iris[-idx,]







library(caret)

ds_kfold = createFolds(iris$Species,
                       k=3,
                       list=TRUE,
                       returnTrain=FALSE)

ds_kfold

table(iris[ds_kfold$Fold1,"Species"] )
table(iris[ds_kfold$Fold2,"Species"] )
table(iris[ds_kfold$Fold3,"Species"] )

table(iris[-ds_kfold$Fold1,"Species"] )
table(iris[-ds_kfold$Fold2,"Species"] )
table(iris[-ds_kfold$Fold3,"Species"] )

summary(ds_kfold)






x = c(0:50, 50)
x
mean(x)

mean(x, trim=0.1)


y = c(12, 7, 4, -5, NA)
y

mean(y)

mean(y, na.rm=TRUE)




library(mlbench)
data(cars)
head(cars)
summary(cars)

mean(cars$Price)







x = c(12, 7, 5, -21, 8, -5)
x

median(x)



y = c(12, 7, 4, -5, NA)
median(y)

median(y, na.rm=TRUE)




median(cars$Price)

cars%>%summarise(median01 = median(Price))







getmode = function(x){
  y = table(x)
  names(y)[which(y==max(y))]
}

x = c(2,1,1,3,1)

table(x)

max(table(x))

getmode(x)

names(table(x))

which(table(x)==3)








v = c(3,4,5,2,4,3,4)

var(v)
sd(v)

var(1:10)
sd(1:10)


v2 = c(1,7,3,5,11,4,6)
diff(range(v2))
diff(range(1:10))

range(v2)






v = c(1, 1, 5, 5, 9, 7)
v

library(dplyr)

row_number(v)

min_rank(v)

dense_rank(v)




library(dplyr)

cars %>%
  arrange(dist) %>%
  mutate(rank=row_number(dist))

cars %>%
  arrange(dist) %>%
  mutate(rank=min_rank(dist))

cars %>%
  arrange(dist) %>%
  mutate(rank=dense_rank(dist))







housing = read.csv("C:\\Users\\Gargantua\\Desktop\\data_set\\Project9_Bigdata_Analysis_Certificate\\housing.csv")

head(housing)
str(housing)
summary(housing)

#순서대로 80%값 추출

help(data.frame)


num = length(row.names(housing))*0.8

num

idx = (1:num)

idx

sample_housing = housing[idx,]
length(row.names(sample_housing))

head(housing)
head(sample_housing)


#

median = median(sample_housing$total_bedrooms, na.rm=TRUE)

sd_before = sd(sample_housing$total_bedrooms, na.rm=TRUE)

sample_housing$total_bedrooms =
  ifelse(is.na(sample_housing$total_bedrooms), median, sample_housing$total_bedrooms)

sd_after = sd(sample_housing$total_bedrooms)

print(abs(sd_before - sd_after))