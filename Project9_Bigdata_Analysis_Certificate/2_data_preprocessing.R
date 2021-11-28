library("plyr")

ddply(iris, .(Species, Petal.Length<1.5), function(sub){
  data.frame(
    mean_to = mean(sub$Sepal.Length), mean_so=mean(sub$Sepal.Width),
    mean_wo = mean(sub$Petal.Length), mean_jo=mean(sub$Petal.Width)
  )
})


ddply(iris, .(Species), summarise, mean_to = mean(Sepal.Length))
  #Species를 기준으로 mean_to에 저장된 Sepal.Length 값의 평균을 요약(summarise)함


transform(iris, Total.w = Sepal.Width+Petal.Width)


library("dplyr")

iris %>% select(Sepal.Length)

iris %>% select(Sepal.Length : Sepal.Width)

iris %>% select(-Sepal.Length, -Petal.Width)




iris %>% filter(Species == "setosa") %>% select(Sepal.Width, Sepal.Length)




iris %>%
  filter(Species == 'virginica') %>%
  mutate(Len = ifelse(Sepal.Length>6, 'L', 'S')) %>%
  select(Species, Len)




iris %>%
  group_by(Species) %>%
  summarise(Petal.Width = mean(Petal.Width))




iris %>%
  filter(Species == 'setosa') %>%
  mutate(Len = ifelse(Sepal.Length > 5, 'L', 'S'))%>%
  select(Species, Len, Sepal.Length) %>%
  arrange(desc(Sepal.Length))





X = data.frame(Department = c(11, 12, 13, 14),
               DepartmentName = c("Production", "Sales", "Marketing", "Research"),
               Manager = c(1, 4, 5, NA))
X

Y = data.frame(Employee = c(1,2,3,4,5,6),
               EmployeeName = c("a","b","c","d","e","f"),
               Department = c(11,11,12,12,13,21),
               Salary = c(80,60,90,100,80,70))
Y

inner_join(X, Y, by = "Department")

left_join(X, Y, by = "Department")

right_join(X, Y, by = "Department")

full_join(X, Y, by = "Department")






x = data.frame(x=1:3, y=1:3)
x

y = data.frame(x=4:6, z=4:6)
y

bind_rows(x,y)


x = data.frame(title=c(1:5),
               a=c(30,70,45,90,65))
x

y = data.frame(b=c(70,65,80,80,90))
y

bind_cols(x,y)






library("reshape2")
library("MASS")

a = melt(data = Cars93, 
         id.vars = c("Type", "Origin"),
         measure.vars = c("MPG.city", "MPG.highway"))
a


dcast(data = a, Type + Origin ~ variable, fun = mean)





library(data.table)

t = data.table(x = c(1:3),
               y = c('가', '나', '다'))

t
str(t)


iris_table = as.data.table(iris)
iris_table



iris_table[1,]

iris_table[c(1:2),]

iris_table[,mean(Petal.Width), by=Species]

iris_table[Petal.Width>1.5,]


help(data.table)


BostonHousing

library("mlbench")
data(BostonHousing)
data = BostonHousing

head(data)

top10 = head(sort(data$crim, decreasing = TRUE), 10)

tenth = top10[10]

data$crim = ifelse(data$crim >= tenth, tenth, data$crim)

library("dplyr")

over80 = data%>%filter(age>=80)
summary(over80$age)

mean = mean(over80$crim)
print(mean)
