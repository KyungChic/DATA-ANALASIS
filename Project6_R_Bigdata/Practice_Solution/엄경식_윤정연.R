<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
setwd("C:\\Users\\Gargantua\\Desktop\\data_analysis\\Project6_R_Bigdata")

library(MASS)
library(dplyr)


#1.

#(a)관측치 수 = 1309 / 변수의 수 = 15

titanic = read.csv('DataSet/titanic.csv', stringsAsFactors = T)

str(titanic)


#(b) 0.2552891

titanic %>% 
  filter(pclass == '3rd') %>% 
  summarise(mean = mean(survived))


#(c)
    # 1 1st            0.619
    # 2 2nd            0.430
    # 3 3rd            0.255

titanic %>% 
  group_by(pclass) %>% 
  summarise(mean_survived = mean(survived))


#(d) 0.7848948

titanic_not_na = titanic %>% filter(!is.na(age))

nrow(titanic_not_na %>% 
       filter(age>=20)) / nrow(titanic_not_na)



#(e)
      # sex    age_group pclass mean_survived
      # <fct>  <chr>     <fct>          <dbl>
      #   1 female Adult     1st           0.969 
      # 2 female Adult     2nd           0.874 
      # 3 female Adult     3rd           0.445 
      # 4 female Child     1st           0.833 
      # 5 female Child     2nd           1     
      # 6 female Child     3rd           0.548 
      # 7 male   Adult     1st           0.329 
      # 8 male   Adult     2nd           0.0833
      # 9 male   Adult     3rd           0.151 
      # 10 male   Child     1st           1     
      # 11 male   Child     2nd           0.786 
      # 12 male   Child     3rd           0.275 


titanic %>%
  filter(!is.na(age)) %>% 
  mutate(age_group = ifelse(age >= 17, "Adult", "Child")) %>% 
  group_by(sex, age_group, pclass) %>% 
  summarise(mean_survived = mean(survived))



#(f)

titanic %>% 
  mutate(family = sibsp + parch)



#(g)

    # Cherbourg  Queenstown Southampton 
    # 270         123         914 

table(titanic[,"embarked"])



#(h) 2개

nrow(titanic[is.na(titanic$embarked),])


#(i)

titanic[is.na(titanic$embarked),"embarked"] = "Southampton"

any(is.na(titanic$embarked))


#(j)

table(titanic$embarked, titanic$pclass)


#(k)

  ##대상 데이터 선정

data_test = titanic_not_na %>% 
  mutate(family = sibsp + parch)


  ##모델 구축
model_logistic = glm(survived ~ pclass + age + fare + sex + family + embarked, data = data_test)

summary(model_logistic)



#(l) 성별이 <남성>일 때 생존하지 못할 확률 대비 생존할 확률은 <-2.643246>이다

    ## 남성이면 생존 확률이 2.643246 감소한다.




#2

install.packages("gapminder")
library(gapminder)
data("gapminder")


#(a) 관측치 1704개 / 변수 6개

str(gapminder)


#(b)

gapminder$totalGDP = with(gapminder, pop + gdpPercap)


#(C)

gapminder %>% 
  group_by(continent) %>% 
  summarise(median_gdp = median(gdpPercap), mean_gdp = mean(gdpPercap), n=n())



#3  k=3 을 기준으로 잡아야 한다고 생각합니다.

=======
setwd("C:\\Users\\Gargantua\\Desktop\\data_analysis\\Project6_R_Bigdata")

library(MASS)
library(dplyr)


#1.

#(a)관측치 수 = 1309 / 변수의 수 = 15

titanic = read.csv('DataSet/titanic.csv', stringsAsFactors = T)

str(titanic)


#(b) 0.2552891

titanic %>% 
  filter(pclass == '3rd') %>% 
  summarise(mean = mean(survived))


#(c)
    # 1 1st            0.619
    # 2 2nd            0.430
    # 3 3rd            0.255

titanic %>% 
  group_by(pclass) %>% 
  summarise(mean_survived = mean(survived))


#(d) 0.7848948

titanic_not_na = titanic %>% filter(!is.na(age))

nrow(titanic_not_na %>% 
       filter(age>=20)) / nrow(titanic_not_na)



#(e)
      # sex    age_group pclass mean_survived
      # <fct>  <chr>     <fct>          <dbl>
      #   1 female Adult     1st           0.969 
      # 2 female Adult     2nd           0.874 
      # 3 female Adult     3rd           0.445 
      # 4 female Child     1st           0.833 
      # 5 female Child     2nd           1     
      # 6 female Child     3rd           0.548 
      # 7 male   Adult     1st           0.329 
      # 8 male   Adult     2nd           0.0833
      # 9 male   Adult     3rd           0.151 
      # 10 male   Child     1st           1     
      # 11 male   Child     2nd           0.786 
      # 12 male   Child     3rd           0.275 


titanic %>%
  filter(!is.na(age)) %>% 
  mutate(age_group = ifelse(age >= 17, "Adult", "Child")) %>% 
  group_by(sex, age_group, pclass) %>% 
  summarise(mean_survived = mean(survived))



#(f)

titanic %>% 
  mutate(family = sibsp + parch)



#(g)

    # Cherbourg  Queenstown Southampton 
    # 270         123         914 

table(titanic[,"embarked"])



#(h) 2개

nrow(titanic[is.na(titanic$embarked),])


#(i)

titanic[is.na(titanic$embarked),"embarked"] = "Southampton"

any(is.na(titanic$embarked))


#(j)

table(titanic$embarked, titanic$pclass)


#(k)

  ##대상 데이터 선정

data_test = titanic_not_na %>% 
  mutate(family = sibsp + parch)


  ##모델 구축
model_logistic = glm(survived ~ pclass + age + fare + sex + family + embarked, data = data_test)

summary(model_logistic)



#(l) 성별이 <남성>일 때 생존하지 못할 확률 대비 생존할 확률은 <-2.643246>이다

    ## 남성이면 생존 확률이 2.643246 감소한다.




#2

install.packages("gapminder")
library(gapminder)
data("gapminder")


#(a) 관측치 1704개 / 변수 6개

str(gapminder)


#(b)

gapminder$totalGDP = with(gapminder, pop + gdpPercap)


#(C)

gapminder %>% 
  group_by(continent) %>% 
  summarise(median_gdp = median(gdpPercap), mean_gdp = mean(gdpPercap), n=n())



#3  k=3 을 기준으로 잡아야 한다고 생각합니다.

>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======
setwd("C:\\Users\\Gargantua\\Desktop\\data_analysis\\Project6_R_Bigdata")

library(MASS)
library(dplyr)


#1.

#(a)관측치 수 = 1309 / 변수의 수 = 15

titanic = read.csv('DataSet/titanic.csv', stringsAsFactors = T)

str(titanic)


#(b) 0.2552891

titanic %>% 
  filter(pclass == '3rd') %>% 
  summarise(mean = mean(survived))


#(c)
    # 1 1st            0.619
    # 2 2nd            0.430
    # 3 3rd            0.255

titanic %>% 
  group_by(pclass) %>% 
  summarise(mean_survived = mean(survived))


#(d) 0.7848948

titanic_not_na = titanic %>% filter(!is.na(age))

nrow(titanic_not_na %>% 
       filter(age>=20)) / nrow(titanic_not_na)



#(e)
      # sex    age_group pclass mean_survived
      # <fct>  <chr>     <fct>          <dbl>
      #   1 female Adult     1st           0.969 
      # 2 female Adult     2nd           0.874 
      # 3 female Adult     3rd           0.445 
      # 4 female Child     1st           0.833 
      # 5 female Child     2nd           1     
      # 6 female Child     3rd           0.548 
      # 7 male   Adult     1st           0.329 
      # 8 male   Adult     2nd           0.0833
      # 9 male   Adult     3rd           0.151 
      # 10 male   Child     1st           1     
      # 11 male   Child     2nd           0.786 
      # 12 male   Child     3rd           0.275 


titanic %>%
  filter(!is.na(age)) %>% 
  mutate(age_group = ifelse(age >= 17, "Adult", "Child")) %>% 
  group_by(sex, age_group, pclass) %>% 
  summarise(mean_survived = mean(survived))



#(f)

titanic %>% 
  mutate(family = sibsp + parch)



#(g)

    # Cherbourg  Queenstown Southampton 
    # 270         123         914 

table(titanic[,"embarked"])



#(h) 2개

nrow(titanic[is.na(titanic$embarked),])


#(i)

titanic[is.na(titanic$embarked),"embarked"] = "Southampton"

any(is.na(titanic$embarked))


#(j)

table(titanic$embarked, titanic$pclass)


#(k)

  ##대상 데이터 선정

data_test = titanic_not_na %>% 
  mutate(family = sibsp + parch)


  ##모델 구축
model_logistic = glm(survived ~ pclass + age + fare + sex + family + embarked, data = data_test)

summary(model_logistic)



#(l) 성별이 <남성>일 때 생존하지 못할 확률 대비 생존할 확률은 <-2.643246>이다

    ## 남성이면 생존 확률이 2.643246 감소한다.




#2

install.packages("gapminder")
library(gapminder)
data("gapminder")


#(a) 관측치 1704개 / 변수 6개

str(gapminder)


#(b)

gapminder$totalGDP = with(gapminder, pop + gdpPercap)


#(C)

gapminder %>% 
  group_by(continent) %>% 
  summarise(median_gdp = median(gdpPercap), mean_gdp = mean(gdpPercap), n=n())



#3  k=3 을 기준으로 잡아야 한다고 생각합니다.

>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======
setwd("C:\\Users\\Gargantua\\Desktop\\data_analysis\\Project6_R_Bigdata")

library(MASS)
library(dplyr)


#1.

#(a)관측치 수 = 1309 / 변수의 수 = 15

titanic = read.csv('DataSet/titanic.csv', stringsAsFactors = T)

str(titanic)


#(b) 0.2552891

titanic %>% 
  filter(pclass == '3rd') %>% 
  summarise(mean = mean(survived))


#(c)
    # 1 1st            0.619
    # 2 2nd            0.430
    # 3 3rd            0.255

titanic %>% 
  group_by(pclass) %>% 
  summarise(mean_survived = mean(survived))


#(d) 0.7848948

titanic_not_na = titanic %>% filter(!is.na(age))

nrow(titanic_not_na %>% 
       filter(age>=20)) / nrow(titanic_not_na)



#(e)
      # sex    age_group pclass mean_survived
      # <fct>  <chr>     <fct>          <dbl>
      #   1 female Adult     1st           0.969 
      # 2 female Adult     2nd           0.874 
      # 3 female Adult     3rd           0.445 
      # 4 female Child     1st           0.833 
      # 5 female Child     2nd           1     
      # 6 female Child     3rd           0.548 
      # 7 male   Adult     1st           0.329 
      # 8 male   Adult     2nd           0.0833
      # 9 male   Adult     3rd           0.151 
      # 10 male   Child     1st           1     
      # 11 male   Child     2nd           0.786 
      # 12 male   Child     3rd           0.275 


titanic %>%
  filter(!is.na(age)) %>% 
  mutate(age_group = ifelse(age >= 17, "Adult", "Child")) %>% 
  group_by(sex, age_group, pclass) %>% 
  summarise(mean_survived = mean(survived))



#(f)

titanic %>% 
  mutate(family = sibsp + parch)



#(g)

    # Cherbourg  Queenstown Southampton 
    # 270         123         914 

table(titanic[,"embarked"])



#(h) 2개

nrow(titanic[is.na(titanic$embarked),])


#(i)

titanic[is.na(titanic$embarked),"embarked"] = "Southampton"

any(is.na(titanic$embarked))


#(j)

table(titanic$embarked, titanic$pclass)


#(k)

  ##대상 데이터 선정

data_test = titanic_not_na %>% 
  mutate(family = sibsp + parch)


  ##모델 구축
model_logistic = glm(survived ~ pclass + age + fare + sex + family + embarked, data = data_test)

summary(model_logistic)



#(l) 성별이 <남성>일 때 생존하지 못할 확률 대비 생존할 확률은 <-2.643246>이다

    ## 남성이면 생존 확률이 2.643246 감소한다.




#2

install.packages("gapminder")
library(gapminder)
data("gapminder")


#(a) 관측치 1704개 / 변수 6개

str(gapminder)


#(b)

gapminder$totalGDP = with(gapminder, pop + gdpPercap)


#(C)

gapminder %>% 
  group_by(continent) %>% 
  summarise(median_gdp = median(gdpPercap), mean_gdp = mean(gdpPercap), n=n())



#3  k=3 을 기준으로 잡아야 한다고 생각합니다.

>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======
setwd("C:\\Users\\Gargantua\\Desktop\\data_analysis\\Project6_R_Bigdata")

library(MASS)
library(dplyr)


#1.

#(a)관측치 수 = 1309 / 변수의 수 = 15

titanic = read.csv('DataSet/titanic.csv', stringsAsFactors = T)

str(titanic)


#(b) 0.2552891

titanic %>% 
  filter(pclass == '3rd') %>% 
  summarise(mean = mean(survived))


#(c)
    # 1 1st            0.619
    # 2 2nd            0.430
    # 3 3rd            0.255

titanic %>% 
  group_by(pclass) %>% 
  summarise(mean_survived = mean(survived))


#(d) 0.7848948

titanic_not_na = titanic %>% filter(!is.na(age))

nrow(titanic_not_na %>% 
       filter(age>=20)) / nrow(titanic_not_na)



#(e)
      # sex    age_group pclass mean_survived
      # <fct>  <chr>     <fct>          <dbl>
      #   1 female Adult     1st           0.969 
      # 2 female Adult     2nd           0.874 
      # 3 female Adult     3rd           0.445 
      # 4 female Child     1st           0.833 
      # 5 female Child     2nd           1     
      # 6 female Child     3rd           0.548 
      # 7 male   Adult     1st           0.329 
      # 8 male   Adult     2nd           0.0833
      # 9 male   Adult     3rd           0.151 
      # 10 male   Child     1st           1     
      # 11 male   Child     2nd           0.786 
      # 12 male   Child     3rd           0.275 


titanic %>%
  filter(!is.na(age)) %>% 
  mutate(age_group = ifelse(age >= 17, "Adult", "Child")) %>% 
  group_by(sex, age_group, pclass) %>% 
  summarise(mean_survived = mean(survived))



#(f)

titanic %>% 
  mutate(family = sibsp + parch)



#(g)

    # Cherbourg  Queenstown Southampton 
    # 270         123         914 

table(titanic[,"embarked"])



#(h) 2개

nrow(titanic[is.na(titanic$embarked),])


#(i)

titanic[is.na(titanic$embarked),"embarked"] = "Southampton"

any(is.na(titanic$embarked))


#(j)

table(titanic$embarked, titanic$pclass)


#(k)

  ##대상 데이터 선정

data_test = titanic_not_na %>% 
  mutate(family = sibsp + parch)


  ##모델 구축
model_logistic = glm(survived ~ pclass + age + fare + sex + family + embarked, data = data_test)

summary(model_logistic)



#(l) 성별이 <남성>일 때 생존하지 못할 확률 대비 생존할 확률은 <-2.643246>이다

    ## 남성이면 생존 확률이 2.643246 감소한다.




#2

install.packages("gapminder")
library(gapminder)
data("gapminder")


#(a) 관측치 1704개 / 변수 6개

str(gapminder)


#(b)

gapminder$totalGDP = with(gapminder, pop + gdpPercap)


#(C)

gapminder %>% 
  group_by(continent) %>% 
  summarise(median_gdp = median(gdpPercap), mean_gdp = mean(gdpPercap), n=n())



#3  k=3 을 기준으로 잡아야 한다고 생각합니다.

>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
  ## 2나 4로 선정할 수 도 있으나 3이후로 기울기 감소 폭이 급격하게 감소하기 때문입니다.