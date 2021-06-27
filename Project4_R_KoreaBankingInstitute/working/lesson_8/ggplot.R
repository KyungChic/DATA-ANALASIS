## 실무에 사용할 수 있는 데이터 시각화

# 1) 패키지 불러오기
library(ggplot2)

# 2) 일변량: 

# 연속형
ggplot(iris,aes(x=Sepal.Length))+stat_bin(binwidth=0.2,aes(fill=..count..),colour="black")+ggtitle("꽃받침의 길이 분포")+theme_bw()
ggplot(iris,aes(x=Sepal.Length))+geom_density(alpha=.2)+ggtitle("꽃받침의 너비 밀도")+theme_bw()

# 범주형
ggplot(infert, aes(education,fill=education))+geom_bar()+ggtitle("환자의 교육수준")+theme_bw()
ggplot(infert, aes(education,fill=education))+geom_bar()+coord_polar(theta = "y")+ggtitle("환자의 교육수준")+theme_bw()


# 3) 이변량:

# 연속형 x 연속형
ggplot(mtcars,aes(wt,disp))+geom_point()+geom_abline(intercept=-80,slope=100,colour="red")+ggtitle("무게에 따른 이동범위")+theme_bw()

# 범주형 x 범주형
ggplot(mtcars, aes(vs,fill=factor(cyl)))+geom_bar(position=position_dodge())+ggtitle("엔진 유형에 따른 실린더 수")+theme_bw()

# 연속형 x 범주형
ggplot(iris, aes(x=factor(Species), y=Sepal.Width))+geom_boxplot(aes(fill=factor(Species)))+ggtitle("붓꽃 유형에 따른 꽃받침의 너비")+theme_bw()
ggplot(iris,aes(x=Sepal.Width,fill=Species))+geom_density(alpha=.3)+ggtitle("붓꽃 유형에 따른 꽃받침의 너비 밀도")+theme_bw()
ggplot(diamonds,aes(price,fill=clarity))+geom_histogram(binwidth = 600)+ggtitle("선명도 수준에 따른 다이아몬드의 가격분포")+scale_fill_brewer("YlGn")+theme_bw()


# 3) 다변량:

# 연속형 x 연속형 + @
ggplot(mtcars,aes(wt,disp))+geom_point(aes(size=cyl))+ggtitle("무게에 따른 이동범위 + 실린더 수")+theme_bw()
ggplot(diamonds,aes(carat,price))+geom_point(aes(colour=clarity))+ggtitle("캐럿에 따른 다이아몬드의 가격 + 선명도 수준")+theme_bw()


# 4) ggplot 부가 기능

# 1) 도표 분할 
ggplot(diamonds,aes(carat))+geom_histogram(binwidth = 0.5,aes(fill=..count..))+facet_wrap(~ cut, ncol=5)+theme_bw()

# 2) 다른 두 산점도를 한번에 시각화
ggplot(iris)+geom_point(aes(Sepal.Length,Sepal.Width),colour="brown",pch=17)+geom_point(aes(Petal.Length,Petal.Width),colour="purple",pch=19)+theme_bw()


