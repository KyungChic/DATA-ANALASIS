#데이터 불러오기
iris <- read.csv("C:/Users/Gargantua/Desktop/data_analysis/Project4_R_KoreaBankingInstitute/working/iris.txt", sep="")

#구조살피기
str(iris)

#상위 6개 데이터 값들 보기
head(iris)

#기술통계 산출
summary(iris)

#데이터 시각화
library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width))+
  geom_point(aes(colour = Species, size=Petal.Width), alpha=I(0.7))+theme_bw()
