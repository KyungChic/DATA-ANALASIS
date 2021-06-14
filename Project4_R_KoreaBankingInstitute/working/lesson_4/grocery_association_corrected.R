# 구매상품 연관성분석 수행

# 0) 관련패키지다운로드
library(arules)
library(datasets)

# 1) 데이터불러오기
data(Groceries)
summary(Groceries)


# 2) 가장많이구매한 상위 20개 상품 시각화
itemFrequencyPlot(Groceries,topN=20,type="absolute")


# 3) 연관성분석 수행 후 확인
rules <- apriori(Groceries, parameter = list(supp=0.001, conf = 0.8))
summary(rules)
inspect(rules)
#---> 규칙이 많다고 중요한 건 아님 규칙 수는 파라미터에 의한 것일 뿐. 사용 가능한 규칙이 많아야 함.


# 4) Whole milk 구매의 연관규칙만 확인
rules2<-sort(apriori(Groceries, parameter=list(supp=0.001,conf = 0.08,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F)),decreasing=TRUE,by="confidence")
inspect(rules2)
summary(rules2)
inspect(rules2[1:5,])


# 5) 연관성규칙 시각화
library(arulesViz)
plot(rules2,method="graph",interactive = TRUE, shading=NA)

