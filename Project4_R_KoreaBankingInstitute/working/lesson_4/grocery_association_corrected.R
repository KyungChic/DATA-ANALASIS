# ���Ż�ǰ �������м� ����

# 0) ������Ű���ٿ�ε�
library(arules)
library(datasets)

# 1) �����ͺҷ�����
data(Groceries)
summary(Groceries)


# 2) ���帹�̱����� ���� 20�� ��ǰ �ð�ȭ
itemFrequencyPlot(Groceries,topN=20,type="absolute")


# 3) �������м� ���� �� Ȯ��
rules <- apriori(Groceries, parameter = list(supp=0.001, conf = 0.8))
summary(rules)
inspect(rules)
#---> ��Ģ�� ���ٰ� �߿��� �� �ƴ� ��Ģ ���� �Ķ���Ϳ� ���� ���� ��. ��� ������ ��Ģ�� ���ƾ� ��.


# 4) Whole milk ������ ������Ģ�� Ȯ��
rules2<-sort(apriori(Groceries, parameter=list(supp=0.001,conf = 0.08,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F)),decreasing=TRUE,by="confidence")
inspect(rules2)
summary(rules2)
inspect(rules2[1:5,])


# 5) ��������Ģ �ð�ȭ
library(arulesViz)
plot(rules2,method="graph",interactive = TRUE, shading=NA)
