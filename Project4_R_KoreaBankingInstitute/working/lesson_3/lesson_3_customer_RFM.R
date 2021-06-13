
#Ư�� ������ �� ����
iris$Sepal.Length

#���ǿ� �´� ������ ����
y <- ifelse(iris$Sepal.Length>5, "more than 5", "less than 5")
y

#DF�� ���� �ڷ� �߰��ϱ�
iris$Sepal.Length_level <- y
head(iris)

#������ ������ ���ġ ���ϱ�(����)
aggregate(Sepal.Length ~ Species, data = iris, mean)
aggregate(Sepal.Length ~ Species, data = iris, sum)
aggregate(. ~ Species, data = iris, mean)


#������ ������ Ȯ��
str(customer_rfm)
head(customer_rfm)

aggregate(monetary ~ monetary_level, data = customer_rfm, mean)
aggregate(recency ~ recency_level, data = customer_rfm, mean)
aggregate(frequency ~ frequency_level, data = customer_rfm, mean)

#�������ϸ� ������ ����
rfm_level <- aggregate(. ~ monetary_level + frequency_level + recency_level, data = customer_rfm, mean)
rfm_level

#��������ȭ ������ �ð�ȭ�غ���
#rgl�󤷺귯�� �ҷ�����
library(rgl)

#3d plot�� �̿��� RFM ���� �ð�ȭ
plot3d(customer_rfm$monetary, customer_rfm$frequency, customer_rfm$recency,
       xlab = 'Monetarym', ylab = 'Frequency', zlab = 'Recency', col = "blue", size = 6)