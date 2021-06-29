## ����������

# 1) ������ �ҷ����� �� Ž��
travel
head(travel)
str(travel)

# 2) ���� ���� �Ѿ��� �Һ��� ���� 10 ����
aggregate(cost~cust_id, data=travel, sum)
cost_sum <- aggregate(cost~cust_id, data=travel, sum)

cost_sum[order(-cost_sum$cost),][1:10,]

# 3) ���� ���� ��� �ݾ��� �Һ��� ���� 10 ����
aggregate(cost~cust_id, data=travel, mean)
cost_mean <- aggregate(cost~cust_id, data=travel, mean)

cost_mean[order(-cost_mean$cost),][1:10,]

# 4) ���� ���� �Ѿ��� ���̴� �� ����
aggregate(cost~destination, data=travel, sum)
cost_sum_destination <- aggregate(cost~destination, data=travel, sum)

cost_sum_destination[order(cost_sum_destination$cost),][1:3,]


# 5) �Ϻ��� �湮�� �� ���� ��
japan_visit <- travel[travel$country=="�Ϻ�",]
aggregate(cost~cust_id, data=japan_visit, sum)
  #sum ��ġ�� �ʿ������ count �ϱ� ���� �ۼ�


# 6) ���� ���� ������ ����

travel$visit <- 1

visit_sum<-aggregate(visit~cust_id, data=travel, sum)
visit_sum[(order(-visit_sum$visit)),][1,]

# 7) ���� ������ ���� �ٴ� ���� 3������ �湮�� ����
top3_city <- subset(travel, travel$cust_id==1 | travel$cust_id==2 | travel$cust_id==3)
aggregate(visit~destination, data=top3_city, sum)