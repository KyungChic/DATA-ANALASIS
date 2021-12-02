
head(USArrests)
str(USArrests)
summary(USArrests)

US.dist_euclidean = dist(USArrests, "euclidean")
US.dist_euclidean

mahalanobis(USArrests, colMeans(USArrests), cov(USArrests))




US.single = hclust(US.dist_euclidean^2, method = "single")
plot(US.single)

US.complete = hclust(US.dist_euclidean^2, method = "complete")
plot(US.complete)



group = cutree(US.single, k=6)
group


rect.hclust(US.single, k=6, border='blue')




install.packages("rattle")
library(rattle)


head(wine)
str(wine)
summary(wine)


ds_wine = scale(wine[-1])

install.packages("NbClust")
library(NbClust)

nclust = NbClust(ds_wine, min.nc = 2, max.nc = 15, method = "kmeans")
table(nclust$Best.nc[1,])


barplot(table(nclust$Best.nc[1,]), xlab = "클러스터 수", ylab = "영역 수", main = "Number")

km = kmeans(ds_wine, 3, nstart=25)
km$size

km$centers

plot(ds_wine, col=km$cluster)
points(km$center)


help(kmeans)
















