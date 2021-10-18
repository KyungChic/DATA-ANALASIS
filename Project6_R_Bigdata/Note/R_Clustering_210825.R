<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD

## ----------------------------------------------------------------
df<-data.frame(x=c(1,2,2,4,5), y=c(1,1,4,4,5))
df


## ----------------------------------------------------------------
dev.off()

plot(df$x, df$y, xlab='x', ylab='y', pch = 16, xlim=c(0, 6), ylim=c(0, 6), col ='blue')
text(df$x, df$y, labels=paste0("p", 1:5), pos=1, cex=0.8)


## ----------------------------------------------------------------
dist(df, method = "euclidean")^2


## ----------------------------------------------------------------
hc_sl <- hclust(dist(df)^2, method="single")
hc_sl


## ----------------------------------------------------------------
par(mfrow = c(1, 2))
plot(hc_sl)
plot(hc_sl, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
r<-rev(hc_sl)
r$method
r$height


## ----------------------------------------------------------------
idx<-which(r$height>=6)
length(idx)+ 1


## ----------------------------------------------------------------
hc_cl <- hclust(dist(df)^2, method="complete")

par(mfrow = c(1, 2))
plot(hc_cl)
plot(hc_cl, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc_avg <- hclust(dist(df)^2, method="average")

par(mfrow = c(1, 2))
plot(hc_avg)
plot(hc_avg, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc_cent <- hclust(dist(df)^2, method="centroid")

par(mfrow = c(1, 2))
plot(hc_cent)
plot(hc_cent, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc_ward<-hclust(dist(df)^2, method="ward.D")

par(mfrow = c(1, 2))
plot(hc_ward)
plot(hc_ward, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
par(mfrow = c(1, 5))
plot(hc_sl, main = "Single")
plot(hc_cl, main = "Complete")
plot(hc_avg, main = "Average")
plot(hc_cent, main = "Centroid")
plot(hc_ward, main = "Ward")


## ----------------------------------------------------------------
data(iris)
str(iris)


## ----------------------------------------------------------------
set.seed(1234)
idx<-sample(1:nrow(iris), 30)
sample_iris<-iris[idx, -5]
label_species<-iris[idx, 5]


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="single")


plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom
#plot(hc, hang = -1) 하면 분류를 알 수 없어 분석이 불가함.


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="complete")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="average")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="centroid")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="ward.D")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
scaled_iris<-scale(iris[, -5])
species<-iris[,5]  #scaled_iris<-iris[, -5]
summary(scaled_iris)

apply(scaled_iris,2 ,mean)
  #matrix라서 sapply 적용 불가
  #표준화(scale) 결과 확인 : 평균을 0으로 만들고 표준편차를 1로 만듬.




## ----------------------------------------------------------------
v.names<-colnames(scaled_iris)

plot(scaled_iris[ ,3:4], pch=16, cex=1, col=species, xlab=v.names[3], ylab=v.names[4])
legend("topleft", legend=levels(species), col=c(1:3), pch=16)

#표준화한 자료에 대해 그래프를 그려본 것.
  ##색깔은 주어진 category


## ----------------------------------------------------------------
result1<-kmeans(scaled_iris, center=3)
result1


## ----------------------------------------------------------------
result2<-kmeans(scaled_iris, center=3, nstart = 25)
result2

#시작 당시에 25군집

## ----------------------------------------------------------------
par(mfrow=c(2,2))

plot(scaled_iris[ ,1:2], col=result1$cluster, pch=(result1$cluster+20), main="K=3") # Sepal.Length/Width
points(result1$centers[,1:2], pch=8, cex=3)
plot(scaled_iris[ ,3:4], col=result1$cluster, pch=(result1$cluster+20), main="K=3") # Petal.Length/Width
points(result1$centers[,3:4], pch=8, cex=3)
plot(scaled_iris[ ,1:2], col=result2$cluster, pch=(result2$cluster+20), main="K=3, nstart=25")
points(result2$centers[,1:2], pch='*', cex=5)
plot(scaled_iris[ ,3:4], col=result2$cluster, pch=(result2$cluster+20), main="K=3, nstart=25")
points(result2$centers[,3:4], pch='*', cex=5)

#K-means 결과값들 비교
  ## 색깔은 군집화 결과임

## ----------------------------------------------------------------
table(iris$Species, result1$cluster)


## ----------------------------------------------------------------
table(iris$Species, result2$cluster)


## ----------------------------------------------------------------
result3<-kmeans(scaled_iris, center=4)
result3


## ----------------------------------------------------------------
result4<-kmeans(scaled_iris, center=4, nstart = 25)
result4


## ----------------------------------------------------------------
par(mfrow=c(2,2))

plot(scaled_iris[ ,1:2], col=result3$cluster, pch=(result3$cluster+20), main="K=4") # Sepal.Length/Width
points(result3$centers[,1:2], pch=8, cex=3)
plot(scaled_iris[ ,3:4], col=result3$cluster, pch=(result3$cluster+20), main="K=4") # Petal.Length/Width
points(result3$centers[,3:4], pch=8, cex=3)
plot(scaled_iris[ ,1:2], col=result4$cluster, pch=(result4$cluster+20), main="K=4, nstart=25")
points(result4$centers[,1:2], pch='*', cex=5)
plot(scaled_iris[ ,3:4], col=result4$cluster, pch=(result4$cluster+20), main="K=4, nstart=25")
points(result4$centers[,3:4], pch='*', cex=5)


#클러스터 개수를 4개로 하면 겹쳐버림을 알 수 있음 -> 개수 조정 필요

## ----------------------------------------------------------------
table(iris$Species, result3$cluster)


## ----------------------------------------------------------------
table(iris$Species, result4$cluster)


## ----------------------------------------------------------------
vWSS<-NULL
for(i in 1:10){
vWSS[i]<-sum(kmeans(scaled_iris, center=i)$withinss)
} # end i
plot(1:10, vWSS, type='b', xlab='The number of clusters', ylab='WSS')


#WSS값이 큰 폭으로 감소되는 구간을 찾기.


## ----------------------------------------------------------------
install.packages('factoextra')
library(factoextra)


factoextra::fviz_nbclust(scaled_iris, kmeans, method="wss")


## ----------------------------------------------------------------
factoextra::fviz_cluster(result2, scaled_iris)

factoextra::fviz_cluster(result4, scaled_iris)


## ----------------------------------------------------------------
data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]
plot(df, pch=20)


## ----------------------------------------------------------------
set.seed(1004)
km.res <- kmeans(df, centers=5, nstart = 25)
factoextra::fviz_cluster(km.res, df, geom = "point")


## ----------------------------------------------------------------
library(dbscan)

kNNdistplot(df, k=5)
abline(h = 0.15, lty = 2)

#꺾이는 지점 찾기 -> 0.15


## ----------------------------------------------------------------
db <- dbscan(df, eps = 0.15, minPts = 5)
factoextra::fviz_cluster(db, df, geom = "point")


## ----------------------------------------------------------------
## ----------------------------------------------------------------

sns0<-read.csv("DataSet/snsdata.csv")
#names(sns0)
 #dim(sns0)

=======

## ----------------------------------------------------------------
df<-data.frame(x=c(1,2,2,4,5), y=c(1,1,4,4,5))
df


## ----------------------------------------------------------------
dev.off()

plot(df$x, df$y, xlab='x', ylab='y', pch = 16, xlim=c(0, 6), ylim=c(0, 6), col ='blue')
text(df$x, df$y, labels=paste0("p", 1:5), pos=1, cex=0.8)


## ----------------------------------------------------------------
dist(df, method = "euclidean")^2


## ----------------------------------------------------------------
hc_sl <- hclust(dist(df)^2, method="single")
hc_sl


## ----------------------------------------------------------------
par(mfrow = c(1, 2))
plot(hc_sl)
plot(hc_sl, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
r<-rev(hc_sl)
r$method
r$height


## ----------------------------------------------------------------
idx<-which(r$height>=6)
length(idx)+ 1


## ----------------------------------------------------------------
hc_cl <- hclust(dist(df)^2, method="complete")

par(mfrow = c(1, 2))
plot(hc_cl)
plot(hc_cl, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc_avg <- hclust(dist(df)^2, method="average")

par(mfrow = c(1, 2))
plot(hc_avg)
plot(hc_avg, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc_cent <- hclust(dist(df)^2, method="centroid")

par(mfrow = c(1, 2))
plot(hc_cent)
plot(hc_cent, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc_ward<-hclust(dist(df)^2, method="ward.D")

par(mfrow = c(1, 2))
plot(hc_ward)
plot(hc_ward, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
par(mfrow = c(1, 5))
plot(hc_sl, main = "Single")
plot(hc_cl, main = "Complete")
plot(hc_avg, main = "Average")
plot(hc_cent, main = "Centroid")
plot(hc_ward, main = "Ward")


## ----------------------------------------------------------------
data(iris)
str(iris)


## ----------------------------------------------------------------
set.seed(1234)
idx<-sample(1:nrow(iris), 30)
sample_iris<-iris[idx, -5]
label_species<-iris[idx, 5]


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="single")


plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom
#plot(hc, hang = -1) 하면 분류를 알 수 없어 분석이 불가함.


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="complete")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="average")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="centroid")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="ward.D")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
scaled_iris<-scale(iris[, -5])
species<-iris[,5]  #scaled_iris<-iris[, -5]
summary(scaled_iris)

apply(scaled_iris,2 ,mean)
  #matrix라서 sapply 적용 불가
  #표준화(scale) 결과 확인 : 평균을 0으로 만들고 표준편차를 1로 만듬.




## ----------------------------------------------------------------
v.names<-colnames(scaled_iris)

plot(scaled_iris[ ,3:4], pch=16, cex=1, col=species, xlab=v.names[3], ylab=v.names[4])
legend("topleft", legend=levels(species), col=c(1:3), pch=16)

#표준화한 자료에 대해 그래프를 그려본 것.
  ##색깔은 주어진 category


## ----------------------------------------------------------------
result1<-kmeans(scaled_iris, center=3)
result1


## ----------------------------------------------------------------
result2<-kmeans(scaled_iris, center=3, nstart = 25)
result2

#시작 당시에 25군집

## ----------------------------------------------------------------
par(mfrow=c(2,2))

plot(scaled_iris[ ,1:2], col=result1$cluster, pch=(result1$cluster+20), main="K=3") # Sepal.Length/Width
points(result1$centers[,1:2], pch=8, cex=3)
plot(scaled_iris[ ,3:4], col=result1$cluster, pch=(result1$cluster+20), main="K=3") # Petal.Length/Width
points(result1$centers[,3:4], pch=8, cex=3)
plot(scaled_iris[ ,1:2], col=result2$cluster, pch=(result2$cluster+20), main="K=3, nstart=25")
points(result2$centers[,1:2], pch='*', cex=5)
plot(scaled_iris[ ,3:4], col=result2$cluster, pch=(result2$cluster+20), main="K=3, nstart=25")
points(result2$centers[,3:4], pch='*', cex=5)

#K-means 결과값들 비교
  ## 색깔은 군집화 결과임

## ----------------------------------------------------------------
table(iris$Species, result1$cluster)


## ----------------------------------------------------------------
table(iris$Species, result2$cluster)


## ----------------------------------------------------------------
result3<-kmeans(scaled_iris, center=4)
result3


## ----------------------------------------------------------------
result4<-kmeans(scaled_iris, center=4, nstart = 25)
result4


## ----------------------------------------------------------------
par(mfrow=c(2,2))

plot(scaled_iris[ ,1:2], col=result3$cluster, pch=(result3$cluster+20), main="K=4") # Sepal.Length/Width
points(result3$centers[,1:2], pch=8, cex=3)
plot(scaled_iris[ ,3:4], col=result3$cluster, pch=(result3$cluster+20), main="K=4") # Petal.Length/Width
points(result3$centers[,3:4], pch=8, cex=3)
plot(scaled_iris[ ,1:2], col=result4$cluster, pch=(result4$cluster+20), main="K=4, nstart=25")
points(result4$centers[,1:2], pch='*', cex=5)
plot(scaled_iris[ ,3:4], col=result4$cluster, pch=(result4$cluster+20), main="K=4, nstart=25")
points(result4$centers[,3:4], pch='*', cex=5)


#클러스터 개수를 4개로 하면 겹쳐버림을 알 수 있음 -> 개수 조정 필요

## ----------------------------------------------------------------
table(iris$Species, result3$cluster)


## ----------------------------------------------------------------
table(iris$Species, result4$cluster)


## ----------------------------------------------------------------
vWSS<-NULL
for(i in 1:10){
vWSS[i]<-sum(kmeans(scaled_iris, center=i)$withinss)
} # end i
plot(1:10, vWSS, type='b', xlab='The number of clusters', ylab='WSS')


#WSS값이 큰 폭으로 감소되는 구간을 찾기.


## ----------------------------------------------------------------
install.packages('factoextra')
library(factoextra)


factoextra::fviz_nbclust(scaled_iris, kmeans, method="wss")


## ----------------------------------------------------------------
factoextra::fviz_cluster(result2, scaled_iris)

factoextra::fviz_cluster(result4, scaled_iris)


## ----------------------------------------------------------------
data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]
plot(df, pch=20)


## ----------------------------------------------------------------
set.seed(1004)
km.res <- kmeans(df, centers=5, nstart = 25)
factoextra::fviz_cluster(km.res, df, geom = "point")


## ----------------------------------------------------------------
library(dbscan)

kNNdistplot(df, k=5)
abline(h = 0.15, lty = 2)

#꺾이는 지점 찾기 -> 0.15


## ----------------------------------------------------------------
db <- dbscan(df, eps = 0.15, minPts = 5)
factoextra::fviz_cluster(db, df, geom = "point")


## ----------------------------------------------------------------
## ----------------------------------------------------------------

sns0<-read.csv("DataSet/snsdata.csv")
#names(sns0)
 #dim(sns0)

>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======

## ----------------------------------------------------------------
df<-data.frame(x=c(1,2,2,4,5), y=c(1,1,4,4,5))
df


## ----------------------------------------------------------------
dev.off()

plot(df$x, df$y, xlab='x', ylab='y', pch = 16, xlim=c(0, 6), ylim=c(0, 6), col ='blue')
text(df$x, df$y, labels=paste0("p", 1:5), pos=1, cex=0.8)


## ----------------------------------------------------------------
dist(df, method = "euclidean")^2


## ----------------------------------------------------------------
hc_sl <- hclust(dist(df)^2, method="single")
hc_sl


## ----------------------------------------------------------------
par(mfrow = c(1, 2))
plot(hc_sl)
plot(hc_sl, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
r<-rev(hc_sl)
r$method
r$height


## ----------------------------------------------------------------
idx<-which(r$height>=6)
length(idx)+ 1


## ----------------------------------------------------------------
hc_cl <- hclust(dist(df)^2, method="complete")

par(mfrow = c(1, 2))
plot(hc_cl)
plot(hc_cl, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc_avg <- hclust(dist(df)^2, method="average")

par(mfrow = c(1, 2))
plot(hc_avg)
plot(hc_avg, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc_cent <- hclust(dist(df)^2, method="centroid")

par(mfrow = c(1, 2))
plot(hc_cent)
plot(hc_cent, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc_ward<-hclust(dist(df)^2, method="ward.D")

par(mfrow = c(1, 2))
plot(hc_ward)
plot(hc_ward, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
par(mfrow = c(1, 5))
plot(hc_sl, main = "Single")
plot(hc_cl, main = "Complete")
plot(hc_avg, main = "Average")
plot(hc_cent, main = "Centroid")
plot(hc_ward, main = "Ward")


## ----------------------------------------------------------------
data(iris)
str(iris)


## ----------------------------------------------------------------
set.seed(1234)
idx<-sample(1:nrow(iris), 30)
sample_iris<-iris[idx, -5]
label_species<-iris[idx, 5]


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="single")


plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom
#plot(hc, hang = -1) 하면 분류를 알 수 없어 분석이 불가함.


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="complete")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="average")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="centroid")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="ward.D")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
scaled_iris<-scale(iris[, -5])
species<-iris[,5]  #scaled_iris<-iris[, -5]
summary(scaled_iris)

apply(scaled_iris,2 ,mean)
  #matrix라서 sapply 적용 불가
  #표준화(scale) 결과 확인 : 평균을 0으로 만들고 표준편차를 1로 만듬.




## ----------------------------------------------------------------
v.names<-colnames(scaled_iris)

plot(scaled_iris[ ,3:4], pch=16, cex=1, col=species, xlab=v.names[3], ylab=v.names[4])
legend("topleft", legend=levels(species), col=c(1:3), pch=16)

#표준화한 자료에 대해 그래프를 그려본 것.
  ##색깔은 주어진 category


## ----------------------------------------------------------------
result1<-kmeans(scaled_iris, center=3)
result1


## ----------------------------------------------------------------
result2<-kmeans(scaled_iris, center=3, nstart = 25)
result2

#시작 당시에 25군집

## ----------------------------------------------------------------
par(mfrow=c(2,2))

plot(scaled_iris[ ,1:2], col=result1$cluster, pch=(result1$cluster+20), main="K=3") # Sepal.Length/Width
points(result1$centers[,1:2], pch=8, cex=3)
plot(scaled_iris[ ,3:4], col=result1$cluster, pch=(result1$cluster+20), main="K=3") # Petal.Length/Width
points(result1$centers[,3:4], pch=8, cex=3)
plot(scaled_iris[ ,1:2], col=result2$cluster, pch=(result2$cluster+20), main="K=3, nstart=25")
points(result2$centers[,1:2], pch='*', cex=5)
plot(scaled_iris[ ,3:4], col=result2$cluster, pch=(result2$cluster+20), main="K=3, nstart=25")
points(result2$centers[,3:4], pch='*', cex=5)

#K-means 결과값들 비교
  ## 색깔은 군집화 결과임

## ----------------------------------------------------------------
table(iris$Species, result1$cluster)


## ----------------------------------------------------------------
table(iris$Species, result2$cluster)


## ----------------------------------------------------------------
result3<-kmeans(scaled_iris, center=4)
result3


## ----------------------------------------------------------------
result4<-kmeans(scaled_iris, center=4, nstart = 25)
result4


## ----------------------------------------------------------------
par(mfrow=c(2,2))

plot(scaled_iris[ ,1:2], col=result3$cluster, pch=(result3$cluster+20), main="K=4") # Sepal.Length/Width
points(result3$centers[,1:2], pch=8, cex=3)
plot(scaled_iris[ ,3:4], col=result3$cluster, pch=(result3$cluster+20), main="K=4") # Petal.Length/Width
points(result3$centers[,3:4], pch=8, cex=3)
plot(scaled_iris[ ,1:2], col=result4$cluster, pch=(result4$cluster+20), main="K=4, nstart=25")
points(result4$centers[,1:2], pch='*', cex=5)
plot(scaled_iris[ ,3:4], col=result4$cluster, pch=(result4$cluster+20), main="K=4, nstart=25")
points(result4$centers[,3:4], pch='*', cex=5)


#클러스터 개수를 4개로 하면 겹쳐버림을 알 수 있음 -> 개수 조정 필요

## ----------------------------------------------------------------
table(iris$Species, result3$cluster)


## ----------------------------------------------------------------
table(iris$Species, result4$cluster)


## ----------------------------------------------------------------
vWSS<-NULL
for(i in 1:10){
vWSS[i]<-sum(kmeans(scaled_iris, center=i)$withinss)
} # end i
plot(1:10, vWSS, type='b', xlab='The number of clusters', ylab='WSS')


#WSS값이 큰 폭으로 감소되는 구간을 찾기.


## ----------------------------------------------------------------
install.packages('factoextra')
library(factoextra)


factoextra::fviz_nbclust(scaled_iris, kmeans, method="wss")


## ----------------------------------------------------------------
factoextra::fviz_cluster(result2, scaled_iris)

factoextra::fviz_cluster(result4, scaled_iris)


## ----------------------------------------------------------------
data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]
plot(df, pch=20)


## ----------------------------------------------------------------
set.seed(1004)
km.res <- kmeans(df, centers=5, nstart = 25)
factoextra::fviz_cluster(km.res, df, geom = "point")


## ----------------------------------------------------------------
library(dbscan)

kNNdistplot(df, k=5)
abline(h = 0.15, lty = 2)

#꺾이는 지점 찾기 -> 0.15


## ----------------------------------------------------------------
db <- dbscan(df, eps = 0.15, minPts = 5)
factoextra::fviz_cluster(db, df, geom = "point")


## ----------------------------------------------------------------
## ----------------------------------------------------------------

sns0<-read.csv("DataSet/snsdata.csv")
#names(sns0)
 #dim(sns0)

>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======

## ----------------------------------------------------------------
df<-data.frame(x=c(1,2,2,4,5), y=c(1,1,4,4,5))
df


## ----------------------------------------------------------------
dev.off()

plot(df$x, df$y, xlab='x', ylab='y', pch = 16, xlim=c(0, 6), ylim=c(0, 6), col ='blue')
text(df$x, df$y, labels=paste0("p", 1:5), pos=1, cex=0.8)


## ----------------------------------------------------------------
dist(df, method = "euclidean")^2


## ----------------------------------------------------------------
hc_sl <- hclust(dist(df)^2, method="single")
hc_sl


## ----------------------------------------------------------------
par(mfrow = c(1, 2))
plot(hc_sl)
plot(hc_sl, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
r<-rev(hc_sl)
r$method
r$height


## ----------------------------------------------------------------
idx<-which(r$height>=6)
length(idx)+ 1


## ----------------------------------------------------------------
hc_cl <- hclust(dist(df)^2, method="complete")

par(mfrow = c(1, 2))
plot(hc_cl)
plot(hc_cl, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc_avg <- hclust(dist(df)^2, method="average")

par(mfrow = c(1, 2))
plot(hc_avg)
plot(hc_avg, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc_cent <- hclust(dist(df)^2, method="centroid")

par(mfrow = c(1, 2))
plot(hc_cent)
plot(hc_cent, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc_ward<-hclust(dist(df)^2, method="ward.D")

par(mfrow = c(1, 2))
plot(hc_ward)
plot(hc_ward, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
par(mfrow = c(1, 5))
plot(hc_sl, main = "Single")
plot(hc_cl, main = "Complete")
plot(hc_avg, main = "Average")
plot(hc_cent, main = "Centroid")
plot(hc_ward, main = "Ward")


## ----------------------------------------------------------------
data(iris)
str(iris)


## ----------------------------------------------------------------
set.seed(1234)
idx<-sample(1:nrow(iris), 30)
sample_iris<-iris[idx, -5]
label_species<-iris[idx, 5]


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="single")


plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom
#plot(hc, hang = -1) 하면 분류를 알 수 없어 분석이 불가함.


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="complete")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="average")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="centroid")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="ward.D")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
scaled_iris<-scale(iris[, -5])
species<-iris[,5]  #scaled_iris<-iris[, -5]
summary(scaled_iris)

apply(scaled_iris,2 ,mean)
  #matrix라서 sapply 적용 불가
  #표준화(scale) 결과 확인 : 평균을 0으로 만들고 표준편차를 1로 만듬.




## ----------------------------------------------------------------
v.names<-colnames(scaled_iris)

plot(scaled_iris[ ,3:4], pch=16, cex=1, col=species, xlab=v.names[3], ylab=v.names[4])
legend("topleft", legend=levels(species), col=c(1:3), pch=16)

#표준화한 자료에 대해 그래프를 그려본 것.
  ##색깔은 주어진 category


## ----------------------------------------------------------------
result1<-kmeans(scaled_iris, center=3)
result1


## ----------------------------------------------------------------
result2<-kmeans(scaled_iris, center=3, nstart = 25)
result2

#시작 당시에 25군집

## ----------------------------------------------------------------
par(mfrow=c(2,2))

plot(scaled_iris[ ,1:2], col=result1$cluster, pch=(result1$cluster+20), main="K=3") # Sepal.Length/Width
points(result1$centers[,1:2], pch=8, cex=3)
plot(scaled_iris[ ,3:4], col=result1$cluster, pch=(result1$cluster+20), main="K=3") # Petal.Length/Width
points(result1$centers[,3:4], pch=8, cex=3)
plot(scaled_iris[ ,1:2], col=result2$cluster, pch=(result2$cluster+20), main="K=3, nstart=25")
points(result2$centers[,1:2], pch='*', cex=5)
plot(scaled_iris[ ,3:4], col=result2$cluster, pch=(result2$cluster+20), main="K=3, nstart=25")
points(result2$centers[,3:4], pch='*', cex=5)

#K-means 결과값들 비교
  ## 색깔은 군집화 결과임

## ----------------------------------------------------------------
table(iris$Species, result1$cluster)


## ----------------------------------------------------------------
table(iris$Species, result2$cluster)


## ----------------------------------------------------------------
result3<-kmeans(scaled_iris, center=4)
result3


## ----------------------------------------------------------------
result4<-kmeans(scaled_iris, center=4, nstart = 25)
result4


## ----------------------------------------------------------------
par(mfrow=c(2,2))

plot(scaled_iris[ ,1:2], col=result3$cluster, pch=(result3$cluster+20), main="K=4") # Sepal.Length/Width
points(result3$centers[,1:2], pch=8, cex=3)
plot(scaled_iris[ ,3:4], col=result3$cluster, pch=(result3$cluster+20), main="K=4") # Petal.Length/Width
points(result3$centers[,3:4], pch=8, cex=3)
plot(scaled_iris[ ,1:2], col=result4$cluster, pch=(result4$cluster+20), main="K=4, nstart=25")
points(result4$centers[,1:2], pch='*', cex=5)
plot(scaled_iris[ ,3:4], col=result4$cluster, pch=(result4$cluster+20), main="K=4, nstart=25")
points(result4$centers[,3:4], pch='*', cex=5)


#클러스터 개수를 4개로 하면 겹쳐버림을 알 수 있음 -> 개수 조정 필요

## ----------------------------------------------------------------
table(iris$Species, result3$cluster)


## ----------------------------------------------------------------
table(iris$Species, result4$cluster)


## ----------------------------------------------------------------
vWSS<-NULL
for(i in 1:10){
vWSS[i]<-sum(kmeans(scaled_iris, center=i)$withinss)
} # end i
plot(1:10, vWSS, type='b', xlab='The number of clusters', ylab='WSS')


#WSS값이 큰 폭으로 감소되는 구간을 찾기.


## ----------------------------------------------------------------
install.packages('factoextra')
library(factoextra)


factoextra::fviz_nbclust(scaled_iris, kmeans, method="wss")


## ----------------------------------------------------------------
factoextra::fviz_cluster(result2, scaled_iris)

factoextra::fviz_cluster(result4, scaled_iris)


## ----------------------------------------------------------------
data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]
plot(df, pch=20)


## ----------------------------------------------------------------
set.seed(1004)
km.res <- kmeans(df, centers=5, nstart = 25)
factoextra::fviz_cluster(km.res, df, geom = "point")


## ----------------------------------------------------------------
library(dbscan)

kNNdistplot(df, k=5)
abline(h = 0.15, lty = 2)

#꺾이는 지점 찾기 -> 0.15


## ----------------------------------------------------------------
db <- dbscan(df, eps = 0.15, minPts = 5)
factoextra::fviz_cluster(db, df, geom = "point")


## ----------------------------------------------------------------
## ----------------------------------------------------------------

sns0<-read.csv("DataSet/snsdata.csv")
#names(sns0)
 #dim(sns0)

>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
=======

## ----------------------------------------------------------------
df<-data.frame(x=c(1,2,2,4,5), y=c(1,1,4,4,5))
df


## ----------------------------------------------------------------
dev.off()

plot(df$x, df$y, xlab='x', ylab='y', pch = 16, xlim=c(0, 6), ylim=c(0, 6), col ='blue')
text(df$x, df$y, labels=paste0("p", 1:5), pos=1, cex=0.8)


## ----------------------------------------------------------------
dist(df, method = "euclidean")^2


## ----------------------------------------------------------------
hc_sl <- hclust(dist(df)^2, method="single")
hc_sl


## ----------------------------------------------------------------
par(mfrow = c(1, 2))
plot(hc_sl)
plot(hc_sl, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
r<-rev(hc_sl)
r$method
r$height


## ----------------------------------------------------------------
idx<-which(r$height>=6)
length(idx)+ 1


## ----------------------------------------------------------------
hc_cl <- hclust(dist(df)^2, method="complete")

par(mfrow = c(1, 2))
plot(hc_cl)
plot(hc_cl, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc_avg <- hclust(dist(df)^2, method="average")

par(mfrow = c(1, 2))
plot(hc_avg)
plot(hc_avg, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc_cent <- hclust(dist(df)^2, method="centroid")

par(mfrow = c(1, 2))
plot(hc_cent)
plot(hc_cent, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc_ward<-hclust(dist(df)^2, method="ward.D")

par(mfrow = c(1, 2))
plot(hc_ward)
plot(hc_ward, hang = -1) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
par(mfrow = c(1, 5))
plot(hc_sl, main = "Single")
plot(hc_cl, main = "Complete")
plot(hc_avg, main = "Average")
plot(hc_cent, main = "Centroid")
plot(hc_ward, main = "Ward")


## ----------------------------------------------------------------
data(iris)
str(iris)


## ----------------------------------------------------------------
set.seed(1234)
idx<-sample(1:nrow(iris), 30)
sample_iris<-iris[idx, -5]
label_species<-iris[idx, 5]


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="single")


plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom
#plot(hc, hang = -1) 하면 분류를 알 수 없어 분석이 불가함.


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="complete")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="average")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="centroid")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
hc<-hclust(dist(sample_iris), method="ward.D")
plot(hc, hang = -1, labels=label_species) # hang = -1 : line from the bottom


## ----------------------------------------------------------------
scaled_iris<-scale(iris[, -5])
species<-iris[,5]  #scaled_iris<-iris[, -5]
summary(scaled_iris)

apply(scaled_iris,2 ,mean)
  #matrix라서 sapply 적용 불가
  #표준화(scale) 결과 확인 : 평균을 0으로 만들고 표준편차를 1로 만듬.




## ----------------------------------------------------------------
v.names<-colnames(scaled_iris)

plot(scaled_iris[ ,3:4], pch=16, cex=1, col=species, xlab=v.names[3], ylab=v.names[4])
legend("topleft", legend=levels(species), col=c(1:3), pch=16)

#표준화한 자료에 대해 그래프를 그려본 것.
  ##색깔은 주어진 category


## ----------------------------------------------------------------
result1<-kmeans(scaled_iris, center=3)
result1


## ----------------------------------------------------------------
result2<-kmeans(scaled_iris, center=3, nstart = 25)
result2

#시작 당시에 25군집

## ----------------------------------------------------------------
par(mfrow=c(2,2))

plot(scaled_iris[ ,1:2], col=result1$cluster, pch=(result1$cluster+20), main="K=3") # Sepal.Length/Width
points(result1$centers[,1:2], pch=8, cex=3)
plot(scaled_iris[ ,3:4], col=result1$cluster, pch=(result1$cluster+20), main="K=3") # Petal.Length/Width
points(result1$centers[,3:4], pch=8, cex=3)
plot(scaled_iris[ ,1:2], col=result2$cluster, pch=(result2$cluster+20), main="K=3, nstart=25")
points(result2$centers[,1:2], pch='*', cex=5)
plot(scaled_iris[ ,3:4], col=result2$cluster, pch=(result2$cluster+20), main="K=3, nstart=25")
points(result2$centers[,3:4], pch='*', cex=5)

#K-means 결과값들 비교
  ## 색깔은 군집화 결과임

## ----------------------------------------------------------------
table(iris$Species, result1$cluster)


## ----------------------------------------------------------------
table(iris$Species, result2$cluster)


## ----------------------------------------------------------------
result3<-kmeans(scaled_iris, center=4)
result3


## ----------------------------------------------------------------
result4<-kmeans(scaled_iris, center=4, nstart = 25)
result4


## ----------------------------------------------------------------
par(mfrow=c(2,2))

plot(scaled_iris[ ,1:2], col=result3$cluster, pch=(result3$cluster+20), main="K=4") # Sepal.Length/Width
points(result3$centers[,1:2], pch=8, cex=3)
plot(scaled_iris[ ,3:4], col=result3$cluster, pch=(result3$cluster+20), main="K=4") # Petal.Length/Width
points(result3$centers[,3:4], pch=8, cex=3)
plot(scaled_iris[ ,1:2], col=result4$cluster, pch=(result4$cluster+20), main="K=4, nstart=25")
points(result4$centers[,1:2], pch='*', cex=5)
plot(scaled_iris[ ,3:4], col=result4$cluster, pch=(result4$cluster+20), main="K=4, nstart=25")
points(result4$centers[,3:4], pch='*', cex=5)


#클러스터 개수를 4개로 하면 겹쳐버림을 알 수 있음 -> 개수 조정 필요

## ----------------------------------------------------------------
table(iris$Species, result3$cluster)


## ----------------------------------------------------------------
table(iris$Species, result4$cluster)


## ----------------------------------------------------------------
vWSS<-NULL
for(i in 1:10){
vWSS[i]<-sum(kmeans(scaled_iris, center=i)$withinss)
} # end i
plot(1:10, vWSS, type='b', xlab='The number of clusters', ylab='WSS')


#WSS값이 큰 폭으로 감소되는 구간을 찾기.


## ----------------------------------------------------------------
install.packages('factoextra')
library(factoextra)


factoextra::fviz_nbclust(scaled_iris, kmeans, method="wss")


## ----------------------------------------------------------------
factoextra::fviz_cluster(result2, scaled_iris)

factoextra::fviz_cluster(result4, scaled_iris)


## ----------------------------------------------------------------
data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]
plot(df, pch=20)


## ----------------------------------------------------------------
set.seed(1004)
km.res <- kmeans(df, centers=5, nstart = 25)
factoextra::fviz_cluster(km.res, df, geom = "point")


## ----------------------------------------------------------------
library(dbscan)

kNNdistplot(df, k=5)
abline(h = 0.15, lty = 2)

#꺾이는 지점 찾기 -> 0.15


## ----------------------------------------------------------------
db <- dbscan(df, eps = 0.15, minPts = 5)
factoextra::fviz_cluster(db, df, geom = "point")


## ----------------------------------------------------------------
## ----------------------------------------------------------------

sns0<-read.csv("DataSet/snsdata.csv")
#names(sns0)
 #dim(sns0)

>>>>>>> 5c5a7e65ae3a3edf20e3212427f4d26207c4addc
