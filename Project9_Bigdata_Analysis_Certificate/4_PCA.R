iris_pca = princomp(iris[,-5],
                    cor=FALSE,
                    scroes = TRUE)

iris_pca

summary(iris_pca)




plot(iris_pca, type="l", main="iris 스크리 산점도")




iris_pca$loadings
iris_pca$scores

biplot(iris_pca, scale = 0, main = "iris biplot")
