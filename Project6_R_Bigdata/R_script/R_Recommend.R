

## ----------------------------------------------------------------
if(!require('arules')) install.packages('arules')
library(arules)


## ----------------------------------------------------------------
data(Groceries)
class(Groceries)
inspect(Groceries[1:5]) # inspecting the first 5 transactions


## ----------------------------------------------------------------
ct<-crossTable(Groceries, measure='count', sort=T)


## ----------------------------------------------------------------
items<-colnames(ct)
length(items)


## ----------------------------------------------------------------
ct[1:4, 1:4]


## ----------------------------------------------------------------
ct['whole milk', 1:10] 


## ----------------------------------------------------------------
sort(ct['whole milk', ], decreasing=T)[2:11]


## ----------------------------------------------------------------
sort(ct['whole milk', ])[2:11]


## ----------------------------------------------------------------
mosaicplot(as.matrix(ct[1:10, 1:10]), color=T, title(main='Products Contingency Mosaic Plot'), las=2)


## ----------------------------------------------------------------
ct <- crossTable(Groceries, measure="count", sort=TRUE)
ct[1:4, 1:4]


## ----------------------------------------------------------------
ct <- crossTable(Groceries, measure="support", sort=TRUE)
ct[1:4, 1:4]


## ----------------------------------------------------------------
ct <- crossTable(Groceries, measure="lift", sort=TRUE)
ct[1:4, 1:4]


## ----------------------------------------------------------------
sort(itemFrequency(Groceries, type="absolute"), decreasing = TRUE)[1:12]


## ----------------------------------------------------------------
itemFrequencyPlot(Groceries,topN=12,type="absolute")


## ----------------------------------------------------------------
df<-data.frame(ID = c(1,1,1,2,2,3), items=c('Item1', 'Item2', 'Item3', 'Item2', 'Item1', 'Item4'))
df


## ----------------------------------------------------------------
item_by_ID<-split(df[ , 'items'], df[ , 'ID'])
trans_data<-as(item_by_ID, 'transactions')
class(trans_data)
inspect(trans_data)


## ----------------------------------------------------------------
crossTable(trans_data, measure='count', sort=T)


## ----------------------------------------------------------------
metric.params <- list(supp=0.001, conf=0.5)
rules <- apriori(Groceries, parameter = metric.params)


## ----------------------------------------------------------------
inspect(rules[1:5])


## ----------------------------------------------------------------
prune.dup.rules <- function(rules){
  rule.subset.matrix <- is.subset(rules, rules, sparse = FALSE)
  rule.subset.matrix[lower.tri(rule.subset.matrix, diag=T)]<-NA
  dup.rules <- colSums(rule.subset.matrix, na.rm=T) >= 1
  pruned.rules <- rules[!dup.rules]
  return(pruned.rules)
}


## ----------------------------------------------------------------
rules.conf <- sort(rules, by="confidence", decreasing=TRUE)
rules.conf2 <- prune.dup.rules(rules.conf)
inspect(rules.conf2[1:5])


## ----------------------------------------------------------------
rules.lift<-sort(rules, by="lift", decreasing=TRUE)
rules.lift2 <- prune.dup.rules(rules.lift)
inspect(rules.lift2[1:5])


## ----------------------------------------------------------------
metric.params <- list(supp=0.001,conf = 0.5, minlen=2)
rules.R<-apriori(data=Groceries, parameter=metric.params,
               appearance = list(default="lhs",rhs="soda"),
               control = list(verbose=F))
rules.R <- prune.dup.rules(rules.R)
rules.R<-sort(rules.R, decreasing=TRUE, by="confidence")
inspect(rules.R[1:5])


## ----------------------------------------------------------------
metric.params <- list(supp=0.001,conf = 0.3, minlen=2)
rules.L<-apriori(data=Groceries, parameter=metric.params,
               appearance = list(default="rhs",lhs=c("yogurt", "sugar")),
               control = list(verbose=F))
rules.L<-sort(rules.L, decreasing=TRUE, by="confidence")
inspect(rules.L[1:5])

## ----------------------------------------------------------------
## ----------------------------------------------------------------
data("Adult")
class(Adult)



## ----------------------------------------------------------------
## ----------------------------------------------------------------
library(recommenderlab)
data("Jester5k")
set.seed(1234)
r<-sample(Jester5k, 1000)


## ----------------------------------------------------------------
best <- which.max(colMeans(Jester5k))
cat(JesterJokes[best])


## ----------------------------------------------------------------
as(r[1,], 'list')


## ----------------------------------------------------------------
rowMeans(r[1,])


## ----------------------------------------------------------------
par(mfrow=c(1,2))
hist(getRatings(r), breaks=100)
hist(getRatings(normalize(r)), breaks=100)


## ----------------------------------------------------------------
par(mfrow=c(1,2))
hist(rowCounts(r), breaks=50)
hist(colMeans(r), breaks=20)


## ----------------------------------------------------------------
r <- Recommender(Jester5k[1:1000], method = "POPULAR")
recom<-predict(r, Jester5k[1001:1002], n=5)
as(recom, 'list')


## ----------------------------------------------------------------
recom <- predict(r, Jester5k[1001:1002], type="ratings")
recom
as(recom, 'matrix')[, 1:10]


## ----------------------------------------------------------------
e <- evaluationScheme(Jester5k[1:1000], method="split", train=0.9, given=15, goodRating=5)
r1 <- Recommender(getData(e, "train"), "UBCF")
r2 <- Recommender(getData(e, "train"), "IBCF")
p1 <- predict(r1, getData(e, "known"), type="ratings")
p2 <- predict(r2, getData(e, "known"), type="ratings")
p2


## ----------------------------------------------------------------
error <- rbind(UBCF = calcPredictionAccuracy(p1, getData(e, "unknown")),
              IBCF = calcPredictionAccuracy(p2, getData(e, "unknown")))
error 


## ----------------------------------------------------------------
scheme <- evaluationScheme(Jester5k[1:1000], method="split", train = .9,
k=1, given=20, goodRating=5)

algorithms <- list(
"random items" = list(name="RANDOM", param=NULL),
"popular items" = list(name="POPULAR", param=NULL),
"user-based CF" = list(name="UBCF", param=list(nn=50)),
"item-based CF" = list(name="IBCF", param=list(k=50)))

results <- evaluate(scheme, algorithms, type = "topNList", n=c(1, 3, 5, 10, 15, 20))


## ----------------------------------------------------------------
plot(results, annotate=c(1,3), legend="bottomright")
plot(results, "prec/rec", annotate=3, legend="topleft")


