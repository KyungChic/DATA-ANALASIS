



mx.ex = matrix(
  c(1,1,1,1,0,
    1,1,0,1,0,
    1,0,0,1,1,
    1,1,1,0,0,
    1,1,1,0,0),ncol=5, byrow = TRUE
)

rownames(mx.ex) = c("p1","p2",'p3','p4','p5')
colnames(mx.ex) = c('a','b','c','d','e')

mx.ex





library(arules)

trx.ex = as(mx.ex, "transactions")
trx.ex

inspect(trx.ex)

summary(trx.ex)









data(Groceries)

head(Groceries)
str(Groceries)  
summary(Groceries)  

apr = apriori(Groceries, parameter = list(support=0.01, confidence = 0.3))  


inspect(sort(apr, by="lift")[1:10])  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
