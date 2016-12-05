### 1
dailycos = read.csv("dailykos.csv", header=TRUE)
head(dailycos,1)

dailycos[1:5,1:10]
str(dailycos)

#dailycosMatrix = as.matrix(dailycos)
#str(dailycosMatrix)

# Turn matrix into a vector
#dailycosVector = as.vector(dailycosMatrix)
#str(dailycosVector)

# Compute distances

distance = dist(dailycos, method = "euclidean")

# Hierarchical clustering
clusterIntensity = hclust(distance, method="ward.D")

plot(clusterIntensity)
rect.hclust(clusterIntensity, k = 4, border = "red")

clusters = cutree(clusterIntensity, k = 7)

str(clusters)
table(clusters)
clusters

cluster1 = dailycos[clusters==1,]
cluster2 = dailycos[clusters==2,]
cluster3 = dailycos[clusters==3,]
cluster4 = dailycos[clusters==4,]
cluster5 = dailycos[clusters==5,]
cluster6 = dailycos[clusters==6,]
cluster7 = dailycos[clusters==7,]


tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

k = 7

# Run k-means
set.seed(1000)
KMC = kmeans(dailycos, centers = k)
str(KMC)
str(dailycos)
# Extract clusters
table(clusters)
table(KMC$cluster)
table(clusters,KMC$cluster)

cluster1 = dailycos[KMC$cluster==1,]
cluster2 = dailycos[KMC$cluster==2,]
cluster3 = dailycos[KMC$cluster==3,]
cluster4 = dailycos[KMC$cluster==4,]
cluster5 = dailycos[KMC$cluster==5,]
cluster6 = dailycos[KMC$cluster==6,]
cluster7 = dailycos[KMC$cluster==7,]

tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

### 2
airlines = read.csv("AirlinesCluster.csv", header=TRUE)
head(airlines)
str(airlines)


summary(airlines)


library(caret)
str(airlines)

preproc = preProcess(airlines)

airlinesNorm = predict(preproc, airlines)
#class(airlinesNorm)

str(airlinesNorm)
summary(airlinesNorm)

airlinesNormMatrix = as.matrix(airlinesNorm)
str(airlinesNormMatrix)

# Turn matrix into a vector
airlinesNormVector = as.vector(airlinesNormMatrix)
str(airlinesNormVector)

# Compute distances
distance = dist(airlinesNorm, method = "euclidean")
# Hierarchical clustering
clusterIntensity = hclust(distance, method="ward.D")

# Plot the dendrogram
plot(clusterIntensity)
rect.hclust(clusterIntensity, k = 2, border = "red")

clusters = cutree(clusterIntensity, k = 5)
str(clusters)
table(clusters)
# Find mean intensity values

str(airlinesNorm)
tapply(airlinesNorm$Balance, clusters, mean)
tapply(airlinesNorm$QualMiles, clusters, mean)
tapply(airlinesNorm$BonusMiles, clusters, mean)
tapply(airlinesNorm$BonusTrans, clusters, mean)
tapply(airlinesNorm$FlightMiles, clusters, mean)
tapply(airlinesNorm$FlightTrans, clusters, mean)
tapply(airlinesNorm$DaysSinceEnroll, clusters, mean)

lapply(split(airlines, clusters), colMeans)


k = 5

# Run k-means
set.seed(88)
KMC = kmeans(airlinesNorm, centers = k, iter.max = 1000)
str(KMC)

# Extract clusters
table(KMC$cluster)
KMC$centers[1:5]




###3

stocks = read.csv("StocksCluster.csv",header=TRUE)
str(stocks)
table(stocks$PositiveDec)
6324/(5256+6324)

pairs(stocks,lower.panel = panel.smooth, upper.panel = panel.cor)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
cor(stocks)

summary(stocks)

colMeans(stocks)
library(caTools)
set.seed(144)

spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)

Train = subset(stocks, spl == TRUE)

Test = subset(stocks, spl == FALSE)
str(Test)
Model = glm(PositiveDec~.,data=Train,family=binomial)

pred  = predict(Model,type="response")

table(Train$PositiveDec,pred>0.5)

(990+3640)/(990+3640+787+2689)

pred  = predict(Model,type="response",newdata=Test)
table(Test$PositiveDec,pred>0.5)

(417+1553)/(417+1553+344+1160)

#baseline accuracy
table(Test$PositiveDec)
1897/3474
1897/(1577 + 1897) 

table(pred>0.5)


limitedTrain = Train

limitedTrain$PositiveDec = NULL

limitedTest = Test

limitedTest$PositiveDec = NULL


library(caret)

preproc = preProcess(limitedTrain)
summary(preproc$mean)

normTrain = predict(preproc, limitedTrain)

normTest = predict(preproc, limitedTest)

colMeans(normTrain)
colMeans(normTest)

set.seed(144)
km= kmeans(normTrain,centers=3)
str(km)
table(km$cluster)

library(flexclust)

km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)


stockTrain1 = Train[clusterTrain==1,]
stockTrain2 = Train[clusterTrain==2,]
stockTrain3 = Train[clusterTrain==3,]

colMeans(stockTrain1)
colMeans(stockTrain2)
colMeans(stockTrain3)

stockTest1 = Test[clusterTest==1,]
stockTest2 = Test[clusterTest==2,]
stockTest3 = Test[clusterTest==3,]


Model1 = glm(PositiveDec~.,data=stockTrain1,family=binomial)
Model2 = glm(PositiveDec~.,data=stockTrain2,family=binomial)
Model3 = glm(PositiveDec~.,data=stockTrain3,family=binomial)

summary(Model1)
summary(Model2)
summary(Model3)
pred1 = predict(Model1,newdata= stockTest1,type="response")
pred2 = predict(Model2,newdata= stockTest2,type="response")
pred3 = predict(Model3,newdata= stockTest3,type="response")


table(stockTest1$PositiveDec,pred1>0.5)
(30+774)/(30+774+23+471)
table(stockTest2$PositiveDec,pred2>0.5)
(388+757)/(388+757+309+626)
table(stockTest3$PositiveDec,pred3>0.5)
(49+13)/(49+13+21+13)

AllPredictions = c(pred1,pred2,pred3)

AllOutcomes = c(stockTest1$PositiveDec, stockTest2$PositiveDec, stockTest3$PositiveDec)

table(AllOutcomes,AllPredictions>0.5)
(467+1544)/(467+1544+353+1110)
