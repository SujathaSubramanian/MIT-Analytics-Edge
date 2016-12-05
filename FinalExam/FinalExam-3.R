hubwayTrips = read.csv("HubwayTrips.csv",stringsAsFactors=FALSE)
summary(hubwayTrips)

mean(hubwayTrips$Duration[hubwayTrips$Weekday==0])

tapply(hubwayTrips$Duration,hubwayTrips$Weekday,mean)

table(hubwayTrips$Morning)
table(hubwayTrips$Afternoon)
table(hubwayTrips$Evening)


table(hubwayTrips$Male)
136505/nrow(hubwayTrips)


library(caret)
preproc = preProcess(hubwayTrips)
HubwayNorm = predict(preproc, hubwayTrips)

summary(HubwayNorm)
k=10
#USing Kmeans clustering
set.seed(5000)
KMC = kmeans(HubwayNorm, centers = k, iter.max = 1000)
str(KMC)

# Extract clusters
table(KMC$cluster)

KMC$centers



set.seed(8000)
k=20
KMC = kmeans(HubwayNorm, centers = k, iter.max = 1000)
str(KMC)

# Extract clusters
table(KMC$cluster)

KMC$centers



data=tapply(HubwayNorm$Age,KMC$cluster,length)
data = as.data.frame(data)
str(ageData)
ggplot(data,aes(y=data,x=1:20)) + geom_bar()
