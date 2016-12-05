statesMap = map_data("state")
str(statesMap)
table(statesMap$group)
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") 
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") 

polling = read.csv("PollingImputed.csv")
str(polling)
Train = subset(polling,Year >= 2004 & Year <= 2008)
Test = subset(polling ,Year ==2012)
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")

TestPredictionBinary = as.numeric(TestPrediction > 0.5)
table(TestPredictionBinary)
mean(TestPrediction)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

str(predictionDataFrame)
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]

str(predictionMap)
str(statesMap)


ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")


ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black",alpha=0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black",size=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

summary(predictionMap)

head(predictionMap)

mean(predictionMap[predictionMap$region == "florida" ,"TestPrediction"])

?geom_polygon



##Assignment 2

edges = read.csv("edges.csv")
users = read.csv("users.csv")

str(users)
table(users$locale)
str(edges)
table(users$school)
(mean(table(edges$V1))+mean(table(edges$V2)))/2



merge()
head(edges)
head(users)
users[users$school == "AB",]
table(users$gender, users$school) 

table(users$school)

install.packages("igraph")
library(igraph)

?graph.data.frame
g=graph.data.frame(edges,FALSE,users)
plot(g, vertex.size=5, vertex.label=NA)
table(degree(g) >= 10)
V(g)$size = degree(g)/2+2
max(V(g)$size)
min(V(g)$size)

summary(degree(g))
plot(g, vertex.label=NA)

V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label=NA)

V(g)$color[V(g)$school == "A"] = "red"

V(g)$color[V(g)$school == "AB"] = "gray"

plot(g, vertex.label=NA)

table(users$locale)

V(g)$color[V(g)$locale == "A"] = "red"

V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)
?igraph.plotting


rglplot(g, vertex.label=NA)

#Assignment 3
library(tm)
tweetData = read.csv("tweets.csv",stringsAsFactors=FALSE)
tweets = Corpus(VectorSource(c(tweetData$Tweet)))
tweets = tm_map(tweets, tolower)
tweets = tm_map(tweets, PlainTextDocument)
tweets = tm_map(tweets, removePunctuation)
tweets = tm_map(tweets, removeWords, c(stopwords("english"),"apple"))
#tweets = tm_map(tweets, stemDocument)
dtmHeadline = DocumentTermMatrix(tweets)
allTweets = as.data.frame(as.matrix(dtmHeadline))
str(allTweets)
str(tweetData)
ncol(head(allTweets,1))

install.packages("wordcloud")
library(wordcloud)
?wordcloud

colnames(allTweets)
colSums(allTweets)


wordcloud(colnames(allTweets),colSums(allTweets),scale=c(4, 0.25))


wordcloud(colnames(allTweets),colSums(allTweets),scale=c(4, 0.25))


negativeTweets = tweetData[tweetData$Avg <= -1,]

ntweets = Corpus(VectorSource(c(negativeTweets$Tweet)))
ntweets = tm_map(ntweets, tolower)
ntweets = tm_map(ntweets, PlainTextDocument)
ntweets = tm_map(ntweets, removePunctuation)
ntweets = tm_map(ntweets, removeWords, c(stopwords("english"),"apple"))
#tweets = tm_map(tweets, stemDocument)
ndtmHeadline = DocumentTermMatrix(ntweets)
nallTweets = as.data.frame(as.matrix(ndtmHeadline))

wordcloud(colnames(nallTweets),colSums(nallTweets),scale=c(4, 0.25))


wordcloud(colnames(allTweets),colSums(allTweets),scale=c(4, 0.25))

library(RColorBrewer)
display.brewer.all() 

display.brewer.pal(7,"Accent")
display.brewer.pal(7,"Set2")
display.brewer.pal(7,"Greys")
brewer.pal.info
