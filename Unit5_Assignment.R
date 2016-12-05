wiki = read.csv("wiki.csv",stringsAsFactors=F)
str(wiki)
head(wiki,1)
table(wiki$Vandal)


library(tm)

# Create corpus

corpusAdded = Corpus(VectorSource(wiki$Added))

corpusAdded[[1]]


corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))

corpusAdded = tm_map(corpusAdded, stemDocument)
# Create matrix

dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded
dtmAdded = removeSparseTerms(dtmAdded, 0.997)
dtmAdded

wordsAdded = as.data.frame(as.matrix(dtmAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))


# Create corpus

corpusRemoved = Corpus(VectorSource(wiki$Removed))

corpusRemoved[[1]]


corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))

corpusRemoved = tm_map(corpusRemoved, stemDocument)
# Create matrix

dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved
dtmRemoved = removeSparseTerms(dtmRemoved, 0.997)
dtmRemoved

wordsRemoved = as.data.frame(as.matrix(dtmRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

wikiWords = cbind(wordsAdded, wordsRemoved) 

wikiWords$Vandal = wiki$Vandal
table(wikiWords$Vandal)

library(caTools)
set.seed(123)
sample = sample.split(wikiWords$Vandal,0.7)
train = subset(wikiWords,sample == TRUE)
test = subset(wikiWords,sample == FALSE)

#Baseline Accuracy
table(test$Vandal)
618/(545+618)

#Building a CART model
library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal~., data=train, method="class")

prp(wikiCART)
#wikiPred = predict(wikiCART,newdata=test,type="class")
#wikiPred = wikiPred[,2]
#table(test$Vandal,wikiPred>=0.5)
str(wikiPred)
str(test$Vandal)

wikiPred = predict(wikiCART,newdata=test,type="class")
table(test$Vandal,wikiPred)

(618+12)/(618+12+533)

#Check the fitting on the triaining set
wikiPred = predict(wikiCART,type="class")
table(train$Vandal,wikiPred)
(1433+33)/(1433+33+1237)

#Improving "bag of words" algorithm by searching for web address in text.specifically for http.
wikiWords2 = wikiWords 
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, sample==TRUE)

wikiTest2 = subset(wikiWords2, sample==FALSE)


wiki2CART = rpart(Vandal~.,data=wikiTrain2,method="class")
prp(wiki2CART)
wiki2Pred = predict(wiki2CART,newdata=wikiTest2,type="class")
table(wiki2Pred,wikiTest2$Vandal)

(609+57)/(609+57+9+488)



wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded+wikiWords2$NumWordsRemoved)

mean(wikiWords2$NumWordsAdded)



wikiTrain2 = subset(wikiWords2, sample==TRUE)

wikiTest2 = subset(wikiWords2, sample==FALSE)
wiki2CART = rpart(Vandal~.,data=wikiTrain2,method="class")
prp(wiki2CART)
wiki2Pred = predict(wiki2CART,newdata=wikiTest2,type="class")
table(wiki2Pred,wikiTest2$Vandal)
(550+143)/(550+143+68+402)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain3 = subset(wikiWords3, sample==TRUE)
wikiTest3 = subset(wikiWords3, sample==FALSE)

wiki3CART = rpart(Vandal~.,data=wikiTrain3,method="class")
prp(wiki3CART)
wiki3Pred = predict(wiki3CART,newdata=wikiTest3,type="class")
table(wiki3Pred,wikiTest3$Vandal)
(539+292)/(539+292+79+253)


##Assignment 2

trials = read.csv("clinical_trial.csv",stringsAsFactors = FALSE)
str(trials)
sort(nchar(trials$abstract),decreasing = FALSE)

summary(nchar(trials$abstract))

nrow(subset(trials,nchar(trials$abstract) == 0))
table(nchar(trials$abstract) == 0)


sort(nchar(trials$title))

trials[nchar(trials$title) == min(nchar(trials$title)),"title"]
which.min(nchar(trials$title))
trials[1258,"title"]

library(tm)


# Create corpus

corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

corpusTitle[[1]]

# Convert to lower-case
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

# Remove punctuation

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)


# Remove stopwords and apple

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))


# Stem document 

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

# Create matrix

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmTitle
# Remove sparse terms

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

sort(colSums(as.matrix(dtmAbstract)))

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial

library(caTools)

set.seed(144)

spl = sample.split(dtm$trial, 0.7)

train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)

# Build a CART model

library(rpart)
library(rpart.plot)

trialCART = rpart(trial~., data=train, method="class")

prp(trialCART)

#Baseline Accuracy
table(test$trial)
313/(313+245)

# Make predictions on the training set
pred = predict(trialCART,data=train)
pred[1:10,]
pred.prob = pred[,2]
head(pred)
str(pred)
sort(pred.prob)
summary(pred.prob)

# Make predictions on the test set
# Compute accuracy
trialPred = predict(trialCART,newdata=test,type="class")
str(test)
str(train)

table(trialPred,test$trial)

table(train$trial,pred.prob>0.5)
(631+441)/(631+441+131+99)
#Sensitivity
441/(441+131)
#Specificity
631/(99+631)

trialPred = predict(trialCART,newdata=test,type="class")
str(test)
str(train)

table(trialPred,test$trial)
(261+162)/(261+162+52+83)

predTest = predict(trialCART, newdata=test)[,2]
table(test$trial, predTest >= 0.5)
predTest=predTest[,2]

library(ROCR)


perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

pred = prediction(predTest, test$trial)
# Compute AUC
as.numeric(performance(pred, "auc")@y.values)


## Assignment 3

emails = read.csv("emails.csv",stringsAsFactors = FALSE)
str(emails)
table(emails$spam)

which.max(nchar(emails$text))
nchar(emails$text[[2651]])

which.min(nchar(emails$text))

corpus = Corpus(VectorSource(emails$text))

corpus[[2]]

# Convert to lower-case
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

dtmText = DocumentTermMatrix(corpus)
dtmText
sparse = removeSparseTerms(dtmText, 0.95)


emailSparse = as.data.frame(as.matrix(sparse))
colnames(emailSparse) = make.names(colnames(emailSparse))

sort(colSums(as.matrix(dtmText)))
which.max(colSums(dtmText))



emailSparse$spam = emails$spam

sort(colSums(subset(emailSparse, spam == 0)))

sort(colSums(subset(emailSparse, spam == 1)))


emailSparse$spam = as.factor(emailSparse$spam)
set.seed(123)
spl = sample.split(emailSparse$spam,.7)
Train = subset(emailSparse,spl==TRUE)
Test = subset(emailSparse,spl==FALSE)

SpamLog = glm(spam~.,data=Train,family="binomial")
pred = predict(SpamLog,type="response")

table(pred <.00001)
table(pred >.99999)

table(pred > 0.00001 & pred < 0.99999)

summary(SpamLog)
table(Train$spam,pred > 0.5)
(3054+954)/(3054+954+0+4)

library(ROCR)

predROCR = prediction(pred, Train$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values


#CART

spamCART = rpart(spam~., data=Train, method="class")
prp(spamCART)
predTrainCART = predict(spamCART,type="class")
table(Train$spam,predTrainCART)

(2885+894)/nrow(Train)
predTrainCART = predict(spamCART)[,2]
predCARTROCR = prediction(predTrainCART, Train$spam)
perfCARTROCR = performance(predCARTROCR, "tpr", "fpr")

plot(perfCARTROCR, colorize=TRUE)
performance(predCARTROCR, "auc")@y.values


#random Forest

set.seed(123)

spamRF = randomForest(spam~., data=Train) 
predTrainRF = predict(spamRF, type="prob")[,2] 
table(Train$spam,predTrainRF > 0.5)
(3015+916)/nrow(Train)

predTrainRFROCR = prediction(predTrainRF, Train$spam)
perfDFROCR = performance(predTrainRFROCR, "tpr", "fpr")

plot(perfDFROCR, colorize=TRUE)
performance(predTrainRFROCR, "auc")@y.values


#test set accuracy
pred = predict(SpamLog,type="response",newdata=Test)
table(Test$spam,pred > 0.5)
(1257+376)/nrow(Test)
predROCR = prediction(pred, Test$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values



wordCount = rowSums(as.matrix(dtmText))
wordCount

hist(wordCount)
hist(log(wordCount))

emailSparse$logWordCount = log(wordCount)
str(emailSparse$spam)
boxplot(logWordCount~spam,data=emailSparse)

set.seed(123)
spl = sample.split(emailSparse$spam,.7)
Train2 = subset(emailSparse,spl==TRUE)
Test2 = subset(emailSparse,spl==FALSE)


spam2CART = rpart(spam~., data=Train2, method="class")
prp(spam2CART)
predCART = predict(spam2CART,newdata= Test2)[,2]
table(Test2$spam,predCART >0.5)
(1224+386)/nrow(Test2)
predROCR = prediction(predCART, Test2$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values


spam2RF = randomForest(spam~., data=Train2) 
predTest2RF = predict(spam2RF, newdata=Test2, type="prob")[,2] 
table(Test2$spam,predTest2RF >0.5)
predROCR = prediction(predTest2RF, Test2$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

