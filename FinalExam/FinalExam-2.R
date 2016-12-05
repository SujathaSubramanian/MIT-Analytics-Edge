eBay = read.csv("ebay.csv",stringsAsFactors=FALSE)

table(eBay$sold)
799/nrow(eBay)

summary(eBay$startprice)
summary(eBay$saleprice)
summary(eBay$sold)
summary(eBay$biddable)

table(eBay$size)

eBay$sold = as.factor(eBay$sold)

eBay$condition = as.factor(eBay$condition)

eBay$heel = as.factor(eBay$heel)

eBay$style = as.factor(eBay$style)

eBay$color = as.factor(eBay$color)

eBay$material = as.factor(eBay$material)


set.seed(144)

library(caTools)

spl = sample.split(eBay$sold, 0.7)
training = subset(eBay,spl==TRUE)
test = subset(eBay,spl==FALSE)


model = glm(sold~ biddable+startprice+condition+heel+style+color+material, data=training,family="binomial")
table(training$sold)
summary(model)



auction (biddable=0)
start price $100
condition "Pre-owned"
"High" heels
style "Open Toe"
color "Black"
material "Satin"
str(training)
table(training$style)

logodds = 0.5990788+(-0.0044423)*100+(-0.4952981)*1+(0.1224260)*1+0.2226547*1+(-1.1078098)*1

odds = exp(logodds)

prob = odds/(1+odds)


#predict on the test set

pred = predict(model,newdata=test,type="response")
table(pred>0.5)

table(test$sold)

ROCRpred = prediction(pred, test$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance( ROCRpred, "tpr", "fpr" )
plot( perf ,colorize=TRUE)

#Kfold cross validation to train using CART model. Finding the cp value with highest accuracy on the training set
set.seed(144)
library(caret)

library(caret)

tc <- trainControl("cv",10)

rpart.grid <- expand.grid(.cp=seq(from=.001,to=.05,length.out=50)) 

train.rpart <- train(sold ~ biddable + startprice + condition + heel+ style +color + material,
                     data=training, method="rpart",
                     trControl=tc,  tuneGrid=rpart.grid)

CARTmodel = rpart(sold ~ biddable + startprice + condition + heel+ style +color + material, data=training,cp=.005)
prp(CARTmodel)

##Building the corpus
library(tm)

# Create corpus

corpus = Corpus(VectorSource(eBay$description))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, 0.90)

max(colSums(as.matrix(dtm)))
dtmDescription = as.data.frame(as.matrix(spdtm))

summary(colSums(dtmDescription))

summary(dtmDescription)

descriptionText = dtmDescription

names(descriptionText) = paste0("D", names(descriptionText))

descriptionText$sold = eBay$sold

descriptionText$biddable = eBay$biddable

descriptionText$startprice = eBay$startprice

descriptionText$condition = eBay$condition

descriptionText$heel = eBay$heel

descriptionText$style = eBay$style

descriptionText$color = eBay$color

descriptionText$material = eBay$material

trainText = subset(descriptionText,spl==TRUE)
testText = subset(descriptionText,spl==FALSE)

str(testText)


model = glm(sold~.,data=trainText,family="binomial")
summary(model)

##Training set AUC
pred = predict(model,type="response")
ROCRpred = prediction(pred, trainText$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)


##Test set AUC

pred = predict(model,newdata=testText,type="response")
ROCRpred = prediction(pred, testText$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)

