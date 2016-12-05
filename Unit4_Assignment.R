Gerber = read.csv("gerber.csv")
str(Gerber)
table(Gerber$voting)/nrow(Gerber)
table(Gerber$hawthorne,Gerber$voting)/nrow(Gerber)
table(Gerber$civicduty,Gerber$voting)/nrow(Gerber)
table(Gerber$neighbors,Gerber$voting)/nrow(Gerber)
table(Gerber$self,Gerber$voting)/nrow(Gerber)


tapply(Gerber$voting,Gerber$hawthorne,mean)
tapply(Gerber$voting,Gerber$civicduty,mean)
tapply(Gerber$voting,Gerber$neighbors,mean)
tapply(Gerber$voting,Gerber$self,mean)

GerberFit= glm(voting~hawthorne+civicduty+neighbors+self,data=Gerber,family="binomial")

library(ROCR)
summary(GerberFit)
GerberPred = predict(GerberFit,type="response")

table(Gerber$voting, GerberPred >= 0.3)
(134513+51966)/(134513+51966+100875+56730)

table(Gerber$voting, GerberPred >= 0.5)
235388/(235388+108696)

ROCRpred = prediction(GerberPred, Gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance( ROCRpred, "tpr", "fpr" )
plot( perf )


CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=Gerber)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=Gerber,cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors+sex, data=Gerber,cp=0.0)
prp(CARTmodel3)

head(Gerber)
str(Gerber)

CARTmodel4 = rpart(voting ~ control, data=Gerber,cp=0.0)
prp(CARTmodel4,digits=6)
abs(.296638-.34)

CARTmodel5 = rpart(voting ~ control+sex, data=Gerber,cp=0.0)
prp(CARTmodel5,digits=6)
PredictCART = predict(CARTmodel5, newdata = Possibilities)

abs(.290456-.302795)
abs(.334176-.345818)


LogModelSex= glm(voting~control+sex,data=Gerber,family="binomial")
summary(GerberFit)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")

abs(0.2908065-0.2904558)
abs(0.2904558-0.2904558)



LogModel2 = glm(voting ~ sex + control + sex:control, data=Gerber, family="binomial")
summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")

CARTmodel5 = rpart(voting ~ control+sex, data=Gerber,cp=0.0)
prp(CARTmodel5,digits=6)
PredictCART = predict(CARTmodel5, newdata = Possibilities)


#Assignment 2
letters = read.csv("letters_ABPR.csv")
str(letters)
letters$isB = as.factor(letters$letter == "B")
table(letters$isB)
set.seed(1000)

s = sample.split(letters$isB,SplitRatio = .5)
head(s)
lettersTrain = subset(letters,s==TRUE)
lettersTest = subset(letters,s==FALSE)

table(lettersTrain$isB)

table(lettersTest$isB)

1175/(1175+383)


CARTb = rpart(isB ~ . - letter, data=lettersTrain, method="class")
PredictCART = predict(CARTb, newdata = lettersTest, type = "class")
table(PredictCART,lettersTest$isB)
#Model Accuracy
(1141+311)/(1141+311+34+72)

library(randomForest)

# Build random forest model

set.seed(1000)
# Try again
lettersTrain$isB = as.factor(lettersTrain$isB)
lettersTest$isB = as.factor(lettersTest$isB)
forest = randomForest(isB ~ .-letter, data = lettersTrain)

# Make predictions
PredictForest = predict(forest, newdata = lettersTest)
table(lettersTest$isB, PredictForest)

(1168+366)/(1168+366+17+7)


letters$letter = as.factor( letters$letter ) 
table(letters$isB)
set.seed(2000)

s = sample.split(letters$letter,SplitRatio = .5)
head(s)
lettersTrain = subset(letters,s==TRUE)
lettersTest = subset(letters,s==FALSE)

table(letters$letter)
table(lettersTest$letter)
401/(nrow(lettersTest))

#building tree model
treeModel = rpart(letter~.-isB,data=lettersTrain,method="class")
treePred = predict(treeModel,newdata = lettersTest,type="class")
table(lettersTest$letter,treePred)

(348+318+363+340)/nrow(lettersTest)

forestModel = randomForest(letter~.-isB,data=lettersTrain,method="class")
forestPred = predict(forestModel,newdata= lettersTest,type="class")
table(lettersTest$letter,forestPred)

(390+380+393+369)/nrow(lettersTest)

#Assignment 3

census = read.csv("census.csv")
set.seed(2000)
ss = sample.split(census,SplitRatio=.6)
censusTrain=subset(census,ss==TRUE)
censusTest=subset(census,ss==FALSE)
censusModel = glm(over50k~.,data=censusTrain,family="binomial")
summary(censusModel)
censusPred = predict(censusModel,newdata=censusTest,type="response")

table(censusTest$over50k,censusPred >=0.5)

pred = prediction(censusPred, censusTest$over50k)
perf = performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)



censusTree = rpart(over50k~.,data=censusTrain,method="class")
prp(censusTree)

censusTreePred = predict(censusTree,newdata=censusTest,type="class")
table(censusTest$over50k,censusTreePred)

(10688+1780)/(10688+1780+1747+544)

censusTreePred = predict(censusTree,newdata=censusTest)
pred = prediction(censusTreePred[,2], censusTest$over50k)
perf = performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)


#random forests
set.seed(1)

trainSmall = censusTrain[sample(nrow(censusTrain), 2000), ]
set.seed(1)

censusForest = randomForest(over50k~.,data=trainSmall,method="class")
censusForestPred = predict(censusForest,newdata=censusTest,type="class")
table(censusTest$over50k,censusForestPred)
(11034+1175)/(11034+1175+2352+198)

#In random forest how to find out the number of times a variable is selected for a split

vu = varUsed(censusForest, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(censusForest$forest$xlevels[vusorted$ix]))
#reducing impurities in random forest
varImpPlot(censusForest)


#Find the new cp value
library(caret)
library(e1071)
numFolds = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) 

train(over50k~.,data=censusTrain, method = "rpart", trControl = numFolds, tuneGrid = cartGrid )


censusTree = rpart(over50k~.,data=censusTrain,cp=0.002,method="class")
prp(censusTree)
censusPred = predict(censusTree,newdata=censusTest,type="class")
table(censusTest$over50k,censusPred)
(10601+2038)/(10601+2038+1489+ 631)
