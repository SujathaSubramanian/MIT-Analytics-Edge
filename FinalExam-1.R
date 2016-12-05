library(caTools)

Airlines = read.csv("AirlineDelay.csv",stringsAsFactors=FALSE)

set.seed(15071)

spl = sample(nrow(Airlines), 0.7*nrow(Airlines))

AirlinesTrain = Airlines[spl,]

AirlinesTest = Airlines[-spl,]


model = lm(TotalDelay~.,data=AirlinesTrain)
summary(model)
model$coefficients

cor(AirlinesTrain$NumPrevFlights ,AirlinesTrain$PrevFlightGap )
cor(AirlinesTrain$OriginAvgWind,AirlinesTrain$OriginWindGust )

cor(AirlinesTrain$AirlinesTrain[,5:22])
pairs(AirlinesTrain)


##Do prediction on test set. Compute SSE,SST,R2
pred = predict(model,newdata=AirlinesTest)

SSE = sum((pred-AirlinesTest$TotalDelay)^2)
SST = sum((mean(AirlinesTrain$TotalDelay) - AirlinesTest$TotalDelay)^2)
R2 = 1-SSE/SST

SSE
SST
R2


##Convert this into multi-class classification problem

Airlines$DelayClass = factor(ifelse(Airlines$TotalDelay == 0, "No Delay", ifelse(Airlines$TotalDelay >= 30, "Major Delay", "Minor Delay")))

table(Airlines$DelayClass)

Airlines$TotalDelay = NULL

set.seed(15071)

spl = sample.split(Airlines$DelayClass,0.7)
Train = subset(Airlines,spl==TRUE)
Test = subset(Airlines,spl==FALSE)

##Use rpart for multiclass classification

library(rpart)
library(rpart.plot)

CARTmodel = rpart(DelayClass ~ ., data=Train)
prp(CARTmodel)

pred = predict(CARTmodel,newdata=Train,type="class")

pred
table(pred,Train$DelayClass)

(3094+188)/6587


pred = predict(CARTmodel,newdata=Test,type="class")
table(pred,Test$DelayClass)
confusionMatrix(pred,Test$DelayClass)

summary(CARTmodel)
