#Part 1
climateChange = read.csv("climate_change.csv")
str(climateChange)
climateChangeTrain = subset(climateChange,climateChange$Year<=2006)
str(climateChangeTrain)
climateChangeTest = subset(climateChange,climateChange$Year>2006)
str(climateChangeTest)

lmClimate = lm(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols,data = climateChange) 
summary(lmClimate)

cor(climateChange)

lmClimate2 = lm(Temp~MEI+N2O+TSI+Aerosols,data = climateChangeTrain) 
summary(lmClimate2)

lmClimate = lm(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols,data = climateChangeTrain) 
summary(lmClimate)
stepModel = step(lmClimate)
summary(stepModel)

predCC= predict(stepModel,newdata=climateChangeTest)
SSE = sum((predCC-climateChangeTest$Temp)^2)
SST = sum((mean(climateChangeTrain$Temp)-climateChangeTest$Temp)^2)

R2 = 1- SSE/SST
R2
#Part 2
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")
str(pisaTrain)
tapply(pisaTrain$readingScore,pisaTrain$male,mean)
summary(pisaTrain)
pisaTrain = na.omit(pisaTrain)

pisaTest = na.omit(pisaTest)
str(pisaTrain)
str(pisaTest)
unique(pisaTrain$grade)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")

pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore~.,data = pisaTrain)
summary(lmScore)
str(lmScore)

SSE = sum(lmScore$residuals^2)
rmseScore = sqrt(SSE/nrow(pisaTrain))

rmseScore

29.542707*2

predTest = predict(lmScore,newdata=pisaTest)
summary(predTest)
637.7-353.2\

pisaTest
SSE = sum((predTest-pisaTest$readingScore)^2)
SSE
rmseScore = sqrt(SSE/nrow(pisaTest))
rmseScore

mean(pisaTrain$readingScore)
SST = sum((517.9629-pisaTest$readingScore)^2)
SST

R2 = 1 - SSE/SST
R2


#part 3


FluTrain = read.csv("FluTrain.csv")
summary(FluTrain)
FluTrain[FluTrain$ILI == max(FluTrain$ILI),]

FluTrain[FluTrain$Queries == max(FluTrain$Queries),]
hist(FluTrain$ILI)
plot(FluTrain$Queries,log(FluTrain$ILI))

FluModel = lm(log(ILI)~Queries,data = FluTrain)
summary(FluModel)


Correlation = cor(FluTrain$Queries, log(FluTrain$ILI))
Correlation^2
log(1/Correlation)
exp(-0.5*Correlation)

FluTest = read.csv("FluTest.csv")

PredTest1 = exp(predict(FluModel, newdata=FluTest))
March 11, 2012

head(FluTest)

PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]

obs = FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"),"ILI"]
pred= PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]

obs
RelativeError = (obs-pred)/obs
RelativeError

summary(PredTest1)

# Sum of Squared Errors
SSE = sum((PredTest1 - FluTest$ILI)^2)

# Root mean squared error
RMSE1 = sqrt(SSE/nrow(FluTest))
RMSE1



install.packages("zoo")

library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain)
head(FluTrain)


plot(log(FluTrain$ILILag2),log(FluTrain$ILI))

FluTrend2 = lm(log(ILI)~Queries+log(ILILag2),data= FluTrain)
summary(FluTrend2)

summary(FluModel)

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)

summary(FluTest)

n = nrow(FluTrain)
tail(FluTrain)
FluTrain$ILI[(n-1):(n)]

FluTest$ILILag2[1:2] = FluTrain$ILI[(n-1):n]
FluTest

PredTest2 = exp(predict(FluTrend2,newdata = FluTest ))
summary(PredTest2)

SSE2 = sum((PredTest2 - FluTest$ILI)^2)
RMSE2 = sqrt(SSE2/(nrow(FluTest)))
RMSE2
summary(FluTrend2)


head(FluTrain,6)

#Part 4(optional)
data(state)
help(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  
                  state.division, state.name, state.region)

str(statedata)

plot(statedata$x,statedata$y)
statedata$HS.grad
statedata$state.region
tapply(statedata$HS.Grad,statedata$state.region,mean)


boxplot(statedata$Murder~statedata$state.region)
statedata[statedata$state.region == "Northeast",c("state.name","Murder")]


LEModel1 = lm(Life.Exp~Population + Income + Illiteracy + Murder + HS.Grad + 
               Frost + Area, data = statedata)

LEModel2 = lm(Life.Exp~Population +  Murder + HS.Grad + 
               Frost , data = statedata)
summary(LEModel1)
summary(LEModel2)

predLE = predict(LEModel2)
plot(statedata$Income, statedata$Life.Exp)

sort(predLE)

statedata[statedata$Life.Exp == min(statedata$Life.Exp),c("state.name","Life.Exp")]

statedata[statedata$Life.Exp == max(statedata$Life.Exp),c("state.name","Life.Exp")]

sort(abs(LEModel2$residuals))

#Part 5

elantra = read.csv("elantra.csv")

str(elantra)
summary(elantra)
elantra_train = subset(elantra,Year <= 2012)
summary(elantra_train)
str(elantra_train)

elantra_test = subset(elantra,Year > 2012)
str(elantra_test)

SalesModel = lm(ElantraSales~Unemployment + CPI_all + CPI_energy + Queries,data = elantra_train)

summary(SalesModel)

SalesModel2 = lm(ElantraSales~Month + Unemployment + CPI_all + CPI_energy + Queries,data = elantra_train)
summary(SalesModel2)

SalesModel2

110.69 * 4

head(elantra_train)

SalesModel3 = lm(ElantraSales~as.factor(Month) + Unemployment + CPI_all + CPI_energy + Queries,data = elantra_train)
summary(SalesModel3)

cor(elantra_train)


SalesModel4 = lm(ElantraSales~as.factor(Month) + Unemployment +  CPI_all + CPI_energy ,data = elantra_train)
summary(SalesModel4)

PredSales = predict(SalesModel4,newdata = elantra_test)

SSE = sum((PredSales - elantra_test$ElantraSales)^2)
SSE

SST = sum((mean(elantra_train$ElantraSales) - elantra_test$ElantraSales)^2)
SST

mean(elantra_train$ElantraSales)

R2 = 1- (SSE/SST)
R2

max(abs(PredSales - elantra_test$ElantraSales))

elantra_test[which.max(abs(PredSales - elantra_test$ElantraSales)),]


plot((abs(PredSales - elantra_test$ElantraSales)),
     elantra_test$Year,text=elantra_test$Month)


