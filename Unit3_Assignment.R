## Assignment 1
songs = read.csv("songs.csv")
str(songs)
songs2010 = subset(songs,year=="2010")
str(songs2010)
summary(songs2010)

songsMJ = subset(songs,artistname == "Michael Jackson")
str(songsMJ)

x=songsMJ[songsMJ$Top10 == 1,]
x$songtitle

songsMJ[c("songtitle","Top10")]
unique(songs$timesignature)



table(songs$timesignature)
summary(songs$tempo_confidence)
class(songs$tempo_confidence)
songs[songs$tempo == max(songs$tempo),"songtitle"]


# Create training and testing sets
SongsTrain = subset(songs, year <= "2009")
SongsTest = subset(songs, year == "2010")
str(SongsTrain)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

SongsTrain = SongsTrain[,!names(SongsTrain) %in% nonvars]
SongsTest = SongsTest[,!names(SongsTest) %in% nonvars]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

cor(SongsTrain$loudness,SongsTrain$energy)
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)


pred1 = predict(SongsLog3, newdata =SongsTest, type="response")
table(SongsTest$Top10, pred1 >= 0.45)

Model3Accuracy = (309+19)/(373)

Sensitivity = 19/(40+19)
Specificity = 309/(309+5)


##Assignment No 2

parole = read.csv("parole.csv")
str(parole)
summary(parole)

table(parole$violator)

parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole)
head(parole)

set.seed(144)

library(caTools)

split = sample.split(parole$violator, SplitRatio = 0.7)

train = subset(parole, split == TRUE)

test = subset(parole, split == FALSE)

model = glm(violator~.,data=train,family="binomial")

summary(model)

logit = -4.2411574 + 0.3869904 + 0.8867192 + (-0.0001756 * 50)+(-0.1238867 * 3.0)+
    (0.0802954 * 12) + 0.6837143
logit
head(parole)
odds = exp(logit)
odds

prob = 1/(1+(exp(-logit)))
prob
library(ROCR)

pred1 = predict(model, newdata =test, type="response")
table(test$violator, pred1 >= 0.4)
summary(pred1)

ROCRpred = prediction(pred1, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)

ROCRperf

# Plot ROC curve
plot(ROCRperf)

24/202 

12/(11+12)
167/(167+12)
(167+12)/(167+12+11+12)
(179/202)
table(test$violator)


##Assignment 3

loans = read.csv("loans.csv")
table(loans$not.fully.paid)
1533/9578
summary(loans)

missing = subset(loans, is.na(log.annual.inc) | 
                   is.na(days.with.cr.line) | is.na(revol.util) 
                 | is.na(inq.last.6mths) | is.na(delinq.2yrs) 
                 | is.na(pub.rec))

summary(missing)



library(mice)

set.seed(144)

vars.for.imputation = setdiff(names(loans), "not.fully.paid")

imputed = complete(mice(loans[vars.for.imputation]))

loans[vars.for.imputation] = imputed

set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)

LoansTrain = subset(loans, split == TRUE)
str(LoansTrain)
LoansTest = subset(loans, split == FALSE)

model = glm(not.fully.paid ~.,data=LoansTrain,family="binomial")
summary(model)


logitA = -9.398e-03*700
logitA
logitB = -9.398e-03*710
logitB
logitA-logitB

oddsA = exp(logitA)
oddsB = exp(logitB)

oddsA/oddsB


pred1 = predict(model, newdata =LoansTest, type="response")
table(LoansTest$not.fully.paid, pred1 >= 0.5)

Model3Accuracy = (2400+3)/(2400+3+13+457)
Model3Accuracy

#BaselineModel Accuracy

table(LoansTest$not.fully.paid)

2413/(3413+460)
library(ROCR)


ROCRpred = prediction(pred1, LoansTest$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)


bivariate = glm(not.fully.paid~int.rate, data=LoansTrain, family="binomial")

summary(bivariate)

bivariatepred = predict(bivariate, newdata =LoansTest, type="response")
summary(bivariatepred)
table(LoansTest$not.fully.paid, bivariatepred >= 0.5)
ROCRpred = prediction(bivariatepred, LoansTest$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

#compound interest
c= 10
r=.06
t=3
ci = c* exp(r*t) 
ci

LoansTest$profit = exp(LoansTest$int.rate*3) - 1

LoansTest$profit[LoansTest$not.fully.paid == 1] = -1

max(LoansTest$profit)*10


i = 0.1071
exp(i*3)

summary(loans$int.rate)
highInterest = subset(loans,int.rate >=.15)
summary(highInterest$Profit)

bivariateHIpred = predict(bivariate, newdata =highInterest, type="response")

lowPred=sort(bivariateHIpred)[1:100]
sort(names(lowPred))

lowRisk=subset(highInterest,row.names(highInterest) %in% names(lowPred))

table(lowRisk$not.fully.paid)

summary(lowRisk$int.rate)

lowRisk$profit = exp(lowRisk$int.rate*3) - 1

lowRisk$profit[lowRisk$not.fully.paid == 1] = -1

mean(LoansTest$profit)

highInterest$predicted.risk = bivariateHIpred
head(highInterest)

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest,predicted.risk <= cutoff)

nrow(selectedLoans)

selectedLoans = subset(highInterest, predicted.risk <= cutoff) 
selectedLoans$profit = exp(selectedLoans$int.rate*3) - 1

selectedLoans$profit[selectedLoans$not.fully.paid == 1] = -1

table(selectedLoans$not.fully.paid) 
sum(selectedLoans$profit)
25/102
mean(selectedLoans$profit,na.rm=T)

table(selectedLoans$not.fully.paid)

25/102


#Optional Exercise
baseball = read.csv("baseball.csv")
str(baseball)

x = ddply(baseball,.(Team,Year),nrow)

length(table(baseball$Year))

baseball = subset(baseball,Playoffs == 1)
x = ddply(baseball,.(Team,Year),nrow)
nrow(x)

table(table(baseball$Year))

PlayoffTable = table(baseball$Year)
PlayoffTable

str(names(PlayoffTable))

PlayoffTable[c("1990","2001")]

baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)] 

table(baseball$NumCompetitors)

baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
nrow(subset(baseball,WorldSeries == 0))
table(baseball$WorldSeries)

str(baseball)
M1 = glm(WorldSeries ~ RA, data=baseball,family="binomial")
summary(M1)



#multi variate models

M1 = glm(WorldSeries ~ Year+RA+RankSeason+NumCompetitors, data=baseball,family="binomial")
summary(M1)

pairs(~Year+RA+RankSeason+NumCompetitors, data=baseball,
      main="Simple Scatterplot Matrix",
      lower.panel = panel.smooth, upper.panel = panel.cor)

cor(baseball[c("Year","RA", "RankSeason", "NumCompetitors")])

Model1 = glm(WorldSeries ~ Year + RA, data=baseball, family=binomial)

Model2 = glm(WorldSeries ~ Year + RankSeason, data=baseball, family=binomial)

Model3 = glm(WorldSeries ~ Year + NumCompetitors, data=baseball, family=binomial)

Model4 = glm(WorldSeries ~ RA + RankSeason, data=baseball, family=binomial)

Model5 = glm(WorldSeries ~ RA + NumCompetitors, data=baseball, family=binomial)

Model6 = glm(WorldSeries ~ RankSeason + NumCompetitors, data=baseball, family=binomial)



