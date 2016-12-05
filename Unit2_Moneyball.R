# VIDEO 2

# Read in data
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)


# VIDEO 3

str(moneyball)

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)
sqrt(0.9296)




RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)

RunsReg = lm(RA ~ OOBP + OSLG, data=moneyball)
summary(RunsReg)



-804.63+2737.77*.311+1584.91*.405

-837.38+2913.60*.297+1514.29*.370

639.6+862.1*.297-285.7*.370



-804.63+2737.77*.338+1584.91*.540
-804.63+2737.77*.391+1584.91*.450
-804.63+2737.77*.369+1584.91*.374
-804.63+2737.77*.313+1584.91*.447
-804.63+2737.77*.361+1584.91*.5


teamRank = c(1,2,3,3,4,4,4,4,5,5)

wins2012 = c(94,88,95,88,93,94,98,97,93,94)

wins2013 = c(97,97,92,93,92,96,94,96,92,90)



cor(teamRank,wins2012)
cor(teamRank,wins2013)

