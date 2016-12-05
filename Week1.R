#Section 1

mvt = read.csv("mvtWeek1.csv")
str(mvt)
mvt[which.max(mvt$ID),"ID"]
mvt[which.min(mvt$Beat),"Beat"]
nrow(mvt["Arrest" ==,])

summary(mvt$Arrest)
nrow(mvt[mvt$LocationDescription=="ALLEY",])
head(mvt$Date)
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
?table
table(months(mvt$Date))
table(weekdays(mvt$Date))
table(months(mvt$Date[mvt$Arrest == "TRUE"],))
head(mvt$Arrest)
hist(mvt$Date, breaks=100)
boxplot(mvt$Date[mvt$Arrest == "TRUE"])

Total2001 = mvt[format((mvt$Date),"%Y") == "2001",]
Total2001Arrest = mvt[format((mvt$Date),"%Y") == "2001" & mvt$Arrest=="TRUE",]
nrow(Total2001Arrest)/nrow(Total2001)

Total2007 = mvt[format((mvt$Date),"%Y") == "2007",]
Total2007Arrest = mvt[format((mvt$Date),"%Y") == "2007" & mvt$Arrest=="TRUE",]
nrow(Total2007Arrest)/nrow(Total2007)

Total2012 = mvt[format((mvt$Date),"%Y") == "2012",]
Total2012Arrest = mvt[format((mvt$Date),"%Y") == "2012" & mvt$Arrest=="TRUE",]
nrow(Total2012Arrest)/nrow(Total2012)

TopLocation = as.data.frame(head(sort(table(mvt$LocationDescription),decreasing=TRUE)))
TopLocation[,1]
names(TopLocation) <- c("Location","Count") 

nrow(mvt)
Top5 = subset(mvt,LocationDescription == "ALLEY")
Top5 = rbind(Top5,subset(mvt,LocationDescription == "GAS STATION"))
Top5 = rbind(Top5,subset(mvt,LocationDescription == "DRIVEWAY - RESIDENTIAL"))
Top5 = rbind(Top5,subset(mvt,LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)"))
Top5 = rbind(Top5,subset(mvt,LocationDescription == "STREET"))


TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")

Top5 = subset(mvt, LocationDescription %in% TopLocations)
str(Top5)
Top5$LocationDescription = factor(Top5$LocationDescription)
str(Top5)

table(Top5$LocationDescription,Top5$Arrest)

439/(439+1672)

table(weekdays(mvt$Date[mvt$LocationDescription == "GAS STATION"],))
table(weekdays(mvt$Date[mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL"],))

table(Top5$LocationDescription, Top5$Weekday)


# Section 2

IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
str(IBM)
str(GE)


summary(IBM)
summary(GE)
summary(CocaCola)
summary(Boeing)
sd(ProcterGamble$StockPrice)

plot(CocaCola$Date,CocaCola$StockPrice,type="l",col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)


plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="blue")
lines(IBM$Date, IBM$StockPrice,col="green")
lines(Boeing$Date, Boeing$StockPrice,col="purple")
lines(GE$Date, GE$StockPrice,col="orange")
abline(v=as.Date(c("1997-09-01")), lwd=2,col="red") 
abline(v=as.Date(c("1997-11-01")), lwd=2,col="red")

abline(v=as.Date(c("2004-01-01")), lwd=2,col="green") 
abline(v=as.Date(c("2005-12-01")), lwd=2,col="green")

tapply(IBM$StockPrice,months(IBM$Date),mean) >= mean(IBM$StockPrice)
tapply(ProcterGamble$StockPrice,months(ProcterGamble$Date),mean) 
tapply(CocaCola$StockPrice,months(CocaCola$Date),mean)
tapply(Boeing$StockPrice,months(Boeing$Date),mean) 
tapply(GE$StockPrice,months(GE$Date),mean) 



CPS = read.csv("CPSData.csv")
str(CPS)
table(CPS$Industry)
sort(table(CPS$State)) 
unique(CPS$Citizenship)

unique(x$Citizenship)
Citizen=subset(CPS,grepl("Native",Citizenship))
Citizen=rbind(Citizen,subset(CPS,grepl("Naturalized",Citizenship)))
nrow(Citizen)


123712/131302

table(CPS$Citizenship)
table(CPS$Race)
table(CPS$Race[CPS$Hispanic == 1])

table(CPS$Race,CPS$Hispanic)

anyNA(CPS$PeopleInHousehold)
anyNA(CPS$Region)
anyNA(CPS$Industry)
anyNA(CPS$PeopleInHousehold)
anyNA(CPS$Region)
anyNA(CPS$State)
anyNA(CPS$MetroAreaCode)
anyNA(CPS$Age)
  anyNA(CPS$Married)
anyNA(CPS$Sex)
anyNA(CPS$Education)
anyNA(CPS$Race)
anyNA(CPS$Hispanic)
anyNA(CPS$Citizenship)
anyNA(CPS$CountryOfBirthCode)
anyNA(CPS$EmploymentStatus)
  
summary(CPS)

table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))


str(CPS)
table(CPS$State,is.na(CPS$MetroAreaCode))

table(CPS$Region,is.na(CPS$MetroAreaCode))
tapply(is.na(CPS$MetroAreaCode),CPS$State,mean)

is.na(CPS$MetroAreaCode)


mean(c(TRUE,FALSE))


MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")
str(MetroAreaMap)
str(CountryMap)
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
str(CPS)
table(is.na(CPS$MetroArea))
summary(CPS)

sort(table(CPS$MetroArea))
tapply(is.na())

table(CPS$MetroArea,CPS$Hispanic)
sort(tapply(CPS$Hispanic,CPS$MetroArea,mean))

CPS$Hispanic
CPS$MetroCodeArea

sort(tapply(CPS$Race == "Asian",CPS$MetroArea,mean))

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE))

str(CPS)
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

summary(CPS)

tapply(CPS$Country != "United States",CPS$MetroArea,mean,na.rm=TRUE)


sort(tapply(CPS$Country == "India", CPS$MetroArea,sum,na.rm=TRUE))

sort(tapply(CPS$Country == "Brazil", CPS$MetroArea,sum,na.rm=TRUE))
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea,sum,na.rm=TRUE))


#Section 4

AnonymityPoll = read.csv("AnonymityPoll.csv")

str(AnonymityPoll)
table(AnonymityPoll$Smartphone)

summary(AnonymityPoll)

table(AnonymityPoll$State,AnonymityPoll$Region)

x=subset(AnonymityPoll,AnonymityPoll$Internet.Use == 0 & AnonymityPoll$Smartphone == 0,
         na.rm=TRUE)

y=subset(AnonymityPoll,AnonymityPoll$Internet.Use != 0 & AnonymityPoll$Smartphone != 0,
         na.rm=TRUE)

z=subset(AnonymityPoll,AnonymityPoll$Internet.Use == 1 & AnonymityPoll$Smartphone == 0,
         na.rm=TRUE)

a=subset(AnonymityPoll,AnonymityPoll$Internet.Use == 0 & AnonymityPoll$Smartphone == 1,
         na.rm=TRUE)



table(AnonymityPoll$Internet.Use, AnonymityPoll$Smartphone) 
summary(AnonymityPoll$Smartphone)

summary(AnonymityPoll$Internet.Use)

limited = subset(AnonymityPoll,AnonymityPoll$Internet.Use == 1 | AnonymityPoll$Smartphone == 1,
                 na.rm=TRUE)

str(limited)
summary(limited)

mean(limited$Info.On.Internet )

nrow(limited[limited$Info.On.Internet ==0,])

nrow(limited[limited$Info.On.Internet ==11,])


table(limited$Info.On.Internet,limited$Worry.About.Info)



x=subset(limited,limited$Worry.About.Info)
worry = nrow(x)
y=subset(x,x$Info.On.Internet > 0)
352/386

summary(limited)

nrow(limited$Worry.About.Info)



hist(limited$Age)
summary(limited$Worry.About.Info)


plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

table(limited$Age, limited$Info.On.Internet)

tapply(limited$Info.On.Internet,limited$Smartphone ,mean)
tapply(limited$Tried.Masking.Identity,limited$Smartphone ,mean,na.rm=TRUE) 
tapply(limited$Tried.Masking.Identity,limited$Smartphone ,table) 



max(table(limited$Age, limited$Info.On.Internet)) 
