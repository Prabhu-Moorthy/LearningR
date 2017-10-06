getwd();
mvt = read.csv("mvtWeek1.csv");
str(mvt);
names(mvt);
which.max(mvt$ID)
max(mvt$ID)
mvt$ID[18134]
min(mvt$Beat)
table(mvt$Arrest)
summary(mvt)
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
mvt$Months = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
summary(mvt)
mvt$Date = DateConvert
summary(mvt)
which.min(table(mvt$Months))
which.max(table(mvt$Weekday))
str(mvt$Arrest)
Arrestmade = subset(mvt,Arrest == TRUE)
str(Arrestmade)
which.max(table(Arrestmade$Months))
table(mvt$Arrest,mvt$Month)
hist(mvt$Date,breaks = 100)
boxplot(mvt$Date ~ mvt$Arrest)
table(mvt$Arrest,mvt$Year)
arrests_made_2007 = nrow(subset(mvt, Arrest == TRUE & Year == "2007"))
arrests_made_2007
arrests_made_tot = nrow(subset(mvt, Year == "2007"))
prop = arrests_made_2007 / arrests_made_tot
prop
sort(table(mvt$LocationDescription))
top5loctheft = subset(mvt, LocationDescription == "STREET" | LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription == "ALLEY" | LocationDescription == "GAS STATION" | LocationDescription == "DRIVEWAY - RESIDENTIAL")
nrow(top5loctheft)
top5loctheft$LocationDescription = factor(top5loctheft$LocationDescription)
top5loctheft$LocationDescription 
table(top5loctheft$LocationDescription) 
table(top5loctheft$LocationDescription, top5loctheft$Arrest)
