barerData <- read.csv("~/Downloads/holtProject/barerData.csv")
nuData <- melt(barerData, id=1:7)
allTrees <- read.csv("../barerData.csv",check.names = FALSE)
nuTrees <- melt(allTrees, id=1:7)
library("reshape2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
wideTrees <- read.csv("/var/folders/vl/1wbs139s4gx0tzbdbd59w4jm0000gp/T//Rtmp5OYHFv/data5f848a266b", check.names=FALSE)
longTrees <- melt(wideTrees, id=1:7, variable.name = "Year", value.name = "Length")
attach(longTrees)
plot(Year,Length)
longTrees.ordered <- longTrees[order(Tree),]

# Feb 24
## Read csvs and set up dataframes
# at Holt:
bareTrees <- read.csv("~/project/calcs/bareData.csv", check.names=FALSE)
bareGaps <- read.csv("~/project/gapsText.csv")
# on mine:
bareTrees <- read.csv("~/Downloads/classes/project/bareData.csv", check.names=FALSE)
bareGaps <- read.csv("~/Downloads/classes/project/gapsText.csv")

library("reshape2", lib.loc="/usr/local/lib/R/site-library")
longTrees <- melt(bareTrees, id=1:7, variable.name = "Year", value.name = "DHt")
#gapTrees <- merge(bareTrees,bareGaps, by="Gap")
lGapTrees <- merge(longTrees,bareGaps,by="Gap")
#ordGapTrees <- lGapTrees[order(Gap,Tree,Year)]
#ordGapTrees <- lGapTrees[order(lGapTrees$Gap,lGapTrees$Tree,lGapTrees$Year)]
ordGapTrees <- lGapTrees[order(lGapTrees$Gap,lGapTrees$Tree,lGapTrees$Year), ]
## colnames(ordGapTrees)[colnames(ordGapTrees)=="Length"]<- "DHt"
ordGapTrees$Time <- as.numeric(as.character(Year)) - 1986
harvestTrees <- ordGapTrees[ordGapTrees$Time > -1,]
cum.na <- function(x) {
x[which(is.na(x))] <- 0
return(cumsum(x))
}
harvestTrees[,"CumHt"] <- ave(DHt,by=Tree, FUN=cum.na)

save(ordGapTrees, harvestTrees, file="treesData.RData", ascii=TRUE)


# Feb 25
names(ordGapTrees)
yr2000 <- ordGapTrees[Year=="2000", ]
plot(Area,DHt)
plot(Gap,DHt)
plot(yr2000$Gap,yr2000$DHt)
plot(yr2000$RadDHt,yr2000$DHt)
plot(yr2000$Area,yr2000$DHt)
ml2000HA <- lm(yr2000$DHt~yr2000$Area)
plot(ml2000HA)
mlHA <- lm(DHt~Area)
plot(mlHA)
anova(ml2000HA)
anova(mlHA)
summary(ml2000HA)
summary(mlHA)
plot(Area,DHt)
abline(mlHA)
summary(mlHA)
pairs(yr2000,panel=panel.smooth)
pairs(ordGapTrees,panel=panel.smooth)
library("mgcv", lib.loc="C:/Program Files/R/R-3.2.3/library")
mdAPV<- gam(DHt~s(Area)+s(Perim)+s(Volume))
md00APV<- gam(yr2000$DHt~s(yr2000$Area)+s(yr2000$Perim)+s(yr2000$Volume))
library("tree", lib.loc="~/R/win-library/3.2")

# Feb 28
# attempting a little mixed models
library(nlme)
mmAPV<- lme(DHt~Area*Perim*Volume, random=~1|Gap/Tree, na.action=na.omit)
mmAPV.e<- lme(DHt~Area*Perim*Volume, random=~1|Gap/Tree, na.action=na.exclude)
summary(mmAPV)
# mmAPV.2<- lme(DHt~(Area+Perim+Volume)^2, random=~1|Gap/Tree, na.action=na.pass)
mmAPV.2<- lme(DHt~(Area+Perim+Volume)^2, random=~1|Gap/Tree, na.action=na.exclude)
plot(mmAPV.2)
plot(mmAPV.2,Dht~fitted(.))
mmAPL<- lme(DHt~Area*Perim*RadLength, random=~1|Year/Gap/Tree, na.action=na.omit)
mmAPV<- lme(DHt~Area*Perim*Volume, random=~1|Year/Gap/Tree, na.action=na.omit)
mmAPV.2<- lme(DHt~(Area+Perim+Volume)^2, random=~1|Year/Gap/Tree, na.action=na.exclude)
plot(mmAPV.2)
plot(mmAPV.2,DHt~fitted(.))
summary(mmAPV.2)

meHrvTr <- lme(DHt~Area*Circularity*Volume*CumHt, random=~1|Year/Gap/Tree, na.action=na.omit)
meHrvTr <- lme(DHt~Volume + Circularity*Volume + Circularity*CumHt + Volume*CumHt + Area*Circularity*CumHt + Area*Volume*CumHt + Area*Circularity*Volume*CumHt*, random=~1|Year/Gap/Tree, na.action=na.omit)

# saving state
save(ordGapTrees, harvestTrees, file="treesData.RData", ascii=TRUE)
load("treesData.RData")

TEST CHANGE