setwd("~/Documents/diplomatic representation readings/humanitiesDataInR/data/ch03")
geodf <- read.csv("~/Documents/diplomatic representation readings/humanitiesDataInR/data/ch03/geodf.csv", as.is=TRUE)

############ 4.2 Scatter Plots page 47 #################
plot(geodf$households, geodf$population)
abline(0,1)
abline(0,2)
abline(0,3)
abline(v=quantile(geodf$households,prob=0.5), lty="dashed")
abline(h=quantile(geodf$households,prob=0.5), lty="dashed")

hhIncome <- read.csv("~/Documents/diplomatic representation readings/humanitiesDataInR/data/ch03/hhIncome.csv", 
                     as.is=TRUE,check.names=FALSE)
hhIncome <- as.matrix(hhIncome)

cexVals <- rep(0.5, nrow(geodf))
cexVals[geodf$csa == "Portland"] = 1
pchVals <- rep(19, nrow(geodf))
pchVals[geodf$csa == "Portland"] = 3

colVals <- rep(grey(0.2), nrow(geodf))
colVals[geodf$csa == "Portland"] <- grey(0.8)

plot(hhIncome[,"50k"]/hhIncome[,"total"],
     hhIncome[,"200k"]/hhIncome[,"total"],
     cex=cexVals,
     pch=pchVals,
     col=colVals)

####### 4.3 Text page 50 ##################





