df <- read.csv("IcecreamTrends.csv",header=T)
ic <- df$icecream

plot(ic, main = "Ice Cream", col="cyan", type = 'l', ylab = "Search Value", xlab = "Months")
Dic <- diff(ic)
sd(ic)
sd(Dic)
plot(Dic, main = "Differenced Ice Cream", col="cyan", type = 'l', ylab = "Differneced Search Value", xlab = "Months")

lagsic <- embed(Dic,13)
ict <- lagsic[,1]
ict1 <- lagsic[,2]
ict2 <- lagsic[,3]
ict3 <- lagsic[,4]
ict12 <- lagsic[,13]

ARic <- lm(ict~ict1+ict2+ict3+ict12)
summary(ARic)
ARicr<- lm(ict~ict12)
anova(ARicr,ARic,test="F")







df <- read.csv("PeppermintTrends.csv",header=T)
ic <- df$icecream
pm <- df$peppermint 
pmic <- df$pepperminticecream

plot(ic, main = "Ice Cream", col="cyan", type = 'l', ylab = "Search Value", xlab = "Weeks")
Dic <- diff(ic)
sd(ic)
sd(Dic)
plot(Dic, main = "Differenced Ice Cream", col="cyan", type = 'l', ylab = "Differneced Search Value", xlab = "Weeks")

plot(pm, main = "Peppermint", col="red", type = 'l', ylab = "Search Value", xlab = "Weeks")
Dpm <- diff(pm)
sd(pm)
sd(Dpm)
plot(Dpm, main = "Differenced Peppermint", col="red", type = 'l', ylab = "Differneced Search Value", xlab = "Weeks")

plot(pmic, main = "Peppermint Ice Cream", col="magenta", type = 'l', ylab = "Search Value", xlab = "Weeks")
Dpmic <- diff(pmic)
sd(pmic)
sd(Dpmic)
plot(Dpmic, main = "Differenced Peppermint Ice Cream", col="magenta", type = 'l', ylab = "Differneced Search Value", xlab = "Weeks")

lagsic <- embed(ic,4)
ict <- lagsic[,1]
ict1 <- lagsic[,2]
ict2 <- lagsic[,3]
ict3 <- lagsic[,4]

lagspm <- embed(pm,4)
pmt <- lagspm[,1]
pmt1 <- lagspm[,2]
pmt2 <- lagspm[,3]
pmt3 <- lagspm[,4]

lagspmic <- embed(pmic,4)
pmict <- lagspmic[,1]
pmict1 <- lagspmic[,2]
pmict2 <- lagspmic[,3]
pmict3 <- lagspmic[,4]

reg <- lm(pmict~pmict1+pmict2+pmt+pmt1+pmt2+pmt3+ict+ict1)
summary(reg)
regr<- lm(pmict~pmict1+pmict2+pmt+pmt1+pmt2+pmt3)
anova(regr,reg,test="F")
