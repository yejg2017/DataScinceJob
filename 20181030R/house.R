#Reading in the data
hd <- read.csv("house.csv",header=T)
str(hd)

#Naming the variables
price <- hd$PRICE
sqft <- hd$SQFT
baths <- hd$BATHS
bedrms <- hd$BEDRMS

#Examining summary statistics of each
summary(price)
summary(sqft)
summary(baths)
summary(bedrms)

#Checking to make sure the variable values make sense
mean(price)
sd(price)
mean(sqft)
sd(sqft)
mean(baths)
sd(baths)
mean(bedrms)
sd(bedrms)

#Creating plots comparing variables
par(mfrow=c(2,2))
plot(price,sqft)
plot(price,bedrms)
hist(bedrms)
plot(density(sqft))

#Regressing two variables to examine coefficients & significance
reg <- lm(price~sqft+bedrms)
summary(reg)

#recording values of the coefficients
sqftm <- 0.14338
sqftsd <- 0.02183
bdrmm <- -15.20168
bdrmsd <- 19.61185

#making density plots of the coefficients
xsqft <- seq(-0.05, 0.35, length=1000)
ysqft <- dnorm(xsqft, mean=sqftm, sd=sqftsd)
plot(xsqft, ysqft, type="l", lwd=1)
abline(v=sqftm,col="red")
abline(v=0)

xbdrm <- seq(-75, 45, length=1000)
ybdrm <- dnorm(xbdrm, mean=bdrmm, sd=bdrmsd)
plot(xbdrm, ybdrm, type="l", lwd=1)
abline(v=bdrmm,col="red")
abline(v=0)

#Generating t-test value for sqft different than zero
tsqft <- sqftm/sqftsd
#testing to see if bedrooms is different than 10
tbdrm <- (bdrmm-10)/bdrmsd

#finding values of 95% two-tailed test
sqftl <- sqftm-2.201*sqftsd
sqfth <- sqftm+2.201*sqftsd
bdrml <- bdrmm-2.201*bdrmsd
bdrmh <- bdrmm+2.201*bdrmsd

#Checking values with confint
confint(reg,level=0.95)

#Creating plots of confidence intervals
xsqft <- seq(-0.05, 0.35, length=1000)
ysqft <- dnorm(xsqft, mean=sqftm, sd=sqftsd)
plot(xsqft, ysqft, type="l", lwd=1)
abline(v=sqftl,col="darkblue")
abline(v=sqfth,col="darkblue")
abline(v=0)

xbdrm <- seq(-75, 45, length=1000)
ybdrm <- dnorm(xbdrm, mean=bdrmm, sd=bdrmsd)
plot(xbdrm, ybdrm, type="l", lwd=1)
abline(v=bdrml,col="darkblue")
abline(v=bdrmh,col="darkblue")
abline(v=0)

#Regressing bedrooms on price to see the effect of adding one bedroom will have on price
reg <- lm(price~bedrms)
summary(reg)

#Plotting bedrooms against price, both scatter plot and regression line
par(mfrow=c(1,2))
plot(bedrms, price)
abline(reg, col="red")
ehat <- reg$resid
plot(bedrms, ehat)
abline(h=0)

#Generating a new variable so price is in dollars instead of thousands of dollars
p1000 <- price*1000
reg <- lm(p1000~bedrms)
summary(reg)

#Plotting the fitted values and the residuals
par(mfrow=c(1,2))
yhat <- reg$fitted
plot(bedrms, p1000, col="orange", pch = 17)
points(bedrms, yhat, col="blue", pch = 8)
ehat <- reg$resid
plot(bedrms, ehat)
abline(h=0)
summary(reg)
