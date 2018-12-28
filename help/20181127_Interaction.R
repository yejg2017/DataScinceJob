library(readxl)
library(tidyverse)
library(ggplot2)
library(corrplot)


## Interaction R Part A

### import data

IceCreamExcel <- read_excel('IceCreamExcel.xlsx')
head(IceCreamExcel)
str(IceCreamExcel)
summary(IceCreamExcel)


### exclude variable x(id) 


ICD <- subset(IceCreamExcel, select = -c(id))
str(ICD)
summary(ICD)


### run correlations

cor(ICD)



corr <- cor(ICD)
# corrplot::corrplot(corr = corr)
corrplot::corrplot(corr = corr, order = "AOE", tl.pos = "tp")
corrplot::corrplot(corr = corr,  
                   add = TRUE, type = "lower", method = "number", order = "AOE", 
                   col = "black", diag = FALSE, tl.pos = "n", cl.pos = "n")



### use Hmisc

library(Hmisc)
rcorr(as.matrix(ICD))

## Interaction R Part B

### create some new var, run correlations 


ICD <- ICD %>% mutate(
  Income_by_price = income * price,
  Income_by_temp  = income * temp ,
  Price_by_temp   = price * temp
) %>% mutate(
  income_mc = scale(income, center = TRUE),
  price_mc  = scale(price, center = TRUE),
  temp_mc   = scale(temp, center = TRUE)
)

summary(ICD)
rcorr(as.matrix(ICD))



### create two_way interaction terms and view corr


ICD <- ICD %>% mutate(
  Income_by_price = income_mc * price_mc,
  Income_by_temp  = income_mc * temp_mc ,
  Price_by_temp   = price_mc * temp_mc
)

summary(ICD)
rcorr(as.matrix(ICD))



## Interaction R Part C
### testing linear model w/no interactions

model <- lm(cons ~ income + price + temp, data = ICD)
summary(model)
library(car)

vif(model)



### testing linear model w/no interactions without pequod, note use of mean center predictoros

model1 <- lm(cons ~ income + price + temp + Income_by_temp + 
               Income_by_price + Price_by_temp, data = ICD)
summary(model1)
vif(model1)



library(pequod)

modelpe <- lmres(cons ~ income + price + temp, data = ICD)
summary(modelpe)

modelpe1 <- lmres(cons ~ income * price + income * temp + price * temp, 
                  centered = c("income", "price", "temp"), data = ICD)
summary(modelpe1)




## Interaction R Part D

### Simple slope test and plot for income by price interatcion

S_slopes <- simpleSlope(modelpe1, pred = "income", mod1 = "price")
PLotIncome_by_proce <- PlotSlope(S_slopes)
PLotIncome_by_proce
summary(S_slopes)


## Interaction R Part E
### creating and testing:
ICD %>% mutate(threeway = income_mc * price_mc * temp_mc)
model2 <- lm(cons ~ income + price + temp + Income_by_temp + 
               Income_by_price + Price_by_temp + threeway, data = ICD)
summary(model2)
vif(model2)

modelpe2 <-  lmres(cons ~ income * price * temp, 
                   centered = c("income", "price", "temp"), data = ICD)
summary(modelpe2)


### Simple slope test and plot for the three-way interatcion

S_slopes_3way <- simpleSlope(modelpe2, pred = "income", mod1 = "temp", mod2 = "price")
Plot_threeway <- PlotSlope(S_slopes_3way)
Plot_threeway
summary(S_slopes_3way)
