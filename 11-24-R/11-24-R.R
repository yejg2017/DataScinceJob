# Load data
sales<-read.table("./APPENC07.txt",header = FALSE)
variables<-c("Id.Number","Sales.Price","Square.Feet",
             "Number.bedrooms","Number.Bathrooms","Air.Condition",
             "Garage.Size","Pool","Year.Built","Quality",
             "Style","Lot.Size","Adjacent.to.Highway")
colnames(sales)<-variables
sales$Air.Condition<-as.factor(sales$Air.Condition)
sales$Pool<-as.factor(sales$Pool)
sales$Quality<-as.factor(sales$Quality)
sales$Style<-as.factor(sales$Style)
sales$Adjacent.to.Highway<-as.factor(sales$Adjacent.to.Highway)
str(sales)
sales<-sales[,-1]



# 2

# scatterplot matrix
pairs(sales[,c("Sales.Price","Square.Feet","Number.bedrooms","Number.Bathrooms","Garage.Size","Year.Built","Lot.Size")])


# fit a first-order model using all predictors
model.1<-lm(Sales.Price~.,data = sales)
par(mfrow=c(2,2))
plot(model.1)


sales.new<-sales
LgSales<-log10(sales.new$Sales.Price)
LgSquare.Feet<-log10(sales.new$Square.Feet)
LgLot.Size<-log10(sales.new$Lot.Size)

sales.new$Sales.Price<-LgSales
sales.new$Square.Feet<-LgSquare.Feet
sales.new$Lot.Size<-LgLot.Size


model.2<-lm(Sales.Price~.,data = sales.new)
par(mfrow=c(2,2))
plot(model.2)


# i
pairs(sales.new[,c("Sales.Price","Square.Feet","Number.bedrooms","Number.Bathrooms","Garage.Size","Year.Built","Lot.Size")])

# ii
require(car)
#qualitative.vars<-sales.new[,c("Sales.Price","Air.Condition","Pool","Quality","Style","Adjacent.to.Highway")]

par(mfrow=c(2,3))
r<-Boxplot(Sales.Price~Air.Condition,data = sales.new)
r<-Boxplot(Sales.Price~Pool,data = sales.new)
r<-Boxplot(Sales.Price~Quality,data = sales.new)
r<-Boxplot(Sales.Price~Style,data = sales.new)
r<-Boxplot(Sales.Price~Adjacent.to.Highway,data = sales.new)

# iii
require(car)
avPlots(model.2,terms = ~Square.Feet+Number.bedrooms+Number.Bathrooms+Garage.Size+Year.Built+Lot.Size)



# 3

## a

##  function : Variable combination to establish a linear model
Combn.FUN<-function(data,criteria="AIC",num.predictors=1){
  predictors<-setdiff(colnames(data),"Sales.Price")
  res<-combn(predictors,m=num.predictors,FUN = function(v){
    if(num.predictors==1){
      form<-paste0("Sales.Price","~",v)
      form<-as.formula(form)
    }else{
      form<-paste0("Sales.Price","~",paste(as.vector(v),collapse = "+"))
      form<-as.formula(form)
    }
    model<-lm(form,data=data)
    if(criteria=="AIC"){
      c<-AIC(model)
    }
    if(criteria=="BIC"){
      c<-BIC(model)
    }
    if(criteria=="adj.r"){
      c<-summary(model)$adj.r.squared
    }
    return(c)
  })
  
}

## For each numver of variable, select only the best variables of the model criteria.
Step.variable<-function(data,criteria="AIC"){
  predictors<-setdiff(colnames(data),"Sales.Price")
  N<-length(predictors)
  result<-list()
  for(i in 1:N){
    
    v.all<-combn(predictors,i)
    cri<-Combn.FUN(data,criteria,num.predictors = i)
    
    if(criteria=="AIC"){
      idx<-which.min(cri)
    }
    if(criteria=="BIC"){
      idx<-which.min(cri)
    }
    if(criteria=="adj.r"){
      idx<-which.max(cri)
    }
    v<-v.all[,idx]
    res<-list(criteria=cri[idx],variables=v)
    
    result[[i]]<-res
  }
  return(result)
}

criteria.to.DF<-function(criteria.list){
  n<-length(criteria.list)
  df<-data.frame()
  for(i in 1:n){
    cri<-criteria.list[[i]]$criteria
    variables<-criteria.list[[i]]$variables
    df<-rbind(df,data.frame(cri,length(variables)))
  }
  colnames(df)<-c("criteria","number.variables")
  return(df)
}


#plot criteria vs number of variables
res.aic<-Step.variable(data = sales.new,criteria = "AIC")
res.bic<-Step.variable(data = sales.new,criteria = "BIC")
res.adj.r<-Step.variable(data = sales.new,criteria = "adj.r")

aic.df<-criteria.to.DF(res.aic)
bic.df<-criteria.to.DF(res.bic)
adj.r.df<-criteria.to.DF(res.adj.r)

par(mfrow=c(1,3))
plot(aic.df$number.variables,aic.df$criteria,type='b',col="red",xlab = "number of variables",ylab = "AIC")

plot(bic.df$number.variables,bic.df$criteria,type='b',col="red",xlab = "number of variables",ylab = "BIC")

plot(adj.r.df$number.variables,adj.r.df$criteria,type='b',col="red",xlab = "number of variables",ylab = "Adj.r.square")



Pick.n.Model<-function(result.list,critera,pick=5){
  df<-criteria.to.DF(result.list)
  if(critera=="AIC"){
    df<-df[order(df$criteria,decreasing = FALSE),]
  }
  if(critera=="BIC"){
    df<-df[order(df$criteria,decreasing = FALSE),]
  }
  if(critera==""){
    df<-df[order(df$criteria,decreasing = FALSE),]
  }
  if(critera=="adj.r"){
    df<-df[order(df$criteria,decreasing = TRUE),]
  }
  idx<-as.integer(df$number.variable[1:pick])
  
  vars<-lapply(idx,function(i){return(result.list[[i]]$variables)})
  return(vars)
}


# AIC criteria
cat("Five model variable subsets in AIC:....","\n")
Pick.n.Model(res.aic,"AIC")

# BIC criteria
cat("Five model variable subsets in BIC:....","\n")
Pick.n.Model(res.bic,"BIC")

# Adi.r.square criteria
cat("Five model variable subsets in Adj.r.square:....","\n")
Pick.n.Model(res.adj.r,"adj.r")


## the modlel from  2.part(d)
model.3<-lm(Sales.Price~Square.Feet+Number.Bathrooms+Garage.Size+Pool+Year.Built+
              Quality+Style+Lot.Size+Adjacent.to.Highway,data = sales.new)

par(mfrow=c(2,2))
plot(model.3)


model.4<-lm(Sales.Price~Square.Feet+Number.Bathrooms+Garage.Size+Year.Built+
              Quality+Lot.Size,data = sales.new)

par(mfrow=c(2,2))
plot(model.4)


## b
require(MASS)
#  function to get Studentized Deleted Residuals
student.res.del<-function(model){
  stud.res<-MASS::studres(model)  # student reesidula
  resid<-residuals(model)  #residual
  
  df<-model$df.residu
  temp<-(resid/stud.res)^2*df  # SSE*(1-hii)
  stud.res.del<-resid*sqrt(df-1)/sqrt(temp-resid^2) # Studentized Deleted Residuals
  return(stud.res.del)
}


resid.3<-residuals(model.3)
deviance.3<-deviance(model.3)
df.3<-model.3$df.residual
semi.student.res.3<-resid.3/(deviance.3/df.3)
stud.re.del.3<-student.res.del(model.3)  #Studentized Deleted Residuals of model 3

plot(fitted(model.3),stud.re.del.3,xlab = "fitted value",ylab = "Deleted Studentized Residual",main = "Deleted Studentized Residual vs Fitted value(model.3)")
abline(h=0,col="red")


cat("Use the t-test to compare difference between semi-studentized
residuals  and deleted studentized residual...","\n")
t.test(semi.student.res.3,stud.re.del.3)
#p.value<-t.test(semi.student.res.3,stud.re.del.3)$p.value

resid.4<-residuals(model.4)
deviance.4<-deviance(model.4)
df.4<-model.4$df.residual
semi.student.res.4<-resid.4/(deviance.4/df.4)
stud.re.del.4<-student.res.del(model.4)  #Studentized Deleted Residuals of model 4

plot(fitted(model.4),stud.re.del.4,xlab = "fitted value",ylab = "Deleted Studentized Residual",main = "Deleted Studentized Residual vs Fitted value(model.4)")
abline(h=0,col="red")


cat("Use the t-test to compare difference between semi-studentized
residuals  and deleted studentized residual...","\n")
t.test(semi.student.res.4,stud.re.del.4)


summary(model.3)
summary(model.4)


##  d
distance.3<-cooks.distance(model.3)  # cook distance
distance.4<-cooks.distance(model.4)

par(mfrow=c(2,2))
plot(model.3, which=4, cook.levels=cutoff,sub.caption = "model.3")
plot(1:length(distance.3),distance.3,xlab = "case number",ylab = "cook.distance",main="case number  vs cook.distance(model.3)")
#abline(h=2/sqrt(length(distance.3)),col="red")


plot(model.4, which=4, cook.levels=cutoff,sub.caption ="(model.4)")
plot(1:length(distance.4),distance.3,xlab = " case number",ylab = "cook.distance",
     main="case number  vs cook.distance(model.3)")
#abline(h=2/sqrt(length(distance.4)),col="red")


sales.new<-sales.new[-c(11,96,161,414),]  # delete outliers
# refit model
model.3<-lm(Sales.Price~Square.Feet+Number.Bathrooms+Garage.Size+Pool+Year.Built+
              Quality+Style+Lot.Size+Adjacent.to.Highway,data = sales.new)



model.4<-lm(Sales.Price~Square.Feet+Number.Bathrooms+Garage.Size+Year.Built+
              Quality+Lot.Size,data = sales.new)



# 5
set.seed(20181201)
n<-dim(sales.new)[1]
test.idx<-sample(1:n,size = ceiling(0.2*n))
test<-sales.new[test.idx,]

pred.3<-predict(model.3,newdata = test)
MSPR.3<-sum((test$Sales.Price-pred.3)^2)/dim(test)[1]  # MSPR
MSE.3<-sum(residuals(model.3)^2)/(model.3$df.residual) #MSE


pred.4<-predict(model.4,newdata = test)
MSPR.4<-sum((test$Sales.Price-pred.4)^2)/dim(test)[1]
MSE.4<-sum(residuals(model.4)^2)/(model.4$df.residual)



# model.3
model.3.val<-lm(Sales.Price~Square.Feet+Number.Bathrooms+Garage.Size+Pool+Year.Built+
                  Quality+Style+Lot.Size+Adjacent.to.Highway,data = test)


# MSE
MSE.3.val<-deviance(model.3.val)/model.3.val$df.residual

# adj.r.squared
val.adj.r.square.3<-summary(model.3.val)$adj.r.squared

# confidence intervals of estimated coefficient
cat("The confidence intervals of estimated coefficient with origin dataset:","\n")
confint.lm(model.3)

cat("The confidence intervals of estimated coefficient with validattion dataset:","\n")
confint.lm(model.3.val)

# model.4
model.4.val<-lm(Sales.Price~Square.Feet+Number.Bathrooms+Garage.Size+Year.Built+
                  Quality+Lot.Size,data = test)

# MSE
MSE.4.val<-deviance(model.4.val)/model.4.val$df.residual

# adj.r.squared
val.adj.r.square.4<-summary(model.4.val)$adj.r.squared

# confidence intervals of estimated coefficient
cat("The confidence intervals of estimated coefficient with origin dataset:","\n")
confint.lm(model.4)

cat("The confidence intervals of estimated coefficient with validattion dataset:","\n")
confint.lm(model.4.val)


summary(model.4)  # with all the dataset