## question 1

### 1.1
ExponentialSmoothing=function(Xt,lambda){
  At=rep(0,length(Xt))
  for(t in 1:length(Xt)){
    if(t==1){
      At[t]=Xt[t]
    }else{
      At[t]=lambda*Xt[t]+(1-lambda)*Xt[t-1]
    }
  }
  return(At)
}

x <- c(2.53, 4.22, 1.15, 4.33, 7.31, 
       5.89, 28.69, 5.19, 8.38, 13.65, 
       13.23, 5.18, 14.61,14.78,14.15,20.19,
       18.09,17.57,21.7)

A1=ExponentialSmoothing(x,0.2)
A2=ExponentialSmoothing(x,0.5)

n=length(x)
plot(1:n,x,type="l",lty=2)
lines(1:n,A1,type="l",col="red",lty=3)
lines(1:n,A2,type="l",col="blue",lty=4)
legend(x=12,y=27,lty = c(2,3,4),
       legend = c("origin","lambda=0.2","lambda=0.5"),
       col = c("black","red","blue"))



### 1.2
GenerateSample<-function(size=50,times=1000){
  Y<-data.frame()
  for(i in 1:times){
    y<-rexp(n=size,rate=1)
    y<-data.frame(y)
    y<-t(y)
    Y<-rbind.data.frame(Y,y)
  }
  name<-paste0("ExpSample_",1:times)
  rownames(Y)<-name
  return(Y)
}

Ymat<-GenerateSample(size = 50,times = 1000)


mu=1 # mu=lamnda
Var=1 # std=lamda
n=50

SampleExp=apply(Ymat,1,function(y){
  y_hat=mean(y)
  yn=sqrt(n)*(y_hat-mu)/sqrt(Var)
  return(yn)
})

StandarNorm=rnorm(n=1000)
plot(1:1000,SampleExp,col="black",lty=2,type="l")
lines(1:1000,StandarNorm,col="red",lty=3,type="l")


## question 2
#psych::kurtosi function

## 2,1
Kurt<-function(x,na.rm=TRUE){
    if(na.rm){
      x<-x[!is.na(x)]
    }
    mx<-mean(x,na.rm = na.rm)
    
    sdx<-sd(x,na.rm = na.rm)
    n<-length(x[!is.na(x)])
    
    kurt<-sum((x-mx)^4,na.rm = na.rm)*n/(sum((x-mx)^2,na.rm = na.rm)^2)-3
    return(kurt)
}

## 2.2
y <- c(-0.90, 0.47, 0.87, -5.37, -0.48,
       0.24, 0.71, 0.58, -0.54 ,-0.41,
       0.09, 0.32, 0.07, 1.70, -0.41,
       0.33, -0.72, -0.74, -0.35, 1.14)


size=10
sims=1000
kurts<-numeric(sims)

for( s in 1:sims){
  bs<-sample(y,size=n,replace = TRUE)
  kurt<-Kurt(bs,na.rm = TRUE)
  kurts[s]<-kurt
}


# t distribution: (x-mu)/(s/sqrt(n)),x:sample mean,s:sample variance,n:sample nummber
n=sims
kurt_hat=mean(kurts)
sample_sd=sd(kurts)
t_alpha_2=qt(p=1-0.05/2,df=n-1)
E=sample_sd/sqrt(n)

## 95% confidence interval
lower=kurt_hat-E*t_alpha_2
upper=kurt_hat+E*t_alpha_2


BootConf<-function(y,size,times=1000,alpha=0.05){
  kurts=numeric(times)
  for( s in 1:times){
    bs<-sample(y,size=size,replace = TRUE)
    kurt<-Kurt(bs,na.rm = TRUE)
    kurts[s]<-kurt
  }
  
  n=times
  kurt_hat=mean(kurts)
  sample_sd=sd(kurts)
  t_alpha_2=qt(p=1-alpha/2,df=n-1)
  E=sample_sd/sqrt(n)
  
  ## confidence interval
  lower=kurt_hat-E*t_alpha_2
  upper=kurt_hat+E*t_alpha_2
  return(list(lower,upper))
  
}

### try size=100,times=1000 to caculate confdence interval
BootConf(y,size=100,times=1000)

## 2.3

### Similar to the 2.2,use the Boostrap method to caculate confidence interval.If 0 in the confidence interval,
### then,accept the null  hypothesis,else,reject

BootConf(y,size=10,times=1000)
BootConf(y,size=20,times=1000)
BootConf(y,size=50,times=1000)
BootConf(y,size=100,times=1000)
BootConf(y,size=200,times=1000)

### the result all reject,so the Kurt!=0


## Question 3
install.packages("DAAG")
library(DAAG)

data("rainforest")
Bmyrtifolia.wood<-subset(rainforest,species%in%c("B. myrtifolia"),wood)$wood
Acmenasmithii.wood<-subset(rainforest,species%in%c("Acmena smithii"),wood)$wood

Bootstrap<-function(x,size,times=1000){
  Y=data.frame()
  for(i in 1:times){
    y=sample(x,size = size,replace = TRUE)
    y=t(data.frame(y))
    Y=rbind.data.frame(Y,y)
  }
  name=paste0("Boostrap_",1:times)
  rownames(Y)=name
  return(apply(Y,1,mean))
}

BmyrWood<-Bootstrap(Bmyrtifolia.wood,size = 50)
AcmeaWood<-Bootstrap(Acmenasmithii.wood,size = 50)

size50<-t.test(BmyrWood,AcmeaWood,conf.level = 0.95,alternative = "two.sided")$conf.int


## Question 4