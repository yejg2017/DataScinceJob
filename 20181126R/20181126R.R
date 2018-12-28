# load necessary packages and dataset
library(rpart)
library(rpart.plot)
library(caret)
library(ROCR)
library(party)

eBaysAuctions<-read.csv("./eBayAuctions.csv",header = TRUE)
#str(eBaysAuctions)  # check struct of dataset
str(eBaysAuctions)



#### Data Preprocessing
eBaysAuctions$Duration<-as.factor(eBaysAuctions$Duration) #  Convert Duration into catergorical variable

eBaysAuctions$Competitive<-as.factor(eBaysAuctions$Competitive.)
eBaysAuctions$Competitive.<-NULL
eBaysAuctions$Competitive<-ifelse(eBaysAuctions$Competitive==0,"unCompetive","Competive")
eBaysAuctions$Competitive<-as.factor(eBaysAuctions$Competitive)
# split dataset into train(60%) and test(40%)
set.seed(20181126)
n<-dim(eBaysAuctions)[1]
train_idx<-sample(1:n,size = ceiling(n*0.6),replace = FALSE)
test_idx<-setdiff(1:n,train_idx)
train<-eBaysAuctions[train_idx,]
test<-eBaysAuctions[test_idx,]



#### (a)

model<-rpart(Competitive~.,data = train,method = "class",
             control = list(minbucket=50,maxdepth=7,minsplit=1)) #,xval=5 # xval=5,5 fold validation

# plot tree
prp(model, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(model$frame$var == "<leaf>", 'green', "gray")) 



####  (b)

train_pred<-predict(model,newdata = train,type = "class")
train_ConfusionMat<-table(train$Competitive,train_pred)
train_acc<-sum(diag(train_ConfusionMat))/sum(train_ConfusionMat)
cat("The Accuracy of train dataset of the origin model is :",train_acc,"\n")


test_pred<-predict(model,newdata = test,type = "class")
test_ConfusionMat<-table(test$Competitive,test_pred)
test_acc<-sum(diag(test_ConfusionMat))/sum(test_ConfusionMat)
cat("The Accuracy of test dataset of the origin model is :",test_acc,"\n")



#### (c)

#As we know ,the model used *OpenPrice,ClosePrice,sellerRati* as the predictors and removed the other variable(*Category,currency,Duration,endDay*),that is,the predictors are the intersting variable and the other variables are not intersting.

plot(density(eBaysAuctions$sellerRating),col="red",main=paste0("sellerRating","(mean :",mean(eBaysAuctions$sellerRating),")"))
plot(density(eBaysAuctions$OpenPrice),col="red",main=paste0("OpenPrice","(mean :",mean(eBaysAuctions$OpenPrice),")"))
plot(density(eBaysAuctions$ClosePrice),col="red",main=paste0("ClosePrice","(mean :",mean(eBaysAuctions$ClosePrice),")"))


unintersting<-eBaysAuctions[,c("Category","currency","Duration","endDay","Competitive")]
#Category<-eBaysAuctions[,"Category"]
#currency<-eBaysAuctions[,"currency"]
#Duration<-eBaysAuctions[,"Duration"]
#endDay<-eBaysAuctions[,"endDay"]

ggplot(data=unintersting)+geom_bar(aes(x=Category,fill=Competitive),position = "stack")+theme(axis.text.x = element_text(angle = 90,face = "bold"))


ggplot(data=unintersting)+geom_bar(aes(x=currency,fill=Competitive),position = "stack")+theme(axis.text.x = element_text(angle = 90,face = "bold"))


ggplot(data=unintersting)+geom_bar(aes(x=Duration,fill=Competitive),position = "stack")+theme(axis.text.x = element_text(angle = 90,face = "bold"))

ggplot(data=unintersting)+geom_bar(aes(x=endDay,fill=Competitive),position = "stack")+theme(axis.text.x = element_text(angle = 90,face = "bold"))





#### (d)

# prune tree
model.2<-rpart(Competitive~OpenPrice+ClosePrice+sellerRating,data = train,method = "class")


model.2<- prune(model.2, cp =model.2$cptable[which.min(model$cptable[,"xerror"]),"CP"])
prp(model.2, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(model.2$frame$var == "<leaf>", 'green', "gray"))



train_pred<-predict(model.2,newdata = train,type = "class")
train_ConfusionMat<-table(train$Competitive,train_pred)
train_acc<-sum(diag(train_ConfusionMat))/sum(train_ConfusionMat)
cat("The Accuracy of train dataset of the prune model is :",train_acc,"\n")


test_pred<-predict(model.2,newdata = test,type = "class")
test_ConfusionMat<-table(test$Competitive,test_pred)
test_acc<-sum(diag(test_ConfusionMat))/sum(test_ConfusionMat)
cat("The Accuracy of test dataset of the prune model is :",test_acc,"\n")


#### (e)

library(ggplot2)
ggplot(data=eBaysAuctions,aes(x=OpenPrice,y=ClosePrice,color=Competitive))+
  geom_point()+geom_abline(intercept = 0.0, slope = 1)+
  theme(legend.title = element_blank())


#### (f)

##### confusionMatrix
cat("The train dataset confusionMatrix:","\n")
confusionMatrix(train_pred,train$Competitive)

cat("The validation dataset confusionMatrix:","\n")
confusionMatrix(test_pred,test$Competitive)


##### lift chart
train_prob<-predict(model.2,newdata = train,type = "prob")
train_pred<-prediction(train_prob[,2],train$Competitive)
train_perf<-performance(train_pred,"lift","rpp")
plot(train_perf, main="lift curve(train)", colorize=T)



test_prob<-predict(model.2,newdata = test,type = "prob")
test_pred<-prediction(test_prob[,2],test$Competitive)
test_perf<-performance(test_pred,"lift","rpp")
plot(test_perf, main="lift curve(validaton)", colorize=T)
