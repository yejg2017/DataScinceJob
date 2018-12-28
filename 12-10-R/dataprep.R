##import data and orgranize as a matrix
##data: WRDS data of 5 stocks and sprtrn from 2007.6.30 to 2018.6.30 

setwd('D:/18LSE/Courses/ST429/project')
remove(list=ls())
DataWrds=read.csv('DataWrds.csv')


attach(DataWrds)
#get the length of stocks and the dates
PermNoUnique  = unique(PERMNO)
DatesUnique   = unique(date)
NStocks       = length(PermNoUnique)
#dates may overlap due to offering divamt and split at the same day
NDates        = length(DatesUnique)

#reorganize so that the dates are not replicated 

for (i in 1:NDates)
{
  if (DataWrds[i,2]==DataWrds[i+1,2])
      DataWrds[i,1]= NaN
}

#organize data as a matrix with dates in rows and stock rets and sprtrn in columns
library(reshape2)
md1=dcast(DataWrds,date~PERMNO,value.var='RET')
md2=dcast(DataWrds,date~PERMNO,value.var='sprtrn')
DataRet=data.frame(md1,md2[,2])
names(DataRet)=c('Date','MSFT','AAPL','AMZN','NFLX','GOOG','SPRTRN')
#DataRet is the data matrix

#compute log-returns in DataLogRet
DataLogRet=log(1+DataRet[,2:7])
DataLogRet=cbind(Date=DataRet$Date,DataLogRet)


#clear dataset list
remove(list=c('DataWrds','DatesUnique','i','md1','md2','NDates','NStocks','PermNoUnique'))

#then DataLogRet is for log returns, DataRet is for returns.

##### plot the trends of log returns #####
summary(DataLogRet)

#histogram and trendline for each stock
dev.new()
par(mfrow=c(3,4))
for (i in 2:7){
  hist(DataLogRet[,i],xlab='logreturns', main = paste('Histogram of',names(DataLogRet[i])))
  plot.ts(DataLogRet[,i], main = paste('Scatter of',names(DataLogRet[i])))
#   plot(as.numeric(DataLogRet[,1]),DataLogRet[,i], xlab='Dates', 
#        ylab=paste('logreturns of',names(DataLogRet[i])), main = paste('Scatter of',names(DataLogRet[i])))
# ????? how to plot the x axis with dates.
  
}

library('tseries')
jarque.bera.test(DataLogRet[,2])$p.value # if =0.reject it is normal(obviously financial datas are not...)


