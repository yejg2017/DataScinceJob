#Inputting stock data (Time Series)

# Making sure we have quantmod installed and called from the library
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

#Setting the dates we want to grab the stocks from
start <- as.Date("2012-05-18")
end <- as.Date("2018-10-01")

#Choosing the stocks we want to grab - saves as xts file
tickers <- c("AAPL", "GOOGL", "MSFT", "FB", "AMZN")
getSymbols(tickers, src = "yahoo", from = start, to = end)

#Saving just the closing values of each
AAPL.Close <- AAPL[,4]
GOOGL.Close <- GOOGL[,4]
MSFT.Close <- MSFT[,4]
FB.Close <- FB[,4]
AMZN.Close <- AMZN[,4]

#Generating the value of the stock at the first day
AAPL1 <- as.numeric(AAPL.Close[1])
GOOGL1 <- as.numeric(GOOGL.Close[1])
MSFT1 <- as.numeric(MSFT.Close[1])
FB1 <- as.numeric(FB.Close[1])
AMZN1 <- as.numeric(AMZN.Close[1])

#Dividing each stock by its price the first day to see returns
AAPL <- AAPL.Close/AAPL1
GOOGL <- GOOGL.Close/GOOGL1
MSFT <- MSFT.Close/MSFT1
FB <- FB.Close/FB1
AMZN <- AMZN.Close/AMZN1

#basket so I don't have to type each out individually
basket <- cbind(AAPL,GOOGL,MSFT,FB,AMZN)
zoo.basket <- as.zoo(basket)
tsRainbow <- rainbow(ncol(zoo.basket))
plot(zoo.basket, main = "Tech", ylab="Cumulative Returns", col = tsRainbow, screens = 1)
legend("topleft", legend = c("AAPL", "GOOGL", "MSFT", "FB", "AMZN"), 
       lty = 1,col=tsRainbow)




#### correlation test:
P=sum((Xi-X_hat)(Yi-Y_hat))/sqrt(sum((X-X_hat))^2+sum((Y-Y_hat)^2))

```{r,echo=FALSE}
print(cor.test(Ham_lag3,LeftOvers_lag2))
```
the correlation test p-value is 0.71 ,which tell us that 2 lags of the leftovers variable, and 3 lags of Ham  has little correlations. 





