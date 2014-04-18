suppressPackageStartupMessages(require (timeSeries))
suppressPackageStartupMessages(require (fPortfolio)) # may also require installing the package require(slam)
suppressPackageStartupMessages(require(quantmod))
suppressPackageStartupMessages(require(caTools))

library(timeSeries)

symbols = c("DELL", "GOOG", "MSFT", "CSCO", "JNPR")
symbols = sort(symbols)


# read closing prices from Yahoo keeping only the closing prices
ClosingPricesRead <- NULL
for (Ticker in symbols) ClosingPricesRead <- cbind(ClosingPricesRead,
                             getSymbols.yahoo(Ticker, from="2012-01-01", 
                                              verbose=FALSE, auto.assign=FALSE)[,6]) 
                            # [,6] = keep the adjusted prices

# keep only the dates that have closing prices for all tickers
ClosingPrices <- ClosingPricesRead[apply(ClosingPricesRead,1,function(x) all(!is.na(x))),]


# convert prices to daily returns
x1 = as.timeSeries(tail(ClosingPrices,-1))
x2 = as.numeric(head(ClosingPrices,-1))
                   
returns <- x1/x2 - 1

par (mfrow = c(1,2))
f1 <- portfolioFrontier(returns)
plot(f1,1)

# calculate the efficient frontier
f2 <- portfolioFrontier(returns, constraints = "minW=rep(.12, length(symbols))")
# plot frontier
plot(f2,1) # can also call the plot routine so it only plots the frontier: plot(Frontier,1)






par(mfrow = c(1,2))

allocations <- getWeights(f1@portfolio) 
colnames(allocations) <- symbols
barplot(t(allocations), col=rainbow(ncol(allocations)+2), legend=colnames(allocations))


allocations <- getWeights(f2@portfolio) 
colnames(allocations) <- symbols
barplot(t(allocations), col=rainbow(ncol(allocations)+2), legend=colnames(allocations))