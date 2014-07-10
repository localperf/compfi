#--noma.r
#--Elliot Noma on face tube

#--http://elliotnoma.wordpress.com/2013/01/22/construct-a-stock-portfolio-using-r/



library (bitops)
library (caTools)
library (timeSeries)
library (quantmod)
library (slam)
library (fPortfolio)


folio = c("adksx", "boh", "bmo", "lqd", "jwn", "he", "ewc", "tnh", "trp", "enb")
jeff = c("ABEMX", "OAKIX", "OAKMX")

noma = c("DELL", "GOOG", "MSFT", "CSCO", "JNPR")

symbols = noma
from = "2013-01-01"


get.data = function (symbols, from = "2013-01-01") {
  ###--return data frame with adjusted close and time series for daily returns
  ###--return only complete cases
  
  prices = NULL
  for (symbol in symbols) {
    adj            = getSymbols.yahoo(symbol, from = from, verbose=F, auto.assign=F)[,6]
    colnames(adj)  = tolower(symbol)
    prices         = cbind(prices, adj)
  }
     
  prices = prices[complete.cases(prices),]
  returns = as.timeSeries(tail(prices,-1) / as.numeric(head(prices, -1)) -1)
  list(adjusted = prices, returns = returns)
}

data = get.data(noma)
returns = data$returns
returns = portfolioData(returns)

frontier = portfolioFrontier(returns, constraints = "LongOnly",
            title = "Noma", description = "2014-01-18")

prices = NULL
for (symbol in TickerList) {
  
  adjusted = getSymbols.yahoo(symbol, from = "2010-01-01", verbose = T, auto.assign = F)[,6]   ###--adjusted close
  
  ClosingPricesRead = cbind(ClosingPricesRead, prices)
}
ClosingPricesRead


ClosingPrices = ClosingPricesRead[complete.cases(ClosingPricesRead),]

returns = as.timeSeries(tail(ClosingPrices,-1) / as.numeric(head(ClosingPrices,-1)) -1)

str(returns)
head(returns$DELL)

frontier = portfolioFrontier(returns)
plot (frontier, 1)
plot (frontier, 2)

getStatistics(frontier)$mean
cor(returns)

riskReturnPoints = frontierPoints(frontier)
annualizedPoints = data.frame(targetRisk    = riskReturnPoints[,"targetRisk"] * sqrt (252), 
                              targetReturn  = riskReturnPoints[,'targetReturn'] * 252)
plot (annualizedPoints)

dev.new()

riskFreeRate = 0
plot ((annualizedPoints[,"targetReturn"] - riskFreeRate) / annualizedPoints[,"targetRisk"],
      xlab = "Point on Efficient Frontier", ylab = "Sharpe Ratio")

allocations = getWeights(frontier@portfolio)
colnames(allocations) = TickerList
barplot (t(allocations), col = rainbow(ncol(allocations)+2)

annualizedPoints

