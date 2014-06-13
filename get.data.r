
get.data = function (symbols, dir = "test", from = "2009-01-01") {
   
   ###--return data frame with adjusted close and time series for daily returns
   ###--drop dates with missing prices
   ###--one warning per symbol is normal
   
   symbols = gsub ("\\.", "-", symbols)
   
   prices = NULL
   for (symbol in symbols) {
      
      print         (paste("fetching", symbol))
      adj           = getSymbols.yahoo(symbol, from = from, verbose=F, auto.assign=F)[,6]
      colnames(adj) = tolower(symbol)
      prices        = cbind(prices, adj)
   }
   
   symbols = colnames(prices)
   
   dim(prices)
   head(prices)
   
   missing = prices[is.na(rowSums(prices)),]
   n.missing = dim(missing)[1]
   n.missing
   if (n.missing > 0) print (missing)
   
   print (paste("###--number of missing rows =", n.missing))
   
   prices = prices[complete.cases(prices),]
   dates = as.Date(row.names(as.data.frame(prices)))
   summary(dates)
   
   
   period    = paste(min(dates), "through", max(dates))
   
   x1 = as.timeSeries(tail(prices,-1))
   x2 = as.numeric(head(prices, -1))
   returns = x1 / x2 - 1
   summary(returns)
   
   main = paste(dir, "\n", min(dates), "-", max(dates))
   data = list(prices = prices, returns = returns, dates = dates, symbols = symbols, main = main, period = period)
   
}
