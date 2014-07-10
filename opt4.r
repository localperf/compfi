
"""
Start Date: January 1, 2011
End Date: December 31, 2011
Symbols: ['AAPL', 'GLD', 'GOOG', 'XOM']
Optimal Allocations: [0.4, 0.4, 0.0, 0.2]
Sharpe Ratio: 1.02828403099
Volatility (stdev of daily returns):  0.0101467067654
Average Daily Return:  0.000657261102001
Cumulative Return:  1.16487261965
"""



library(quantmod)
library(Defaults)
library(xts)
library(zoo)


returns.f = function(z) {
  a = as.vector(tail(z, -1))
  b = as.vector(head(z, -1))
  returns = a / b - 1
  returns
}

p1 = .4
p2 = .4
p3 = 0
p4 = .2

evaluate = function (p1, p2, p3, p4) {
  weights = c(p1,p2,p3,p4)
  principal = 1000
  values = NULL
  for (i in 1:nrow(returns)) {
    change = weights %*% as.numeric(returns[i,2:5])
    principal = (1 + change) * principal
    values = c(values, principal)
  }
}

symbols = c("GOOG", "AAPL", "GLD", "XOM")
getSymbols(symbols, from = "2011-01-01", to = "2011-12-31", verbose = T)


df$aapl = AAPL$AAPL.Adjusted
df$goog = GOOG$GOOG.Adjusted
df$gld  = GLD$GLD.Adjusted
df$xom  = XOM$XOM.Adjusted

symbols = tolower(symbols)

returns = data.frame(date = df$date[2:nrow(df)])
for (symbol in symbols) {
  print (symbol)
  returns[,symbol] = returns.f(df[,symbol])
}
head(returns)

for (p1 in seq(0, 1, by = 0.1)) {
  for (p2 in seq(0, 1, by = .1)) {
    for (p3 in seq(0, 1, by = .1)) {
      for (p4 in seq (0, 1, by = .1)) {
        if (abs(p1 +p2 + p3 + p4 - 1.0) < epsilon) {
          result = evaluate (p1, p2, p3, p4)
        }
          
      }
    }
  }
}

