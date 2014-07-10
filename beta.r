

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

beta.f = function (z, spy, xlab = "GSPC", ylab = "Other") {
    z = as.vector(z)
    spy = as.vector(spy)
    main = paste (ylab, "vs", xlab)
    plot (z ~ spy, main = main, xlab = xlab, ylab = ylab)
    fit = lm (z ~ spy)
    print (summary(fit))
    print (anova(fit))
    r2 = cor(z, spy) ^ 2
    print (r2)
    cat("R^2 =", round(r2,3))
    r2
}

x = 1:3
y = 1:3
symbol = "mqifx"

compare.returns = function (x, y, symbol) {
  #--plot two series vs date or index
  main = paste(symbol, "Close & Adjusted Returns vs Trading Day")
  
  plot (x, xlab = "Date", main = main, type = "l")
  lines (y, col = "blue")
  
  delta = x - y
  plot (delta, type = "l", main = paste(symbol, "difference in close and adj returns"))
}

symbols = c("MQIFX", "^GSPC")

quote = getSymbols(symbols, from = "2012-01-01", to = "2014-10-17", verbose = T)

spy = GSPC
spy.close = spy$GSPC.Close
spy.adjusted = spy$GSPC.Adjusted

#--each symbol becomes a data frame

mqifx.close.returns = returns.f(MQIFX$MQIFX.Close)
mqifx.adj.returns   = returns.f(MQIFX$MQIFX.Adjusted)
plot (mqifx.close.returns ~ mqifx.adj.returns)
plot (mqifx.close.returns, type = "l", pch = ".")
lines (mqifx.adj.returns, col = "blue", pch = ".")

beta.f(MQIFX$MQIFX.Close, spy.close, xlab = "GSPC Close", ylab = "MQIFX Close")
beta.f(MQIFX$MQIFX.Adjusted, spy.adjusted, xlab ="GSPC Adjusted", ylab = "MQIFX Adjusted")

compare.returns (mqifx.close.returns, mqifx.adj.returns, symbol = "MQIFX")

