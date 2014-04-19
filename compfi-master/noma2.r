#--noma2.r

#--Elliot Noma on face tube

#--http://elliotnoma.wordpress.com/2013/01/22/construct-a-stock-portfolio-using-r/


library (timeSeries)
library (quantmod)
library (slam)
library (fPortfolio)
library (plotrix)
library (corrplot)
source  ("c://python_projects//compfi//plot.drawdowns.ggplot2.r")
source  ("c://python_projects//compfi//multiplot.r")

from = "2014-01-01"
dir = "folio"

get.data = function (symbols, dir = "test", from = "2009-01-01") {
  
  ###--return data frame with adjusted close and time series for daily returns
  ###--returns only symbols with data for all dates
  ###--one warning per symbol is normal
  symbols =sort(unique(symbols))
  
  prices = NULL
  for (symbol in symbols) {
    print         (paste("fetching", symbol))
    adj           = getSymbols.yahoo(symbol, from = from, verbose=F, auto.assign=F)[,6]
    colnames(adj) = tolower(symbol)
    prices        = cbind(prices, adj)
    }
  
  symbols = colnames(prices)
  complete.symbols = NULL
  dim(prices)
  head(prices)
  for (symbol in symbols) {
    sigma = sum(prices[,symbol])
    print (paste(symbol, sigma))
    if (! is.na(sigma)) complete.symbols = c(complete.symbols, symbol) 
    }
  print (complete.symbols)
  print (length(complete.symbols))
 
  prices = prices[complete.cases(prices),]
  dates = as.Date(row.names(as.data.frame(prices)))
  summary(dates)
  
  summary(prices)
  prices = interpNA(prices, method = "linear")
  summary(prices)
  
  period    = paste(min(rownames(prices)), "through", max(rownames(prices)))
  
  x1 = as.timeSeries(tail(prices,-1))
  x2 = as.numeric(head(prices, -1))
  returns = x1 / x2 - 1
  summary(returns)
  
  main = paste(dir, min(dates), "-", max(dates))
  data = list(prices = prices, returns = returns, dates = dates, symbols = complete.symbols, main = main)
 
  }

symbol = "fxsix"

show.relative.prices = function(data, ylim = c(.75, 1.25)) {
  #-- can work on finding ylim dynamically
  prices  = data$prices
  symbols = data$symbols 
  
  dates     = time(prices)
  par (mfrow = c(2, 3))
  for (symbol in symbols) {
    local = as.numeric(prices[, symbol]) 
    local = local / local[1]  
    plot (dates,local, main = symbol, ylab = "Relative Adj Close", col="blue", type = "s", ylim = ylim)
    abline (h=1, col = "gray")
  }
}

show.return.distributions = function (data, xmax = .04, ymax = 200, precision = 4) {
  
  #-- mshould put period in legend
  
  returns = data$returns
  symbols = colnames(as.data.frame(returns))
  par     (mfrow = c(2,3))
  par     (oma = c(1,1,3,1))
  for (symbol in symbols) {
    symbol.returns  = returns[,symbol]
    z               = density(symbol.returns, na.rm=T)
    plot (z, main = symbol, xlim = c(-xmax, xmax), ylim = c(0, ymax), col = "blue", lwd = 2, xlab = "")
    msg.1 = paste("mean =", round(mean(symbol.returns), precision))
    msg.2 = paste("sdev =", round(sd(symbol.returns), precision))
    legend ("topleft", c(msg.1, msg.2), bty = "n")
    segments(0, 0, 0, max(z$y), col = "red", lty = 2)
  }
  mtext(outer=T, side=3, "Daily Return Densities", cex = 1.5, col = "blue")
}

crunch = function (data) {
  prices  = data$prices
  returns = data$returns
  main    = data$main
  symbols = data$symbols
  dates   = data$dates
  msg.1   = main
  msg.2   = paste(min(dates), "through", max(dates))
  
  returns = returns[complete.cases(returns),]   ###--now redundant
  dim(returns)
  
  frontier = portfolioFrontier(data=returns)  
  returns       = as.data.frame(data$returns)
  
  plot(frontier, 1, main = main)
  legend ("bottomright", c(msg.1, msg.2))
  getStatistics(frontier)
  cor(returns)
     
  points = frontierPoints(frontier)
  annualized.points = data.frame(
    target.risk   = points[,"targetRisk"] * sqrt(252),
    target.return = points[,"targetReturn"] * 252)
  plot (annualized.points, main = "Annual Return vs Risk")
  legend ("bottomright", msg.1)
  annualized.points
  
  risk.free.rate = 0
  sharpe = (annualized.points[,"target.return"] - risk.free.rate) / 
    annualized.points[,"target.risk"]
  max.sharpe = max(sharpe)
  max.sharpe
  
  plot ((annualized.points[,"target.return"] - risk.free.rate) / 
          annualized.points[,"target.risk"],
        main = "Sharpe Ratios",
        xlab = "Point on Efficient Frontier",
        ylab = "Sharpe Ratio")  
  legend ("bottomright", msg.1)
  
  allocations = getWeights(frontier@portfolio)
  colnames(allocations) = tolower(symbols)
  barplot.main = paste(main, "Optimal Allocations")
  
  epsilon = 0.005     #--half a percent
  z = t(allocations)
  z = z[rowSums(z) > epsilon,]
  z
  names = rep("", 49)
  at = c(1, 10, 20, 30, 40)
  for (k in at) names[k] = as.character(k)
  n.symbols = nrow(z)
  barplot(z, col= rainbow(n.symbols), legend = rownames(z), main = barplot.main, 
          xlab = "Portfolio Weights vs Index")
  
  getMu     (frontier)
  
  export    = cbind(allocations, annualized.points, sharpe)
  export
  
}



barp.allocation = function(allocations, index) {
  ###--show a Pareto bar plot of recommended weights for a portfolio selected by index
  main          = paste("Index =", index)
  allocation    = allocations[index,]
  target.return = allocation$target.return
  sharpe = allocation$sharpe
  allocation = as.data.frame( t (allocation[,tolower(symbols)]))
  msg.sharpe = paste("Sharpe =", round(sharpe,2))
  
  allocation$symbol = rownames(allocation)
  colnames(allocation)[1] = "weight"
  allocation = allocation[order(- allocation$weight),]
  
  allocation = allocation[allocation$weight > 0,]
  allocation$weight = 100 * allocation$weight
  msg.0 = paste("index =", index)
  msg.1 = paste("exp rtn =", round(100*target.return), "%")
  
  barp(allocation$weight, names.arg = allocation$symbol, main = main, ylab = "Weight (%)")
  legend("topright", legend = c(msg.0, msg.1, msg.sharpe), col = "blue", cex = .9)
  text (1:nrow(allocation), 10, paste (round(allocation$weight), "%"), col = "blue")
}


plot.composite = function (data, allocations, index,  ylim = NA) {
  ###--plot value of optimal portfolio[index]
  
  symbol    = colnames(allocations)[1]
  symbols   = colnames(allocations)[1:(ncol(allocations)-3)]
  returns   = data$returns
  dates     = as.Date(rownames(data$returns[,symbol]))
  df        = data.frame(j = 1:length(dates), date = dates)
  principal = 1000
  
  for (symbol in symbols) df[1,symbol] = allocations[index,symbol] * 1000
  
  for (j in 2:nrow(df)) {
    date = dates[j]
    
    for (symbol in symbols) {
      #if (j <= 5) print (paste(j, date, symbol, "return =", returns[,symbol][j]))
      old = df[,symbol][df$date == dates[j-1]] 
      new = old * (1 + returns[,symbol][j]) 
      df[,symbol][df$date==date] = new
      #if (j <= 5) print (paste(j, date, symbol, "new =", new))
    }
  }
  df$total = 0
  for (symbol in symbols) df$total = df$total + df[,symbol]
  
  if (! complete.cases(ylim)[1]) ylim = range(df$total)
  
  plot (df$date, df$total, main = paste("Index =", index), type = "s", xlab = "", ylab = "NAV", ylim = ylim)
  segments(min(dates), 1000, max(dates), 1000, col = "gray")
 
  msg.1 = paste("target annual return =", round(100 * allocations$target.return[index],1), "%")
  msg.2 = paste("Sharpe ratio =", round(allocations$sharpe[index], 2))
  legend ("topleft", c(msg.1, msg.2), col = "blue")
}

plot.all.relative = function(data) {
  prices = data$prices
  dates = data$dates
  symbols = data$symbols
  relative = prices
  for (symbol in symbols) relative[,symbol] = relative[,symbol] / relative[1, symbol]
  head(prices)
  head(relative)
 
  par(mfrow = c(1,1))
  palette = rainbow(length(symbols))
  plot (dates, relative[,1], main = data$main, ylim = c(.8, 1.3), col = palette[1])
  for (k in 2:length(symbols)) lines (dates, relative[,k], col =palette[k])
  legend ("topleft", symbols, lty=1, col = palette) dates = data$dates
}
  
barp.returns = function(data) {
  #--pareto chart of returns over period
  #--may need to drop columns in prices
  
  df      = data.frame(symbols = data$symbols)
  df$symbols = as.character(df$symbols)
  symbols = data$symbols
  str(df)
 
  df$return    = t((tail(data$prices,1) - head(data$prices,1)) / head(data$prices,1))
  df$return    = as.numeric(df$return)
  str(df)
  df
  df           = df[order(-df$return),]
  lo           = round(min(df$return),2) - .01
  hi           = round(max(df$return),2) + .01
  ylim         = c(lo, hi)
  ylim
  symbols      = as.character(df$symbols)
  barp (df$return, main = data$main, ylim = ylim, names.arg = symbols)
  abline       (h = seq (-2, 2, by = .05), col = "gray")
  }

#-----------------------------------------------------------------------------------------------------

index = 25

get.symbols = function(dir) {
  dir   = tolower(dir)
  setwd ("c://python_projects//compfi")
  setwd (dir)
  if (dir == "jeff")        symbols = c("ABEMX", "OAKIX", "OAKMX")
  if (dir == "folio")       symbols = c("ANN", "ADKSX", "ARMH", "BMO", "BOH", "DVY", "EAT", "ENB", "EWC", 
          "EWS", "HE", "IYR", "JWN", "LQD", "POT", "THI", "TNH", "TRP", "TU")
  if (dir == "mitre")       symbols = c("FCNKX", "AMANX", "FAGIX", "FICDX", "FXSIX", "FDIKX")
  if (dir == "tsp")         symbols = c("VFINX", "VEXMX","VDMIX","VBMFX")
  if (dir == "cyndi")       symbols = c("VWINX", "VWELX", "VFINX", "ADKSX", "MQIFX")
  if (dir == "smm_jpm_agg") symbols = c("ECON","XLV","TLT","IEI","LQD","IEMG","MBB","EWC","EZU",
                                   "EPP","EWD","EWL","EWU","IBB","TIP","EMLC","PCY","PGX","XLF","JNK","BWX","VCR",
                                   "VDC","VDE","VIS","VGT","BIV","VAW","BSV","VOX","DXJ")
  if (dir == "cyndi") {
      symbols = c("adksx", "gsra", "hyls",  "vlu", "tsla")
      
      symbols = unique(symbols)
      length(symbols)
      }
  symbols
  }

show.composites = function() {
  ###--slow.  Mya need to run twice to get ylims reasonable
  par (mfrow = c(2,2))
  for (index in seq(15, 45, by = 10)) {
    print (index)
    plot.composite (data=data, allocations=allocations, index=index, ylim = c(0, 7500))
  }
  mtext (side=3, outer=T, "Composite Reconstructions", cex=1.5, col = "blue")
}

get.smm.weights = function() {
  weights               = read.csv("weights_20140321.csv")
  colnames(weights)[3]  = "symbol"
  weights$weight        = weights$Allocation
  weights$symbol        = tolower(weights$symbol)
  weights$Allocation    = NULL
  weights               = weights[complete.cases(weights),]
  
  head(weights)
  row = data.frame(index=1)
  for (symbol in weights$symbol) {
    w = weights$weight[weights$symbol == symbol]
    row[,symbol] = w
    }
  row
  }

    
}
 

score.custom.weights = function(data, weights) {
  dates         = tail(data$dates, -1)   #--there are no returns on day 1
  returns       = as.data.frame(data$returns)
  returns$date  = dates
  composite     = data.frame(date = dates, return = NA)
  for (date in dates) {
 
    date = as.Date(date)
   
    daily.return = 0
    for (symbol in symbols) {
     
      w             =  weights[,symbol]
      r             = returns[,symbol][returns$date == date]
      this.return   = w * r
      daily.return  = daily.return + this.return
    }
    composite$return[composite$date == date] = daily.return
  }
  m       = mean(composite$return)
  s       = sd(composite$return)
  sharpe  = sqrt(252) * m / s
  
  portfolio.period.return = 1
  for (i in composite$return) portfolio.period.return = portfolio.period.return * ( 1 + i)
  interval  = max(dates) - min(dates) + 1
  years     = as.numeric(interval) / 365.25
  arr       = ( portfolio.period.return) ^ (1 / years) - 1
  
  stat      = data.frame (n = length(composite$return), mean = m, sd = s, sharpe = sharpe, arr = arr)
  print (stat)
  stat
}

custom.report = function (allocations) {

  smm.weights = get.smm.weights()
  weights     = smm.weights
  row         = score.custom.weights(data, smm.weights)
  results     = row

  n  = dim(allocations)[1]
  for (index in 1:n) {
    allocation = allocations[index,]
    row     = score.custom.weights(data, allocation)
    results = rbind(results, row)
    }
  results
  
  par(mfrow = c(2,2))
  plot (results$sd, main = "risk")
  points (1, results$sd[1], col = "red", pch = 19)
  
  plot (results$arr, main = "arr")
  points (1, results$arr[1], col = "red", pch = 19)
  
  plot (results$sd, results$arr, main = "arr vs risk")
  points (results$sd[1], results$arr[1], pch = 19, col = "red", xlab = "risk = sd of daily returns",
          ylab = "ARR")  
  
  plot (results$sharpe, main = "Sharpe Ratios", xlab = "index")
  points(1, results$sharpe[1], pch = 19, col = "red")

}
  
show.multiplot.drawdown = function (data, plots.per.page = 8, ylim = c(.8, 1.2)) {
  #--plot drawdowns for each symbol, multiple plots per pag
  source  ("c://python_projects//compfi//plot.drawdowns.ggplot2.r")
  source  ("c://python_projects//compfi//multiplot.r")
  plots = list()
  index = 0
  for     (symbol in data$symbols) {
    index = index + 1
    print (symbol)
    p     = plot.drawdowns(data, symbol, ylim = ylim)
    plots [[symbol]] = p
    if ((index %% plots.per.page == 0) | (index == length(symbols))) {
      multiplot (plotlist = plots, cols = 2)
      plots = list()
    }
  } 
}


#-------------------------------------------symbols----------------------------------------------------------

#--need to make symbols available: maybe always lower case with a toupper in get.data
main = "your main here"

#symbols = c(symbols, "SPY")

setwd    ("c://python_projects//compfi")

dir      = "cyndi"

symbols = get.symbols(dir)
data    = get.data(symbols, dir, from = "2014-03-31")
symbols  
data$main
prices = data$prices
range(data$dates)

symbols = data$symbols   #--prices have no NAs
symbols
     
show.relative.prices (data, ylim = c(.90, 1.15))

show.return.distributions(data, ymax = 200, xmax = .03)

par(mfrow = c(1,1))
barp.returns(data)
   
par (mfrow = c(2,2))
#--maybe worry if all returns are neagtive???
allocations = crunch (data)    

par (mar = c(4,3,2,2))
par (mfrow = c(1,1))
plot (1:dim(allocations)[1], allocations$sharpe, main = "Sharpe vs Index", xlab = "Index", ylab = "Sharpe Ratio")

par (mfrow = c(2,2))
par (oma = c(1,1,3,1))
for (index in c(1, 2, 5, 10, 15,20, 25,30, 35,40, 45, 49)) barp.allocation (allocations = allocations, index=index)
mtext(outer=T, side=3, main, cex =1.75)

#--show.composites()
dates     = data$dates 
msg.2     = paste(min(dates), "through", max(dates))
fname     = paste(data$main, "csv", sep = ".")
fname
write.csv (allocations, fname)
fname
