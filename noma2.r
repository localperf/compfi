#--noma2.r
#----------------------------------------------------------------------------------------------------------

#--noma2.r

#--Elliot Noma on face tube

#--http://elliotnoma.wordpress.com/2013/01/22/construct-a-stock-portfolio-using-r/

library (corrplot)
library (fPortfolio)
library (plotrix)
library (quantmod)
library (slam)
library (timeSeries)
library (grid)
library (ggplot2)


setwd ("d://python_projects//compfi")

source ("get.data.r")
source ("barp_returns.r")
source ("composite.r")
source ("crunch.r")
source ("get_symbols.r")
source ("ggcorplot.r")
source ("multiplot.r")
source ("plot.drawdowns.ggplot2.r")
source ("show.relative.prices.r")
source ("show.return.distributions.r")
source ("get.sp500.symbols.r")

#----------------------------------------------------------------------------------------------------------





barp.allocation = function(allocations, index) {
  ###--show a Pareto bar plot of recommended weights for a portfolio selected by index
  main          = paste("Index =", index)
  allocation    = allocations[index,]
  target.return = allocation$target.return
  sharpe        = allocation$sharpe
  allocation = as.data.frame( t (allocation[,tolower(symbols)]))
  msg.sharpe = paste("Sharpe =", round(sharpe,2))
  
  allocation$symbol = rownames(allocation)
  colnames(allocation)[1] = "weight"
  allocation = allocation[order(- allocation$weight),]
  
  allocation = allocation[allocation$weight > 0,]
  allocation$weight = 100 * allocation$weight
  msg.0 = paste("index =", index)
  msg.1 = paste("exp rtn =", round(100*target.return), "%")
  
  par (mar = c(3, 4, 3, 1))
  barp(allocation$weight, names.arg = allocation$symbol, main = main, ylab = "Weight (%)")
  legend("topright", legend = c(msg.0, msg.1, msg.sharpe), col = "blue", cex = .9)
  text (1:nrow(allocation), 10, paste (allocation$symbol, round(allocation$weight), "%"), col = "blue", srt = 90)
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
  prices    = data$prices
  dates     = data$dates
  symbols   = data$symbols
  relative  = prices
  for (symbol in symbols) relative[,symbol] = relative[,symbol] / relative[1, symbol]
  head      (prices)
  head      (relative)
 
  par(mfrow = c(1,1))
  palette   = rainbow(length(symbols))
  plot      (dates, relative[,1], main = data$main, type = "l", ylim = c(.9, 1.75), col = palette[1])
  for (k in 2:length(symbols)) lines (dates, relative[,k], col = palette[k])
  abline (h = 1, col = "gray")
  legend ("topleft", symbols, lty=1, col = palette)
}

plot.corr = function (data) {
  par (cex.main = 1)
  returns = data$returns
  head(returns)
  correlation = cor(data$returns)
  par (mfrow = c(1,1))
  par (mar = c(1,2,7,1))
  corrplot(correlation, diag= F, type = "lower", main = "", order = "AOE", method = "number")
  n = dim(returns)[2]
  x.txt = 0.5 * n
  y.txt = 0.9 * n
  text (x.txt, y.txt, data$main, col = "blue", cex = 1.75)
  
}

#-----------------------------------------------------------------------------------------------------

index = 25

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

summarize = function(data) {
  prices    = as.data.frame(data$prices)
  df        = data.frame(symbol = as.character(colnames(prices)))
  
  n = dim(data$prices)[1]
  
  df$first  = as.numeric(prices[1,])
  df$last   = as.numeric(prices[n,])
  df$return = df$last / df$first
  
  df$minprice = as.numeric(colMins(prices))
  df$maxprice = as.numeric(colMaxs(prices))
  
  df$minrel = df$minprice / df$first
  df$maxrel = df$maxprice / df$first
  
  dates = as.Date(rownames(as.data.frame(prices)))
  summary(dates)
  years = as.numeric(max(dates) - min(dates)) / 365.25
  years
  
  y = as.numeric(dates[n] - dates[1]) / 365.25
  p = df$first
  v = df$last
  n.symbols = dim(prices)[2]
  start = rep(dates[1], n.symbols)
  end   = rep(dates[n], n.symbols)
  df$years = as.numeric (end - start) / 365.25
  df$arr = log(v/p) / y
  
  #--sharpe
  df$mean.return  = colMeans(as.data.frame(data$returns))
  df$sd.return    = colSds(as.data.frame(data$returns))
  df$sharpe = sqrt(252) * df$mean.return / df$sd.return
  
 
  df
}

show.return.vs.sharpe = function(data) {
  par (mfrow = c(1,1))
  par (mar = c(5,5,3,1))
  
     
  s = data$summary
  arr = 100 * s$arr
  plot (arr, s$sharpe, main = data$main, type = "n", 
        xlab = "Annualized Rate of Return (%)",
        ylab = "Sharpe Ratio", 
        xlim = range(c(0, arr)),
        ylim = range(c(0, s$sharpe)))
  abline (h = 0, v = 0, col = "gray")
  text (arr, s$sharpe, s$symbol, col = "blue")
 
}

show.allocations = function(data) {
  
  allocations = data$allocations
  
  par (mfrow = c(1,1))

  allocations$index = 1:dim(allocations)[1]
  par (mar = c(4,3,2,2))
  par (mfrow = c(1,1))
  plot (1:dim(allocations)[1], allocations$sharpe, main = "Sharpe vs Index", xlab = "Index", ylab = "Sharpe Ratio")
  best = allocations[allocations$sharpe == max(allocations$sharpe),]
  best = best[1,]
  best
  segments (best$index, -10, best$index, best$sharpe, col = "red", lwd = 2)
  
  
  par (mfrow = c(1,2))
  par (oma = c(1,1,3,1))
  indices = seq (5, 45, by = 5)
  indices = c(indices, 1, 49, best$index, best$index+1, best$index-1)
  indices = sort(unique(indices))
  indices
  for (index in indices) {
    barp.allocation (allocations = allocations, index=index)
    weights = as.numeric(allocations[index, 1:length(data$symbols)])
    main  = paste("index =", index)
    composite.f (data, weights , main)
    mtext(outer=T, side=3, data$main, cex =1.75)
  }
}
   
show.basic.barps = function (data) {
   #--barp arr, sharpe, 
   par(mfrow = c(1,1))
   par (cex.axis = 1.5)
   par (cex.lab = 1.5)
   par (cex.main = 1.5)
   par (mar = c(4,3,5,1))
   main = paste("ARR", data$main, sep = "\n")
   s = data$summary
   s = s[order(s$arr, decreasing = T),]
   p = barp (100 * s$arr, main = main)
   tags = paste(s$symbol, round(100 * s$arr), "%")
   text (1:dim(s)[1], p$y[1]/2, tags, srt=90, col = "blue") 
   abline (h = seq(25, 200, by = 25), col = "gray")
   
   main = paste("Standard Deviation", data$main, sep = "\n")
   s = data$summary
   s = s[order(s$sd.return, decreasing = T),]
   p = barp (s$sd.return, main = main)
   tags = paste(s$symbol, round(s$sd.return, 3))
   text (1:dim(s)[1], p$y[1]/2, tags, srt=90, col = "blue") 
   #abline (h = seq(25, 200, by = 25), col = "gray")
   
   main = paste("Sharpe Ratio", data$main, sep = "\n")
   s = data$summary
   s = s[order(s$sharpe, decreasing = T),]
   p = barp (s$sharpe, main = main)
   tags = paste(s$symbol, round(s$sharpe, 3))
   text (1:dim(s)[1], p$y[1]/2, tags, srt=90, col = "blue") 
}  

show.all = function (data) {
   
   returns = as.data.frame(data$returns)
   
   show.relative.prices (data)
   
   show.return.distributions(data)
   
  
  barp.returns(data)
 
  show.basic.barps(data)
  
  show.return.vs.sharpe(data)
  
  
  plot.corr (data)
  #-- may need to export p to see it
  p = ggcorplot(returns, var_text_size = 10, cor_text_limits = c(2, 30))
  p
  
  show.multiplot.drawdown (data$returns, plots.per.page=2)
  
  allocations = crunch (data)    
  data$allocations = allocations
  show.allocations(data)
  
}

 
setup = function (dir, symbols, from) {
  #--from is yyyy-mm-dd
  

  data          = get.data(symbols, dir, from = from)
  data$symbols  = sort(data$symbols)
  
  data$summary  = summarize(data)
  data$summary
  data$main
  range(data$dates)
  data
  }  
  


#-------------------------------------------symbols----------------------------------------------------------

#--need to make symbols available: maybe always lower case with a toupper in get.data
main = "your main here"

setwd ("d://python_projects//compfi")
dir = "cyndi"
from = "2013-01-01"

sp500 = sample.sp500(10)
#symbols = sp500
#symbols

symbol.list = get.symbols()
symbols = symbol.list[[dir]]
symbols = sort(symbols)
symbols


symbols = sort(unique(symbols))
symbols

data = setup(dir, symbols, from = from)

symbols = data$symbols   #--prices have no NAs
symbols
data$main

show.all(data)

#--show.ctomposites()
dates     = data$dates 
msg.2     = paste(min(dates), "through", max(dates))
fname     = paste(main, "csv", sep = ".")
fname
write.csv (allocations, fname)
fname
