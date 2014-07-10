
crunch    = function (data) {
  prices  = data$prices
  returns = data$returns
  main    = data$main
  symbols = colnames(data$prices) #--make sure we preserve order
  dates   = data$dates
  msg.1   = main
  msg.2   = paste(min(dates), "through", max(dates))
  
  returns = returns[complete.cases(returns),]   ###--now redundant
  dim(returns)
  
  frontier = portfolioFrontier(data=returns)  
  returns       = as.data.frame(data$returns)
  
  par (mar = c(5, 5, 3, 1))
  plot(frontier, 1, main = main)
  legend ("bottomright", msg.1)
  
  getStatistics(frontier)
  cor(returns)
  
  
  
  points = frontierPoints(frontier)
  annualized.points = data.frame(
     target.risk   = points[,"targetRisk"] * sqrt(252),
     target.return = points[,"targetReturn"] * 252)
  plot (annualized.points, main = "Annual Return vs Risk",
        xlab = "Target Risk", ylab = "Target Return")
  legend ("bottomright", msg.1)
  #annualized.points
  
  risk.free.rate = 0
  sharpe = (annualized.points[,"target.return"] - risk.free.rate) / 
    annualized.points[,"target.risk"]
  max.sharpe = max(sharpe)
  max.sharpe
  
  plot ((annualized.points[,"target.return"] - risk.free.rate) / 
          annualized.points[,"target.risk"],
        main = "Sharpe Ratios",
        xlab = "Index on Efficient Frontier",
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

