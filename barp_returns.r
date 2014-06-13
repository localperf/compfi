barp.returns = function(data) {
  
  #--pareto chart of returns over period
  #--may need to drop columns in prices
  prices = data$prices
  head(prices, 1)
  tail(prices, 1)
  n = nrow(prices)
  print (prices[c(1, n),])
  
  
  
  df        = data$summary
  
 
  df        = df[order(-df$return),]
  symbols   = df$symbol
  print     (df)
  par (mar = c(3,4,5,3))
  par (cex.main = 1.5)
  par (mfrow = c(1,1))
  main = paste(data$main, "Total Return", sep = "\n")
  barp (df$return - 1, main = main, ylab = "Raw growth")
  y.text = max(df$return -1) / 2
  text (1:length(symbols), y.text, paste(df$symbol, round(df$return,2)-1), srt = 90, col = "blue", cex = 1.2)
  
  

  main = paste(data$main, "Annualized Rate of Return", sep = "\n")
  barp (100 * df$arr, main = main, ylab = "ARR (%)")
  y.text = 100 * min(df$arr)
  text (1:length(symbols), y.text, paste(df$symbol, round(100*df$arr,1), "%"), 
        srt = 90, col = "blue", cex = 1.2, adj = 0)
   
}
