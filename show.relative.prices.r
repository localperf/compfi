
show.relative.prices = function(data) {
  #-- can work on finding ylim dynamically
  prices  = as.data.frame(data$prices)
  symbols = sort(data$symbols) 
  
  dynamics = data.frame (symbol = symbols)
  for (symbol in symbols) {
    dynamics$min[data$symbol == symbol] = min(prices[,symbol])
    dynamics$max[data$symbol == symbol] = max(prices[,symbol])
    dynamics$first [data$symbol == symbol]= prices[1,symbol]
    dynamics$min.ratio = dynamics$min / dynamics$first
    dynamics$max.ratio = dynamics$max / dynamics$first
    ylim = c(min(dynamics$min.ratio), max(dynamics$max.ratio))
  }
  dynamics
  ylim
  
  ylim = ylim - c(1,1)
  ylim
  
  dates     = as.Date(rownames(prices))
  head(dates)
  class(dates)
  par (mfrow = c(2, 3))
  par (mar = c(2,4,3,2))
  par (oma = c(1,1,5,1))
  summary = data$summary
  for (symbol in symbols) {
    local = as.numeric(prices[, symbol]) 
    local = local / local[1]  
    plot (dates,local - 1, main = symbol, ylab = "Relative Adj Close", col="blue", 
          type = "s", ylim = ylim)
    abline (h=0, col = "gray")
  
    msg.1 = paste("Sharpe =", round(summary$sharpe[summary$symbol == symbol], 2))
    msg.2 = paste("ARR =", round(100 * summary$arr[summary$symbol == symbol], 2), "%")
    legend ("topleft", c(msg.1, msg.2), bty ="n")
  }
  mtext (outer=T, side=3, data$main, cex = 1.5)
  
  #---------------------------
  df = data.frame(symbolo = symbols, last = NA)
  par (mfrow = c(1,1))
  par (mar = c(3, 5, 3, 1))
  par (cex.lab = 1.5)
  lwd = 2
  plot (dates, rep(0, length(dates)), type = "n", 
      ylim = 100 * ylim, main = data$main, xlab = "", ylab = "Relative Adj Close (%)")
  col = 0
   for (symbol in symbols) {
      y = as.numeric(data$prices[,symbol])
      y = y / y[1]
      col = col + 1
      lines (dates, 100 * (y - 1), col = col, lwd = lwd)
      df$last[df$symbol == symbol] = tail(y, 1)
      df$col[df$symbol == symbol] = col
   }
  df
  df = df[order(- df$last),]
  df
  abline (h = 0, col = "gray")
  legend ("topleft", legend = df$symbol, lty=1, col = df$col, lwd = lwd, bty = "n")
  
  #-----------
}