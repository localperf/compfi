
show.return.distributions = function (data, xmax = .04, precision = 4) {
   
   #-- mshould put period in legend
   
   returns  = data$returns
   symbols  = colnames(as.data.frame(returns))
   par      (mfrow = c(2,3))
   par      (oma = c(1,1,3,1))
   par      (cex.lab = 1.5)
   par      (mar = c(5,3,3,1))
   ymax = 0
   x = NULL
   for (symbol in symbols) {
      z = density (data$returns[,symbol], na.rm = T)
      ymax = max(ymax, max(z$y))
      print (paste("max density so far", symbol, ymax))
      x =  c(x, z$x)
   }
   ylim = c(0, ymax)   

   for (symbol in symbols) {
      symbol.returns  = returns[,symbol]
      z               = density(symbol.returns, na.rm=T)
      plot (z, main = symbol, xlim = c(-xmax, xmax), ylim = ylim, 
            col = "blue", lwd = 2, xlab = "Daily Return", ylab = "")
      msg.1 = paste("mean =", round(mean(symbol.returns), precision))
      msg.2 = paste("sdev =", round(sd(symbol.returns), precision))
      legend ("topleft", c(msg.1, msg.2), bty = "n")
      segments(0, 0, 0, max(z$y), col = "red", lty = 2)
   }
   mtext(outer=T, side=3, "Daily Return Densities", cex = 1.5, col = "blue")
}
