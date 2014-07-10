


composite.f = function (data, weights, main="your title here") {
    #--for the given weights, plot trajectory, compute sharpe and ARR
    #--weights is a vector
   
   weights = abs(weights)
   weights = weights / sum(weights)

   weights = t(t(weights))
   weights

   r = data$returns
   s = as.matrix(r) %*% weights
   dates = as.Date(rownames(r))
   sp1 = s + 1
   composite = cumprod(sp1)
   
   plot (dates, composite - 1, type = "l", main = main, xlab = "", ylab = "Return", col = "blue")
   abline (h = 0, col = "gray")
  
   df = data.frame (symbol = colnames(r))
   df$weight = as.vector(weights)
   df = df[df$weight != 0,]
   df$label = paste(df$symbol, round(100 * df$weight,1), "%")
   legend ("topleft", df$label, bty = "n", lty = 1)

   sharpe.v = sqrt(252) *  mean(s) / sd(s)
   return.v = 100 * tail(composite - 1, 1)
   years    = as.numeric (max(dates) - min(dates)) / 365.25
   arr.v = round(100 * log (tail(composite, 1)) / years, 1)
   arr.v
    stats = data.frame (symbol = c("Sharpe", "ARR"))
    stats$label = paste(stats$symbol, round(c(sharpe.v, arr.v), 2))
   legend ("bottomright", stats$label)
   
   }

  