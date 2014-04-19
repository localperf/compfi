
#--plot max drawdown
#--plot.drawdowns.ggplot2.r

library (ggplot2)
library (reshape2)
source ("c://python_projects//compfi//multiplot.r")

#-- http://stackoverflow.com/questions/19387202/fill-the-region-between-two-lines-with-ggplot2-in-r



plot.drawdowns = function (data, symbol, ylim.parm = c(0, 2)) {
  
  prices       = as.data.frame(data$prices)
     
  prices$date  = data$dates
  dates        = data$dates
  
  data.1 = data.frame(x=prices$date,y=prices[,symbol])
  data.1 = data.1[complete.cases(data.1),]
  data.1$y = data.1$y / data.1$y[1]
  x = data.1$x
  y = data.1$y
  z = cummax(data.1$y)
  
  n = length(x)
  df.1 = data.frame(type = "price", date = x, value = y)[1:n,]
  df.2 = data.frame(type = "max", date = x, value = z)[1:n,]
  df = data.frame (date = x, price = y, cmax = z)[1:n,]
  summary(df)
  
  n = dim(df)[1]
  
  
  print (paste("ready to plot", symbol, dim(df)[1]))
  
  poly_df = rbind(
      setNames(df.1[,2:3], c("x","y")),
      setNames(df.2[n:1,2:3], c("x","y"))
      )
  
  main = paste(toupper(symbol), "Drawdown")
  
  p = ggplot (df) +
    geom_path(aes(date, cmax)) +
    geom_path(aes(date, price)) +
    geom_polygon(data=poly_df, aes(x=x,y=y), fill = "purple", alpha = 0.25) 
   
   print    (class(ylim.parm))
   print    (ylim.parm)
   print    (paste(symbol, length(ylim.parm), ylim.parm[1], ylim.parm[2]))
   p         = p + ylim(ylim.parm)
  
  x         = dates[floor(.1 * length(dates))]
  y         = ylim.parm[1] + .9 * (ylim.parm[2] - ylim.parm [1])
  n         = length(dates)
  gain      = round(100 * (data.1$y[n] - 1), 1)
  gain.txt  = paste("Gain = ", gain, "%", sep = "")
  print     (paste("gain", gain.txt, "%"))
  
  #p         = p + geom_text(x = as.numeric(x), y = y, family = "Courier", fontface = "plain", label = "a")
  
  p = p + ggtitle(main) +
    theme(plot.title = element_text(size = rel(2.5), lineheight = 4.5, face = "bold")) 
  
  p = p + labs (x= "", y = "Relative Price") +
    theme(axis.title = element_text(size = rel(2.5), lineheight = 4.5)) 
  
  p = p + theme(axis.text.x = element_text(size = rel(2)))
  p = p + theme(axis.text.y = element_text(size = rel(2)))
  print (class(p))

  df = data.frame(date=dates, y=y, z = z)
  fname = paste(symbol, ".csv", sep = "")
  write.csv(df, fname)
  
  p
  
}


