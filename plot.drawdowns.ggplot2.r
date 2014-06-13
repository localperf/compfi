
#--plot max drawdown
#--plot.drawdowns.ggplot2.r

library (ggplot2)
library (reshape2)
#source ("d://python_projects//compfi//multiplot.r")

#-- http://stackoverflow.com/questions/19387202/fill-the-region-between-two-lines-with-ggplot2-in-r
symbol = "adksx"


find.common.ylim = function (data) {
   relative = NULL
   for (symbol in data$symbols) {
      sym.relative = as.numeric(data$prices[,symbol]) / data$prices[1, symbol]
      relative = c(relative, sym.relative)
   }
   ylim = range(relative)
   ylim = c(.9 * ylim[1], 1.1 * ylim[2])
   ylim
}

annualized.return.f = function (data, symbol) {
    #--return the annualized return (continuous compounding)
    #-- p = v * exp (rt)
    prices = as.data.frame(data$prices[,symbol])
    colnames(prices) = "price"
    n = dim(prices)[1]
    p = prices$price[n]
    v = prices$price[1]
    prices$date = as.Date(rownames(prices))
    y = (as.numeric(prices$date[n] - prices$date[1]) + 1) / 365.25
    r = log(p/v) / y
    print (paste(symbol, prices$date[1], prices$price[1],
                 prices$date[n], prices$price[n], round(100*r, 2), "%"))
    r
}



plot.drawdowns = function (data, symbol, ylim) {
  
  
  prices = as.data.frame(data$prices)
  
  prices$date = data$dates
  
  data.1 = data.frame(x=prices$date,y=prices[,symbol])
  data.1 = data.1[complete.cases(data.1),]
  data.1$y = data.1$y / data.1$y[1]
  x = data.1$x
  y = data.1$y
  z = cummax(data.1$y)
  drawdown = z - y
  drawdown.pct = 100 * drawdown/z
  stats = data.frame (x=x, y=y, cummax = z, drawdown = drawdown, drawdown.pct = drawdown.pct)
  stats
  max.index = which.max(stats$drawdown.pct)
  msg = paste("Max drawdown =", round(max(drawdown.pct),2),"%")
  msg
  drawdown = stats[max.index,]
  drawdown
     
  n = length(x)
  df.1 = data.frame(type = "price", date = x, value = y)[1:n,]
  df.2 = data.frame(type = "max", date = x, value = z)[1:n,]
  df = data.frame (date = x, price = y, cmax = z)[1:n,]
  summary(df)
  
  n = dim(df)[1]
  annualized.return = data$summary$arr[data$summary$symbol == symbol]
  return.txt = paste("ARR =", round(100 * annualized.return, 2),  "%")
  
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
  
 
  p = p + geom_segment (data = drawdown, 
         mapping = aes(x=x, y=cummax, xend=x, yend=y), 
         colour = "red", alpha = 0.5, size = 2,arrow = arrow())
   
   
   p         = p + ylim(ylim)
  
  #x.txt         = dates[floor(.1 * length(dates))]
  #y.txt         = ylimit[1] + .9 * (ylimit[2] - ylimit [1])
 
  
  #p         = p + geom_text(x = as.numeric(x), y = y, family = "Courier", fontface = "plain", label = "a")
  
  p = p + ggtitle(main) +
    theme(plot.title = element_text(size = rel(2.5), lineheight = 4.5, face = "bold")) 
  
  p = p + labs (x= "", y = "Relative Price") +
    theme(axis.title = element_text(size = rel(2.5), lineheight = 4.5)) 
  
  p = p + theme(axis.text.x = element_text(size = rel(2)))
  p = p + theme(axis.text.y = element_text(size = rel(2)))
 

  df0 = data.frame(date=prices$date, y=y, z = z)
  fname = paste(symbol, ".csv", sep = "")
  write.csv(df0, fname)
  
  text = paste(return.txt, msg, sep = "\n")
  
  x.txt = df$date[1] + .3 * (df$date[n] - df$date[1])
  y.txt = ylim[1] + 0.9 * (ylim[2] - ylim[1])
  p = p + annotate("text", x = x.txt, y =y.txt, label = text, lineheight=2)
  
  p
  
}

#--Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
   require(grid)
   
   # Make a list from the ... arguments and plotlist
   plots <- c(list(...), plotlist)
   
   numPlots = length(plots)
   
   print (paste("multiplot is looking at", numPlots, "plots"))
 
   # If layout is NULL, then use 'cols' to determine layout
   if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
   }
   
   if (numPlots==1) {
      print(plots[[1]])
      
   } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
         # Get the i,j matrix positions of the regions that contain this subplot
         matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
         
         print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                         layout.pos.col = matchidx$col))
      }
   }
}


show.multiplot.drawdown = function (data, plots.per.page = 6) {
  #--plot drawdowns for each symbol, multiple plots per pag
  #source  ("d://python_projects//compfi//plot.drawdowns.ggplot2.r")

  ylim = find.common.ylim(data)
  plots = list()
  index = 0
  for     (symbol in data$symbols) {
    index = index + 1
    print (symbol)
    p     = plot.drawdowns(data, symbol, ylim)
    plots [[symbol]] = p
    if ((index %% plots.per.page == 0) | (index == length(data$symbols))) {
      multiplot (plotlist = plots, cols = 2)
      plots = list()
    }
  } 
}


show.all.drawdowns = function (data, plots.per.page = 4) {
   plots    = list()
   index    = 0
   symbols  = data$symbols
   symbols  = sort(symbols)
   
   ylim = find.common.ylim(data)
   
   for (symbol in data$symbols) {
      print (symbol)
      p = plot.drawdowns(data, symbol, ylim = ylim)
      index = index + 1
      plots[[index]] = p
     
      if (index %% plots.per.page == 0) {
         multiplot(plotlist=plots, cols=2,  ylim=ylim)
         plots = list()
         index = 0
      }
   }
   if (index > 0) multiplot (plotlist = plots, cols = 2)
}

#show.all.drawdowns(data)
