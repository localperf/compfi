###compare

get.data = function() {
   	print ("reading")
   	setwd("d://python_projects//compfi")
   	data = read.table ("changes.csv", header=T, sep = ",", as.is = T)
	
	print (class(data$date))
	data$snp = data$X.gspc
   	data$date= as.Date(data$date)
  	data
   	}



show = function (names, lwd = 3) {
   
	data 		= get.data()
	print		("read ok")
   	n.stocks 	= length(names)
	print 	(n.stocks)
	values 	= data.frame(trading.day = 1:dim(data)[1])

   	for (k in 1:length(names)) {
		print 	(paste("valuing", k))
      	pct		= 1 + data[,names[k]]
      	value 	= 1000 * cumprod(pct)
      	values[,k]	= value
      	colnames(values)[k] = names[k]
		}
   	head(values)

	stats =  data.frame(name = names, sharpe = NA)
	n	= dim(data)[1]
	for (k in 1:length(names)) {
		name = names[k]
		xbar = mean(data[,name])
		std  = sqrt(var(data[,name]))
		sharpe = round(sqrt(n) * xbar / std, 3)
		print (paste(name, n, xbar, std, sharpe))
		stats$sharpe[stats$name == name] = sharpe
		}
	stats
	
    	ylim = range(values)
    	values$trading.day = 1:dim(values)[1]		
	print (ylim[1])
	print (ylim[2])
   
    	plot (values$trading.day, values[,names[1]], 
		type = "n", 
		ylim = ylim, 
		xlab = "Trading Day From 1/1/12", 
		ylab = "Value", 
		main = "Starting $ Comparison")
	
      #axis (side = 1, at = at, labels = labels)
	abline (h = 1000, col = "gray")
    	for (k in 1:n.stocks) { 
		lines (values$trading.day, values[,k], col = k, lwd = lwd)
	 	}
   	x.text = 0.95 * max(values$trading.day)
   	last.day = dim(values)[1]
	sharpes = paste(names, stats$sharpe)
	print (sharpes)
   	text (x.text, values[last.day, 1:n.stocks], sharpes, col = 1:n.stocks)
	legend ("topleft", c("Sharpe Ratios", sharpes), col = 1:n.stocks)
	values
	}

data = get.data()
head(data)
names = colnames(data)[2:length(colnames(data))]
names
values = show (names)





