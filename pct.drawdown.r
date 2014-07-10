

pct.maxdrawdown = function (df) {

	#--		expects a df with date, price, symbol, sharpe

	print		(paste("*****", "pct.maxdrawdown", df$symbol[1]))

	df 		= df[complete.cases(df),]

	n 		= dim(df)[1]
	
	df$cmax 	= cummax (df$price)
	df$delta 	= df$cmax - df$price
	df$pct 	= df$delta / df$cmax

	max.pct 	= max(df$pct)
	
	bottom.x 	= which (df$pct == max.pct)[1]
	bottom.y 	= df$price[bottom.x]

	prior 	= df[ 1:(bottom.x -1),]
	top.y		= max(prior$price)
	tops 		= which (prior$price == top.y)
	top.x 	= tops[1]
	
	par 		(cex.axis = 1.5)
	par 		(cex.main = 2)

	main 		= paste(df$symbol[1], "\nfrom", min(df$date), "through", max(df$date))
	
	plot 		(df$date, df$price, main = main, type = "s", 
				xlab = "", ylab = "Adjusted Close", col = "blue")

	abline 	(v = as.Date(c("2013-04-10", "2013-10-09")), col = "gray") 

	points 	(df$date[bottom.x], bottom.y, 	pch = 19, col = "red")
	points 	(df$date[top.x], 	     top.y, 	pch = 19, col = "red")
	indices 	= seq(top.x, bottom.x, by = 1)
	lines 	(df$date[indices], df$price[indices], col = "red", lwd = 2, type = "s")


	
	n		= length(indices)
	start 	= df$date[indices[1]]	
	end		= df$date[indices[n]]
	first 	= df$price[indices[1]]
	last		= df$price[indices[n]]
	max.pct.str	= paste(round(100*max.pct, 1), "%")
		
	x		= c(start, start, end, end, start)
	y		= c(first, last, last, first, first)	
	#polygon	(x, y, lty = 2, border = "blue")
	
	msg.1 		= paste("Max drawdown =", max.pct.str, "from", first, "on", start, "to", last, "on", end)
	n 			= dim(df)[1]
	total.return 	= 100 * (df$price[n] - df$price[1]) / df$price[1]
	msg.2 		= paste("Total Return =", round(total.return,2), "%")
	arr 			= annualized_return(df)
	arr 			= round(100 * arr, 1)	
	msg.3 		= paste("Annualized return =", arr, "%")
	msg.4 		= paste("Sharpe =", round(df$sharpe[1], 2))

	legend 		("topleft", c(msg.1, msg.2, msg.3, msg.4))
	max.pct
	}
	

	