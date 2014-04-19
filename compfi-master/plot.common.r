
top.n = 12

plot.common 	= function (prices, top.n = 12) {

	prices 	= prices[complete.cases(prices),]
	prices$Date = NULL
	head		(prices)
	n 		= length(symbols)
	latter 	= symbols[2:n]
	relative 	= prices
	for (symbol in symbols) {
		relative[,symbol] = relative[,symbol] / relative[,symbol][1]
		}
	par		(mfrow = c(1,1))
	ylim 		= range(relative[,symbols], na.rm = T)

	dim		(prices)
	dim		(relative)

	n.symbols 	= length(symbols)
	n.dates 	= dim(prices)[1]

	last		= as.numeric(as.vector(relative[nrow(relative), 1:n.symbols]))

	days		= as.numeric((relative$date[nrow(relative)] - relative$date[1])) + 1
	years		= days / 365.25
	years
	
	symbols 	= colnames(relative)[1:n.symbols]
	df 		= data.frame (symbol = symbols, last = last, color = 1:n.symbols)
	df		= df [order(- df$last),]
	df$arr 	= df$last ^ (1/years) - 1

	df$gain	= df$last - 1
	df$arr 	= (1 + df$gain) ^ (1/years) - 1	#--df$gain	= df$last - 1

	df$color[df$symbol == "spy"]  = 1
	df$lwd 	= 2
	df$lwd[df$symbol == "spy"] = 6
	df 

	par 		(cex.main = 1.5)
	par 		(mar = c(3,5,5,2))
	par 		(cex.axis = 1.5)
	main 		= paste("Top", top.n, "Relative Price Gains\n%s shown are Annualized Rates of Return\n",min(prices$date), "-", max(prices$date))
	plot 		(prices$date, relative[,symbols[1]], main = main, xlab = "", ylab = "", type = "n", ylim = ylim)

	old.df 	= df
	n.symbols = min (top.n, nrow(df))
	df 		= df[1:n.symbols,]
	if (! "spy" %in% df$symbol) {
		df = rbind(df, old.df[old.df$symbol == "spy",])
		}
	for (i in 1:nrow(df)) {
		if (df$symbol[i] != "spy") df$color[i] = i
		} 


	for (symbol in df$symbol) {
		print (symbol)
		lines (prices$date, relative[,symbol], type = "s", col = df$color[df$symbol == symbol], lwd = df$lwd[df$symbol == symbol])
		}
	legend("topleft", legend = paste(df$symbol, round(df$arr * 100), "%"), col = df$color, lty = 1, lwd = df$lwd, bty = "n")

	abline (h = seq(0, 5, by = 0.1),  col = "gray")

	final 	= relative[dim(relative)[1],symbols]
	final 	= t(final)
	final 	=  as.data.frame(final)
	colnames(final) = "return"
	final

	final2 	= final[order(final$return),]
	final2 	= as.data.frame(final2)
	
	names 	= symbols[order(final$return)]
	row.names(final2) = names
  
  abline (h=1, lwd = 2, col = "blue")
  
	final2	




	}

#=====================================





