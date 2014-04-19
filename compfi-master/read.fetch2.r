
read.fetch2 = function(prefix) {
  
  #--drop a symbol if it is has NAs

	dirname 	  = paste ("d://python_projects//compfi", prefix, sep = "//")
	setwd 	    (dirname)
	format 	    = "%m/%d/%y"
	price.name 	= "prices.csv"
	print 	    (price.name)
	prices 	    = read.table(price.name, header=T, as.is=T, sep = ",")
  
  symbols = colnames(prices)[2:dim(prices)[2]]


  for (symbol in symbols) {
    if (is.na(sum(prices[,symbol]))) prices[,symbol] = NULL 
  }
  prices
  
	good 	 	  = complete.cases(prices)
	#prices 	= prices[good,]
	dim		    (prices)

	prices$date = substr(prices$Date, 1, 10)
	prices$date = as.Date(prices$date, format)
	if (is.na(prices$date[1])) {
		prices$date = substr(prices$Date, 1, 10)
		prices$date = as.Date(prices$date)
		}
	head(prices)

	ncols 	= length(colnames(prices))
	symbols 	= colnames(prices)[2:(ncols-1)]
	symbols

	prices.t = data.frame (symbol = NA, price = NA)

	#prices = prices[prices$date >= as.Date(ISOdate(2011,1,1)),]
	head(prices)
	
	change.name = "changes.csv"
	changes = read.table(change.name, header=T, as.is = T, sep = ",")
	good = complete.cases(changes)
	#changes = changes[good,]
	head(changes)

	changes$date = substr(changes$Date, 1, 10)
	changes$date = as.Date(changes$date)
	class(changes$date)
	head(changes)

	transpose = data.frame (symbol = NA, change = NA)
	for (symbol in symbols) {
		z = changes[,symbol]
		s = rep(symbol, length(z))
		new = data.frame(symbol = s, change = z)
		transpose = rbind(transpose, new)

		z = prices[,symbol]
		s = rep(symbol, length(z))
		new = data.frame(symbol = s, price = z)
		prices.t = rbind(prices.t, new)
		}
	
	transpose = transpose[! is.na(transpose$change),]
	prices.t 	= prices.t[! is.na(prices.t$price),]
	changes.t 	= transpose[! is.na(transpose$change),]

	means = tapply(transpose$change, transpose$symbol, mean)
	sds   = tapply(transpose$change, transpose$symbol, sd)
	sharpes = sqrt(252) * means / sds
	
	list (symbols = symbols, changes = changes, prices = prices, prices.t = prices.t, sharpes = sharpes, changes.t = changes.t)
	}


