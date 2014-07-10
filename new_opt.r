

#-- http://blog.streeteye.com/blog/2012/01/portfolio-optimization-and-efficient-frontiers-in-r/

##-- http://statsadventure.blogspot.com/2011/10/learning-r-project-1-part-2.html

library (quantmod)
require (fImport)
require (PerformanceAnalytics)


importSeries = function (symbol, from, to) {

	##--#Function to load stock data into a Time Series object

	##--http://statsadventure.blogspot.com/2011/10/learning-r-project-1-part-2.html

	#Read data from Yahoo! Finance
	input = yahooSeries(symbol,from=from,to=to)

	#Character Strings for Column Names
	adjClose = paste(symbol,".Adj.Close",sep="")
	inputReturn = paste(symbol,".Return",sep="")
	CReturn = paste(symbol,".CReturn",sep="")

	#Calculate the Returns and put it on the time series
	input.Return = returns(input[,adjClose])
	colnames(input.Return)[1] = inputReturn
	input = merge(input,input.Return)

	#Calculate the cumulative return and put it on the time series
	input.first = input[,adjClose][1]
	input.CReturn = fapply(input[,adjClose],FUN=function(x) log(x) - log(input.first))
	colnames(input.CReturn)[1] = CReturn
	input = merge(input,input.CReturn)

	#Deleting things (not sure I need to do this, but I can't not delete things if
	# given a way to...
	rm(input.first,input.Return,input.CReturn,adjClose,inputReturn,CReturn)

	#Return the timeseries
	return(input)

	}




from 		= "2001-01-01"
to 		= "2011-12-16"
tlt 		= importSeries("tlt", from, to)
shy 		= importSeries("shy", from, to)
ief 		= importSeries("ief", from, to)
merged 	= merge(tlt, shy)
merged 	= merge(merged, ief)
vars 		= c("tlt.Return", "shy.Return", "ief.Return")
t 		= table.AnnualizedReturns(merged[,vars], Rf=mean(merged[,"shy.Return"], na.rm = T))




adksx = importSeries("ADKSX", from = "2013-01-01", to = "2014-01-04")
mqifx = importSeries("MQIFX", from = "2013-01-01", to = "2014-01-04")
merged = merge(adksx, mqifx)


chart.CumReturns(merged[,c("ADKSX.Return", "MQIFX.Return"), drop=F], legend.loc = "topleft")
chart.Correlation (merged[,c("ADKSX.Return", "MQIFX.Return")], histogram = T, pch = "+")



symbols = c("SPY", "ADKSX", "BOH", "HE", "BMO", "EAT", "DVY", "LQD")
symbol.names = symbols

getSymbols(symbols, from = "2013-01-01", auto.assign = T) 

candleChart(SPY, theme = "white", type = "candles")

nominal = data.frame (
	spy 	= SPY$SPY.Adjusted, 
	adksx = ADKSX$ADKSX.Adjusted, 
	boh 	= BOH$BOH.Adjusted, 
	he 	= HE$HE.Adjusted,
	bmo 	= BMO$BMO.Adjusted,
	eat 	= EAT$EAT.Adjusted, 
	dvy 	= DVY$DVY.Adjusted, 
	lqd 	= LQD$LQD.Adjusted)
head(nominal)
colnames(nominal) = symbols
head(nominal)

returns = apply(nominal, 2, mean)
returns



