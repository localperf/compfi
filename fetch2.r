

#--fetch2.r
#--uses data from fetch2.py
library (tseries)
library (psych)
library (gridExtra)

setwd ("d://python_projects//compfi")

source ("plot.common.r")

source ("pct.drawdown.r")

source ("read.fetch2.r")

source ("plot.drawdowns.ggplot2.r")

portfolioMarkowitz <- function (x, targetReturn, title = NULL, description = NULL) {
	x = as.matrix(x)
	opt = .portfolio.optim(x = x, pm = targetReturn, covmat = cov(x))
	pfolio <- NULL #added to fPortfolio original code
	pfolio$what = "portfolio"
	pfolio$method = "QP"
	pfolio$opt = opt
	pfolio$pw = opt$pw
	pfolio$pm = opt$pm
	pfolio$ps = opt$ps
	if (is.null(title)) 
		title = "Mean-Variance Portfolio Optimization"
	if (is.null(description)) 
		description = as.character(date())
	new("fPFOLIO", call = as.call(match.call()), method = "Quadratic Programming", 
		model = "Markowitz Portfolio", data = as.data.frame(x), 
		pfolio = pfolio, title = as.character(title), description = as.character(description))
	} 


effFrontier = function (averet, rcov, nports = 20, shorts=F, wmax=1) {
	mxret = max(abs(averet))
	mnret = -mxret
	n.assets = ncol(averet)
	reshigh = rep(wmax,n.assets)
	if( shorts ) {
		reslow = rep(-wmax,n.assets)
	} else {
		reslow = rep(0,n.assets)
	}

	min.rets = seq(mnret, mxret, len = nports)
	vol = rep(NA, nports)
	ret = rep(NA, nports)
	for (k in 1:nports) {
		port.sol = NULL
		try(port.sol <- portfolio.optim(x=averet, pm=min.rets[k], covmat=rcov,
		reshigh=reshigh, reslow=reslow,shorts=shorts),silent=T)
		if ( !is.null(port.sol) )	{
			vol[k] = sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
			ret[k] = averet %*% port.sol$pw
			}
		}
	return(list(vol = vol, ret = ret))
	}

symbol = "BOH"

	


least.correlated = function (s, p, correlation) {
	###--find sym with smallest correlation, s.t. positive return

	p[,"spy"] = NULL
	n = dim(prices)[1]
	
	row = correlation[s,]
	row [row <= 0] = NA
	min.cor = min(row, na.rm = t)
	min.s = which(row == min.cor)
	min.s
	}

annualized_return = function(df) {
	n = dim(df)[1]
	total.pct = (df$price[n] - df$price[1]) / df$price[1]
	
	n.days = as.numeric (df$date[n] - df$date[1] + 1)
	n.years = n.days / 365.25
	arr = (1 + total.pct) ^ (1/n.years) - 1
	print (paste(df$symbol[1], n, df$price[n], df$price[1], df$price[n] - df$price[1],
	   round(total.pct, 3), n.days, round(n.years,2), round(100 * arr, 1)))
	arr
	}

	
#   annualized_return(data)

setup = function(syms) {
	syms = sort(syms)
	syms = unique(syms)
	par (mfrow = c(2,2))
	df = NA
	for (symbol in syms) {
		prices = get.hist.quote(symbol, start = start, end=end, 
			quote = "AdjClose", retclass = "ts")
		
		data = data.frame(prices)
		colnames(data)[1] = "price"
		start = tsp(prices)[1] - magic
		stop  = tsp(prices)[2] - magic
		start = as.Date(start, origin = ISOdate(1970,1,1))
		stop = as.Date(stop, origin = ISOdate(1970,1,1))
		start
		stop
		data$date = seq(start, stop, by = 1)
		data$symbol = symbol
		head(data)
		tail(data)
		data = data[! is.na(data$price),]
		dim(data)
		m = pct.maxdrawdown(data)
		#print (paste(symbol, "Sharpe =", s, "maxdd =", m))
		}
	}

correlation = function (changes) {
	changes 	= changes[complete.cases(changes),]
	ncols 	= dim(changes)[2]
	mat 		= changes[,2:(ncols-1),]
	mat 		= as.matrix(mat)
	correlation = cor(mat)
	cor.plot 	(correlation)
	pairs 	= data.frame(s1 = NA, s2 = NA, corr = NA)
	symbols 	= colnames(changes)
	symbols 	= symbols[symbols != "Date"]
	symbols 	= symbols[symbols != "date"]
	symbols
	for (s1 in symbols) {
		for (s2 in symbols) {
			corr 	= correlation[s1, s2]
			row 	= data.frame(s1 = s1, s2 = s2, corr = corr)
			pairs = rbind(pairs, row)
			}
		}
	pairs = pairs[! is.na(pairs$corr),]
	pairs = pairs[pairs$corr != 1,]
	pairs = pairs[order(pairs$corr),]
	pairs = pairs [pairs$s1 < pairs$s2,]

	print (head(pairs))
	print (tail(pairs))
	list (correlation = correlation, pairs = pairs)
	
	}

i = 1

show.fetch2.graphs = function (symbols, prices, sharpes, jpeg.v = F) {
	par (mfrow = c(2,2))
	n = length(symbols)
	for (i in 1:n) {
		if (jpeg.v) {
			dev.off()
			fname = paste(prefix, symbols[i], "jpg", sep = ".")
			jpeg(fname, h=1000, w = 1600, q=100)
			}
		symbol 	= symbols[i]
		p 		= prices[,symbol]
		d 		= prices$date
		df 		= data.frame(symbol = symbol, date = d, price = p)
		df 		= df[! is.na(df$price),]
		df$sharpe 	= sharpes[i]
		pct.maxdrawdown(df)
		}
	if (jpeg.v) dev.off()
	}
	
yearly.target = .15
changes = data$changes

get.summary = function() {
    #--find annual rates of return, and return i  n a data frame 
  df = read.csv ("summary.csv")
  names = c("Symbol", "Annualized.Rate.of.Return....", "Sharpe.Ratio", "Max.Draw.Down....") 
  df = df[,names]
  colnames(df) = c("symbol", "arr", "sharpe", "mdd")
  df
}

optimize = function (changes, yearly.target) { 
  
	dim			(changes)
	ok 			= complete.cases(changes)
	changes 		= changes[ok,]	
	changes$spy		= NULL
	dim(changes)

	last.col 		= dim(changes)[2] - 1
	mat 			= changes[,2:last.col]
	mat 			= as.matrix(mat)
	class			(mat)
	dim 			(mat)
	changes$spy 	= NULL
	symbols 		= colnames(mat)
	#--trading days
	
	daily.target 	= (1 + yearly.target) ^ (1/252) - 1
	print 		(yearly.target)
	print 		(daily.target)

	averet 		= matrix(colMeans(mat), nrow = 1)
	covmat 	  = cov(mat)
	q 			  = portfolio.optim(x = averet, pm = daily.target, covmat = covmat, shorts=F, reslow = rep(0.0, length(symbols)))

	q$pw 			= round(100 * q$pw, 1)
	print 		(100 	* yearly.target)
	weights 		= data.frame(symbol = symbols, weight = q$pw)
	weights 		= weights[order(- weights$weight),]
	weights$cs 		= cumsum(weights$weight)
	weights 		= weights[weights$weight >= 0.5,]   ##drop if rounded weight less than 1%
	
	summary = get.summary()
  weights = merge (weights, summary, by = "symbol", all.y = F)
  weights = weights[order(-weights$weight),]
  weights$expected = weights$weight/100 * weights$arr
  weights$cumsum.expected = cumsum(weights$expected)
  print (paste("these weights imply an expected return of", round(100*sum(weights$expected),1), "%"))
	weights
	}

show.boxes = function(changes.t) {

	par (mfrow = c(1,1))

	boxplot (changes.t$change ~ changes.t$symbol, main = "Daily % Changes")

	medians = tapply (changes.t$change, changes.t$symbol, median)
	medians
	}
	
show.drawdowns = function (prefix) {
  #-- http://stackoverflow.com/questions/6069362/arranging-multiple-ggplot2-plots?rq=1
  setwd ("d://python_projects//compfi")
  data   	  = read.fetch2(prefix)
  print   ("data read ok")
  
  plots = list()
  index = 0
  
  par (mfrow = c(2,2))
  symbols = data$symbols
  for (symbol in symbols) {
    index = index + 1
    print (paste("drawdown", symbol))
    p = plot.drawdowns(data, symbol)
    plots[[index]] = p
    if (index == 4) {
      print (do.call(grid.arrange, plots))
      plots = list()
      index = 0
    }
  }
  if (length(plots) != 0) {  ## capture remaining plots that have not been written out
    print (do.call(grid.arrange,  plots))
  }
}

total.returns.f = function (p) {
	p[,"Date"] 	= NULL
	p[,"spy"] 	= NULL
	p[,"date"] 	= NULL

	n 		= dim(prices)[1]
	returns 	= (p[n,] - p[1,]) / p[1,]
	returns
	}

total.returns = total.returns.f(prices)


symbol = "ANN"
symbol = "TNH"

prefix = "vwenx"
prefix = "blair"
prefix = "dana"
prefix = "sharebuilder"




prefix = "temp"

prefix = "matsuba"
prefix = "nick"



prefix = "edelman"

prefix = "bernstein"

prefix = "farr"

prefix = "book"
prefix = "folio"
prefix = "gcb"
prefix = "folio"
prefix = "jeff"

prefix = "folio"

prefix = "cyndi"
prefix = "smm_jpm_agg"


###-----------------------------------------------------------------------


data 		  = read.fetch2(prefix)

symbols 	= data$symbols
changes 	= data$changes
prices 	  = data$prices
prices.t	= data$price.t
sharpes 	= data$sharpes
changes.t = data$changes.t

dim		    (prices)
length	  (complete.cases(prices))
range		  (prices$date)

returns 	= total.returns.f(prices) 

show.fetch2.graphs(symbols, prices, sharpes, jpeg.v = T)
corr      = correlation(changes)

show.drawdowns(data)

optimal = optimize  (changes, yearly.target = .20)
head      (prices)
plot.common(prices)
#------------------------------------------------------------------------------------------------------------------

magic = 25569




