
lib.loc = "C://Users//Gerard//AppData//Local"
lib.loc = paste(lib.loc, "//Temp//RtmpOwJ8ye//downloaded_packages", sep = "")


library (tseries)
library (zoo)
    

symbols = c("GE", "BMO", "EWC", "ENB", "TRP", "TNH", "ARMH", "ANN")


symbols = "GE"

get.data = function (symbols, start, end) {
	symbols = sort(symbols)
	
	for (sym in symbols) {
		quote = get.hist.quote(instrument=sym,
			quote  = "AdjClose",
			start=start, end=end, 
			provider = "yahoo", retclass = "zoo")
		print (paste(sym, dim(quote)[1], dim(quote)[2]))
		if (sym == symbols[1]) {
			df = data.frame(date = row.names(as.data.frame(quote)))
			}
		df[sym] = quote$AdjClose
		}
	print 	(df)
	last.col 	= length(symbols) + 1
	mat 		=  as.matrix(df[,2:last.col])
	list (df = df, mat = mat)
	}
	

symbols = c("BMO", "EWC", "ENB", "TRP", "TNH", "ARMH", "ANN",
   "BOH", "CNDA", "DVY", "EAT", "ENB", "ENY", "EWC", "EWS", "HE",
   "HNR", "IYR", "JUNR", "JWN", "LQD", "POT", "TRP")
symbols = sort(unique(symbols))
print (length(symbols))

symbols



start = as.Date("2012-06-01")
end 	= as.Date("2013-05-30")

data = get.data(symbols, start, end)

cleanWeights = function (symbols,  sol) {
	weights = data.frame(sym = symbols, weight = round(100 * sol$pw))
	weights
	weights = weights [weights$weight >= 1.0,]
	weights
	}

df = data$df
mat = data$mat
q = cov(mat)
q

X = diff(log(as.zoo(mat)))
X

port.sol = portfolio.optim(X, covmat = cov(X))
sol = port.sol
	
opt = cleanWeights(symbols, port.sol)
opt



