#--packages.r

require ("zoo")
require ("quantmod")
require ("rpart")
require ("tseries")
require ("forecast")
require ("xts")
library ("timeSeries")
       
library (zoo)
library (quantmod)
library (xts)
library(forecast)
library (rugarch)
library (Quandl)
library (fPortfolio)
library (PerformanceAnalytics)



setwd ("d://python_projects//compfi")


aapl = read.zoo("aapl.csv", sep = ",", header=T, format = "%Y-%m-%d")
head(aapl)
plot (aapl, main = "Apple closes")
aapl[which.max(aapl)]
ret.simple = diff(aapl) / lag(aapl, k = -1) * 100
ret.cont = diff(log(aapl)) * 100 
summary(coredata(ret.simple))
ret.simple[which.min(ret.simple)]
hist(ret.simple, breaks = 100, main = "AAPL simple returns")
quantile (ret.simple, p = .01)

#=============

library ("forecast")

hp = read.zoo("UKHP.csv", sep = ",", header=T, 
   format = "%Y-%m", FUN = as.yearmon)
frequency(hp)
hp.ret = diff(hp) / lag(hp, k = -1) * 100
mod = auto.arima(hp.ret, stationary = T, seasonal = F, ic = "aic")
tsdiag(mod)


====

IT = read.csv ("book//prices.csv")
IT$Date = as.Date(IT$Date, format = "%d-%B-%y")
head(IT)
assets = IT[,-1]	#--drop the date
IT$spy = NULL	#--drop spy

mu = .005

minvariance = function (assets, mu = .005) {
	return = log (tail(assets, -1) / head(assets, -1))
	Q = rbind (cov(return), rep(1,ncol(assets)), colMeans(return))
	Q = cbind(Q, rbind(t(tail(Q,2)), matrix(0,2,2)))
	b = c(rep(0, ncol(assets)), 1, mu)
	solve (Q, b)
	}


minvariance(assets)






frontier = function (assets) {
	return = log (tail(assets, -1) / head(assets, -1))
	Q = cov (return)
	n = ncol(assets)
	r = colMeans(return)
	Q1 = rbind (Q,  rep(1,n), r)
	Q1 = cbind (Q1, rbind(t(tail(Q1, 2)), matrix(0,2,2)))
	rbase = seq (min(r), max(r), length = 100)  
	s = sapply (rbase, 
		function(x) {
			y = head(solve(Q1, c(rep(0,n), 1, x)), n)
			y %*% Q %*% y
			}
		)
	plot (s, rbase, xlab = "Return", ylab = "Variance")
	}

frontier (assets)

library (timeSeries)
colnames(IT)[1] = "xyzzy"
colnames(IT)[6] = "att"
IT$xyzzy  = as.character(IT$xyzzy)
head(IT)
class(IT)
str(IT)
it.series = timeSeries(data = IT[,2:6], 
	charvec = IT$xyzzy,
	format = "%Y-%m-%d")

head(it.series$aapl)
head(it.series$msft)

returns(it.series$aapl)

it_return = returns(it.series)
chart.CumReturns(it_return, legend.loc = "topleft", main = "")






	
	
	





