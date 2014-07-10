

library (corrplot)

get.data = function() {
	setwd("c://python_projects//compfi")
	data = read.table("tsphistory.csv", sep = ",", as.is = T)
	colnames(data) = c("symbol", "date", "close")

	data$close = as.numeric(data$close)
	data = data[! is.na(data$close),]
	table(data$close)
	class(data$close)
	
	data$date = as.Date(data$date, format = "%m/%d/%y")
	range(data$date)

	data$symbol = substr(data$symbol,4, 10)
	n = nchar(data$symbol) - 1
	data$symbol = substr(data$symbol,1,n)
	print (sort(unique(data$symbol)))
	data = data[data$date >= as.Date(ISOdate(2005,8,1)),]
	dim(data)
	data = data[! duplicated(data),]
	dim(data)

	df = data.frame(date = sort(unique(data$date)))
	dim(df)
	table(data$symbol)
	symbols = sort(unique(data$symbol))
	for (symbol in symbols) {
		column = data$close[data$symbol == symbol]
		print (symbol)
		print (length(column))
		df = cbind(df, symbol = column)
		}
	colnames(df)[2:11] = symbols
	head(df)
	df
	}

df = get.data()
head(df)
mat = df[,2:11]
head(mat)
nrow = dim(df)[1]
mat = as.matrix(mat, nrow = nrow, by.row = T)
head(mat)

dim(mat)
correlation = cor (mat)

par (mar = c(1,1,1,1))
par (oma = c(1,1,1,1))
par (cex.main = 1)
corrplot(correlation, method = "color", type = "lower", diag = F, title = paste("TSP", min(df$date), max(df$date)))
	
	
		
 