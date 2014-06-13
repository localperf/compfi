#--folio.r
#--read latest csv file

get.data = function() {
  fnames = dir(".")
  fnames = sort(fnames)
  fnames
  fname = tail(fnames, 1)
  xacts = read.csv(fname, stringsAsFactors = F) 
  colnames(xacts) =c ("date", "xact", "type", "symbol", "name", 
                     "price", "quantity", "amount", "folio", "commission", "notes")
  xacts[,"name"] = NULL
  xacts[,"folio"] = NULL
  xacts[,"commission"] = NULL
  xacts[,"notes"] = NULL
  symbols = sort(unique(xacts$symbol))
  symbols = symbols[symbols != ""]
  dates = sort(xacts$date)
  symbols
  dates
  xacts = xacts[order(xacts$date),]
  
  deposits = xacts[xacts$xact == "Client Deposits",]
  deposits = as.data.frame(tapply(deposits$amount, deposits$date, sum))
  deposits$date = as.Date(rownames(deposits), "%m/%d/%Y")
  colnames(deposits)[1] = "amount"
  deposits$cumsum = cumsum(deposits$amount)   
  deposits
  
  buys = xacts[xacts$xact == "Buy",]
  
  dividends = xacts[xacts$type == "Dividend",]
  
  data = list(symbols=symbols, deposits = deposits, buys = buys, dividends = dividends, xacts = xacts)
  
}

show.symbol = function  (data, symbol) {
   local = data$xacts[data$xacts$symbol == symbol,]
   local = local[order(local$date),]
   local$amount[local$type == "Dividend"] = - local$amount[local$type == "Dividend"]
   local$quantity[local$type == "Dividend"] = 0
   local
   history = as.data.frame(tapply(local$amount, local$date, sum))
   history$date = as.Date(rownames(history), "%m/%d/%Y")   
   history = history[order(history$date),]
   colnames(history) = "invested"
   history$date = as.Date(rownames(history), "%m/%d/%Y")
   history$cumin = round(cumsum(history$invested), 2)
   history$shares = tapply(local$quantity, local$date, sum)
   history$cumshares = cumsum(history$shares)
   history$price = tapply(local$price, local$date, mean)
   history$value = history$price * history$cumshares
   history$net = round(history$value - history$cumin, 2)
   history[history$price != 0,]
  
  
} 
#--------------------------------------------------------------------------------
setwd ("d://folio//cyndi")
data = get.data()