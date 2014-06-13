
get.sp500.symbols = function () {
   #--return the symbols in the s&p 500
   
   library(XML)
   
   #-- usess the list from Wikipedia, saved as .htm locally
   
   u = "http://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
   
   u = "List of S&P 500 companies - Wikipedia, the free encyclopedia.htm"
   
   tables   = readHTMLTable(u)
   n.rows   =  unlist(lapply(tables, function(t) dim(t)[1]))
   
   #--  the picked table is the longest one on the page
   
   z = tables[[which.max(n.rows)]]
   
    #--change . to - in symbol names
   
   symbols = as.character(z[,1])
   symbols = gsub ("\\.", "\\-", symbols)
   symbols
   
}

sample.sp500 = function (n = 10) {
  #--return n random symbols from tne S&P 500
  symbols = get.sp500.symbols()
  symbols = sample(symbols, n, replace = F)
  symbols = sort(symbols)
  symbols
}