
"""--find the initial weights that maximize the portfolio sharpe
   possibly with some constratint on total return
"""


import pandas as pd
import collections
import sys

def get_data(fname):
    
    f       = open (fname, "r")
    
    data    = pd.read_csv(fname, index_col = 0)
    print data
    print data.head()
    

    return data

def compute(data, weights):
    """use weights to calculate portfolio changes by day
    return sharpe, total return
    """
    
    data["portfolio"] = 0
    
    for index in xrange(5):
        print "bmo", index, "%12.6f" % data.ix[index]["bmo"]
    print
    
    for index in xrange(nrows):
        for symbol in symbols:
            
            frac =    weights[symbol] * data.ix[index][symbol]
            data.ix[index].portfolio += frac   
            print ix, symbol, frac, data.ix[index].portfolio
        
    print data.shape
         
    print
    print data.head()
    
            
###----------------------------------------------------------------------------

data    = get_data("changes.csv")
print data.shape
nrows   = data.shape[0]
ncols   = data.shape[1]
print   "%6d" % nrows, "rows"
print   "%6d" % ncols, "columns"
symbols = data.columns[1:]
print   "symbols", symbols, "\n"

print   "bmo[1]", data.bmo[1]
data.bmo[1] = 0.5


weights = {}
for symbol in symbols:
    weights[symbol] = 1.0 / len(symbols)
print weights

compute(data, weights)


        
    
    
    
    

