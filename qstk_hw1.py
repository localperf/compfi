

"""qstk_hw1"""
import collections
import datetime
import numpy as np
import pandas
import sys
import ystockquote

def date8_f(date):
    date = "%4d%02d%02d" % (date.year, date.month, date.day)
    return date

def date_f(s):
    if "/" in s: sep = "/"
    if "-" in s: sep= "-"
    parts = s.split(sep)
    if parts[0] > 1000:
        y, m, d = parts
    else:
        m, d, y = parts
    return datetime.date(int(y), int(m), int(d))

    
def get_parms():
    if len(sys.argv) == 1:
        symbols = ["tnh", "lqd"]
        start   = "20110101"
        stop    = "20111231"
    else:
        start   = sys.argv[1]
        stop    = sys.argv[2]
        symbols = sys.argv[3:]
        assert len(symbols) > 1
        
    weights = collections.defaultdict(float)
    for symbol in  symbols:
        weights[symbol] = 1.0 / len(symbols)
    
    for index, symbol in enumerate(symbols):
        print "\t%6s %12.6f" % (symbol, weights[symbol])
        
    symbols.append("spy")
    weights["spy"] = 0.0
    print
    return start, stop, symbols, weights

def fetch(start, stop, symbols):
    df                  = {}
    symbols.sort()
    for symbol in symbols:
        quotes          = ystockquote.get_quotes(symbol, start, stop)
        trading_days    = [q.date for q in quotes]
        adj_close       = [q.adj_close for q in quotes]
        data            = {"date":trading_days, symbol : adj_close}
        df[symbol]      = pandas.DataFrame(data,  columns = ["date",  symbol])
       
    data = df[symbols[0]]
    for symbol in symbols[1:]:
        data = pandas.merge(data, df[symbol], how = 'outer')
        
    print   "\tall quotes are on hand"
    return data

def compute_pct_changes(data):
    for symbol in symbols:
        
       
        change      = data[symbol].diff()
        change[0]   = 0
        
        pct_change = [0]
        n = len(data[symbol])
        for k in xrange(1, n):
            pct_change.append(change[k] / data[symbol][k-1])
        
       
        change = pandas.DataFrame({symbol + "_chg":pct_change, \
            "date":data["date"]})
    
        data = pandas.merge(data, change, how = 'outer')
        
        
       
    print data.head()
    print data.tail()
    return data

def compute_cumpct(data):
    for index, symbol in enumerate(symbols):
        
        changes = (1 + data[symbol + "_chg"]).cumprod()
        changes = pandas.DataFrame({symbol+"_cumpct":changes, \
            "date":data["date"]}) 
        data = pandas.merge(data, changes, how  = 'outer') 
    print data.head()
    return data

def compute_weighted_values(data):
    """compute a weighted sum"""
    
    for index, symbol in enumerate(symbols):
        value   = weights[symbol] * starting_principal * data[symbol + "_cumpct"]
        colname = symbol + "_value"
        print   colname
        value   = pandas.DataFrame({colname:value, \
            "date":data["date"]})
        data    = pandas.merge(data, value, how = 'outer')
        
    print data.head()
    portfolio_value = data[symbols[0] + "_value"]
    for index in xrange(1, len(symbols)):
        symbol  = symbols[index]
        colname = symbol + "_value"
        print colname
        portfolio_value += data[colname]
        
    portfolio_value = pandas.DataFrame({"portfolio_value":portfolio_value, \
            "date":data["date"]})
            
    data = pandas.merge(data, portfolio_value, how = 'outer')
            
    change = data["portfolio_value"].diff()
    change[0]   = 0
        
    pct_change = [0]
    n = len(data[symbol])
    for k in xrange(1, n):
        pct_change.append(change[k] / data["portfolio_value"][k-1])
    

    change = pandas.DataFrame({"portfolio_chg":pct_change, \
        "date":data["date"]})

    data = pandas.merge(data, change, how = 'outer')
    
    
    data = pandas.merge(data, portfolio_value)
   
    return data
        
        
    

def export(data):
    print "\texporting"
   
    data.to_csv ("qstk_hw1.csv", index= False)
    
    print "\tfinis"
    
    
    
        

start, stop, symbols, weights = get_parms()
starting_principal = 1e6

print "HW1"
print "\tstart = %s" %start
print "\tstop  = %s" % stop
print "\tsymbols = %s" % symbols

data = fetch(start, stop, symbols)
print data.head()
print data.tail()

data    = compute_pct_changes(data)
data    = compute_cumpct(data)
data    = compute_weighted_values(data)

export (data)