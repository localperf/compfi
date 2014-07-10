
###hw1

import math
import os
import sys
import pandas as pd
import pickle
import ystockquote

from pandas import DataFrame, Series


class Quotes:
    
    log = open ("yahoo.log", "w")
    
    @classmethod
    def today_f(cls):
        """return a yyyymmdd string for today's date"""
        today = datetime.date.today()
        y = str(today.year)
        m = str(today.month)
        if len(m) < 2: 
            m = "0" + m
        d = str(today.day)
        if len(d) < 2:
            d = "0" + d
        return y + m + d
            
            
    def __init__(self):
        """makes a pandas DataFrame with closing prices 
        from start through today"""
        
        log     = Quotes.log
        
        print >> log, "version", 0.1
    
        pickles = os.listdir("pickles")
        fname = "hw1.pickle"
        
        
        if fname in pickles:
            f = open("pickles//" + fname, "r")
            self.frame = pickle.load(f)
            print >> log, "quotes restored from %s\n" % fname
            print self.frame
            return 
        
        
            
    
       
        ###symbols = ["CNC"]
    
        symbols.sort()
        df      = {}
        start   = "20110101"
        stop    = "20111231"
        
        for index, symbol in enumerate(symbols):
            quotes      = ystockquote.get_quotes(symbol, start, stop)
            df[symbol]  = DataFrame(quotes, columns = ["date", symbol])
            print       >> log, df[symbol][:1]
            
            
        print quotes[0]
        
        for symbol in symbols:
            dates = df[symbol]['date']
            print >> log, "\t\t", symbol, len(dates), min(dates), max(dates)
        print >> log
        
        self.frame = df[symbols[0]]
        for symbol in symbols[1:]:
            self.frame = pd.merge(self.frame, df[symbol], on = 'date', how = 'outer')
            print >> log, "\tadded", symbol, "len is now", len(self.frame['date'])
        
        print self.frame
        
        
        dates = self.frame['date']
        print len(dates)
        
        ofile = open ("quotes.2011.csv", "w")
        print >> ofile, "date",
        for symbol in symbols:
            print >> ofile, ",%s" % symbol,
        print >> ofile
       
        for index, date in enumerate(dates):
            print >> ofile, "%s" % date,
            for symbol in symbols:
                print >> ofile, ",%9.3f" % self.frame[symbol][index],
            print >> ofile
        ofile.close()
            
        
        print >> log, "\ndate range", min(dates), max(dates), len(dates)
        
        f = open("pickles//" + "hw1" + ".pickle", "w")
        pickle.dump(self.frame, f)
        
        
class Symbol:
    
    list = []
    
    def __init__(self, symbol, q):
        """q is pandas DataFrame"""
        
        self.symbol         = symbol
        prices              = list(q.frame[symbol])
        annual_return       = prices[-1] / prices[0] - 1
        daily_changes       = [0] + [prices[i] / prices[i-1] - 1 for i in xrange(1, len(prices))]
        mean_daily_change   = sum(daily_changes) / len(daily_changes)
        s2                  = sum([daily_change ** 2 for daily_change in daily_changes])
        s0                  = len(daily_changes)
        s1                  = sum(daily_changes)
       
        stdev               = math.sqrt (  (s0 * s2 - s1 * s1) / (s0 * (s0 -1)) )
        sharpe              = math.sqrt(252) * mean_daily_change / stdev
        
        self.prices = prices
        self.annual_return = annual_return
        self.daily_changes = daily_changes
        self.mean_daily_change = mean_daily_change
        self.stdev          = stdev
        self.sharpe         = sharpe
        Symbol.list.append(self)
        
        
       

    @classmethod
    def rank(cls):
        cls.list.sort(key = (lambda x: - x.sharpe))
        for s in cls.list:
            print "%10s %6.2f %6.2f %9.6f %9.6f %9.6f %9.6f" % \
            (s.symbol, s.prices[-0], s.prices[-1], s.annual_return, s.mean_daily_change, s.stdev, s.sharpe)
            
        
        
        
symbols = ["ANN", "ARMH","BOH", "BMO", "DVY", "EAT", "ENY", 
        "EWS", "HE", "IYR", "LQD", "POT",
        "TNH", "CNDA", "EWC", "JWN","HNR", "EWY", 
        "CNC", "TRGP", "RoST", "HUM", "VFC", "BIIB", "MA", "WCG"]
      
      
print "_" * 60
q = Quotes()
for s in symbols:
    Symbol(s, q)
    
Symbol.rank()
    
print "finis"
            
