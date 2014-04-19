
###fetch

import collections
import datetime
import math
import os
import sys
import matplotlib
import ystockquote

from pandas import DataFrame, merge

class Quotes:
    
    log = open ("fetch.log", "w")
    
    def __init__(self, start, stop, symbols):
        """makes a pandas DataFrame with prices from Yahoo
        be careful what price we get!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
        from start through today"""
        
        log     = Quotes.log
        
        print >> log, "version", 0.1
    
        symbols.sort()
        df      = {}
        print "\n\t%d symbols\n" % len(symbols)
        
        for index, symbol in enumerate(symbols):
            if trace_v: 
                print "\t>>> fetching", symbol, start, stop
            quotes      = ystockquote.get_quotes(symbol, start, stop)
            df[symbol]  = DataFrame(quotes, columns = ["date", \
                symbol + "_close", symbol + "_adj_close"])
            print       >> log, df[symbol][:1]
            
        for symbol in symbols:
            dates = df[symbol]['date']
            print >> log, "\t\t", symbol, len(dates), min(dates), max(dates)
        print >> log
        
        self.frame = df[symbols[0]]
        for symbol in symbols[1:]:
            self.frame = merge(self.frame, df[symbol], on = 'date', how = 'outer')
            print >> log, "\tadded", symbol, "len is now", len(self.frame['date'])
        
        if trace_v:
            print self.frame.head()
        
        dates = self.frame['date']
        print len(dates), "trading days"
        
        ###----------------------------
        
        ofile   = open ("prices.csv", "w")
        dfile   = open ("changes.csv", "w")
        s       = "date,"+ ",".join(symbols)
        print   >> ofile, s
        print   >> dfile, s
       
        last        = {}
        clows       = {}
        adj_close   = {}
    
        for index, date in enumerate(dates):
            print >> ofile, "%s" % date,
            print >> dfile, "%s" % date,
            for symbol in symbols:
                clows[symbol] = self.frame[symbol + "_close"][index]
                adj_close[symbol] = self.frame[symbol + "_adj_close"][index]
                print >> ofile, ",%9.3f" % clows[symbol],
                
                if index == 0: 
                    pct = 0
                    
                else:
                    pct = (adj_close[symbol] - last[symbol]) / last[symbol] 
                    
                    
                last[symbol] = adj_close[symbol]
                
                
                print >> dfile, ",%12.6f" % pct,
                    
            print >> ofile
            print >> dfile
            
        ofile.close()
        dfile.close()
            
        print >> log, "\ndate range", min(dates), max(dates), len(dates)
        print "\tQuotes complete"
        
    @classmethod
    def today_f(cls):
        """return a yyyymmdd string for today's date"""
        today = datetime.date.today()
        today = date8_f(today)
        return today
        
        
        
class Symbol:
    
    list = []
    
    def __init__(self, symbol, q):
        """q is a Pandas DataFrame
        now uses population standard deviation"""
        
        self.symbol             = symbol
        prices                  = list(q.frame[symbol + "_adj_close"])
        total_return            = prices[-1] / prices[0] - 1
        daily_changes           = [0] + [prices[i] / prices[i-1] - 1 \
            for i in xrange(1, len(prices))]
    
        mean, stdev, sharpe     = sharpe_f(daily_changes, "daily")
    
        self.prices             = prices
        self.total_return       = total_return
        """assumes we have all the prices from start to stop"""
        self.annual_return      = annual_return_f(prices[0], prices[-1], 
                                    start, stop)
        self.daily_changes      = daily_changes
        self.mean_daily_change  = mean
        self.stdev              = stdev
        self.sharpe             = sharpe
        self.drop               = self.max_drop_f(q)
       
        Symbol.list.append(self)
        

    def max_drop_f(self, q):
        """find max draw down percent in data
        max delta as a percent of any local min to greatest previous max
        """
        
        Drop = collections.namedtuple( \
            "Drop", 
            "drop, max_price, max_price_date, min_price, min_price_date") 
        
        prices  = self.prices
        dates   = list(q.frame["date"])
        assert len(prices) == len(dates)
        
        drop = Drop(drop = 0, 
            max_price = prices[0], max_price_date = dates[0],
            min_price = prices[0], min_price_date = dates[0])
            
        max_price       = prices[0]
        max_price_date  = dates [0]
       
        
        for index, price in enumerate(prices):
            if price > max_price:
                max_price           = price
                max_price_date      = dates[index]
                
            this_drop               = 1.0 - price / max_price
            if trace_v:
                print \
                    "%10s %12s max = %10.2f on %12s today = %10.2f %10.3f" % \
                    (self.symbol, dates[index], 
                    max_price, max_price_date, 
                    price, 100.0 * this_drop)
                    
            if this_drop > drop.drop:
                max_drop            = max(this_drop, drop.drop)
                max_drop_date       = dates[index]
                drop = Drop(
                    drop            = this_drop,
                    max_price       = max_price, 
                    max_price_date  = max_price_date,
                    min_price       = price,
                    min_price_date  = dates[index]
                    )
                if trace_v:
                    print "\t%8s" % self.symbol, \
                        "\tmax drop = %6.2f pct: %6.2f on %10s vs %6.2f on %10s" \
                        % drop
                
        return drop
    
    @classmethod
    def show_drops(cls):
        cls.list.sort(key = lambda x: x.drop.drop)
        
        print "\nRanked by Maximum drops\n"
        for index, s in enumerate(cls.list, 1):
            
            print "%6d." % index,

            print "\t%8s" % s.symbol, 
            print "\tmax drop = %6.2f pct     " % (100.0 * s.drop.drop), 
                    
            print "%6.2f on %10s    vs    %6.2f on %10s" \
                    % s.drop[1:],
                    
            print "%6d days" % \
                (s.drop.min_price_date - s.drop.max_price_date).days
    
    @classmethod
    def rank(cls):
        print "\nRanked by Sharpe ratios\n"
        cls.list.sort(key = (lambda x: - x.sharpe))
        for index, s in enumerate(cls.list, 1):
            print "%6d. %10s %12.2f %12.2f %12.6f %12.6f %12.6f %15.6f" % \
            (index, s.symbol, s.prices[-0], s.prices[-1], 
                100.0 * s.total_return, 
                s.mean_daily_change, s.stdev, s.sharpe)
                
    @classmethod
    def export(cls):
        if trace_v:
            print "\texport for %s" % symbols
        cls.list.sort (key = lambda x: x.symbol)
        duration    = (from_date8_f(stop) - from_date8_f(start)).days
        fname       = "sharpe_history.csv"
        try:
            f = open(fname, "r")
            f.close()
        except IOError:
            print "\tcreating", fname
            f       = open (fname, "w")
            print   >> f, "start,stop,duration",
            for symbol in cls.list:
                print >> f, ",%s" % symbol.symbol,
            print   >> f
            f.close ()
            
        ofile = open (fname, "a")
        print >> ofile, "%s,%s,%d" % \
            (from_date8_f(start), from_date8_f(stop), duration),
        for symbol in cls.list:
            print >> ofile, ",%f" % symbol.sharpe,
        print >> ofile
        ofile.close()
        
def annual_return_f (p1, p2, d1, d2):
    years   = (from_date8_f(d2) - from_date8_f(d1)).days / 365.25
    cagr    = (p2/p1) ** (1.0 / years) - 1
    return cagr
        
    
def to_date8_f(date):
    date = "%4d%02d%02d" % (date.year, date.month, date.day)
    return date

def from_date8_f(s):
    year    = int(s[:4])
    month   = int(s[4:6])
    day     = int(s[-2:])
    return datetime.date(year, month, day)
    

def date_f(s):
    if "/" in s: sep = "/"
    if "-" in s: sep= "-"
    parts = s.split(sep)
    if parts[0] > 1000:
        y, m, d = parts
    else:
        m, d, y = parts
    return datetime.date(int(y), int(m), int(d))

def sharpe_f (s, freq = "daily"):
    """uses population standard deviation, not sample
    
    
    To add to what Tucker's said in the hope that it's helpful: 
    Sharpe ratio intends to give you a score that is annual in nature. 
    Your underlying data however is, say daily (so you calculate the average 
    daily return and corresponding daily stdev), therefore it follows that 
    you need to multiply the result by the number of days in a year to adjust 
    it back to an annual value. This is typically 252 or 250 depending. 
    Similarly, if your underlying data is monthly (so you calculate a monthly 
    average and stdev) then it follow to adjust back to a yearly score 
    you must multiply by 12.
    
    
    K is a function of the frequency of sampling. 
    SQRT(252) for daily, SQRT(12) for monthly, SQRT(1) for annually.
    
    ---
    
    This is perhaps a minor point, but in the Excel demo of calculating 
    the Sharpe ratio for Apple, it seems to me that the daily return for 12/1 
    should not have been included in the calculation 
    of the average or standard deviation. It was set to 0 since 
    there was no data for the previous day, 
    but it actually was unknown rather than 0.  
    
    Instructor: you are correct.
    
    """
    
    
    if freq == "daily":
        k = math.sqrt(252.0)
    elif freq == "monthly":
        k = math.sqrt(12.0)
    elif freq == "yerarly":
        k = 1
    else:
        print "expected daily or monthly or yearly"
        raise ValueError
    
    ss      = s[1:] ###--exclude artificial 0, see above
        
    s0      = len(ss)
    s1      = sum(ss)
    s2      = sum([p**2 for p in ss])
    mean    = 1.0 * s1 / s0
    stdev   = math.sqrt (  (s0 * s2 - s1 * s1) / (s0 ** 2) )
    sharpe  = k * mean / stdev
    return mean, stdev, sharpe

def sharpe_history():
    print   "\tsharpe history"
    f       = open("changes.csv", "r")
    lines   = f.readlines()
    header  = lines[0]
    symbols = header.strip().split(",")[1:]
    changes = collections.defaultdict(list)
    lines   = lines[1:]
    dates   = []
    ofile   = open ("sharpe.csv", "w")
    comma = ","
    print >> ofile, "date,", 
    for symbol in symbols:
        print >> ofile, symbol + comma,
    print >> ofile
    
    for line in lines:
        parts   = line.strip().split(",")
        date    = date_f(parts[0])
        dates.append(date)
        for index, symbol in enumerate(symbols):
            changes[symbol].append(float(parts[index+1]))
    for ending in xrange(30, len(lines)):
        date = dates[ending - 1]
        print >> ofile, date,
        for symbol in symbols:
            data    = changes[symbol][:ending]
            sharpe  = sharpe_f(data)
            print   >> ofile, ",%f" % sharpe,
        print >> ofile
    ofile.close()
            
    

            
def usage():
    """if 2 dates and symbols, use dates and symbols"""

    for k in xrange(2):
        print "_" * 140

    try:
        start   = sys.argv[1]
        stop    = sys.argv[2]
        symbols = sys.argv[3:]
    except IndexErrror:
        print "\tusage is fetch start stop symbol symbol ..."
        print "\te.g., fetch 20110101 20121219 AAPL MA EWC"
        print "\t\tstart    =", start
        print "\t\tstop     =", stop
        print "\t\tsymbols  =", symbols
        sys.exit()        
        
    assert len(symbols) >= 1
    assert len(start) == len(stop) == 8
    
    symbols = [symbol.strip().lower() for symbol in symbols]

    if "all" in symbols:
        symbols.remove("all")
        symbols += ["mitre", "folio", "mqifx"]

    if "folio" in symbols:
        symbols.remove("folio")
        for f in folio: symbols += f
        
    if "mitre" in symbols:
        symbols.remove("mitre")
        for m in mitre: 
            symbols.append(m)
            
    if "morgan" in symbols:
        symbols.remove ("morgan")
        symbols += morgan
            
    if not "spy" in symbols:
        symbols.append("spy")
   
    symbols = list(set(symbols))
    symbols.sort()
    
    print "\n\tfetching data from Yahoo for", 
   
    print "\t", symbols
    
    print "\n\toutputs are in prices.csv and changes,csv (% changes)\n" 
    
    return  start,  stop, symbols
        

    
    
      
###----------------------------------------------------------------------------

    
###--want to be able to get close and adj_close
###--so make rtwo price files and two change files    

"""We use SPY as a symbol for S&P500."""

trace_v = False

folio   = ["ann", "armh", "bmo", "boh", "cnda", "dvy", "eat", "eny", "ewc", \
    "ews", "he", "hnr", "iyr", "junr", "jwn", "lqd", "pot", "tnh"],


mitre   = ["fcnkx", "amanx", "fagix", "fdikx", "ficdx", "fslcx", "fxsix"]

all     = ["mitre", "folio", "mqifx"]

morgan = ["vgt" , "vis", "vde", "vcr", "ezu", "xlv"] 
    
start, stop, symbols = usage()
        
print "_" * 60
q = Quotes(start, stop, symbols)
  
for s in symbols:
    Symbol(s, q)
    
print "\tSymbols instantiated"
    
Symbol.rank()


Symbol.export()

Symbol.show_drops()

#sharpe_history()    
    
print "finis"
            
