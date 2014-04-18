###marketsim

import collections
import datetime
import math
import random
import sys
import ystockquote

class Order:
   
    quotes      = {}                                ###--key is (symbol, date): value is price
    orders      = collections.defaultdict(list)     ###indexed sam e way
    holdings    = collections.defaultdict(float)    ###--indexed by symbol; value is shares
    symbols     = set([])
   
    
    
    def __init__(self, date, symbol, shares):
        self.symbol = symbol
        self.shares = shares
        self.date   = date
        Order.orders[date].append(self)
        Order.symbols.add(symbol)
       
    @classmethod
    def get_orders(cls):
        """need to handle multiple orders for same symbol, date tuple"""
        print "\treading orders"
        fname   = sys.argv[2]
        f       = open (fname, "r")
        lines   = f.readlines()
        print   "\tread %d lines" % len(lines)
        for line in lines:
            line        = line.strip()
           
            parts       = tuple(line.split(","))
            y,m,d,symbol, order_type, shares = parts
            date        = datetime.date(int(y), int(m), int(d))
           
            shares      = float(shares)
            if order_type.upper().strip()[0] == "S":
                shares = -shares
            order = Order(date, symbol, shares)
        print "\t\treturning %d orders" % len(Order.orders)
        cls.symbols = list(cls.symbols)
        cls.symbols.sort()
        print "\tsymbols", len(cls.symbols), cls.symbols
        return cls.orders
    
   
    
    @classmethod
    def get_quotes(cls):
        

        dates = cls.orders.keys()
        print "\t", "dates", len(dates), dates[0], dates[-1]
        dates.sort()
        print "\t", "dates", len(dates), dates[0], dates[-1]
        mindate = dates[0]
        maxdate = dates[-1]
        mindate = date8_f(mindate)
        maxdate = date8_f(maxdate)
        symbols = cls.symbols
        
        for symbol in symbols:
            print "\tfetching %s" % symbol
            quotes = ystockquote.get_quotes(symbol, mindate, maxdate)
            for quote in quotes:
                date, price, adj_close = quote
                cls.quotes[symbol, date] = price
            date    = datetime.date(2011,6,10)
            print   "\t%10s %10s %10.2f\n" % \
                (symbol, date, cls.quotes[symbol, date])
        print   "\tall quotes are on hand"
        
    @classmethod
    def stub_get_quotes(cls):
        dates = xdaterange(datetime.date(2011,1,10), datetime.date(2011,12,20))
        for index, symbol in enumerate(cls.symbols):
            quotes = [random.uniform(10 + 10*index, 10 + 10*index + index + 1) \
                for d in dates]
            quotes = [round(q,1) for q in quotes]
            for dindex, date in enumerate(dates):
                cls.quotes[symbol, date] = quotes[dindex]
        print   "\tall (faked) quotes are on hand"    

        ofile = open ("quotes.csv", "w")
        print >> ofile, "date", ",", ",".join(cls.symbols)
        dates = list(set([key[1] for key in cls.quotes]))
        dates.sort()
        for date in dates:
            print >> ofile, date,
            for symbol in cls.symbols:
                print >> ofile, ",", cls.quotes[symbol, date], 
            print >> ofile
        
    @classmethod
    def get_trading_days(cls):
        cls.trading_days = list(set([key[1] for \
            key in cls.quotes.keys()]))
        cls.trading_days.sort()
        return cls.trading_days
        
    @classmethod
    def iterate(cls, starting_cash):
        print "\tsimulating"
        cash    = starting_cash
        log     = open ("marketsim.log", "w")
        fname   = sys.argv[3]
        ofile   = open(fname, "w")
        xfile   = open ("xacts.txt", "w")
        print "\twriting dates and values to %s" % fname
        
        symbols = cls.symbols
        comma   = ","
        
        print >> log, symbols, "\n"
        
        
        running = 0
        
        hfile = open ("history.csv", "w")
        
        dates = Order.orders.keys()
        dates.sort()
        for date in dates:
            print >> log, "\txacts on %s" % date
            for order in cls.orders[date]:
                print >> log, "\t\tOrder: %10s %10.2f" % \
                    (order.symbol, order.shares)
            print >> log, "\n"
        print >> log, "\n"
        
        print >> hfile, "date",
        for symbol in cls.symbols:
            print >> hfile, ",%s,%s,%s" % (symbol + "_quantity", \
                symbol + "_price",
                symbol + "_value"),
        print >> hfile, ",cash,total"
        
        values = []
    
        for date in cls.trading_days:
            print >> log, "\n", "_" * 40, "\n"
            print >> log, "%10s end of day" % date
            
            print >> hfile, date,
            
            for order in cls.orders[date]:
                symbol                  = order.symbol
                price                   = cls.quotes[symbol, date]
                quantity                = order.shares
                amount                  = price * quantity
                cash                    -= amount
                cls.holdings[symbol]    += quantity
                
                running -= amount
                
                print >> log, "xact %-6s %9.0f shares at %9.2f = %9.2f" % \
                    (symbol, quantity, price, amount)
                print >> xfile, "xact %-10s %-6s %9.0f shares at %9.2f = %15.2f running = %15.2f" % \
                    (date, symbol, quantity, price, amount, running)
                    
               
            todays_worth = cash
            for symbol in symbols:
                price       = cls.quotes[symbol, date]
                quantity    = cls.holdings[symbol]
                value       = price * quantity
                print >> log, "\t%-10s %8d shares @ %9.2f value = %9.2f" % \
                    (symbol, quantity, price, value)
                todays_worth += price * quantity
                
                print >> hfile, ",%9.3f,%9.3f,%9.3f" % \
                    (quantity, price, value),
                    
            print >> hfile, ",%9.3f" % cash,
            print >> hfile, ",%12.2f" % todays_worth
                
                
            print >> log, "cash  %9.2f" % cash
            print >> log, "total %9.2f \n" % todays_worth
            
            
            values.append(todays_worth)
            
            if cls.holdings[symbol]  < 0:
                print >> log, "WARNING: negative shares in %s" % \
                    symbol
            if cash < 0:
                    print >> log, "WARNING: Net worth is negative"
                        
            cls.worth = cash + \
                sum ([cls.holdings[symbol] * cls.quotes[symbol, date] \
                for symbol in symbols])    
                    
        
            y, m, d = date.year, date.month, date.day
            print >> ofile, y, comma, m, comma, d, comma, cls.worth  
        
        print "\t Final: %10s cash = %10.2f net worth = %10.2f" % \
            (date, cash, cls.worth)
            
        return values

def date8_f(date):
    date = "%4d%02d%02d" % (date.year, date.month, date.day)
    return date

def xdaterange (d1, d2):
    assert d2 >= d1
    oneday = datetime.timedelta(days=1)
    r = [d1]
    while r[-1] < d2: r.append(r[-1] + oneday)
    return r

def score(values):
    s0 = s1 = s2 = 0
    returns = [0]
    for index in xrange(1, len(values)):
        ret = values[index] / values[index-1] - 1
        returns.append(ret)
    assert len(returns) == len(values)
    s0                  = len(returns)
    s1                  = sum(returns)
    s2                  = sum([ret ** 2 for ret in returns])
    mean_daily_change   =  s1 / s0
    stdev               = math.sqrt (  (s0 * s2 - s1 * s1) / (s0 * (s0 -1)) )
    sharpe              = math.sqrt(len(returns)) * mean_daily_change / stdev
    total_return        = 100.0 * values[-1] / values[0] - 100.0
    print "\n\tportfolio:"
    print "\t\tstarting value           = %10.2f" % values[0]
    print "\t\tending value             = %10.2f" % values[-1]
    print "\t\ttotal return             = %11.3f percent" % total_return
    print "\t\tstd dev daily returns    = %11.3f" % stdev
    print "\t\tSharpe ratio             = %11.3f" % sharpe
    
    
print "\n", "_" * 80, "\n"
print "\tUsage is marketsim cash orders.csv values.csv"
print "\n\tcash is starting cash (float)"
print "\torders.csv is an input  file with y,m,d,symbol,{BUY | SELL}, shares"
print "\tvalues.csv is an output file with y,m,d,value\n"""

if len(sys.argv) != 4:
    print "\n\tExpected 3 arguments, got %d" % len(sys.argv)
    sys.exit()
    
random.seed(766) 
    
starting_cash   = float(sys.argv[1])
orders          = Order.get_orders()    ###--returns a dict
quotes          = Order.stub_get_quotes()
trading_days    = Order.get_trading_days()

values          = Order.iterate(starting_cash)
score           (values)

