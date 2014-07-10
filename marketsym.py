###marketsim


import datetime
import sys
import ystockquote

class Order:
    list        = []
    quotes      = {} #  ##--key is (symbol, date): value is price
    orders      = {}   ###indexed sam e way
    holdings    = {}    ###--indexed by symbol; value is shares
    
    ###assume at most one order per sumbol per day
    
    
    def __init__(self, date, symbol, shares):
        self.symbol = symbol
        self.shares = shares
        self.date = date
        Order.list.append(self)
        
    @classmethod
    def get_orders(cls):
        fname   = sys.argv[2]
        f       = open (fname, r)
        lines   = f.readlines()
        for line in lines:
            line        = line.strip()
            parts       = tuple(line.split(","))
            y,m,d,symbol, order_type, shares = parts
            date        = datetime.date(int(y), int(m), int(d))
            shares      = float(shares)
            if order_type.upper().strip() != "BUY":
                shares = shares
            if (symbol, date) in Order.orders:
                print >> log, \
                "Duplicate symbol, date in order list for %s on %s: ignored" % \
                (symbol, date) 
            Order.orders[symbol, date] = shares
            
        return cls.list
    
    @classmethod
    def get_quotes(cls):
        symbols = list(set([order.symbol for order in cls.list]))
        symbols.sort()
        dates = list(set([order.date for order in cls.list]))
        dates.sort()
        mindate = dates[0]
        maxdate = dates[-1]
        mindate = date8_f(mindate)
        maxdate = date8_f(maxdate)
        
        for symbol in symbols:
            quotes = ystockquote.get_quotes(symbol, mindate, maxdate)
            for quote in quotes:
                date, price = quote
                cls.quotes[symbol, date] = price
        
        @classmethod
        def get_trading_days(cls):
            cls.trading_days = list(set([key[1] for \
                key in cls.quotes.keys()]))
            return cls.trading_days
        
    @classmethod
    def iterate(cls, starting_cash):
        cash    = starting_cash
        log     = open ("marketsim.log", "w")
        for date in cls.trading_dates:
            for symbol in cls.symbols:
                if (symbol, date) in cls.orders:
                    price                   = cls.quotes[symbol, date]
                    quantity                = cls.orders[symbol, date]
                    dollars                 = price * quantity
                    cash                    += dollars
                    cls.holdings[symbol]    += shares
                    print >> log, date, symbol, quantity, price, cash, \
                        cls.holdings[symbol]
                    if cls.holdings[symbol]  < 0:
                        print >> log, "WARNING: negative shares in %s" % symbol
            cls.worth = cash + sum ([cls.holdings[symbol] * cls.price[symbol, date] for symbol in symbols])    
                    
        
        
            

def date8_f(s):
    date = "%4d%02d%02d" % (date.year, date.month, date.day)
    return date
    


starting_cash   = float(sys.argv[1])
orders          = Order.get_orders()
quotes          = Order.get_quotes()
trading_days    = Order.get_trading_days()

Order.iterate(starting_cash)

