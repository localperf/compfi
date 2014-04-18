


#!/usr/bin/env python
#
#  Copyright (c) 2007-2008, Corey Goldberg (corey@goldb.org)
#
#  license: GNU LGPL
#
#  This library is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public
#  License as published by the Free Software Foundation; either
#  version 2.1 of the License, or (at your option) any later version.


import collections
import datetime
import urllib


"""
This is the "ystockquote" module.

This module provides a Python API for retrieving stock data from Yahoo Finance.

sample usage:
>>> import ystockquote
>>> print ystockquote.get_price('GOOG')
529.46
"""


def __request(symbol, stat):
    url = 'http://finance.yahoo.com/d/quotes.csv?s=%s&f=%s' % (symbol, stat)
    return urllib.urlopen(url).read().strip().strip('"')


def get_all(symbol):
    """
    Get all available quote data for the given ticker symbol.
    
    Returns a dictionary.
    """
    values = __request(symbol, 'l1c1va2xj1b4j4dyekjm3m4rr5p5p6s7').split(',')
    data = {}
    data['price'] = values[0]
    data['change'] = values[1]
    data['volume'] = values[2]
    data['avg_daily_volume'] = values[3]
    data['stock_exchange'] = values[4]
    data['market_cap'] = values[5]
    data['book_value'] = values[6]
    data['ebitda'] = values[7]
    data['dividend_per_share'] = values[8]
    data['dividend_yield'] = values[9]
    data['earnings_per_share'] = values[10]
    data['52_week_high'] = values[11]
    data['52_week_low'] = values[12]
    data['50day_moving_avg'] = values[13]
    data['200day_moving_avg'] = values[14]
    data['price_earnings_ratio'] = values[15]
    data['price_earnings_growth_ratio'] = values[16]
    data['price_sales_ratio'] = values[17]
    data['price_book_ratio'] = values[18]
    data['short_ratio'] = values[19]
    return data
    
    
def get_price(symbol): 
    return __request(symbol, 'l1')


def get_change(symbol):
    return __request(symbol, 'c1')
    
    
def get_volume(symbol): 
    return __request(symbol, 'v')


def get_avg_daily_volume(symbol): 
    return __request(symbol, 'a2')
    
    
def get_stock_exchange(symbol): 
    return __request(symbol, 'x')
    
    
def get_market_cap(symbol):
    return __request(symbol, 'j1')
   
   
def get_book_value(symbol):
    return __request(symbol, 'b4')


def get_ebitda(symbol): 
    return __request(symbol, 'j4')
    
    
def get_dividend_per_share(symbol):
    return __request(symbol, 'd')


def get_dividend_yield(symbol): 
    return __request(symbol, 'y')
    
    
def get_earnings_per_share(symbol): 
    return __request(symbol, 'e')


def get_52_week_high(symbol): 
    return __request(symbol, 'k')
    
    
def get_52_week_low(symbol): 
    return __request(symbol, 'j')


def get_50day_moving_avg(symbol): 
    return __request(symbol, 'm3')
    
    
def get_200day_moving_avg(symbol): 
    return __request(symbol, 'm4')
    
    
def get_price_earnings_ratio(symbol): 
    return __request(symbol, 'r')


def get_price_earnings_growth_ratio(symbol): 
    return __request(symbol, 'r5')


def get_price_sales_ratio(symbol): 
    return __request(symbol, 'p5')
    
    
def get_price_book_ratio(symbol): 
    return __request(symbol, 'p6')
       
       
def get_short_ratio(symbol): 
    return __request(symbol, 's7')
    
    
"""
"http://ichart.finance.yahoo.com/table.csv?s=AAPL&c=2011"
"""
    
    
def get_historical_prices(symbol, start_date, end_date, fname = ""):
    """
    Get historical prices for the given ticker symbol.
    Date format is 'YYYYMMDD'
    returns a list of day lists: each day has open, close, ...
    """


    url = 'http://ichart.yahoo.com/table.csv?s=%s&' % symbol + \
          'd=%s&' % str(int(end_date[4:6]) - 1) + \
          'e=%s&' % str(int(end_date[6:8])) + \
          'f=%s&' % str(int(end_date[0:4])) + \
          'g=d&' + \
          'a=%s&' % str(int(start_date[4:6]) - 1) + \
          'b=%s&' % str(int(start_date[6:8])) + \
          'c=%s&' % str(int(start_date[0:4])) + \
          'ignore=.csv'
    
    print "\n", url, "\n"
        
    days = urllib.urlopen(url).readlines()
    print "\tlen(days) =", len(days)
    print
    print "\theader", days[0]
    print "\tfirst  ",days[1]
    print "\tlast  ", days[-1], "\n"
    
    data = [day[:-1].split(',') for day in days]
    if False:
        print "data\n"
        for index, row in enumerate(data):
            print "\trow %3d \t" % index, row
    
    return data



###----------------------------------------------------------------------------

def get_quotes(symbol, start = 20040101, stop = 20131231, qtype = "ADJ_CLOSE"):
    """returns a list of tuples for symbol
    each tuple is date, close, adj_closae
    """
    
    field_map = {"CLOSE" : 4, "ADJ_CLOSE" : 6, "VOLUME" : 5}
    
    if qtype.upper() in field_map:
        field = field_map[qtype]
    else:
        print "Unknown field type:", qtype
        sys.exit()
    
    
    print "\tpulling", qtype, "for", symbol, "..."
   
    quotes  = []
    data    = get_historical_prices(symbol, start, stop)
    
    """4 is close, 6 is adj_close, 5 is volume"""
    
    if "404 Not Found" in data[1]:
        print symbol, "Not Found"
        return None
    for index, row in enumerate(data):
        if index > 0:
            try:
                y, m, d     = tuple(row[0].split("-"))
                date        = datetime.date(int(y), int(m),  int(d))
                close       = float(row[4])
                adj_close   = float(row[6])
                quote = (date, close, adj_close)
                quotes.append(quote)
                if date == datetime.date(2011, 4, 10):
                    print "\n\t>>> %10s %10s %12.6f" % \
                        (date, symbol,  quote[1], quote[2])
                    print "\n", row
            except ValueError:
                print symbol
                print row[0]
                raise ValueError
                
    
    quotes.sort(key = lambda x: x[0])
   
    return quotes
        
if __name__ == "__main__":
    symbol  = "AAPL"
    quotes  = get_quotes(symbol, start = "20110609", stop = "20110611")
    print   "\t", symbol, len(quotes)
    print   "\t\t", quotes[ 0][0], quotes[ 0][1]
    print   "\t\t", quotes[-1][0], quotes[-1][1] 
    
  
        
        
    