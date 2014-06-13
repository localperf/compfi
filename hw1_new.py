

import datetime
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
        symbols = ["AAPL",  "XOM", "IBM"]
        start   = datetime.date(2011,1,1)
        stop    = datetime.date(2011,12,31)
    else:
        start   = date8_f (sys.argv[1])
        stop    = date8_f (sys.argv[2])
        synbols = sys.argv[3:]
        assert len(symbols) > 1
    return start, stop, symbols
        

start, stop, symbols = get_parms()
print "HW1"
print "\tstart = %s" %start
print "\tstop  = %s" % stop
print "\tsymbols = %s" % symbols
