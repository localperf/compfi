### analyze.py
###QSTK HW 3


import datetime
import math
import sys
import ystockquote

def get_portfolio_values():
    """read values file made by marketsim.py
    return a dictionary of values, indexed by trading date
    """
    fname   = sys.argv[1]
    f       = open (fname, "r")
    lines   = f.readlines()
    values  = {}
    print "\n\tread %d lines from %s" % (len(lines), fname)
    for line in lines:
        y, m, d, value  = tuple(line.strip().split(","))
        date            = datetime.date(int(y), int(m), int(d))
        value           = float(value)
        values[date]    = value
    print "\tread portfolio data for %d trading days" % len(values)
    return values
    
def stats_f(x):
    """return mean and sample standard deviation"""
    s0      = len(x)
    s1      = sum(x)
    s2      = sum([z**2 for z in x])
    mean    = s1 / s0
    stdev   = math.sqrt ((s0 * s2 - s1 * s1) / (s0 * (s0 - 1)))
    return mean, stdev

def compute_summary_stats(values, label):
    """compute total return, Sharpe ratio, standard deviation 
    from daily returns"""
    
    trading_days = values.keys()
    trading_days.sort()
    start               = values[trading_days[0]]
    end                 = values[trading_days[-1]]
    total_return        = end / start - 1.0
    
    daily_change_pcts   = [0]
    dates                = values.keys()
    dates.sort           ()
    pct_change          = [0]
    print "\tthere are %d dates" % len(dates)
    for index, date in enumerate(dates):
        if index > 0: 
            old         = values[dates[index-1]]
            delta       = values[date] - old
            pct_change  = delta / old
            daily_change_pcts.append(pct_change)
            ###--print date, pct_change
    
    n               = len(trading_days)
    mean, std       = stats_f(daily_change_pcts)
    sharpe          = math.sqrt(n) * std / mean
    print           "\n", label
    print           "\tStart                    = %12.2f" % start
    print           "\tEnd                      = %12.2f" % end
    print           "\tTrading days             = %9d"    % len(trading_days)
    print           "\tMean pct daily change    = %12.6f" % mean
    print           "\tStd Dev  daily change    = %12.6f" % std
    
    print           "\tTotal Return             = %12.2f percent" % \
        (100 * total_return)
        
    print           "\tSharpe ratio             = %12.2f" % sharpe 


def get_benchmark (bench_symbol, start, end):
    """get quotes for S&P 500 in date range"""
    
    return [0] * len(xdaterange(start, end))

    start   = date8_f(start)
    end     = date8_f(end)
    spy     = {}
    print   "\n\tfetching %s from %s through %s" % (bench_symbol, start, end)
    quotes  = ystockquote.get_quotes(bench_symbol, start, end)
    bench     = {}
    for q in quotes:
        bench[q[0]] = q[1]
    return bench
    
def date8_f(date):
    """return a YYYYMMDD string, given a date"""
    date = "%4d%02d%02d" % (date.year, date.month, date.day)
    return date

def xdaterange (d1, d2):
    assert d2 >= d1
    oneday = datetime.timedelta(days=1)
    r = [d1]
    while r[-1] < d2: r.append(r[-1] + oneday)
    return r

###----------------------------------------------------------------------------

def plot (dates, values, bench):
    import datetime
    import numpy as np
    import matplotlib.pyplot as plt
    import matplotlib.dates as mdates
    import matplotlib.cbook as cbook

    years    = mdates.YearLocator()   # every year
    months   = mdates.MonthLocator()  # every month
    yearsFmt = mdates.DateFormatter('%Y')

    # load a numpy record array from yahoo csv data with fields date,
    # open, close, volume, adj_close from the mpl-data/example directory.
    # The record array stores python datetime.date as an object array in
    # the date column

    fig     = plt.figure()
    ax      = fig.add_subplot(111)
    
    assert len(dates) == len(values)
    print len(dates), len(values)
    print type(dates), type(values)
    
    dates.sort()
    vlist  = [values[d] for d in dates]
    print   dates[:5]
    print   vlist[:5]
    
    ax.plot (dates, vlist)

    # format the ticks
    ax.xaxis.set_major_locator(years)
    ax.xaxis.set_major_formatter(yearsFmt)
    ax.xaxis.set_minor_locator(months)

    datemin = datetime.date(dates[0].year,1,1)
    datemax = datetime.date(dates[-1].year + 1, 1, 1)
    ax.set_xlim(datemin, datemax)

    # format the coords message box
    def price(x): return '$%1.2f'%x
    ax.format_xdata = mdates.DateFormatter('%Y-%m-%d')
    ax.format_ydata = price
    ax.grid(True)

    # rotates and right aligns the x labels, and moves the bottom of the
    # axes up to make room for them
    fig.autofmt_xdate()

    plt.show()
    
###----------------------------------------------------------------------------

print "_" * 80
print "\n\tUsage is analyze fname"
print "\twhere fnames is a file of dates and adjusted_close values"
print "\n\tcomputes total return and Sharpe ratio, contrats with SPY"
print "\n"
    
if len(sys.argv) != 3:   
    print "\n\tExpecting exactly one argument, not %d" % (len(sys.argv) - 1)
    print "\n\tE.g. nan alyze values.csv $spx"
    sys.exit() 
    
bench_symbol = sys.argv[2]
print "\n\tBenchmark is", bench_symbol, "\n"    
    
portfolio_values = get_portfolio_values()
compute_summary_stats(portfolio_values, label = "Portfolio")

benchmark = get_benchmark(bench_symbol, \
    min(portfolio_values.keys()), max(portfolio_values.keys())) 
#compute_summary_stats(spy)

dates   = portfolio_values.keys()


plot    (dates, portfolio_values, [])