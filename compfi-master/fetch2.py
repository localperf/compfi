"""fetch2. nbased on pandas"""

import  collections
import  csv
import  datetime
import  math
import  matplotlib as plot
import  optparse
from    optparse import OptionParser
import  os
import  pandas as pd
import  pylab
from    pprint import pprint
from    pandas.io.data import DataReader
import  sys
import  time
import  urllib2


"""

ARR formula from 

http://www.readyratios.com/reference/analysis/annualized_rate.html


defaults to last 400 calendar days

"""

class Symbol:
    dict = {}
    list = []
    
    def __init__(self, symbol):
        print "\tinstantiating", symbol
        self.symbol = symbol
        Symbol.dict[symbol] = self      #--class dict of all symbol instances
        Symbol.list.append(self)        #--class of list of all symbols
        
        self.sharpe         = None
        self.total_return   = None
        self.trading_days   = None     #--trading days with data
        self.calendar_days  = None
        
    def __repr__(self):
        s = "%10s" % self.symbol
        s += " total return ($) = "
        try: 
            s += "%9.3f" % self.total_return 
        except TypeError:
            s += "%9s" % "None"
            
        s += " sharpe ratio = "  
        try:
            s += "%9.3f" % self.sharpe
        except TypeError:
            s += "%9s" % "None"
            
        s += " ARR (%) = " 
        s += "%9.3f" % (100.0 * self.arr)
        
        s += " max draws down (%) = "
        s += "%9.3f" % (100 * self.max_draw_down)
            
        s += " trading days = "
        try:
            s += "%6d" % self.trading_days
        except TypeError:
            s += "%6s" % "None"
            
        s += " Calendar days ="
        try:
            s += "%6d" % self.calendar_days
        except TypeError:
            s += "%6s" % "None"
            
        return s
    
    @classmethod
    def export(cls):
        mkdir   (prefix)
        fname   = prefix + ".csv"
        f       = open ("//".join([prefix, "summary.csv"]), "w")
        writer  = csv.writer(f, lineterminator = "\n")
        header  = ["Symbol", "First day", "Last Day", 
            "Calendar Days", 
            "Trading Days", 
            "First Adj Close", "Last Adj Close",
            "Total Return ($/share)",
            "Annualized Rate of Return (%)",
            "Sharpe Ratio", "Max Draw Down (%)"]
        writer.writerow(header)
        
        symbols = cls.list
        symbols.sort (key = lambda x: x.symbol)
        for symbol in symbols:
            row  = [symbol.symbol]
            row += [symbol.first_day, symbol.last_day]
            row += [symbol.calendar_days, symbol.trading_days]
            row += [symbol.series[0]]
            row += [symbol.series[-1]]
            row += [symbol.total_return, symbol.arr]
            row += [symbol.sharpe, symbol.max_draw_down]
            writer.writerow(row)
            
#==============================================================================
            

def get_data():
    """symbols are specified on command line
    mitre, folio, smm are allowed abbreviations
    period defaults to 400 calendar days
    """
    
    start       = get_starting_date()
    print       "\ttoday is %s" % datetime.datetime.today().date()
   
    print       "\tstart date is %s\n" % start
    
    series      = {}
    
    not_found   = []
    for index, symbol in enumerate(symbols, 1):
        print "\t%6d. fetching %s" % (index, symbol.upper()),
        try:
            data    = DataReader(symbol.upper(),"yahoo", start)
            print "\t", type(data), data.shape
            print data.head()
            print "first five adj close values:", data["Adj Close"][:5]
            
            x       = data["Adj Close"]
            print "\n\t x starts with ", x[:5]
            
            z = pd.DataFrame(index = data.index, \
                data = x.values, columns = [symbol])
            
            print "\n\tz\n", z.head()
        
            series[symbol] = z
            print series[symbol]
            Symbol(symbol)
            Symbol.dict[symbol].adjclose = series[symbol]
            print
        except urllib2.HTTPError:
            print "\tnot found"    
            not_found.append(symbol)
            
    for symbol in not_found:
        symbols.remove(symbol)
        print "\tnot found:", symbol
        
    df      = series[symbols[0]]
    print "head and tail for", symbols[0]
    print df.head()
    print df.tail()
    print df.shape
    print "\n", "_" * 40, "\n"
    
  
    
    #-------------------------
    for symbol in symbols[1:]:
        new = series[symbol]
        
        df = pd.merge(left = df, 
            right       = new, 
            left_index  = True, 
            right_index = True,
            how         = "outer")
    print "\njoined df"
    print df.head()
    print df.tail()
    print df.shape
    print "_" * 30
    
    print "\ntotal returns"
    for symbol_name in Symbol.dict:
        symbol              = Symbol.dict[symbol_name]
        series              = df[symbol_name].dropna()
      
        total_return        = series[-1] - series[0]
        total_return_pct    = 100.0 * total_return / series[0] 
        print               "%10s" % symbol.symbol,
        print "total return = %10.2f " % total_return,
        print "total return = %10.2f" % total_return_pct, "%"
        
        symbol.total_return     = total_return
        symbol.total_return_pct = total_return_pct
        
        symbol.trading_days = series.shape[0]
        print "%10s has %6d trading days" % \
            (symbol_name, series.shape[0]) 
        
        calendar_days = (
            series.index[-1] - series.index[0]).days + 1
        print "%10s has %6d calendar days" % (symbol_name, calendar_days)
        symbol.calendar_days =   calendar_days
        
        years = calendar_days / 365.25
        
        arr = (1.0 + total_return_pct / 100) ** ( 1.0 / years) - 1.0
        
        symbol.first_day        = series.index[0]
        symbol.last_day         = series.index[-1]
        symbol.calendar_days    = (symbol.last_day - symbol.first_day).days + 1
       
        symbol.arr              = arr
        symbol.years            = years
        symbol.max_draw_down    = max_draw_down_f(symbol, series)
        symbol.series           = series 
       
        
    print
    return df
        
            
        
        
        
def max_draw_down_f(symbol, series):
    
    Drawdown = collections.namedtuple("Drawdown",  \
        ["start", "end", "first", "last", "pct"])
        
    print       "\nmax draw down calc for", symbol.symbol, len(series), series[:5]
    cum_max     = [max(series[:i]) for i in xrange(1, len(series)+1)]
    
    cum_max     = series.cummax() 
    print     cum_max[:5]
    draw_down = cum_max - series
    print "draw_down head:", draw_down[:5]
    
    print     "\ndrawdowns", draw_down[:5], "\n" 
    print     "\tmax drawdown is", max(draw_down)
    
    pcts        = draw_down / cum_max
    print     "\tpcts head:", pcts[:5]
    print     "\tlen(pcts) = %5d" % len(pcts)

    max_pct     = max(pcts)
    print     "\tmax pct drawdown is %9.3f" % max_pct
    indices     = [i for i in xrange(len(series)) if pcts[i] == max_pct]
    
    
    end         = series[indices[0]]
    last        = series.index[indices[0]]
    pct         = max_pct
    
    print "\twhere was biggest pct drawdown?"
    for i in indices:
        print "\t%5d %10.2f %10.2f %10.2f" % \
            (series[i], cum_max[i], draw_down[i], pcts[i])
   
    print
   
    #df = pd.dataframe(series, columns = "adj_close")
    #df  = pd.merge(left = df, right = cum_max)
    #print df.head()
   
    return max_pct
            

def get_starting_date():
    """-parse either a yyyymmdd stgart, 
        or a numnber of calendar days to go back
        default is 1000 days
        start date or number can be in any position
        """
        
    for arg in sys.argv:
        try:
            start   = arg
            year    = int(start[:4])
            month   = int(start[4:6])
            day     = int(start[6:])
            start   = datetime.date(year, month, day)
            sys.argv.remove(arg)
            return start
        except ValueError:
            try:
                n_days = int(arg)
                sys.argv.remove(arg)
                print "\tPeriod is %d calendar days" % n_days
                start = datetime.datetime.today().date() - \
                    datetime.timedelta(days = n_days)
                return start
            except ValueError:
                pass
    start = datetime.datetime.today().date() - \
                    datetime.timedelta(days = 1000)
    return start
   
   

        
def make_naive_allocation():
    """ignores correlations, based on sum of positive sharpes"""
    
    total_sharpe = sum([Symbol.dict[name].sharpe for name in Symbol.dict if \
        Symbol.dict[name].sharpe > 0])
        
    symbols = Symbol.dict.keys()
    symbols.sort (key = lambda x: Symbol.dict[x].sharpe, reverse = True)
    print "\nNaive allocations (ignoring correlations)"
    sigma = 0.0
    for name in symbols:
        symbol = Symbol.dict[name]
        if symbol.sharpe > 0:
            symbol.naive = symbol.sharpe / total_sharpe
            sigma += 100.0 * symbol.naive
            print "%10s %10.3f %10.3f %15.3f" % \
            (name, symbol.sharpe, 100 * symbol.naive, sigma)
        else:
            symbol.naive = 0
        
        
def compute_daily_returns(df):
    """compute daily change percentages"""
   
    print "\tcomputing daily change"
    
    change = df.pct_change(periods=1)
    
    change = change[1:]     #--drop first row witn NaNs
    print df.shape
    print change.shape
    
    print change.head()
    print "..."
    print change.tail()
    
    means = change.sum() / change.shape[1]
   
    print means.head()
    means   = change.mean()
    print means.head()
    
    print "\n\tstdevs"
    stdevs  = change.std() 
    print stdevs
    
    sharpes = math.sqrt(252.0) * means / stdevs
    print "\nsharpes"
    print sharpes
    
    for symbol in Symbol.dict:
        Symbol.dict[symbol].sharpe = sharpes[symbol]
        
    return change

def save_changes(change, prefix):
    mkdir(prefix)
    fname =  "//".join([prefix, "changes.csv"])
    print prefix, fname
    change.to_csv(fname)
    
def save_prices(df, prefix):
    mkdir(prefix)
    fname = "//".join([prefix, "prices.csv"])
    print fname
    df.to_csv(fname)
    
def mkdir(dirname):
    if dirname == "":
        return
    
    try:
        os.mkdir(dirname)
    except WindowsError:
        pass
        
def get_symbols(dir):
    os.chdir(dir)
    print "\tlooking for %s symbols" % dir
    fnames = [fname for fname in os.listdir(".") if \
        fname.lower().startswith(dir) and \
        fname.lower().endswith(".csv") and \
        "holdings" in fname.lower()]
    fnames.sort()
    last = fnames[-1]
    print "\treading symbols from", last
    f       = open(last, "r")
    reader  = csv.reader(f)
    symbols = []
    for row in reader:
        
        if dir == "edelman":    symbol = row[1][2:]
        if dir == "bernstein":  symbol = row[3]
        if len(symbol) > 0 and not "$" in symbol:
            symbols.append(symbol)
            print "\t\t%6d\t%s" % (len(symbols[-1]), symbols[-1])
        
    symbols.sort()
    print "\t%6d symbols from %s through %s" % \
        (len(symbols), symbols[0], symbols[-1])
    os.chdir ("..")
    return symbols
    
def get_options():

    folio   = list(set(["ann", "he", "boh", "bmo", "tnh", "armh", \
        "ewc", "ews", "lqd", "dvy", "eat", "iyr",  "jwn",
        "enb", "trp" ,"adksx", "tu", "thi"]))
        
    mitre       = ["fcnkx", "amanx", "fagix", "ficdx", "fxsix", "fdikx"]
    
    farr        = ["cvs", "fdo", "slb", "jnj", "qcom", "abt", "acn", "pdco", "jpm", "utx"] 
    
    jeff        = ["oakmx", "abemx", "oakix"]
    smm_jpm_agg = ["ECON","XLV","TLT","IEI","LQD","IEMG","MBB","EWC","EZU",
        "EPP","EWD","EWL","EWU","IBB","TIP","EMLC","PCY","PGX","XLF","JNK","BWX","VCR",
        "VDC","VDE","VIS","VGT","BIV","VAW","BSV","VOX","DXJ"]
    
    smm_jpm_agg = [s.lower() for s in smm_jpm_agg]

    gcb         = folio + mitre + ["mqifx"]
    edelman     = get_symbols("edelman")
    bernstein   = get_symbols("bernstein")

    dana        = ["vwenx", "rergx", "mdlvx", "iacix"]
    
    sharebuilder = ["ewc", "ann", "he"]
    
    nick = ["focpx", "hiacx", "prfdx","naesx", "poagx", "trbcx", 
        "vavsx", "sksex", "adksx"]
        
    cyndi = ["adksx", "hyls", "lglv", "gsra", "vlu", "qdef", "qdf", "xslv", "tsla"]

    parser = OptionParser()
    
    parser.add_option ("-p", "--prefix", action = "store", type = "str", 
        dest = "prefix", default = "", 
        help = "name of a subdirectory where results will be stored")
        
    parser.add_option ("-s", "--symbols", action = "store", type = "str",
        dest = "symbols", default = "spy",
        help = "an input of list of symbols, separated by spaces, in quotes")
        
    parser.add_option ("--start", action = "store", type = "str",
        dest = "start", default = "20120101",
        help = "First date to retrieve, in yyyymmdd format")
        
    options, args = parser.parse_args()
    
    print "\nraw start =", options.start
    start = options.start
    start = datetime.date(int(start[:4]), int(start[4:6]), int(start[6:]))
    print "\nstart = ", start
    
    print "\noptions", options
    
    print "\nargs", args
    
    print "options.symbols", options.symbols
    symbols = options.symbols.strip('"')
    symbols = symbols.strip('"')
    symbols = symbols.split()
    print   "\nraw symbols", symbols, "\n"
    
    if "mitre" in symbols:
        symbols.remove("mitre")
        symbols += mitre
    if "folio" in symbols:
        symbols.remove("folio")
        symbols += folio
    if "edelman" in symbols:
        symbols.remove("edelman")
        symbols+= edelman
    if "bernstein" in symbols:
        symbols.remove ("bernstein")
        symbols += bernstein
    if "dana" in symbols:
        symbols.remove("dana")
        symbols += dana
    if "sharebuilder" in symbols:
        symbols.remove("sharebuilder")
        symbols += sharebuilder
    if "nick" in symbols:
        symbols.remove("nick")
        symbols += nick
    if "gcb" in symbols:
        symbols.remove ("gcb")
        symbols += gcb
    if "farr" in symbols:
        symbols.remove("farr")
        symbols += farr
    if "jeff" in symbols:
        symbols.remove("jeff")
        symbols += jeff
    if "smm_jpm_agg" in  symbols:
        symbols.remove("smm_jpm_agg")
        symbols += smm_jpm_agg    
    if "cyndi" in symbols:
        symbols.remove("cyndi")
        symbols += cyndi
            
        
    symbols = list(set(symbols))
        
    symbols.sort()
    print "\n", len(symbols)
    print "\n", symbols, "\n"
        
    symbols.append ("spy")
    symbols = list(set(symbols))
        
    symbols.sort()
    print "\t", symbols
    print "\t", len(symbols), "final symbols"
    
    return options.prefix, symbols, start
        
        
    
    
    
#------------------------------------------------------------------------------

print "_" * 100

prefix, symbols, start = get_options()

#new = DataReader(symbols, "yahoo",  datetime(2013,1,1))
#print "\t new is ok"

df = get_data()

start = min([symbol.first_day for symbol in Symbol.list])
print "\tFirst date is %s" % start

change = compute_daily_returns(df)

save_prices     (df, prefix)
save_changes    (change, prefix)

print "\nSymbols in alphabetical order"
symbol_names = Symbol.dict.keys()
symbol_names.sort()
for symbol_name in symbol_names:
    symbol = Symbol.dict[symbol_name]
    print symbol
 
print "\nSymbols in Sharpe order"   
symbol_names.sort (key = lambda x: Symbol.dict[x].sharpe, reverse = True)
for symbol_name in symbol_names:
    symbol = Symbol.dict[symbol_name]
    print symbol
    

print "\nSymbols in ARR order"   
symbol_names.sort (key = lambda x: Symbol.dict[x].arr, reverse = True)
for symbol_name in symbol_names:
    symbol = Symbol.dict[symbol_name]
    print symbol    
    
print "\nSymbols in % max draw down order"

symbol_names.sort (key = lambda x: Symbol.dict[x].max_draw_down)
for symbol_name in symbol_names:
    symbol = Symbol.dict[symbol_name]
    print symbol    
    
Symbol.export()



    
make_naive_allocation()