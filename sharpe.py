
import collections
import math
import random
import sys

"""sharpe
command line args are start stop symbols
retrieves adj close for all symbols and date range
guesses to find optimal mix; displays optima in passing
"""

class Symbol:
    list    = []
    symbols = []
    def __init__(self, symbol, changes):
        self.symbol     = symbol
        self.changes    = changes
        Symbol.list.append(self)
        if not symbol in Symbol.symbols: 
            Symbol.symbols.append(symbol)
            Symbol.symbols.sort()
        
    @classmethod
    def fetch(cls):
        cfile   = open ("changes.csv", "r")
        lines   = cfile.readlines()
        print lines[0]
        symbols = lines[0].strip().split(",")[1:]
        print symbols
        data    = collections.defaultdict(list)
        for line in lines[1:]:
            parts = line.strip().split(",")[1:]
            for index, part in enumerate(parts):
                data[symbols[index]].append(float(part))
        for symbol in symbols:
            s = Symbol(symbol, data[symbol])
            #s.head()
            s.compute_sharpe()
            print "%10s %9.6f %9.6f %9.6f" % (symbol, s.mean, s.stdev, s.sharpe)
            
        cls.n_symbols = len(symbols)
                
    def head(self, n = 5):
        for k in xrange(n):
            print "%10s %10.6f" % (self.symbol, self.changes[k])
        print
        
    def compute_sharpe(self):
        s0      = len(self.changes)
        s1      = sum(self.changes)
        s2      = sum([x * x for x in self.changes])
        self.mean    = 1.0 * s1 / s0
        self.stdev   = math.sqrt (    (s0 * s2 - s1 ** 2) / (s0 * (s0-1))   )
        self.sharpe  = math.sqrt(s0) * self.mean / self.stdev
     
    @classmethod
    def fund(cls, raw_weights):
       
        s       = sum(raw_weights)
        weights = [w/s for w in raw_weights]
        
       
        first   = Symbol.list[0]
        changes = [0.0] * len(first.changes)
        
        for d_index in xrange(len(changes)):
            for s_index in xrange(cls.n_symbols):
                symbol = Symbol.symbols[s_index]
                try:
                    changes[d_index] += weights[s_index] * \
                        Symbol.list[s_index].changes[d_index] 
                except IndexError:
                    print "bad index"
                    print symbol
                    print len(changes)
                    print d_index
                    print s_index
                    raise IndexError
                
        fund = Symbol("fund", changes)
      
        fund.compute_sharpe()
        #print "%10s %9.6f %9.6f %9.6f" % ("fund", fund.mean, fund.stdev, fund.sharpe)
        return fund
                
def randn(n):
    """return a list of n uniforms that sum to 1"""
    rands = [random.random() for k in xrange(n)]
    s = sum(rands)
    rands = [rand / s for rand in rands]
    rands = [round(rand, 4) for rand in rands]
    rands[-1] = 1.0 - sum(rands[:-1])
    return rands

def trial(reps= int(sys.argv[1])):
    print       "_" * 100
    raw_weights = [1.0] * len(Symbol.symbols)
    fund        = Symbol.fund(raw_weights)
    
    fund            = Symbol.fund(raw_weights)
    best_sharpe     = fund.sharpe
    best_weights    = raw_weights
    
    for rep in xrange(reps):
        raw_weights = randn(len(Symbol.symbols))
        fund = Symbol.fund(raw_weights)
        
        if fund.sharpe > best_sharpe:
            best_sharpe = fund.sharpe   
            best_weights = raw_weights
            print "%6d." % rep,
            for index, w in enumerate(raw_weights):
                print "%5.2f" % w,
            print "%12.6f" % fund.sharpe
    

Symbol.fetch()
trial()


  