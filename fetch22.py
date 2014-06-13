
#--fetch22

#-read weights, prices, changes for a portfolio based on prefix

#--compite backtest return, sharpe

import csv
import sys
import datetime

def date_f(s):
    """ parse 1/3/2011 0:00"""

    s = s.split()[0]
   
    y, m, d = tuple(s.split("-"))
  
    
    date_value = datetime.date(int(y), int(m), int(d))
    return date_value
    
    
    
    
def read_weights(prefix): 
    """read an optimal weights file from R"""
    weights = {}
    fname = prefix + "_" + "weights.txt"
    ifile = open (fname, "r")
    lines = ifile.readlines()
    for index, line in enumerate(lines):
        
        if index >= 1:
            parts = line.split()
            
            symbol = parts[1][1:-1]
            weight = float(parts[2])
            weights[symbol] = weight/100
            
    for symbol in weights:
        print "%10s %10.3f" % (symbol, weights[symbol])
    return weights


def read_changes(prefix):
    
    changes         = {}
    dates           = set([])
    missing_dates   = set([])
    fname           = prefix + "_" + "changes.csv"
    ifile           = open(fname, "r")
    lines           = ifile.readlines()
    for index, line in enumerate(lines):
        parts = line.split(",")
        if index == 0:
            symbols = parts[1:]
        else: 
            date = date_f(parts[0])
            parts = parts[1:]
            for column, part in enumerate(parts):
                symbol = symbols[column]
                try:
                    if not date in missing_dates:
                        changes[symbol, date] = float(parts[column])
                except ValueError:
                    missing_dates.add(date)
                dates.add(date)
            
            
    keys = changes.keys()
    keys.sort()
    for key in keys:
        if key[1] in missing_dates:
            del changes[key]
           
            
    for date in missing_dates:
        dates.remove(date)
            
    print "\tmissing dates", min(missing_dates), max(missing_dates)
    
    return changes, list(dates)
        
def score(weights, dates, changes):
    dates.sort()
    symbols = weights.keys()
    symbols.sort()
    base = 1000.0
    ofile = open (prefix + "_" + "weighted.csv", "w")
    writer = csv.writer(ofile, lineterminator = "\n")
    for date in dates:
        row = [date]
        change = 0
        for symbol in weights:
            change += weights[symbol] * changes[symbol,date]
            row.append(symbol)
            row.append(weights[symbol])
            row.append(changes[symbol, date])
            row.append(change)
        base *= (1 + change)
        row.append(base)
        writer.writerow(row)
    print "\t%10.2f" % base   
    
    
prefix = sys.argv[1]

weights = read_weights(prefix)

changes, dates = read_changes(prefix)

print "\tincluded dates", min(dates), max(dates)

score (weights, dates, changes)
        
