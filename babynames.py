# -*- coding: utf-8 -*-
"""
Spyder Editor

This temporary script file is located here:
C:\Users\gblais\.spyder2\.temp.py
"""


import os

from pandas import DataFrame
from pandas import Series

import pandas       as pd
import matplotlib   as plt
import numpy        as np

import pylab

from matplotlib.lines import Line2D

def setup_data():
    

   
    
    ofile = open("babies.csv", "w")
    
    fnames = os.listdir(".")
    
    fnames = [f for f in fnames if f.startswith("yob")]
    
    for fname in fnames:
        year = int(fname[3:7])
        f = open(fname,"r")
        lines = f.readlines()
        for index, line in enumerate(lines,1):
            print >> ofile, line.strip() + ",%d,%d" % (year, index)
            
    ofile.close()
    
def get_data():
        
    names = ["name", "gender", "count", "yob", "rank"]
    data = pd.read_table("babies.csv", header = None, sep = ",",
        names = names, index_col = "yob")
        
    print 
    print data[:5]
    
    return data



def get_name (data, iname, igender):
    print "\nget_name"
    print data[:5]
    
    this_name = data[data.name == iname]
    print "\n", this_name[:10]
    
    this_name = this_name[this_name.gender == igender]
    print "\n",this_name[:10]
    
    return this_name

#------------------------------------------------------------------------------



directory   = "c://python27//Lib//site-packages//pandas//data//babynames"
    
os.chdir(directory)
data = get_data()
name = "Clarence"
gender = "M"
ndata = get_name(data, name, gender)

"""plot = tz_counts[:10].plot(kind = 'barh', rot = 0)"""

print ndata["rank"][:5]

s = Series(ndata["rank"])

print
print s
print
print "\n", s[:10]

print ndata

ax = s.plot(kind = "line")
ax.set_title("%s rank in %s names by YOB" % (name, gender))

#   a.add_line(Line2D([0.5, 0.5], [0, 1], transform=a.transAxes, linewidth=2, color='b'))

#ax.add_line(Line2D([1953, 0], [1953,6000], linewidth = 2, color = "r"))

ax.axvline(x=1945, color='blue')

pylab.show()



    
            
    