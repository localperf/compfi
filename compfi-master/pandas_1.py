

#---panda_1

import json

from pandas import DataFrame
from pandas import Series

import pandas       as pd
import matplotlib   as plt
import numpy        as np

import pylab

directory   = "c://python27//Lib//site-packages//pandas//data"

path        = directory + "//ch02//" + "usagov_bitly_data2012-03-16-1331923249.txt"

records     = [json.loads(line) for line in open(path)]

frame       = DataFrame(records)

print
print frame

tz_counts   = frame['tz'].value_counts()

print tz_counts[:10]

clean_tz    = frame['tz'].fillna("Missing")

clean_tz[clean_tz == ''] = "Unknown"

tz_counts   = clean_tz.value_counts()

print
print   tz_counts[:10]

plot = tz_counts[:10].plot(kind = 'barh', rot = 0)

pylab.show()

print "\nplot ok"






