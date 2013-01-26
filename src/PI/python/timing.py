

from os.path import join, exists

import pandas as pd
import matplotlib as plt

datadir = "../data"
D = { 1000 : "R1_1000_wjittertiming_2013-01-20_12-45-31.810308_EST/time.txt",
         500 : "R1_500_wjittertiming_2013-01-20_12-43-23.900711_EST/time.txt",
         600: "R1_600_wjittertiming_2013-01-20_12-43-41.697841_EST/time.txt",
         700: "R1_700_wjittertiming_2013-01-20_12-44-01.5959_EST/time.txt",
         800: "R1_800_wjittertiming_2013-01-20_12-44-25.596314_EST/time.txt",
         900: "R1_900_wjittertiming_2013-01-20_12-44-56.474792_EST/time.txt"}


d = {}
for k, v in D.iteritems():
    filename = join(datadir, v)
    assert(exists(filename))
    n = float(open(filename, 'r').read()[:-1])
    d[k] = n


