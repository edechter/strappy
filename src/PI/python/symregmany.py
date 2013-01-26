
from glob import glob
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

import symreg as SR

files = glob("../data/*wjittermany*21*/numhit.csv")

tuples = [(i, []) for i in range(1000,11000, 1000)]

for i, (n, xs) in enumerate(tuples):
    for f in files:
        if f.find(str(n)+"_") >= 0:
            tuples[i][1].append(f)

def loadFile(file):
    x = open(file, 'r').read()
    return [float(i) for i in x.split(",")]


vals = {}
for (n, vs) in tuples:
    ys = [loadFile(f) for f in vs]
    if len(ys)>0:
        vals[n] = ys

def padToArray(length, lists):
    d = []
    for list in lists:
        d.append(list + [np.nan for i in range(0, length - len(list))])
    return np.array(d)

length = 15
padded = {}
means = {}
stds = {}
for (k, v) in vals.iteritems():
    padded[k] = pd.DataFrame(padToArray(length, v)).T/1331.0
    means[k] = padded[k].mean(1)
    stds[k] = padded[k].std(1)

means_df = pd.DataFrame(means)
stds_df = pd.DataFrame(stds)

y = means_df.xs(14)
yerr = stds_df.xs(14)
x = means_df.columns

w, h = plt.figaspect(1.5)
fig = plt.figure(figsize=(w,h))
ax = fig.add_subplot(111)
ax.set_xlabel("frontier size")
ax.set_ylabel("% tasks solved at 15 iterations")
plt.errorbar(x, y, yerr, linewidth=2)
labels = ax.get_xticklabels() 
for label in labels: 
    label.set_rotation(80) 

fig.savefig("/Users/edechter/Dropbox/Projects/ProgramInduction/drafts/ijcai2013/figures/performanceVsFrontierSize2.pdf")


fig_lc = plt.figure(figsize=(w,h))
ax = fig_lc.add_subplot(111)
ax.set_xlabel("iteration")
ax.set_ylabel("% tasks solved")
ax.set_ylim([0,1])

means_df.plot(linewidth=2, ax=ax, legend=False)

plt.legend(loc=2, prop={'size':8})


fig_lc.savefig("/Users/edechter/Dropbox/Projects/ProgramInduction/drafts/ijcai2013/figures/learningCurves2.pdf")



    
    







