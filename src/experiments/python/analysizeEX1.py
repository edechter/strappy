
from glob import glob
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib

import symreg as SR

files = glob("../data/EX1*/numhit.csv")


def loadFile(file):
    x = open(file, 'r').read()
    return [float(i) for i in x.split(",")]

D = {1000:[], 5000:[], 10000: []}
for file in files:
    if not file.find("_1000_") == -1:
      D[1000].append(loadFile(file))
    elif not file.find("_5000_") == -1:
      D[5000].append(loadFile(file))
    elif not file.find("_10000_") == -1:
      D[10000].append(loadFile(file))  

for k, v in D.items():
    D[k] = np.array(v).T/(1000.0) *(100.0)

matplotlib.rcParams.update({'font.size': 20})
w, h = plt.figaspect(.5)
fig = plt.figure(figsize=(w,h))
ax = fig.add_subplot(111)
ax.set_xlabel("iteration")
ax.set_ylabel("% tasks solved")
ax.plot(D[1000], 'r-', label="1000", )
ax.plot(D[5000], 'g--', label="5000")
ax.plot(D[10000], 'b-*', label="10000")
ax.plot(range(15), [2.7 for i in range(15)], linewidth=4, linestyle="--", color="black", label="baseline")
for l in ax.lines:
    l.set_alpha(.5)
handles, labels = ax.get_legend_handles_labels()
last = len(handles)
display = (0, 50, 100, last-1)
ax.legend( [handle for i, handle in enumerate(handles) if i in display], 
            [label for i, label in enumerate(labels) if i in display], loc=2)
ax.set_ylim([-5, 105])
plt.tight_layout()
fig.savefig("/Users/edechter/Dropbox/Projects/ProgramInduction/drafts/ijcai2013/figures/symreg3frontiers.pdf")





