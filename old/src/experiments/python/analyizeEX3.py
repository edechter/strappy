
from glob import glob
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib

import symreg as SR

files = glob("../data/EX2/ex3*/numhit.csv")


def loadFile(file):
    x = open(file, 'r').read()
    return [float(i) for i in x.split(",")]

D = {100:[], 500:[], 1000: []}
for file in files:
    if not file.find("_100_") == -1:
      D[100].append(loadFile(file))
    elif not file.find("_500_") == -1:
      D[500].append(loadFile(file))
    elif not file.find("_1000_") == -1:
      D[1000].append(loadFile(file))  

for k, v in D.items():
    D[k] = np.array(v).T/(1000.0) *(100.0)

matplotlib.rcParams.update({'font.size': 20})
w, h = plt.figaspect(.5)
fig = plt.figure(figsize=(w,h))
display = []
ax = fig.add_subplot(111)
ax.set_xlabel("iteration")
ax.set_ylabel("% tasks solved")
ax.plot(D[100], 'b-.', label="100")
display.append(0)
ax.plot(D[500], 'r--', label="500")
display.append(D[500].shape[1] + 1)
ax.plot(D[1000], 'g-', label="1000")
display.append(D[1000].shape[1] + D[100].shape[1] + 2)
handles, labels = ax.get_legend_handles_labels()
ax.legend( [handle for i, handle in enumerate(handles) if i in display], 
            [label for i, label in enumerate(labels) if i in display], loc=4,
           labelspacing=.2, borderaxespad=.05)
plt.tight_layout()
fig.savefig("/Users/edechter/Dropbox/Projects/ProgramInduction/drafts/ijcai2013/figures/booleancircuits3frontiers.pdf")


w, h = plt.figaspect(1.5)
fig = plt.figure(figsize=(w,h))
ax = fig.add_subplot(111)
counts = np.load("../data/EX3/circuitCounts.npy")
counts = np.sort(counts)[::-1]
n = len(counts)
ax.bar(range(0,n), counts, color="gray", edgecolor="k")
ax.set_xlabel("boolean functions")
ax.set_ylabel("# of circuits")
ax.set_xlim([0,82])
fig.savefig("/Users/edechter/Dropbox/Projects/ProgramInduction/drafts/ijcai2013/figures/booleancircuitsBars.pdf")






