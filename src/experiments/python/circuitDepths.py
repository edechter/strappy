
import sys, os
from os.path import join, exists
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib
import csv


circuitDepthsFile = "/Users/edechter/Dropbox/Projects/Strappy/src/experiments/" + "data/EX1_R1_10000_2013-01-27_07-18-59.231023_UTC/circuitDepths.txt"
assert(exists(circuitDepthsFile))
circuitDepthsBruteFile = "/Users/edechter/Dropbox/Projects/Strappy/src/experiments/" + "data/EX1_15000_cDepths.txt"
assert(exists(circuitDepthsBruteFile))

reader = csv.reader(open("/Users/edechter/Dropbox/Projects/Strappy/src/experiments/" + "data/EX1_R1_10000_2013-01-27_07-18-59.231023_UTC/circuitDepths.txt", 'r'))
D = []
for row in reader: D.append([int(r) for r in row])
df = pd.DataFrame(D)

reader = csv.reader(open(circuitDepthsBruteFile, 'r'))
F = []
for row in reader: F.append([int(r) for r in row])
df2 = pd.DataFrame(np.array(F).reshape(15, 10000))



matplotlib.rcParams.update({'font.size': 18})
w, h = plt.figaspect(2)
fig = plt.figure(figsize=(w,h))
figname = "symregCircuitDepths.pdf"
ax = fig.add_subplot(111)
df.mean(1).plot(linewidth=3, style="g-", ax=ax, label="mean depth", grid=False)
df.max(1).plot(linewidth=3, style="b--", ax=ax, label="max depth", grid=False)
ax.add_line(plt.Line2D([0, 14], [7, 7], linewidth=1, color="gray"))
ax.set_xlabel("iteration")
ax.set_ylabel("circuit depth")
ax.set_ylim([0, 11])
ax.legend(loc=4)
figdir = "/Users/edechter/Dropbox/Projects/Strappy/drafts/ijcai2013/figures/"
plt.tight_layout()
fig.savefig(join(figdir, figname))

