

import sys
import os
from os.path import join, exists

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
import matplotlib.cm as cm

def fileToDataFrame(file, name):
    indexes = []
    nums = []
    for line in open(file, 'r').readlines():
        bin, n = line[:-1].split(" ")
        n = int(n)
        indexes.append(bin)
        nums.append(n)
    df = pd.DataFrame(nums, index = indexes, columns = [name])
    return df
        

def allbin(n):
    if n ==1:
        return ["0", "1"]
    else:
        return [s +"0" for s in allbin(n-1)] + [s + "1" for s in allbin(n-1)]

datadir = '../data/EX3'

circuitDistrFile = join(datadir, "circuitTTCount.txt")
initCombsDistrFile = join(datadir, "initCombsTTCount.txt")
afterCombsDistrFile = join(datadir, "afterCombsTTCount.txt")

def normalize(x, col):
    x[col] = x[col]/float(np.sum(x[col])) * 100.0
    return x

df1 = normalize(fileToDataFrame(circuitDistrFile, "circuits"),
                "circuits")
df2 = normalize(fileToDataFrame(initCombsDistrFile, "initCombs"), "initCombs")
df3 = normalize(fileToDataFrame(afterCombsDistrFile, "learnedCombs"), "learnedCombs")


index = allbin(8) + allbin(4) + allbin(2)
df4 = pd.DataFrame(range(len(index)), index=index)

def merge(x, y): 
    return pd.merge(x, y, "outer", left_index=True, right_index=True)

matplotlib.rcParams.update({'font.size': 10})
w, h = plt.figaspect(.7)
fig = plt.figure(figsize=(w,h))
ax1 = fig.add_subplot(311)
ax2 = fig.add_subplot(312)
ax3 = fig.add_subplot(313)
axs = [ax1, ax2, ax3]
for ax in axs: 
    ax.set_ylim(0, 30)
    ax.set_xticks([])
    ax.set_xticklabels([])
df = merge(merge(merge(df1, df2), df3), df4)
df['circuits'].plot(kind='bar', ax=ax1, xticks=[], grid=False, color="gray")
df['initCombs'].plot(kind='bar', ax=ax2, xticks=[], grid=False, color="gray")
df['learnedCombs'].plot(kind='bar', ax=ax3, xticks=[], grid=False, color="gray")
index.reverse()
df = df.ix[index]
ax1.set_xlabel("from boolean circuit distribution    ")
ax2.set_xlabel("from grammar before learning    ")
ax3.set_xlabel("from grammar after learning    ")
for ax in axs: 
    ax.set_ylabel("% of functions")

fig.savefig("/Users/edechter/Dropbox/Projects/ProgramInduction/drafts/ijcai2013/figures/circuitDistrComp.pdf")
#plt.show()


matplotlib.rcParams['ytick.major.pad']='10'
matplotlib.rcParams.update({'font.size': 25})
w, h = plt.figaspect(.2)
fig = plt.figure(figsize=(w,h))
ax = fig.add_subplot(111)
ax.imshow(np.log(np.array([df.circuits, df.initCombs, df.learnedCombs])),
           aspect=20, interpolation='none', cmap=cm.binary)
ax.set_xticks([])
ax.set_xlabel("enumerated Boolean function in lexicographic order")
ax.set_yticks([0, 1, 2])
ax.add_line(plt.Line2D([0, 276],[.5, .5], color="black", linewidth=3))
ax.add_line(plt.Line2D([0, 276],[1.5, 1.5], color="black", linewidth=3))
ax.set_yticklabels(["(a) circuit distribution", "(b) grammar before learning", "(c) grammar after learning"])
plt.tight_layout()
fig.savefig("/Users/edechter/Dropbox/Projects/ProgramInduction/drafts/ijcai2013/figures/circuitDistrComp.pdf")
plt.show()
