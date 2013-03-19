


from glob import glob
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib

import symreg as SR

files = glob("../data/EX5/*frontier10000*/numhit.csv")

def loadFile(file):
    x = open(file, 'r').read()
    return [float(i) for i in x.split(",")]

categories = [
    "NoConstants", 
    "NoLinear", 
    "OnlyQuadratic", 
    "OnlyComplexQuadratic", 
#    "UpToQuadratic"
    ]
totals = { "NoConstants" : 10 * 10 * 10 - 10,
           "NoLinear" : 10 * 10 * 10 - 10,
           "OnlyQuadratic" : 900,
           "OnlyComplexQuadratic" : 729,
           "UpToQuadratic" : 1000}
names = { "NoConstants" : "no constants",
           "NoLinear" :  "no linear",
           "OnlyQuadratic" : "only quadratics",
           "OnlyComplexQuadratic" : "only complex quadratics",
           "UpToQuadratic" : "up to quadratics"}


D = {}
for category in categories:
    D[category] = []
    for file in files:
        if not file.find(category)== -1:
            D[category].append(loadFile(file))

for k, v in D.items():
    D[k] = np.array(v).T/float(totals[k]) * 100.0
    


styles = ["rx-", "g*-", "bs-.", "k^-"] 
matplotlib.rcParams.update({'font.size': 20})
w, h = plt.figaspect(.5)
fig = plt.figure(figsize=(w,h))
ax = fig.add_subplot(111)

display = []
c = 0
for i, (k, v) in enumerate(D.items()):
    display.append(c)
    print v
    ax.errorbar(range(15), np.mean(v, 1).T, 
                yerr=np.std(v,1).T, 
                fmt=styles[i % len(styles)], 
                lw=3,
                elinewidth=2,
                ms=10,
                label=names[k])
    try:
        c = c + v.shape[1]
    except IndexError:
        pass

# handles, labels = ax.get_legend_handles_labels()
# ax.legend( [handle for i, handle in enumerate(handles) if i in display], 
#             [label for i, label in enumerate(labels) if i in display], loc=4)

ax.legend(loc=4)
ax.set_xlabel("iteration")
ax.set_ylabel("% tasks solved")

fig.savefig("/Users/edechter/Dropbox/Projects/ProgramInduction/drafts/ijcai2013/figures/symRegDiffCurricula.pdf")

plt.show()


