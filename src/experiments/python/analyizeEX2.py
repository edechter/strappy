
from glob import glob
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib

import symreg as SR

files = glob("../data/*digarith_ec2*/numhit.csv")


def loadFile(file):
    x = open(file, 'r').read()
    return [float(i) for i in x.split(",")]

df = pd.DataFrame([loadFile(f) for f in files]).T/(2.**4 + 2.**8) * 100
matplotlib.rcParams.update({'font.size': 20})
w, h = plt.figaspect(.5)
fig = plt.figure(figsize=(w,h))
ax = fig.add_subplot(111)
ax.set_ylim([0,100])
ax.set_xlabel("iteration")
ax.set_ylabel("% tasks solves")
df.plot(style="k-", ax=ax, legend=None)
plt.tight_layout()
fig.savefig("/Users/edechter/Dropbox/Projects/ProgramInduction/drafts/ijcai2013/figures/booleanfunctionsUniform.pdf")
