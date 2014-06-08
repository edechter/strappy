

from os.path import join, exists, dirname
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

import launchEC2DigArith as DA

grammarFiles = [join(dirname(f), "grammars.csv") for f in DA.files]
badGrammarFiles = [grammarFiles[i] for i in [0,2,3,4,5,7]]
goodGrammarFiles = [grammarFiles[i] for i in [1,6,8,9]]

def getGrammar(file):
    fid = open(file, 'r')
    rows = []
    combs = []
    for row in fid.readlines():
        combs.append(row.split(",")[0])
        strvals = row.split(",")[1:]
        vals = []
        for sv in strvals:
            if sv.strip() == 'nan':
                vals.append(np.nan)
            else:
                vals.append(float(sv))
        rows.append(vals)
        grammar = np.array(rows)
    return combs, grammar


    
# grammarFiles = badGrammarFiles + goodGrammarFiles
# grsColumns = [[getGrammar(f)[1][:, i] for f in grammarFiles] for i in range(15)]
# # gs =  [[len([v for v in g.tolist() if not np.isnan(v)]) for g in grs] for grs in grsColumns]
# initLogProbs = [[v for v in g.tolist() if not np.isnan(v)] for g in grsColumns[0]]


df = pd.DataFrame([DA.loadFile(f) for f in DA.files]).T/(272.0)
means_df = df.mean(1)
std_df = df.std(1)

w, h = plt.figaspect(1.5)
fig = plt.figure(figsize=(w,h))
ax = fig.add_subplot(111)
ax.set_xlabel("iteration")
ax.set_ylabel("% tasks solved")
df.plot(linewidth=2, legend=False, ax = ax)
fig.savefig("/Users/edechter/Dropbox/Projects/ProgramInduction/drafts/ijcai2013/figures/boolLearningCurves.pdf")
