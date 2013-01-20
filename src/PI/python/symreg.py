#!/usr/bin/python
#
# Author: Eyal Dechter
# Date: 1/17/2012
##########################


import pandas as pd
import matplotlib.pyplot as plt
import numpy as np


numhitFiles = [ 
{"filename" : "../data/R1_2000_2013-01-18_00-18-55.214122_EST/numhit.csv",
                 "frontier_size" : 2000},
{"filename" : "../data/R1_3000_2013-01-18_00-23-00.233885_EST/numhit.csv",
                 "frontier_size" : 3000},
{"filename" : "../data/R1_4000_2013-01-18_00-30-19.574117_EST/numhit.csv",
                 "frontier_size" : 4000},

{"filename" : "../data/R1_5000_2013-01-17_14-47-24.736361_EST/numhit.csv",
                 "frontier_size" : 5000},
{"filename" : "../data/R1_6000_2013-01-18_06-54-30.370197_EST/numhit.csv",
                 "frontier_size" : 6000},
{"filename" : "../data/R1_7000_2013-01-18_07-48-39.671237_EST/numhit.csv",
                 "frontier_size" : 7000},
{"filename" : "../data/R1_8000_2013-01-18_08-57-06.004799_EST/numhit.csv",
                 "frontier_size" : 8000},
{"filename" : "../data/R1_9000_2013-01-18_11-00-42.396298_EST/numhit.csv",
                 "frontier_size" : 9000},

                {"filename" : "../data/R1_10000_2013-01-17_15-34-32.810923_EST/numhit.csv",
                 "frontier_size" : 10000},
                {"filename" : "../data/R1_1000_2013-01-17_14-44-46.649237_EST/numhit.csv",
                 "frontier_size" : 1000}
             ]

numhitFiles2 = [ 
    {"filename" : "../data/R2_1000_2013-01-18_00-18-15.884901_EST/numhit.csv",
     "frontier_size" : 1000},
    {"filename" : "../data/R2_2000_2013-01-18_00-21-10.850648_EST/numhit.csv",
     "frontier_size" : 2000},
    {"filename" : "../data/R2_3000_2013-01-18_00-27-02.711662_EST/numhit.csv",
     "frontier_size" : 3000},
    {"filename" : "../data/R2_4000_2013-01-18_06-05-11.35682_EST/numhit.csv",
     "frontier_size" : 4000},
    {"filename" : "../data/R2_5000_2013-01-17_15-27-53.396156_EST/numhit.csv",
     "frontier_size" : 5000},
    {"filename" : "../data/R2_6000_2013-01-18_07-35-41.597988_EST/numhit.csv",
     "frontier_size" : 6000},
    {"filename" : "../data/R2_7000_2013-01-18_08-35-24.45969_EST/numhit.csv",
     "frontier_size" : 7000},
    {"filename" : "../data/R2_8000_2013-01-18_10-23-01.714354_EST/numhit.csv",
     "frontier_size" : 8000},
    ]

def loadNumhitFile(filename):
    return pd.read_csv(filename, header=None).T
    
def loadNumhitFiles(numhitDict):
    f =  numhitDict[0]
    df = loadNumhitFile(f['filename'])
    df.columns = [f['frontier_size']]

    for d in numhitDict[1:]:
        x = loadNumhitFile(d['filename'])
        df[d['frontier_size']] = x
    return df


def learningCurvesPlot(numhitDict, ax=None):
    df = loadNumhitFiles(numhitDict)
    df.index=range(1,16)
    perc = df/1331.0
    xlabel = "Iterations"
    ylabel = "% Tasks Solved"
    ax = perc.plot(style="-", 
                   use_index=True,
                   sort_columns=True,
                   linewidth=2,
                   
                   xlim = (1, 15))
                   
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    
    return ax

def performanceVsFrontierSize(numhitDict, ax=None):
    df = loadNumhitFiles(numhitDict)
    perc = df/1330.0
    finalVals = perc.xs(14).sort_index()
    xlabel = "Frontier Size"
    ylabel = "% Tasks solved after 15 iterations"
    ax = finalVals.plot(style="-",
                        sort_columns=True,
                        linewidth=2)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    return ax

    
    
    
                   
                   


    


if __name__ == "__main__":
    pass
