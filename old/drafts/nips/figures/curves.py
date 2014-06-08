
import os, sys
from matplotlib import pyplot as plt


# frontier 5000
ntasks = 12
xs = range(1, 6)
solved = [5,7,8,8,8]
productions = [0, 10, 13, 13, 14]

fig = plt.figure()
ax = fig.add_subplot(111)
ax.set_title("Learning over iterations.")
ax.plot(xs, solved)
ax.set_ylim(0, 12)

