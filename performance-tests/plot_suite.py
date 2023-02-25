import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os

ROUND_DECIMALS = 4
FILE_FORMAT = 'png'
for d in os.scandir('./runs'):
    if not os.path.isdir(d): continue
    if d.name == 'plots': continue
    means = []
    fnames = []
    for f in os.scandir(d):
        if not f.name.endswith('.csv'): continue
        data = pd.read_csv(f.path).to_numpy()

        mean = np.mean(data, axis=0)
        means.append(mean)

        fname = f.name[:-8]
        threads = fname[fname.rfind('_')+1:]
        fname = fname[:fname.rfind('_')]
        work_groups = fname[fname.rfind('_')+1:]
        fnames.append(f'WG: {work_groups}, Threads: {threads}')

    if len(fnames) == 0: continue

    means = np.asarray(means).T
    means = np.round(means, ROUND_DECIMALS)
    measurement = ('real','user','sys')

    x = np.arange(len(fnames))
    width = 0.25
    multiplier = 0

    fig, ax = plt.subplots(constrained_layout=True)

    for i, m in enumerate(means):
        offset = width * multiplier
        rects = ax.bar(x + offset, m, width, label=measurement[i])
        ax.bar_label(rects, padding=3)
        multiplier += 1

    ax.set_ylabel('Runtime in sec')
    ax.set_title(d.name)
    ax.set_xticks(x + width, fnames)
    ax.legend(loc='upper right')
    ax.set_ylim(0, np.max(means)*1.4)

    plt.setp(ax.get_xticklabels(), rotation=15, ha="right",
             rotation_mode="anchor")
    plt.savefig('./runs/plots/' + d.name + '.' + FILE_FORMAT, format=FILE_FORMAT)