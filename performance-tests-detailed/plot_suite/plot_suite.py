import numpy as np
import matplotlib.pyplot as plt
import os
import pandas as pd


TITLE="Reduce_Shared_Mem"
CU_FOLDER="/home/olivers/code/descend-pjs-22-23/performance-tests-detailed/plot_suite/results/reduce_shared_mem_cu"
CL_FOLDER="/home/olivers/code/descend-pjs-22-23/performance-tests-detailed/plot_suite/results/reduce_shared_mem_cl"

FILE_FORMAT = 'png'

def plot():
    means_cu = []
    means_cl = []
    fnames = []

    files_cl = []
    for f in os.scandir(CL_FOLDER):
        if not f.name.endswith('.csv'): continue
        files_cl.append(f)

    files_cl.sort( key= lambda f: f.name )

    for f in files_cl:
        if not f.name.endswith('.csv'): continue
        data = pd.read_csv(f.path).to_numpy()
        mean = np.mean(data, axis=0)
        means_cl.append(mean)


    files_cu = []
    for f in os.scandir(CU_FOLDER):
        if not f.name.endswith('.csv'): continue
        files_cu.append(f)

    files_cu.sort( key= lambda f: f.name )

    for f in files_cu:
        if not f.name.endswith('.csv'): continue
        print(f.name)
        
        data = pd.read_csv(f.path).to_numpy()
        mean = np.mean(data, axis=0)
        means_cu.append(mean)

        fname = f.name[:-8]
        threads = fname[fname.rfind('_')+1:]
        fname = fname[:fname.rfind('_')]
        work_groups = fname[fname.rfind('_')+1:]
        print(f'WG: {work_groups}, Threads: {threads}')
        fnames.append(f'WG: {work_groups}, Threads: {threads}')

    means_cu = np.asarray(means_cu).T
    means_cu = np.round(means_cu, 4)
    measurement = ('kernel_cu',"kernel_cl")

    x = np.arange(len(fnames))
    width = 0.25
    multiplier = 0

    fig, ax = plt.subplots(constrained_layout=True)

    # means_cu = [means_cu[0], means_cl[0]]

    m = means_cu[0]
    i = 0
    offset = width * multiplier
    rects = ax.bar(x + offset, m, width, label=measurement[i])
    # ax.bar_label(rects, padding=3)
    multiplier += 1

    m = means_cl[0]
    i = 1
    offset = width * multiplier
    rects = ax.bar(x + offset, m, width, label=measurement[i])
    # ax.bar_label(rects, padding=3)
    multiplier += 1



    ax.set_ylabel('Mean runtime in sec [50 rounds]')
    ax.set_title(TITLE)
    ax.set_xticks(x + width, fnames)
    ax.legend(loc='upper right')
    ax.set_ylim(0, np.max(means_cl)*1.4)

    plt.setp(ax.get_xticklabels(), rotation=45, ha="right",
             rotation_mode="anchor")
    plt.savefig('./plots/' + TITLE + '.' + FILE_FORMAT, format=FILE_FORMAT)


if __name__ == "__main__":
    plot()