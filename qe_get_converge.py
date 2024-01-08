#!/usr/bin/env python
import numpy as np
import argparse
import os
import sys
from pathlib import Path
sys.path.append(os.environ['SCRIPT'])
from qe_inter_abc import get_celldm
import matplotlib.pyplot as plt


parser = argparse.ArgumentParser(description='convergence data, example python  $SCRIPT/qe_get_converge.py 1/2/3 --f test*')
parser.add_argument('mode', type=int, choices=[1,2,3],
        help='convergence type 1:kpoint 2:cutoff 3:cut_density')
parser.add_argument('--f', metavar='f',  type=str, nargs='+', default=[],
        help='convergence folders')
parser.add_argument('--inp', type=str, default='in.qe',
        help='QE input file')
parser.add_argument('--outp', type=str, default='out.qe',
        help='QE output file')
args = parser.parse_args()
mode = args.mode
folders = args.f
fil1 = args.inp
fil2 = args.outp

### get default input from in.qe
def extract_in(fil, ):
    with open(fil, 'r') as f:
        lines = f.readlines()
    for j, l in enumerate(lines):
        if mode==1 and 'K_POINTS' in l:
            print(lines[j+1])
            xlabel = 'k points'
            var=int(lines[j+1].split()[0])
            return var, xlabel
            break
        elif mode==2 and 'ecutwfc' in l:
            print(l)
            xlabel = 'ecutwfc'
            var = float(l.split()[-1])
            return var, xlabel
            break
        elif mode==3 and 'ecutrho' in l:
            print(l)
            xlabel = 'ecutrho'
            var = float(l.split()[-1])
            return var, xlabel
            break

x=[]
y=[]
for fol in folders:
    abc = get_celldm(Path(fol) / fil2)
    var, xlabel = extract_in(Path(fol) / fil1)
    x.append(var)
    y.append(abc)

SMALL_SIZE = 12
MEDIUM_SIZE = 13
BIGGER_SIZE = 14

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title

num_plots = 3
ylabels = ['a','b','c']
y=np.array(y)
for i in range(num_plots):
    plt.subplot(num_plots, 1, i+1)
    plt.plot(x, y[:,i],'-')
    plt.ylabel(ylabels[i])
plt.xlabel(xlabel)

plt.tight_layout()
figname="qe_converge_%s.pdf" % (mode)
print('save figure:%s'% (figname))
plt.savefig(figname,dpi=150, transparent=True)
plt.close()


