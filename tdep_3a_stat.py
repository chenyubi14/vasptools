#!/usr/bin/env python
import numpy as np
import sys
import os
import matplotlib.pyplot as plt

SMALL_SIZE = 12
MEDIUM_SIZE = 14
BIGGER_SIZE = 16

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title


###################################################################
mode=1 # 1 don't specify the energy range; 2 limit the energy range
###################################################################

free_energy=1 ## use 1 for the energy of material!

## read data
data = np.loadtxt('infile.stat')
std = round(np.std(data[:,5]),2) # standard deviation of temperature

fig, (ax1, ax2 ) = plt.subplots(nrows=1, ncols=2,figsize=(6,3))
#xlabel='Time Step [fs]'
xlabel='Step number'
if free_energy == 1:
    ## total energy of material, use this one!!
    ax1.scatter(data[:,0],data[:,3],s=0.1,c='y')
else:
    ## total energy of material + thermostat
    ax1.scatter(data[:,0],data[:,2],s=0.1,c='y')
ax1.set_xlabel(xlabel)
ax1.set_ylabel('Total Energy [eV]')
ax2.scatter(data[:,0],data[:,5],s=0.1,c='yellowgreen')
ax2.set_xlabel(xlabel)
ax2.title.set_text("sd: {}".format(std))



## save
plt.tight_layout()
figname = 'tdep_stats.png'
plt.savefig(figname,transparent=True)
print('file saved as ', figname)

