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
mode=3 # 1 don't specify the energy range; 2 limit the energy range
###################################################################


## read data
data = np.loadtxt('infile.stat')
std = round(np.std(data[:,5]),2) # standard deviation of temperature


fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(nrows=2, ncols=2)
'''
fig = plt.figure()
ax1 = fig.add_subplot(221)   #top left
ax2 = fig.add_subplot(222)   #top right
ax3 = fig.add_subplot(223)   #bottom left
ax4 = fig.add_subplot(224)   #bottom right 
'''
#xlabel='Time Step [fs]'
xlabel='Step number'
## total energy 
#ax1.scatter(data[:,0],data[:,2],s=0.1,c='y')
ax1.scatter(data[:,0],data[:,3],s=0.1,c='y')
ax1.set_xlabel(xlabel)
ax1.set_ylabel('Total Energy [eV]')
ax2.scatter(data[:,0],data[:,5],s=0.1,c='yellowgreen')
ax2.set_xlabel(xlabel)
ax2.title.set_text("sd: {}".format(std))
ax2.set_ylabel('Temperature [K]')
ax3.scatter(data[:,0],data[:,6],s=0.1,c='darkolivegreen')
ax3.set_xlabel(xlabel)
ax3.set_ylabel('Pressure [GPa]')
#ax3.set_ylim(-4,0)
ax4.scatter(data[:,0],data[:,7],s=0.1,c='forestgreen')
ax4.set_xlabel(xlabel)
ax4.set_ylabel('Stress [GPa]')

if mode == 2:
    energy=data[:, 2]
    emin=energy[0] - 0.5 #20
    emax=energy[0] + 0.5 #20
    energy=energy[np.where(np.logical_and(energy>=emin, energy<=emax))]
    emax=energy.max()
    emin=energy.min()
    ax1.set_ylim([emin,emax])


## save
plt.tight_layout()
figname = 'tdep_stats.png'
plt.savefig(figname,dpi=600,transparent=True)
print('file saved as ', figname)

