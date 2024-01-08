#!/usr/bin/env python
import numpy as np
import sys
import matplotlib.pyplot as plt
import os

SMALL_SIZE = 16
MEDIUM_SIZE = 18
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=BIGGER_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title
plt.figure(figsize=(7,5))



filename= 'BTE.cumulative_kappa_scalar'
print('Run in a folder within a temperature folder like T300K/.' )
tensor_ind = [11,12,13,21,22,23,31,32,33] # column number 1, 2, 3, 4, 5, 6, 7, 8, 9
comps = [11] ## this works for scalar value
if len(sys.argv) < 2:
	print('Plot %s by default. If plot any component of tensor, enter ij (i,j=1,2,3). Like 11 22 33' % filename )
else:
	filename = 'BTE.cumulative_kappa_tensor'
	comps = sys.argv[1:] # components
	comps = np.array(comps).astype(int) 
	comps = np.unique(comps)
	for comp in comps:
		assert comp in tensor_ind, 'Error! argument %s is not recognized' % (comp)
	
print('Plot cumulative kappa for each temperature, using file %s' % (filename) )


# get temperature from PWD, like T300K
temp = os.getcwd()
from pathlib import Path
temp = Path(temp)
temp = temp.parts[-1]


# load data
data=np.loadtxt(filename) 
x=data[:,0] # mean free path or diameter (nm)
abc=['$\kappa_{\mathrm{ph}}^a$',
     '$\kappa_{\mathrm{ph}}^b$',
     '$\kappa_{\mathrm{ph}}^c$']
for i,comp in enumerate(comps):
    column = tensor_ind.index(comp) + 1 # column number
    yi = data[:,column]
    if filename == 'BTE.cumulative_kappa_scalar':
        #plt.plot(x, yi, label='scalar' )
        plt.plot(x, yi, label='%s K' % (temp[1:-1] ) ,linewidth=3) # 300K
    else:
        plt.plot(x, yi, label=r'%s' % (abc[i]) ,linewidth=3)







# edit lables
#####################################################################
labeltype=0
if labeltype == 0:
    plt.legend(framealpha=0.)
    #title='Cumulative Thermal Conductivity' #% (temp)
    title=''
    xlabel='Mean Free Path'
    xlabelunit=' [nm]'
    #ylabel='Cumulative Thermal Conductivity' #r'$\kappa$'
    ylabel='Cumulative $\kappa_{\mathrm{ph}}$' #r'$\kappa$'
    ylabelunit=' [W/mK]'
#####################################################################


plt.title(title,fontsize=18) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
import matplotlib.ticker as tck
ax=plt.gca()
ax.set_xscale('log')
ax.tick_params(top=False,bottom=True,right=True,left=True,direction='in')
ax.xaxis.set_major_locator(tck.LogLocator(numticks=999))
ax.xaxis.set_minor_locator(tck.LogLocator(numticks=999,subs=(.2,.4,.6,.8)))
ax.yaxis.set_minor_locator(tck.AutoMinorLocator())
plt.tick_params(axis='y', which='minor', color='k', right=True) # set minor thinkness, interval
#plt.tick_params(axis='y', which='minor', width=1., length=2, color='k', right=True) # set minor thinkness, interval
#plt.tick_params(axis='x', which='minor', width=1., length=2, color='k', top=True) # set minor thinkness, interval

#plt.xscale('log')
plt.xlim([1e-1,1e4])
plt.ylim([0.0,1.5])


plt.tight_layout()
figname= filename + '.%s' % temp + '.pdf'
print('remotesingle %s' % (figname))
plt.savefig(figname,dpi=600,transparent=True)
plt.show()
plt.close()
