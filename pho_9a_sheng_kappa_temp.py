#!/usr/bin/env python
import numpy as np
import matplotlib.pyplot as plt
import os
import sys
from pathlib import Path
import matplotlib.ticker as tck

SMALL_SIZE = 16
MEDIUM_SIZE = 18
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title
#plt.figure(figsize=(7,5))
plt.figure()

#######################################################

print('Run in a folder with ShengBTE job. Make sure you are outside a temperature folder like T300K/' )
filename = 'BTE.kappa_scalar'
tensor_ind = [11,12,13,21,22,23,31,32,33,'scalar'] # column number 1, 2, 3, 4, 5, 6, 7, 8, 9
comps = [11] # scalar is @ the second row of data, use this to make the column number consistent
labels = ['$\kappa_\mathrm{x}$','$\kappa_\mathrm{xy}$','$\kappa_\mathrm{xz}$','$\kappa_\mathrm{yx}$','$\kappa_\mathrm{y}$','$\kappa_\mathrm{yz}$','$\kappa_\mathrm{zx}$','$\kappa_\mathrm{zy}$','$\kappa_\mathrm{z}$','scalar' ]
if len(sys.argv) < 2:
        print('Plot %s by default. If plot any component of tensor, enter ij (i,j=1,2,3). Like 11 22 33' % filename )
else:
        filename = 'BTE.kappa_tensor'
        comps = sys.argv[1:] # components
        comps = np.array(comps).astype(int)
        comps = np.unique(comps)
        for comp in comps:
                assert comp in tensor_ind, 'Error! argument %s is not recognized' % (comp)
labels = [ labels[tensor_ind.index(comp)] for comp in comps  ]

## find all temperture directories
temps = []
lists = []
files = os.listdir()
for item in files:
	if os.path.isdir(item):
		lists.append(item)
		temp = item.split('T')[-1]
		temp = temp.split('K')[0]
		temps.append(float(temp))
print('The temperatures in the current directory: %s' % temps)
x=np.array(temps) # temperatures


## Read thermal conductivity data
y=[] # y(temp_index, comp_index) # temperature index, kappa component index
for item in lists:
	f = Path(item) / filename
	data = np.loadtxt(f)
	data = data[-1] # the last row is the converged kappa
	yi = []
	for comp in comps:
		column = tensor_ind.index(comp) + 1 # column number
		yi.append(data[column])
	y.append(yi)
y = np.round(y,5)
##edit the comment back for scalar
if filename == 'BTE.kappa_scalar':
	comps = ['scalar'] 



#######################################################
# when plotting diagonal terms, check wheter a==b==c
if np.all(comps == [11,22,33]):
    if np.all(y[:,0] == y[:,1]) and np.all(y[:,1] == y[:,2]) :
        print('Diagonal terms are identical')
        labels = ['$\kappa_\mathrm{x}=\kappa_\mathrm{y}=\kappa_\mathrm{z}$']
        comps = [11] # no longer needed
    elif np.all(y[:,0] == y[:,1]):
        print('In-plane are identical a=b')
        labels = ['$\kappa_\mathrm{x}=\kappa_\mathrm{y}$','$\kappa_\mathrm{z}$']
        comps = [11,33] # no longer needed
        y=y[:,0:3:2] 
    else:
        print('Three diagonal terms are not the same')
#######################################################



## edit lables
labeltype=0
if labeltype == 0:
    title=''
    ylabel='Thermal Conductivity'
    #ylabel=r'$\kappa$'
    xlabel='T'
    xlabelunit=' [K]'
    ylabelunit=' [W/mK]'
elif labeltype == 1:
    title=''
    ylabel='Thermal Conductivity'
    #ylabel=r'$\kappa$'
    xlabel='T'
    xlabelunit=' [K]'
    y = y/100
    ylabelunit=' [W/cmK]'
colors=['tab:blue','tab:orange','tab:green']


for i in range(len(labels)):
    plt.plot(x,y[:,i],label=labels[i], #[tensor_ind.index(comp)],
            color=colors[i],
            marker='o',markersize=6,
            linewidth=3,linestyle='-')

print('x=%s' % x )
print('y=%s' % y )

# settings
ax=plt.gca()
#plt.xscale('log')
#plt.yscale('log')
plt.legend()
plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label


### advanced settings
#####################################################################
ax.tick_params(top=True,bottom=True,right=True,left=True,direction='in')
ax.tick_params(which='minor', top=True,bottom=True,right=True,left=True,direction='in')
#####################################################################
labeltype=1
if labeltype == 0:
    ## y log scale, x normal scale
    ax.xaxis.set_minor_locator(tck.AutoMinorLocator())
    plt.yscale('log')
    #ax.yaxis.set_minor_locator(tck.LogLocator(numticks=999,subs=(.1,.2,.3,.4,.5,.6,.7,.8,.9)))
    ax.yaxis.set_minor_locator(
            tck.LogLocator(numticks=999,subs='auto') )
else:
    ## x/y both are normal scale
    ax.xaxis.set_minor_locator(tck.AutoMinorLocator())
    ax.yaxis.set_minor_locator(tck.AutoMinorLocator())
    plt.grid(linestyle='dashed')
    #ax.grid(axis='x')
## limits
xlimits=[0,400]
plt.xlim(xlimits)
#ylimits=[0,50]
#plt.ylim(ylimits)
#####################################################################

#save
plt.tight_layout()
figname= 'graph_%s.pdf' % (filename[4:])
print('remotesingle %s' % (figname))
plt.savefig(figname,dpi=600,transparent=True)
plt.close()
#plt.show()
