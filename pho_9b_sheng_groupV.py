import numpy as np
import sys
import matplotlib.pyplot as plt
import os

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
#plt.figure(figsize=(4,6))

ax=plt.subplot(111)


filename1= 'BTE.omega'
filename2= 'BTE.v'

print('Run in a folder with ShengBTE job. Will read file %s and %s. Make sure you are outside a temperature folder like T300K/' % (filename1, filename2) )	

### load data
## phonon angular frequency
data=np.loadtxt(filename1) 
x=data  ## shape=(num_qpoint, num_branch)
x = x/2/np.pi # convert rad/ps to 1/ps
## phonon group velocity
data=np.loadtxt(filename2) 
y=data ## shape=( num_qpoint*num_branch , 3)
## velocity is a 3-D vector
print('group velocity maximum absolute value',np.max(np.abs(y)))


# plot for acoustic bands
x1=x[:,0] # acoustic 1
x2=x[:,1] # acoustic 2
x3=x[:,2] # acoustic 3
shape=x.shape
print('shape is (kpoints=%s,freq=%s)' % (shape[0],shape[1]) )

### change the shape of y
y=np.linalg.norm(y,axis=1) ## take the norm of the 3-D vector
## y (num_branch, num_qpoint)
y=y.reshape((shape[1],shape[0])) # increase q(kpoint) first, and then band
## y (num_qpoint, num_branch)
y=y.T # shape now agree with x,omega,freq
y1=y[:,0] # group velocity for acoustic 1
y2=y[:,1] # group velocity for acoustic 2
y3=y[:,2] # group velocity for acoustic 3


# plot for optical bands
x=x[:,3:].flatten()
y=y[:,3:].flatten()



size=5

plt.scatter(x,y,s=size,label='optical')
plt.scatter(x1,y1,s=size,label='acou 1')
plt.scatter(x2,y2,s=size,label='acou 2')
plt.scatter(x3,y3,s=size,label='acou 3')



# edit lables
#####################################################################
labeltype=0
if labeltype == 0:
	plt.legend()
	title='' 
	xlabel='Frequency'
	xlabelunit=' (THz)'
	ylabel='mode group velocity'
	ylabelunit=' km/s (nm THz)'
#####################################################################


plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
plt.tight_layout()


import matplotlib.ticker as tck
### make minor appear
ax.yaxis.set_minor_locator(tck.AutoMinorLocator()) # have y minor ticks
ax.xaxis.set_minor_locator(tck.AutoMinorLocator()) # if don't want x ticks, don't need this line


figname= filename2 + '.pdf'
print('remotesingle %s' % (figname))
plt.savefig(figname,dpi=600)
plt.show()
plt.close()
