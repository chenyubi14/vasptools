#!/usr/bin/env python
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as tck
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



filename = 'BTE.w_anharmonic'
print('Run in a folder within a temperature folder like T300K/. Will read file %s ' % (filename) )
data=np.loadtxt(filename)
x=data[:,0]/2/np.pi # data[:,0] is rad/ps
# dividing 2pi makes it 1/ps, which is THz
y=data[:,1]

# get temperature from PWD, like T300K
temp = os.getcwd()
from pathlib import Path
temp = Path(temp)
temp = temp.parts[-1]


plt.scatter(x, y, s=2, label='%s K'%temp[1:-1] ) #,  label="anharmonic") # put legend here
#####################################################################
##poly fit
from scipy.optimize import curve_fit
fitrange=100
xlimit=[1e-1,1e0] ## only plot this range
color='tab:orange'
arg_order = np.argsort(x)
x_order = x[arg_order]
y_order = y[arg_order]
def func(x, a ):
    return a * x ** 2  
popt, _ = curve_fit(func, x_order[:fitrange], y_order[:fitrange])
xnew=np.linspace(*xlimit,100)
ynew=func(xnew, *popt)
plt.plot(xnew,ynew,color=color,label='$\omega^2$')
x_text = 10**np.average([np.log10(xlimit[0]), np.log10(xlimit[1])])
y_text = func( 2*xlimit[0], *popt )
plt.text(x_text, y_text, '$\omega^2$',color=color)
#####################################################################



ax=plt.gca()
# edit lables
#####################################################################
labeltype=1
if labeltype == 0:
    title=''
    xlabel='Frequency'
    xlabelunit=' [THz]'
    ylabel='Scattering Rate'
    ylabelunit=' [$\mathrm{ps}^{-1}$]' # ' [THz]'
    plt.xlim([0,6.2])
    plt.ylim([0,1.2])
    ax.yaxis.set_minor_locator(tck.AutoMinorLocator())
    ax.xaxis.set_minor_locator(tck.AutoMinorLocator())
elif labeltype == 1:
    title=''
    xlabel='Frequency'
    xlabelunit=' [THz]'
    ylabel='Scattering Rate'
    ylabelunit=' [$\mathrm{ps}^{-1}$]' # ' [THz]'
    plt.xscale("log")
    plt.yscale("log")
    xlimit=[1e-2,1e1]
    ylimit=[1e-3,1e1]
    #plt.xlim(xlimit)
    #plt.ylim(ylimit)
    ax.yaxis.set_minor_locator(tck.LogLocator(numticks=999,subs=(.2,.4,.6,.8)))
    ax.xaxis.set_minor_locator(tck.LogLocator(numticks=999,subs=(.1,.2,.3,.4,.5,.6,.7,.8,.9)))
#####################################################################

plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
ax.tick_params(top=True,bottom=True,right=True,left=True)
ax.tick_params(which='minor', top=True,bottom=True,right=True,left=True)

if xlimit:
    plt.xlim(xlimit)
if ylimit:
    plt.ylim(ylimit)


plt.tight_layout()
#save
figname = filename + '.%s' % temp + '.png'
print('remotesingle \t%s' % (figname))
plt.savefig(figname,dpi=600,transparent=True)
plt.close()
#plt.show()
