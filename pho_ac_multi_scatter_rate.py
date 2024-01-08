#!/usr/bin/env python
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as tck
import os
import argparse

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


parser = argparse.ArgumentParser(description='plot scattering rates from multuple folders')
parser.add_argument('folders', type=str, nargs='+',
        help='enter each temperature subfolder')
args = parser.parse_args()
folders = args.folders

filename = 'BTE.w_anharmonic'
colors = ['tab:blue','tab:orange','tab:green','goldenrod']
colors = colors[:len(folders)]
colors = colors[::-1]
plot_omega2 = True

ax=plt.gca()
comments=[]
for i,fol_i in enumerate(folders):
    ## get data
    file_i = fol_i + '/'+ filename
    data=np.loadtxt(file_i)
    x=data[:,0]/2/np.pi # data[:,0] is rad/ps
    # dividing 2pi makes it 1/ps, which is THz
    y=data[:,1]
    dict_i=dict(np.loadtxt(fol_i + '/SAVEINFO', comments='#',dtype=str, delimiter='='))
    comments.append(dict_i['legend'])
    plt.scatter(x, y, s=5, label=comments[-1], color=colors[i]) 

    #####################################################################
    if plot_omega2:
        ##poly fit
        from scipy.optimize import curve_fit
        fitrange=100
        xlimit=[2e-1,1e0] ## only plot this range
        #color=colors[i]
        color='tab:green'
        arg_order = np.argsort(x)
        x_order = x[arg_order]
        y_order = y[arg_order]
        def func(x, a ):
            return a * x ** 2  
        popt, _ = curve_fit(func, x_order[:fitrange], y_order[:fitrange])
        xnew=np.linspace(*xlimit,100)
        ynew=func(xnew, *popt)
        plt.plot(xnew,ynew,color=color,linestyle='--')
        x_text = 10**np.average([np.log10(xlimit[0]), np.log10(xlimit[1])]) * 0.8
        y_text = func( xlimit[0], *popt )
        plt.text(x_text, y_text, '$\omega^2$',color=color)
        plot_omega2 = False
    #####################################################################


title=dict_i['title']


#####################################################################
plt.text(6e-1,3e0,'Antiferro',color=colors[0])
plt.text(6e-1,3e-3,'Ferro',color=colors[1])
#plt.legend(framealpha=0.0,loc='upper left', bbox_to_anchor=(-0.05, 1.0))
#####################################################################

# edit lables
#####################################################################
labeltype=1
if labeltype == 0:
    # x,y normal scale
    xlabel='Frequency'
    xlabelunit=' [THz]'
    ylabel='Scattering Rate'
    ylabelunit=' [$\mathrm{ps}^{-1}$]' # ' [THz]'
    #plt.ylim([0,1.2])
    ax.xaxis.set_minor_locator(tck.AutoMinorLocator())
    ax.yaxis.set_minor_locator(tck.AutoMinorLocator())
elif labeltype == 1:
    # y log scale
    xlabel='Frequency'
    xlabelunit=' [THz]'
    ylabel='Scattering Rate'
    ylabelunit=' [$\mathrm{ps}^{-1}$]' # ' [THz]'
    plt.xscale("log")
    plt.yscale("log")
    plt.xlim([1e-1,4e0])
    plt.ylim([1e-3,1e1])
    #ax.yaxis.set_minor_locator(tck.LogLocator(numticks=999,subs=(.2,.4,.6,.8)))
    ax.yaxis.set_minor_locator(tck.LogLocator(numticks=999,subs='auto'))
    ax.xaxis.set_minor_locator(tck.LogLocator(numticks=999,subs='auto'))
#####################################################################


plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
ax.tick_params(top=True,bottom=True,right=True,left=True,direction='in')
ax.tick_params(which='minor', top=True,bottom=True,right=True,left=True,direction='in')

plt.tight_layout()
#save
figname = 'graph_combine_scatter_rates.pdf' #filename + '.%s' % temp + '.pdf'
print('remotesingle \t%s' % (figname))
plt.savefig(figname,dpi=600,transparent=True)
plt.close()
#plt.show()
