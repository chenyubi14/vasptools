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


parser = argparse.ArgumentParser(description='plot cumulative kappa from multiple folders')
parser.add_argument('folders', type=str, nargs='+',
        help='enter each temperature subfolder')
#parser.add_argument('--comps', metavar='n',type=int,dest='comps',default=[11,22,33], 
parser.add_argument('--comps', nargs='+',type=int,dest='comps',default=[11,22,33], 
        choices=[11,12,13,21,22,23,31,32,33],
        help='Which kappa component to plot')
args = parser.parse_args()
folders = args.folders
comps = args.comps #[11,22,33] 
comps_labels =[ '$\kappa_{\mathrm{ph}}^a$',
                '$\kappa_{\mathrm{ph}}^{ab}$',
                '$\kappa_{\mathrm{ph}}^{ac}$',
                '$\kappa_{\mathrm{ph}}^{ba}$',
                '$\kappa_{\mathrm{ph}}^b$',
                '$\kappa_{\mathrm{ph}}^{bc}$',
                '$\kappa_{\mathrm{ph}}^{ca}$',
                '$\kappa_{\mathrm{ph}}^{cb}$',
                '$\kappa_{\mathrm{ph}}^c$']

tensor_ind = [11,12,13,21,22,23,31,32,33] # column number 1, 2, 3, 4, 5, 6, 7, 8, 9
filename = 'BTE.cumulative_kappa_tensor'
colors = ['tab:blue','tab:orange','tab:green','goldenrod']
colors = colors[:len(folders)]
colors = colors[::-1]
shapes=['o','*','s']
markersize=[6,10,6]
linestyles = ['-','--','.']

ax=plt.gca()
comments=[]
for i,fol_i in enumerate(folders):
    ## get data
    file_i = fol_i + '/'+ filename
    data=np.loadtxt(file_i)
    ## get legend
    dict_i=dict(np.loadtxt(fol_i + '/SAVEINFO', comments='#',dtype=str, delimiter='='))
    comments.append(dict_i['legend'])
    xi=data[:,0] ## mean free path or diameter (nm)
    for j, comp_j in enumerate(comps):
        column_index = tensor_ind.index(comp_j) + 1 ## column index number to get data
        yi = data[:,column_index]
        comp_j_label = comments[-1]+ ' ' + comps_labels[column_index-1]
        plt.plot(xi, yi, label=r'%s' % ( comp_j_label ), 
                color = colors[i],
                #marker=shapes[j],markersize=markersize[j],
                linewidth=3, linestyle=linestyles[j] )


title=dict_i['title']


#####################################################################
#plt.legend(framealpha=0.0,loc='upper left', bbox_to_anchor=(-0.05, 1.0))
plt.legend(framealpha=0.0)
#####################################################################

# edit lables
#####################################################################
labeltype=1
ax=plt.gca()
if labeltype == 1:
    # y log scale
    xlabel='Mean Free Path'
    xlabelunit=' [nm]'
    ylabel='Cumulative $\kappa_{\mathrm{ph}}$'
    ylabelunit= ' [W/mK]'
    plt.xscale("log")
    plt.xlim([1e-1,1e4])
    #plt.ylim([])
    ax.tick_params(top=False,bottom=True,right=True,left=True,direction='in')
    ax.yaxis.set_minor_locator(tck.AutoMinorLocator())
    #ax.yaxis.set_minor_locator(tck.LogLocator(numticks=999,subs=(.2,.4,.6,.8)))
    ax.xaxis.set_major_locator(tck.LogLocator(numticks=999))
    ax.xaxis.set_minor_locator(tck.LogLocator(numticks=999,subs='auto'))
    ax.tick_params(which='minor', top=False,bottom=True,right=True,left=True,direction='in')
#####################################################################


plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label

#save
plt.tight_layout()
figname = 'graph_combine_cumulative_kappa.pdf' #filename + '.%s' % temp + '.pdf'
print('remotesingle \t%s' % (figname))
plt.savefig(figname,dpi=600,transparent=True)
plt.close()
#plt.show()
