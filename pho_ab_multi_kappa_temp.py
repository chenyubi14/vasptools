#!/usr/bin/env python
import numpy as np
import os
import itertools
import argparse
from pathlib import Path
import matplotlib.pyplot as plt
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

parser = argparse.ArgumentParser(description='python thermal conductivity from multuple folders')
parser.add_argument('folders', type=str, nargs='+',
        help='enter shengbte folders, not go into each temperature folder')
#parser.add_argument('--comps', metavar='n',type=int,dest='comps',default=[11,22,33], 
parser.add_argument('--comps', nargs='+',type=int,dest='comps',default=[11,22,33], 
        choices=[11,12,13,21,22,23,31,32,33],
        help='Which kappa component to plot')
args = parser.parse_args()
folders = args.folders
comps = args.comps #[11,22,33] 

filename = 'BTE.kappa_tensor'
tensor_ind = [11,12,13,21,22,23,31,32,33] # column number 1, 2, 3, 4, 5, 6, 7, 8, 9
labels = ['$\kappa_\mathrm{x}$',
          '$\kappa_\mathrm{xy}$',
          '$\kappa_\mathrm{xz}$',
          '$\kappa_\mathrm{yx}$',
          '$\kappa_\mathrm{y}$',
          '$\kappa_\mathrm{yz}$',
          '$\kappa_\mathrm{zx}$',
          '$\kappa_\mathrm{zy}$',
          '$\kappa_\mathrm{z}$']
labels = [ labels[tensor_ind.index(comp)] for comp in comps ]
shapes=['o','*','s']
markersize=[6,10,6]
colors= ['tab:blue','tab:orange','tab:green','tab:red']

def flip(items, ncol):
    return itertools.chain(*[items[i::ncol] for i in range(ncol)])

#######################################################
data = {}
comments = []

for i,fol_i in enumerate(folders):
    ## find all temperture 
    temps = [] # temperatures
    lists = [] # list of temperature directory
    files = os.listdir(fol_i)
    for item in files:
        item = str(Path(fol_i)/item)
        if os.path.isdir(item):
            lists.append(item)
            temp = item.split('T')[-1]
            temp = temp.split('K')[0]
            temps.append(float(temp))
    assert len(lists)>0, 'Not found temperature folders in %s' % fol_i
    print('The temperatures in %s: %s' % (fol_i, temps) )
    x_i = np.array(temps) # temperatures
    ## Read thermal conductivity data
    y_i = [] # y(temp_index, comp_index) # temperature index, kappa component index
    for item in lists:
        f = Path(item) / filename
        data = np.loadtxt(f)
        data = data[-1] # the last row is the converged kappa
        yj = []
        for comp in comps:
            column = tensor_ind.index(comp) + 1 # column number
            yj.append(data[column])
        y_i.append(yj)
    y_i = np.round(y_i,5)
    # when plotting diagonal terms, check wheter a==b==c
    if np.all(comps == [11,22,33]):
        if np.all(y_i[:,0] == y_i[:,1]) and np.all(y_i[:,1] == y_i[:,2]) :
            print('Diagonal terms are identical')
            label_i = ['$\kappa_\mathrm{x}=\kappa_\mathrm{y}=\kappa_\mathrm{z}$']
            #comps = [11] # no longer needed
        elif np.all(y_i[:,0] == y_i[:,1]):
            print('In-plane are identical a=b')
            label_i = ['$\kappa_\mathrm{x}=\kappa_\mathrm{y}$','$\kappa_\mathrm{z}$']
            #comps = [11,33] # no longer needed
            y_i=y_i[:,0:3:2]
        else:
            print('Three diagonal terms are not the same')
            label_i = labels
    ## save data as a dictionary
    #data[fol_i] = {'x':x_i, 'y':y_i, 'labels':label_i} #[x_i, y_i]
    dict_i=dict(np.loadtxt(fol_i + '/SAVEINFO', comments='#',dtype=str, delimiter='='))
    comments.append(dict_i['COMMENT'])
    color = colors[i]
    if 'color' in dict_i.keys():
        color=dict1['color']
    for j in range(len(label_i)):
        plt.plot(x_i,y_i[:,j],
                label=label_i[j],color=color,
                marker=shapes[j],markersize=markersize[j],
                linewidth=3,linestyle='-')
    print('x=%s\ny=%s' % (x_i, y_i) )
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


# settings
ax=plt.gca()
#plt.xscale('log')
#plt.yscale('log')
plt.legend(ncol=len(folders))
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
#xlimits=[0,400];plt.xlim(xlimits)
#ylimits=[0,50];plt.ylim(ylimits)
## for BaMnSb2
xlimits=[100,400];plt.xlim(xlimits)
ylimits=[0.5,7];plt.ylim(ylimits)

#####################################################################
## put the comments in front of legend
print('assume symmetry is the same for rewriting labels')
num_fol = len(folders)
num_label = len(label_i)
handles, labels = ax.get_legend_handles_labels()
## add to handle
handles = [plt.plot([],marker="", ls="")[0]] * num_fol + list(flip(handles,num_label))
## add to label, comments=['neighbor3', 'neighbor4'], added as a row of legends
labels = comments + list(flip(labels,num_label))
leg=plt.legend(handles, labels, loc='best', ncol=num_label+1, columnspacing=0.5 , framealpha=0)
for vpack in leg._legend_handle_box.get_children()[:1]:
    for hpack in vpack.get_children():
        hpack.get_children()[0].set_width(0)
#####################################################################



#save
plt.tight_layout()
figname= 'graph_combine_kappa_tensor.pdf' #% (filename[4:])
print('remotesingle %s' % (figname))
plt.savefig(figname,dpi=600,transparent=True)
plt.close()
#plt.show()
