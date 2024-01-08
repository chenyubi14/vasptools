#!/usr/bin/env python
import numpy as np
import matplotlib.pyplot as plt
import os
import sys
from pathlib import Path
import matplotlib.ticker as tck
import itertools

def flip(items, ncol):
    return itertools.chain(*[items[i::ncol] for i in range(ncol)])

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


print('Run in a folder with ShengBTE job. Make sure you are outside a temperature folder like T300K/' )

filename = 'BTE.kappa_tensor'
tensor_ind = [11,12,13,21,22,23,31,32,33,'scalar'] # column number 1, 2, 3, 4, 5, 6, 7, 8, 9
comps = [11,22,33]
labels = ['$\kappa_\mathrm{x}$','$\kappa_\mathrm{xy}$','$\kappa_\mathrm{xz}$','$\kappa_\mathrm{yx}$','$\kappa_\mathrm{y}$','$\kappa_\mathrm{yz}$','$\kappa_\mathrm{zx}$','$\kappa_\mathrm{zy}$','$\kappa_\mathrm{z}$','scalar' ]

################################################################

x=np.array([ 50., 100., 150., 200., 250., 300., 350., 400.])
# PBE+U antiferro
y2=np.array([
 [12.43496181 , 9.6280945  , 6.95167005] ,
 [ 5.71716317 , 4.56583829 , 2.93061476] ,
 [ 3.79326815 , 3.06861851 , 1.88628379] ,
 [ 2.84624218 , 2.31487778 , 1.39728033] ,
 [ 2.27889298 , 1.85835183 , 1.11149367] ,
 [ 1.90042935 , 1.55202136 , 0.92343827] ,
 [ 1.62985615 , 1.3322572  , 0.79009929] ,
 [ 1.42675781 , 1.16693315 , 0.6905488 ] ,
 ])

# PBE+U+SOC antiferro
y3=np.array([
 [13.9250004  ,11.1732746  , 8.79571357] ,
 [ 6.34657927 , 5.24372017 , 3.72454894] ,
 [ 4.20380263 , 3.51550334 , 2.39982774] ,
 [ 3.15262477 , 2.64963673 , 1.77836439] ,
 [ 2.52365986 , 2.12623859 , 1.4148856 ] ,
 [ 2.10433071 , 1.77538037 , 1.1756145 ] ,
 [ 1.80463015 , 1.5238085  , 1.0059239 ] ,
 [ 1.57970616 , 1.33461829 , 0.87921565] ,
 ])

# PBE+U ferro
y1=np.array([
 [14.06689395 ,10.73547872 , 7.45148694] ,
 [ 6.5714656  , 5.19028251 , 3.09785084] ,
 [ 4.3975385  , 3.52019697 , 1.9892234 ] ,
 [ 3.31220543 , 2.66606131 , 1.47239841] ,
 [ 2.65708724 , 2.14452368 , 1.17085769] ,
 [ 2.21824256 , 1.79302031 , 0.97259464] ,
 [ 1.9037102  , 1.54018528 , 0.83207962] ,
 [ 1.66723199 , 1.34966365 , 0.72719897] ,
 ])

# experimental data total
#exp_style='dotted'
exp_style='-'
data1=np.loadtxt('experiment.2.silu-total.dat')
#data1=np.loadtxt('experiment.1.read-total.dat') # read by software
u1=data1[:,0]
v1=data1[:,1]
#selected=np.where(u1>100)
#u1=u1[selected]
#v1=v1[selected]
data2=np.loadtxt('experiment.3.silu-electron.dat') # read by software
u2=data2[:,0]
v2=data2[:,1]
#selected=np.where(u2>100)
#u2=u2[selected]
#v2=v2[selected]
data3=np.genfromtxt('experiment.4.silu-phonon.dat',invalid_raise=False) # read by software
u3=data3[:,0]
v3=data3[:,1]
#selected=np.where(u3>100)
#u3=u3[selected]
#v3=v3[selected]

# avoid below 100K and above 350K
#x=x[1:]
#y1=y1[1:]
#y2=y2[1:]
#y3=y3[1:]

# edit lables
#####################################################################
labeltype=0
if labeltype == 0:
	title='' #'thermal conductivity'
	xlabel='Temperature'
	xlabelunit=' [K]'
	ylabel=r'Thermal Conductivity' #' $\kappa$'
	ylabelunit=' [W/mK]'
elif labeltype == 1:
	title='' #'thermal conductivity'
	xlabel='Temperature'
	xlabelunit=' [K]'
	y = y/100
	ylabel=r'Thermal Conductivity' #' $\kappa$'
	ylabelunit=' [W/cmK]'
#####################################################################


colors=['goldenrod','tab:red','tab:blue','tab:purple']
var_names=['PBE+U ferro', 'PBE+U', 'PBE+U+SOC', 'experiment']
shapes=['o','*','s']
markersize=[6,10,6]
xyz=['$\kappa_{\mathrm{ph}}^a$','$\kappa_{\mathrm{ph}}^b$','$\kappa_{\mathrm{ph}}^c$'] # PBE+U+SOC labels


# line plot component by component
#for i,comp in enumerate(comps):
#    plt.plot(x, y2[:,i],color=colors[1],marker=shapes[i],markersize=markersize[i], linewidth=3, label=var_names[1]) 
#	#plt.scatter(x, y2[:,i],color=colors[1], marker=shapes[i], s=markersize[i]) 

for i,comp in enumerate(comps):
	plt.plot(x, y3[:,i],color=colors[2],marker=shapes[i],markersize=markersize[i], linewidth=3, label=xyz[i]) 
	#plt.scatter(x, y3[:,i],color=colors[2], marker=shapes[i], label=labels[tensor_ind.index(comp)], s=markersize[i] ) 

# plot experimental data
plt.plot(u1,v1,color=colors[-1], label='$\kappa^{ab}$', linewidth=3, linestyle=exp_style)
plt.plot(u3,v3,color='tab:red', label='$\kappa_{\mathrm{ph}}^{ab}$', linewidth=3, linestyle=exp_style)
plt.plot(u2,v2,color='goldenrod', label='$\kappa_{\mathrm{el}}^{ab}$', linewidth=3, linestyle=exp_style)



plt.legend(loc='lower right',ncol=3)
#plt.legend(borderpad=2)
ax=plt.gca()
#leg = ax.get_legend()
#handles, labels = ax.get_legend_handles_labels()
#ax.legend(flip(handles, 3), flip(labels, 3), loc='lower center', ncol=3, columnspacing=0.5)

handles, labels = ax.get_legend_handles_labels()
handles = [plt.plot([],marker="", ls="")[0]]*2 + list(flip(handles,3))
labels=['Calc:','Expr:'] + list(flip(labels,3))
leg=plt.legend(handles, labels, loc='lower center', ncol=4, columnspacing=0.5, framealpha=0.)
#leg=plt.legend(handles, labels, loc='lower center', ncol=4, columnspacing=0.5 )

for vpack in leg._legend_handle_box.get_children()[:1]:
    for hpack in vpack.get_children():
        hpack.get_children()[0].set_width(0)


# settings
plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
# limits
labeltype=0
if labeltype == 0:
    #plt.xscale('log')
    plt.yscale('log')
    plt.xlim([0,410])
    plt.ylim([1e-2,2e1])
    ax.tick_params(which='minor', top=True,bottom=True,right=True,left=True)
    #ax.yaxis.set_minor_locator(tck.LogLocator(numticks=999,subs=(.1,.2,.3,.4,.5,.6,.7,.8,.9)))
    ax.yaxis.set_minor_locator(tck.LogLocator(numticks=999,subs='auto'))
else:
    #plt.xscale('log')
    #plt.yscale('log')
    #plt.xlim([0,300])
    plt.ylim([0,8])

ax.tick_params(top=True,bottom=True,right=True,left=True)
ax.xaxis.set_minor_locator(tck.AutoMinorLocator())
#plt.grid(axis='y',linestyle='--')
plt.grid(linestyle='dashed')
#save
plt.tight_layout()
figname= 'graph2_soc_%s.v2.pdf' % (filename[4:])
print('remotesingle %s' % (figname))
plt.savefig(figname,dpi=600,transparent=True)
plt.show()
plt.close()

#########################################################################
