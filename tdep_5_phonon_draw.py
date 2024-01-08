#!/usr/bin/env python
import numpy as np
import sys
import matplotlib.pyplot as plt


SMALL_SIZE = 16
MEDIUM_SIZE = 18
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title
fig = plt.figure()
ax = fig.add_subplot(111)


# load data
if len(sys.argv) < 2:
	print('Error! Enter the folders')
	sys.exit()
print('Will plot the two columns in .dat or .txt file.')

folders = sys.argv[1:]
filename = 'outfile.dispersion_relations'
fileinfo = 'SAVEINFO'
fileinfo2 = 'disprel_xtck'
#colors = ['tomato','royalblue','tab:green','goldenrod']
colors = ['tab:blue','tab:red','tab:green','goldenrod']
colors = colors[:len(folders)]
colors = colors[::-1]

# get the number of points in q-grid
print('folder for plotting info: %s' % folders[0] )
dict1=np.loadtxt(folders[0] + '/' + fileinfo, comments='#',dtype=str, delimiter='=') # this one works for data with several columns.
title=dict(dict1)['title']
dict2=np.loadtxt(folders[0] + '/' + fileinfo2, comments='#',dtype=str ) # this one works for data with several columns.
#dict2=np.transpose(dict2)
#dict2 = [ [dict2[i,0].strip(),dict2[i,1].strip()] for i in range(len(dict2)) ]
tick_label=dict2[0, :]
tick_position = dict2[1, :].astype(float)

###############################################################################
# To match the k-path proportionally, use the same x (k-path) for each plot
data=np.loadtxt(folders[0] + '/' + filename, comments='#')
x=data[:,0]
###############################################################################

# draw phonon dispersion
for i,f_i in enumerate(folders):
    data=np.loadtxt(f_i + '/' + filename, comments='#') # this one works for data with several columns.
    num=len(data[0]) # number of points for each segment
    dict1=np.loadtxt(f_i + '/' + fileinfo, comments='#',dtype=str, delimiter='=') # this one works for data with several columns.
    dict1=dict(dict1)
    y=data[:,1:]
    begin=0
    end=num
    color=colors[i]
    if 'color' in dict1.keys():
        color=dict1['color']
    #plt.plot(x,y,color=color,label=dict1['legend']) # avoid so many labels
    plt.plot(x,y,color=color)
    ## use plt.plot with a loop, to avoid plotting a straight line from end to beginning
    ## but for this data format, loop is not needed
    #plt.plot(x, y, 'o',markersize=0.2, color=colors[i], label=dict1['legend'])

# draw the separations q-points
#tick_position=[0]
#for i in range(num-1,len(x)-1,num):
#    tick_position.append(x[i])
#tick_position = np.unique(tick_position)

for tick in tick_position:
    plt.axvline(x=tick,c ='grey',ls='--')
plt.xticks(tick_position,tick_label)
## Note the last label might not appear,
## tick_position of last label is rounded, so it might be larger than x[-1] right limit
## Can think of a way to avoid this problem later. Could redefine the right limit

#####################################################################
labeltype=0 # no labels
#####################################################################

# edit lables
#####################################################################
if labeltype == 0:
	#plt.legend(loc='upper right',framealpha=1.0)
	plt.legend(loc='upper right')
	title=title + ' Phonon Dispersion'
	xlabel=''
	xlabelunit=' '
	ylabel='Frequency'
	ylabelunit=' [THz]'
	#ylabelunit=' [meV]'
#####################################################################


#leg = ax.get_legend()
#for i in range(len(folders)):
#	leg.legendHandles[i].set_linestyle('--')
plt.xlim([min(x),max(x)])
#plt.ylim([-0.,6.2])
#plt.ylim([-4,7])
#plt.ylim([-1,6])
plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
plt.tight_layout()


figname= filename + '.pdf'
print('\tfigure saved as \n%s' % (figname))
#plt.savefig(figname,dpi=600)
plt.savefig(figname, transparent=True)

plt.show()
plt.close()
