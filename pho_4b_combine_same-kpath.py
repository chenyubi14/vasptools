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
filename = 'band.plot.dat'
fileinfo = 'SAVEINFO'
fileinfo2 = 'band.conf'
colors = ['royalblue', 'tomato','limegreen','goldenrod']
#colors = ['tab:blue','tab:red','tab:green','goldenrod']
#colors = ['tab:blue','tab:orange','tab:green']
colors = colors[:len(folders)]
colors = colors[::-1]

# get the number of points in q-grid
print('folder for plotting info: %s' % folders[0] )
dict1=np.loadtxt(folders[0] + '/' + fileinfo, comments='#',dtype=str, delimiter='=') # this one works for data with several columns.
title=dict(dict1)['title']
dict2=np.loadtxt(folders[0] + '/' + fileinfo2, comments='#',dtype=str, delimiter='=') # this one works for data with several columns.
dict2 = [ [dict2[i,0].strip(),dict2[i,1].strip()] for i in range(len(dict2)) ]
dict2=dict(dict2)
num=int(dict2['BAND_POINTS']) # number of points for each segment
tick_label=dict2['BAND_LABELS']
tick_label=tick_label.split()

###############################################################################
# To match the k-path proportionally, use the same x (k-path) for each plot
data=np.loadtxt(folders[0] + '/' + filename, comments='#')
x=data[:,0]
###############################################################################

# draw phonon dispersion
for i,f_i in enumerate(folders):
    data=np.loadtxt(f_i + '/' + filename, comments='#') # this one works for data with several columns.
    dict1=np.loadtxt(f_i + '/' + fileinfo, comments='#',dtype=str, delimiter='=') # this one works for data with several columns.
    dict1=dict(dict1)
    #x=data[:,0]
    y=data[:,1]
    begin=0
    end=num
    color=colors[i]
    if 'color' in dict1.keys():
        color=dict1['color']
    plt.plot(x[begin:end],y[begin:end],color=color,label=dict1['legend'])
    print('color=',color)
    ## use plt.plot with a loop, to avoid plotting a straight line from end to beginning
    for j in range(1,int(len(x)/num)):
        begin=end
        end=begin+num
        plt.plot(x[begin:end],y[begin:end],color=color,linestyle='-',alpha=1)
    #plt.plot(x, y, 'o',markersize=0.2, color=colors[i], label=dict1['legend'])

# draw the separations q-points
tick_position=[0]
for i in range(num-1,len(x)-1,num):
    tick_position.append(x[i])
tick_position = np.unique(tick_position)

## Don't draw ticks on the left/right ends
for tick in np.sort(tick_position)[1:-1]:
    plt.axvline(x=tick,c ='grey',ls='--')
plt.xticks(tick_position,tick_label)


# edit lables
#####################################################################
labeltype=0 # no labels
if labeltype == 0:
    #plt.legend(loc='upper right',frameon=False, bbox_to_anchor=(0.5, 0.4))
    #plt.legend(loc='upper right',framealpha=1.0)
    #plt.legend(loc='lower right')
    plt.legend(framealpha=0.0,bbox_to_anchor=(0.11,0.3))
    #plt.legend(framealpha=0.0,bbox_to_anchor=(0.65,0.3))
    title=title + ' Phonon Dispersion'
    xlabel=''
    xlabelunit=' '
    ylabel='Frequency'
    ylabelunit=' [THz]'
    import matplotlib.ticker as tck
    #ax.yaxis.set_major_locator(tck.MultipleLocator(1))
    ax.yaxis.set_major_locator(tck.AutoLocator())
    ax.yaxis.set_minor_locator(tck.AutoMinorLocator())
    ax.tick_params(
            top=False,bottom=True,right=True,left=True,
            direction='in',width=1.2)
    ax.tick_params(which='minor',
            top=False,bottom=False,right=True,left=True,
            direction='in',width=1.0)
#####################################################################

ylimits = None
#leg = ax.get_legend()
#for i in range(len(folders)):
#	leg.legendHandles[i].set_linestyle('--')
handles, labels = ax.get_legend_handles_labels()
ax.legend(reversed(handles), reversed(labels))
#ylimits = [-0.5, 6.5]
#ylimits = [-4, 7]
#ylimits = [-7, 7]
ylimits = [-0.1, 1.6]
plt.xlim([min(x),max(x)])
if ylimits:
    plt.ylim(ylimits)
plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
plt.hlines(0,x.min(),x.max(),linestyle='dashed',color='black')
plt.tight_layout()


figname= filename[:-3] + 'pdf'
print('\tfigure saved as \n%s' % (figname))
#plt.savefig(figname,dpi=600)
plt.savefig(figname, transparent=True)

plt.show()
plt.close()
