import numpy as np
import sys
import matplotlib.pyplot as plt
import warnings

#with warnings.catch_warnings():
#    #warnings.filterwarnings('ignore', r'All-NaN (slice|axis) encountered')
#    warnings.filterwarnings('ignore',message='RuntimeWarning') 

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
fig = plt.figure()
ax = fig.add_subplot(111)


# load data
if len(sys.argv) < 2:
	print('Error! Enter the folders')
	sys.exit()
print('Will plot the BS data in .npz file.')

folders = sys.argv[1:]
filename = 'bs_numpy.npz'
fileinfo1 = 'SAVEINFO'
#colors = ['tomato','royalblue','tab:green','gold']
colors = [ 'tab:blue', 'tab:orange' ]
colors = colors[:len(folders)]
colors = colors[::-1]


ticks=[]
x=[]
y=[]
# read phonon dispersion
for i,f_i in enumerate(folders):
    data=np.load(f_i + '/' + filename,allow_pickle=True) # this one works for data with several columns.
    ticks.append(data['ticks'])
    x.append(data['x'])
    y.append(data['y'])


# ticks should be adjusted to have the same length
# draw phonon dispersion
x=x[-1]
# take x (k-path) of one specific file
# This is because, all segements are expanded proportionally
# I can directly change x to be the last one, thus each segment can be properly treated
divided=np.array(ticks[-1].item()['distance'])
divided=np.where(divided==0,np.inf,divided)
print('k-path fractional to the one of last folder')
for i in range(len(folders)):
    tick_position=np.array(ticks[i].item()['distance'])
    #print('tick %s %s' %(i,tick_position))
    print('%s %s' % (i,tick_position/divided) ) 
    info1=np.loadtxt(folders[i] + '/' + fileinfo1, comments='#',dtype=str, delimiter='=') # this one works for data with several columns.
    info1=dict(info1)
    plt.plot(x,y[i][:,:-1],color=colors[i])
    plt.plot(x,y[i][:,-1],color=colors[i],label=info1['legend'])

title=info1['title'] + ' Band Structure'
print('ticks are proportionally expanded/contracted for each segment, thus only use k-path for the last folder ' )
tick=ticks[i]
tick_position=tick.item()['distance']
tick_label=tick.item()['label']

# draw the separations q-points

for tick in np.sort(tick_position)[1:-1]:
    plt.axvline(x=tick,c ='grey',ls='--')
plt.axhline(y=0,c ='grey',ls='--')
plt.xticks(tick_position,tick_label)

#####################################################################
labeltype=0 # no labels
# save your often used labels
#####################################################################

# edit lables
#####################################################################
if labeltype == 0:
	plt.legend(loc='best',framealpha=0,bbox_to_anchor=(0.25,0.5))
	xlabel=''
	xlabelunit=' '
	ylabel='Energy'
	ylabelunit=' [eV]'
#####################################################################


#leg = ax.get_legend()
#for i in range(len(folders)):
#	leg.legendHandles[i].set_linestyle('--')
plt.xlim([min(x),max(x)])
plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
plt.ylim([-1, 1])

ax = plt.gca()
import matplotlib.ticker as tck
ax.yaxis.set_major_locator(tck.MultipleLocator(0.2))
ax.yaxis.set_minor_locator(tck.AutoMinorLocator())
ax.tick_params(top=False,bottom=True,right=True,left=True,direction='in',width=1.2)
ax.tick_params(which='minor', top=False,bottom=True,right=True,left=True,direction='in',width=1.0)

plt.tight_layout()
figname= 'bs_combine.pdf'
print('\tfigure saved as \n%s' % (figname))
plt.savefig(figname,dpi=600,transparent=True)
#plt.savefig(figname)

plt.show()
plt.close()
