import numpy as np
import sys
import matplotlib.pyplot as plt


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
colors = ['tomato','royalblue','tab:green','gold']
colors = colors[:len(folders)]
colors = colors[::-1]


# draw phonon dispersion
for i,f_i in enumerate(folders):
    data=np.load(f_i + '/' + filename,allow_pickle=True) # this one works for data with several columns.
    info1=np.loadtxt(f_i + '/' + fileinfo1, comments='#',dtype=str, delimiter='=') # this one works for data with several columns.
    info1=dict(info1)
    x=data['x']
    y=data['y']
    plt.plot(x,y[:,:-1],color=colors[i])
    plt.plot(x,y[:,-1],color=colors[i],label=info1['legend'])


# ticks should be the same
print('ticks are from %s' % f_i )
ticks=data['ticks']
tick_position=ticks.item()['distance']
tick_label=ticks.item()['label']


# draw the separations q-points

for tick in tick_position:
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
	#plt.legend()
	plt.legend(loc='upper right')
	title='Band Structure'
	xlabel=''
	xlabelunit=' '
	ylabel='Energy'
	ylabelunit=' (eV)'
#####################################################################


#leg = ax.get_legend()
#for i in range(len(folders)):
#	leg.legendHandles[i].set_linestyle('--')
plt.xlim([min(x),max(x)])
plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
plt.tight_layout()
plt.ylim([-1, 1])


figname= 'bs_combine.pdf'
print('\tfigure saved as \n%s' % (figname))
#plt.savefig(figname,dpi=600)
plt.savefig(figname)

plt.show()
plt.close()
