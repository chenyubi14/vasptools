import numpy as np
import sys
import matplotlib.pyplot as plt
import argparse


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


parser = argparse.ArgumentParser(description='Plot planer average potential')
parser.add_argument('--filename', metavar='f',type=str,dest='filename',default='PLANAR_AVERAGE.dat')
parser.add_argument('--percent1', metavar='p1',type=float,dest='percent1',default=0.)
parser.add_argument('--percent2', metavar='p2',type=float,dest='percent2',default=1.)
args = parser.parse_args()

#filename= 'PLANAR_AVERAGE.dat' 
filename = args.filename
percent1 = args.percent1
percent2 = args.percent2

## load data
data=np.loadtxt(filename) # this one works
x=data[:,0]
y=data[:,1]


#####################################################################
labeltype=1 # for 'PLANAR_AVERAGE.1e.a.dat'
if labeltype == 1:
	# set up labels for plotting plane potential 'PLANAR_AVERAGE.1e.a.dat'
	legend=''
	#plt.legend()
	title='Planar average potential'
	xlabel='Distance'
	xlabelunit=' ($\AA$)'
	ylabel='Potential'
	ylabelunit=' (eV $\AA$/e)'
	color='tab:blue'
	unittype=1 # use the default angstrom unit
	#unittype=2 # use the bohr unit
	if unittype==2:
		x=x*1.88973 # 1A = 1.88973 Bohr
		xlabelunit=' (bohr)'
#####################################################################



### vacuum level
arg=np.argmax(y)
### electro-static average in bulk
length = len(x)
len1 = int(percent1*length)
len2 = int(percent2*length)
## here len2 can == length
selected = y[len1:len2]

### use integration to find average
## Don't use the direct average method
#average = (selected_ymin + selected_ymax) / 2
integrated = np.sum(selected)
print('Integration by summing all=%s' % integrated )

selected_ymin = selected.min()
selected_ymax = selected.max()


ax = plt.gca()
## here len2 must be < length
if len2 >=length :
    ## to avoid the x[len2] outside range
    len2 = length - 1
ax.fill_betweenx(np.linspace(selected_ymin,selected_ymax,50), x[len1], x[len2], color='green', alpha=0.3 )
#plt.vlines(x[len1],selected_ymin,selected_ymax,color='purple')
#plt.vlines(x[len2],selected_ymin,selected_ymax,color='purple')
print('The average electro-static potential is %s eV' % (integrated))
plt.hlines(integrated, x[0],x[-1],color='purple')



# black, tab:blue, tab:green, tab:orange
#plt.plot(x, y, color=color, label=legend)
plt.scatter(x, y, color=color, label=legend)
plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
plt.tight_layout()


figname= filename[:-3] + 'unit.integrate.pdf'
print('\tfigure saved as \n%s' % (figname))
plt.savefig(figname,dpi=600,transparent=True)

plt.show()
plt.close()

