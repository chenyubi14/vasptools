import numpy as np
import sys
import matplotlib.pyplot as plt
import argparse


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
#plt.figure(figsize=(4,6))


parser = argparse.ArgumentParser(description='Plot planer average potential')
parser.add_argument('--filename', metavar='f',type=str,dest='filename',default='PLANAR_AVERAGE.dat')
parser.add_argument('--percent1', metavar='p1',type=float,dest='percent1',default=0.20)
parser.add_argument('--percent2', metavar='p2',type=float,dest='percent2',default=0.30)
parser.add_argument('--percent3', metavar='p3',type=float,dest='percent3',default=0.70)
parser.add_argument('--percent4', metavar='p4',type=float,dest='percent4',default=0.80)
args = parser.parse_args()

#filename= 'PLANAR_AVERAGE.dat' 
filename = args.filename
percent1 = args.percent1
percent2 = args.percent2
percent3 = args.percent3
percent4 = args.percent4

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

ax = plt.gca()
def find_ave(p1,p2):
    length = len(x)
    len1 = int(p1*length)
    len2 = int(p2*length)
    selected = y[len1:len2]
    selected_ymin = selected.min()
    selected_ymax = selected.max()
    average = (selected_ymin + selected_ymax) / 2
    ax.fill_betweenx(np.linspace(selected_ymin,selected_ymax,50), x[len1], x[len2], color='green', alpha=0.3 )
    print('The average electro-static is %s eV' % (average))
    plt.hlines(average, x[0],x[-1],color='purple')
    return average
ave1 = find_ave(percent1, percent2)
ave2 = find_ave(percent3, percent4)
align = ave1 - ave2
print('Band alignment is %s eV' % align)

# For first material in 0.0-0.5
# For second material in 0.5-1.0
title = 'Bulk1=%.2f, Bulk2=%.2f, align=%.2f' % (ave1, ave2, align)



# black, tab:blue, tab:green, tab:orange
#plt.plot(x, y, color=color, label=legend)
plt.scatter(x, y, color=color, label=legend)
plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
plt.tight_layout()


figname= filename[:-3] + 'png'
print('\tfigure saved as \n%s' % (figname))
plt.savefig(figname,dpi=600,transparent=True)

plt.show()
plt.close()

