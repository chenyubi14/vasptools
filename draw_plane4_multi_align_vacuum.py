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
parser.add_argument('filename1', metavar='f1',type=str,dest='filename1')
parser.add_argument('filename2', metavar='f2',type=str,dest='filename2')
parser.add_argument('--percent1', metavar='p1',type=float,dest='percent1',default=0.4)
parser.add_argument('--percent2', metavar='p2',type=float,dest='percent2',default=0.6)
args = parser.parse_args()

#filename= 'PLANAR_AVERAGE.dat' 
filename1 = args.filename1
filename2 = args.filename2
percent1 = args.percent1
percent2 = args.percent2

## load data
data1=np.loadtxt(filename1) # this one works
x1=data1[:,0]
y1=data1[:,1]

data2=np.loadtxt(filename2) # this one works
x2=data2[:,0]
y2=data2[:,1]

#####################################################################
labeltype=1 
if labeltype == 1:
	# set up labels for plotting 
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
		x1=x1*1.88973 # 1A = 1.88973 Bohr
		x2=x2*1.88973 # 1A = 1.88973 Bohr
		xlabelunit=' (bohr)'
#####################################################################



### vacuum level
arg=np.argmax(y)
ymin = y.min()
ymax = y[arg]
plt.vlines(x[arg],ymin,ymax,color='red')
print('Vacuum energy: Maximum=%s eV, farthest=%s eV' % (y[arg], y[0]) )
### electro-static average in bulk
# Use 40% - 60% of xlimit to calculate average electro-static energy
length = len(x)
len1 = int(percent1*length)
len2 = int(percent2*length)
selected = y[len1:len2]
selected_ymin = selected.min()
selected_ymax = selected.max()
average = (selected_ymin + selected_ymax) / 2
ax = plt.gca()
ax.fill_betweenx(np.linspace(selected_ymin,selected_ymax,50), x[len1], x[len2], color='green', alpha=0.3 )
#plt.vlines(x[len1],selected_ymin,selected_ymax,color='purple')
#plt.vlines(x[len2],selected_ymin,selected_ymax,color='purple')
print('The average electro-static potential is %s eV' % (average))
plt.hlines(average, x[0],x[-1],color='purple')
align = ymax - average
print('Band alignment is %s eV' % align)
title = 'Vacuum=%.2f, Bulk=%.2f, align=%.2f' % (ymax, average, align)




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

