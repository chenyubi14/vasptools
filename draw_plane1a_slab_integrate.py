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
parser.add_argument('--filename', metavar='f',type=str,dest='filename',default='PLANAR_AVERAGE.dat',
        help='The default filename to read planar average')
parser.add_argument('--index1',type=int,default=None, 
        help='Index of x on the left of boundary. Use index to manually determine range.')
parser.add_argument('--index2',type=int,default=None, 
        help='Index of x on the right of boundary')
parser.add_argument('--extremum', metavar='ext',type=str, choices=['max','min'], default='min',
        help='find max/min within the boundary. Use a series of setup to automatically determine range')
parser.add_argument('--range1', metavar='p1',type=float,default=0.4,
        help='percentage of left boundary')
parser.add_argument('--range2', metavar='p2',type=float,default=0.6,
        help='percentage of right boundary')
parser.add_argument('--distance', metavar='dist',type=float, default=1, 
        help='The minimum distance between extremums, unit is same as x (Angstrom)')
parser.add_argument('--precision', metavar='pre',type=float, default=0.001,
        help='Within what precision is considered as maximum or minimum. Default is 0.01 or 1%')
args = parser.parse_args()

filename = args.filename
index1 = args.index1
index2 = args.index2
extremum = args.extremum # 'max' or 'min'
range1 = args.range1
range2 = args.range2
distance = args.distance
precision = args.precision



## load data
data=np.loadtxt(filename) # this one works
x=data[:,0]
y=data[:,1]
length = len(x)

if (index1 == None) or (index2 == None):
    print('Will automatically determine the range')
    ## only consider extremums within [range1, range2]
    range1 = int( length * range1 )
    range2 = int( length * range2 )
    xrange = x[range1:range2]
    yrange = y[range1:range2]
    xargmax = np.argmax(yrange)
    ymax = yrange[xargmax] #np.max(yrange)
    xargmin = np.argmin(yrange)
    ymin = yrange[xargmin] #np.min(yrange)
    ydiff = ymax - ymin
    precision = abs(ydiff * precision)
    if extremum == 'max':
        allowed = ymax + np.array([-precision,0]) 
    else:
        allowed = ymin + np.array([0,precision]) 
    selected_ind = np.where(np.logical_and(yrange>=allowed[0], yrange<=allowed[1]))[0]
    selected_x = xrange[selected_ind]
    selected_y = yrange[selected_ind]
    ## differences between selected x points
    differences = np.diff(selected_x)
    if np.any(differences < distance):
        print('precision=%s too large. Find multiple extremum within the distance range. x=%s' % (args.precision, selected_x) )
        sys.exit()
    elif len(selected_x) < 2:
        print('precision=%s too low. Need at least two selected points to determine the region' % (args.precision) )
        sys.exit()
    else:
        print('selected points are x=%s' % (selected_x) )
        print('selected points are y=%s' % (selected_y) )
        len1 = selected_ind[0]  + range1
        len2 = selected_ind[-1] + range1 + 1
else:
    ## if arguments provide both indices, directly use these
    len1 = index1
    len2 = index2 + 1


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
ymin = y.min()
ymax = y[arg]
plt.vlines(x[arg],ymin,ymax,color='red')
plt.vlines(x[0],ymin,y[0],color='red')
print('Vacuum energy: Maximum=%s eV, farthest=%s eV' % (y[arg], y[0]) )

### electro-static average in bulk
selected_x = x[len1:len2]
left=selected_x[0]
right=selected_x[-1]
selected_y = y[len1:len2]
bottom=selected_y[0]
top=selected_y[-1]
print('Selected range boundary: x1=%.5f,x2=%.5f y1=%.5f,y2=%.5f' % (left,right,bottom,top) )
selected_ymin = selected_y.min()
selected_ymax = selected_y.max()
## 1. direct sum the parameters within this range
interval = np.diff(selected_x)
#print('Default interval is', interval)
interval = interval[0]
## area = sum f(x) * dx
integrate1_direct = np.sum(selected_y) * interval 
## average = area / len(x)
integrate1_direct /= right - left
## 2. interpolate and sum
dense_x = np.linspace( left, right, num=1001 )
interval = np.diff(dense_x)[0]
interp_y = np.interp(dense_x, selected_x, selected_y ) 
integrate2_interp = np.sum(interp_y) * interval
integrate2_interp /= right - left
print('The integrated electro-static potential is %.5f(direct sum), %.5f(interpolated sum)' % (integrate1_direct, integrate2_interp))
ax = plt.gca()
ax.fill_betweenx(np.linspace(selected_ymin,selected_ymax,50), left, right, color='green', alpha=0.3 )
plt.hlines(integrate2_interp, x[0],x[-1],color='purple')
align = y[0] - integrate2_interp
print('Band alignment is %s eV' % align)
title = 'Vacuum=%.2f, Bulk=%.2f, align=%.2f' % (y[0], integrate2_interp, align)




plt.plot(dense_x, interp_y, color='yellow', label='interpolated')
plt.scatter(x, y, color=color, label=legend)
plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
plt.tight_layout()


figname= filename[:-3] + 'slab.integrate.pdf'
print('\tfigure saved as \n%s' % (figname))
plt.savefig(figname,dpi=600,transparent=True)

plt.show()
plt.close()

