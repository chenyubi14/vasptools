# In[1]:
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import MultipleLocator
import os
import sys
sys.path.append(os.environ['SCRIPT'])

# this file plots the transition levels
# Go to a graphdata folder, run 'python ${SCRIPT}/draw_transition_levels.py *.txt' 
# the last argument is the file with transition level data stored in ${ZEROFOL}/berylliumO/graphdata
# format1: use straight lines to represent VB, CB
# format2: use blue regions to represent VB, CB

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
plt.figure(figsize=(5,4),dpi=200)



defecttypes=['$\mathit{V}_\mathrm{O}$','$\mathit{V}_\mathrm{O}$-$\mathrm{Li}_\mathrm{Be}$','$\mathit{V}_\mathrm{Be}$','$\mathit{V}_\mathrm{Be}$-$\mathrm{F}_\mathrm{O}$']
charges=np.array([[1,0],[0,-1],[1,0,-1],[1,0]])
huangrhys=np.array([[3.12,14.50],[3.0,19.2],[3.12,8.22,15.56],[8.44,14.33]])

def charge_format(charge):
	if charge == 0:
		return '0'
	elif charge == 1:
		return '$+$'
	elif charge == -1:
		return '$-$'
	elif charge > 0 :
		return '%d$+$' % charge
	else:
		return '%d$-$' % abs(charge)

ax=plt.subplot(111)
num_defect = len(defecttypes) # split x axis in 2*num_defect segments -> Use 1/(2*num_defect) as the length of each piece
xlimits=[0,2*num_defect]
# two ends split one piece, so the segments are [0.5, 1.7], [2.5, 3.7], ...
ax.set_xlim(xlimits)
xx = np.array([0.5, 1.7]) # the length for the first one. Will iterate by xx=xx+2
xticks = range(1,2*num_defect,2)
write_transitionlevels=True
level_fontsize=12 # transition level font size

maxy  = max([item for sublist in huangrhys for item in sublist]) #max(huangrhys) does not work

for j in range(len(defecttypes)):
	defect_one = defecttypes[j]
	print(defect_one)
	defect_charge = charges[j]
	defect_huangrhys = huangrhys[j]
	xx=np.linspace(xx[0],xx[1],len(defect_charge))
	yy= defect_huangrhys
	plt.scatter(xx,yy,color='black') #,linestyle='-'
	plt.plot(xx,yy,color='black') #,linestyle='-'
	for i in range(0, len(defect_charge)):
		if write_transitionlevels:
			plt.text(xx[i]+0.2,yy[i],  '%s'%( charge_format(defect_charge[i]) ), fontsize=level_fontsize )
	xx = xx+2




xlabel='' 
xlabelunit=''
ylabel='Huang-Rhys factor'
ylabelunit=''
plt.ylabel(ylabel+ylabelunit) # y label



## set x tick labels to defect types
ax.set_xticks(xticks) # make sure the tick occupies the right position, with the right interval
ax.set_xticklabels(defecttypes, fontsize=BIGGER_SIZE) # changes the x tick labels


# draw VB CB blue regions
#cbmregion = 2
#vbmregion = 2
#largestx=xlimits[1]
#x=np.linspace(0,largestx,50)
#ax.fill_between(x,-vbmregion, 0, color='skyblue') #defectenergyCBM,defectenergyCBM+cbmregion)
#ax.fill_between(x,bandgap,bandgap+cbmregion,color='skyblue') #'#1f77b4') # defectenergyVBM,defectenergyVBM-vbmregion)
#plt.text( largestx/15,-vbmregion+0.6,'VB' ) # text of valence band
#plt.text( largestx/15, bandgap+vbmregion-1.4,'CB' ) # text of conduction band

#ax.set_ylim([defect_fermilevel[0]-vbmregion,defect_fermilevel[-1]+cbmregion])


## spine: make y axis appear, x axis disappear
import matplotlib.ticker as tck
ax.spines['left'].set_linewidth(1.5) # set the length of spines
ax.spines['right'].set_linewidth(1.5)
ax.spines['top'].set_linewidth(1.5)
ax.spines['bottom'].set_linewidth(1.5)

## locator: x only need labels, no locator.
#ax.xaxis.set_major_locator(MultipleLocator(0.5)) # MultipleLocator the interval between y axis major ticks, like 0-0.5-1-1.5-...
#ax.yaxis.set_major_locator(MultipleLocator(5)) # MultipleLocator the interval between y axis major ticks, like 0-5-10
ax.yaxis.set_minor_locator(tck.AutoMinorLocator()) # have y minor ticks
#ax.xaxis.set_minor_locator(tck.AutoMinorLocator()) # don't want x ticks, don't uncomment this line
plt.tick_params(axis='y', which='minor', width=1., length=2, color='k', right=True)
plt.tick_params(axis='y', width=2,length=4)  #, labelsize=15
#plt.tick_params(axis='x', length=0) # length=0 makes the major tick disappear, works the same as next line, which='minor', width=1.5,, color='k', right=True
# remove x major locators, add y major locators on the right.
ax.tick_params(top=False,bottom=False,right=True,left=True) # whether the major ticks will appear
#plt.xticks(rotation=-35)
plt.xticks(rotation=0)

# save
figname= 'graph_y%s_x%s.pdf' % (ylabel.replace(' ','_'), xlabel.replace(' ','_'))
print('\tremotesync \n%s' % (figname))
plt.tight_layout()
plt.show()
plt.savefig(figname, dpi=600)
#plt.close()


