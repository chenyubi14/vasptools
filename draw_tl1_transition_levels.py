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
plt.figure(figsize=(6,5),dpi=200)


if len(sys.argv)>1:
	filename=sys.argv[1] 
else:
	print('Error! input the file to draw as an argument')
	sys.exit()

# Define the function to read transition levels
def read_transition_level(filename):
	with open(filename, 'r') as f:
		lines = f.readlines() # lines is a LIST, different line contents are separated with '\n' at the end
	defecttypes=[] # list of defect types
	charges={} # dictionary, keyword is defecttypes
	fermilevels={} # dictionary, keyword is defecttypes
	for i in range(len(lines)):
		splitted1=lines[i].split('DEFECTTYPE=') # for one type of defect
		if len(splitted1)>1: ## if contains keyword DEFECTTYPE
			#keyword=splitted[0].split('#')[-1].split()[-1] # keyword in INCAR: the changes are '#MAGMOM'->'MAGMOM' 
			defecttypes.append(splitted1[1].split('#')[0].split('!')[0]) # values of defect type like $\mathrm{Be}_\mathrm{O}$
			splitted2=lines[i+1].split('TRANSITIONCHARGE=')[1] # n charges
			splitted3=lines[i+2].split('TRANSITIONFERMILEVEL=')[1] # n-1 transition levels + 2 end points; end points (VBM, CBM) are same for all defects
			charges[defecttypes[-1]]=np.array(splitted2.split()).astype(float)
			fermilevels[defecttypes[-1]]=np.array(splitted3.split()).astype(float)
	return defecttypes, charges, fermilevels

defecttypes, charges, fermilevels=read_transition_level(filename) # a dictionary 

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
# two ends split one piece, so the segments are [0.5, 1.5], [2.5, 3.5], ...
ax.set_xlim([0,2*num_defect])
xx = np.array([0.5, 1.7]) # the length for the first one. Will iterate by xx=xx+2
xticks = range(1,2*num_defect,2)
write_transitionlevels=False
level_fontsize=12 # transition level font size

for defect_one in defecttypes:
	print(defect_one)
	defect_charge = charges[defect_one]
	defect_fermilevel = fermilevels[defect_one]
	maxy = defect_fermilevel[-1]
	for i in range(1, len(defect_fermilevel)-1):
		yy = defect_fermilevel[i]
		yy = [yy, yy]
		ax.plot(xx, yy, color='tab:red')
		# write energy of transition levels above the level line
		#plt.text(xx[0],yy[0]+maxy/200,  '%s/%s %.2f'%(charge_format(defect_charge[i-1]), charge_format(defect_charge[i]),yy[0] ) )
		tolerance = 0.6
		if write_transitionlevels:
			if defect_fermilevel[i+1]-yy[0]> tolerance:
				plt.text(xx[0],yy[0]+maxy/200,  '%s/%s %.2f'%( charge_format(defect_charge[i-1]), charge_format(defect_charge[i]),yy[0] ), fontsize=level_fontsize )
			else:
				plt.text(xx[0],yy[0]-maxy/25,  '%s/%s %.2f'%(charge_format(defect_charge[i-1]), charge_format(defect_charge[i]),yy[0] ),fontsize=level_fontsize)
		else:
			if defect_fermilevel[i+1]-yy[0]> tolerance:
				plt.text(xx[0],yy[0]+maxy/200,  '%s/%s'%(charge_format(defect_charge[i-1]), charge_format(defect_charge[i]) ), fontsize=level_fontsize )
			else:
				plt.text(xx[0],yy[0]-maxy/27,  '%s/%s'%(charge_format(defect_charge[i-1]), charge_format(defect_charge[i]) ),fontsize=level_fontsize )
		# write the charge transitions below the level line
		#plt.text(xx[0],yy[0]-maxy/40,  )
	xx = xx+2

# plot VBM and CBM
# HERE!!! add words like value of bandgap 'VBM' and 'CBM'
bandgap=defect_fermilevel[-1]-defect_fermilevel[0]
ax.set_ylim([defect_fermilevel[0],defect_fermilevel[-1]])

plt.text(0.1, defect_fermilevel[0]+bandgap*0.01, 'VBM' )
plt.text(0.1, defect_fermilevel[-1]+bandgap*0.01, 'CBM')
plt.text(1.7*num_defect, defect_fermilevel[0]+bandgap*0.01, '%.2f' % defect_fermilevel[0])
plt.text(1.7*num_defect, defect_fermilevel[-1]+bandgap*0.01, '%.2f' % defect_fermilevel[-1])



xlabel='' 
xlabelunit=''
ylabel='Energy'
ylabelunit='[eV]'

comment=(filename.split('rich_')[-1]).split('_transitionlevel')[0]

##plt.legend()
#plt.title(title) # add title
#plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label


## set x tick labels to defect types
ax.set_xticks(xticks) # make sure the tick occupies the right position, with the right interval
ax.set_xticklabels(defecttypes, fontsize=BIGGER_SIZE) # changes the x tick labels


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
ax.tick_params(top=False,bottom=False,right=True,left=True) # whether the major ticks will appear


# save
figname= 'graph_%s_y%s_x%s.pdf' % (comment.replace(' ','_'), ylabel.replace(' ','_'), xlabel.replace(' ','_'))
print('\tremotesync \n%s' % (figname))
plt.tight_layout()
plt.savefig(figname, dpi=600)
plt.close()
#plt.show()