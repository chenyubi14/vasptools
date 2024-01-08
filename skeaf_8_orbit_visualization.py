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
#plt.figure(figsize=(4,6))

print('2pi is NOT in k definition')

string='results_orbitoutlines_invAng.out'
# load data
filename= string
if len(sys.argv) < 2:
	print('You may also Enter the dat filename as one argument. By default will read file %s' % string )
else:
	filename= sys.argv[1]


#data=np.loadtxt(filename) # this one works
assert filename[-len(string):]==string, 'Wrong filename type'
#data=np.loadtxt(filename,skiprows=1,delimiter=',')
data=np.genfromtxt(filename, dtype=str, skip_header=5, invalid_raise=False)
freq=np.genfromtxt(filename, dtype=str, skip_header=1, invalid_raise=False)
if len(freq.shape) < 2: # only one extremal orbit. freq will be an one-dimensional array
        freq = freq[np.newaxis,:] # make it a two-dimensional array, like the multi orbital case
sep = np.where(data == ['kx', 'ky', 'kz']) # (array([69, 69, 69]), array([0, 1, 2]))
sep = np.unique(sep[0]) # separation is given by 'kx ky kz' in the txt file

left=0
# right=0 # no need for this initialization
fig = plt.figure()
ax = plt.axes(projection='3d')
sep = np.append(sep,len(data)+1 )
for i, sep_i in enumerate(sep): # initial and final is not well considered yet
	right = sep_i - 1 
	orb_i = data[left:right] # sep[sep[i]+1, sep[i+1]-1]
	orb_i = orb_i.astype(float)
	orb_i = orb_i / 2 / np.pi # the BZ vectors don't have 2pi
	left = sep_i + 1
	# the current order of defining left and right will avoid the judgement of i+1 at the end
	ax.plot3D(orb_i[:,0], orb_i[:,1], orb_i[:,2],label= "%.2f T" % ( float(freq[i,-1])*1000 ) )
	#ax.scatter3D(orb_i[:,0], orb_i[:,1], orb_i[:,2],label= "%s T" % ( float(freq[i,-1])*1000 ) )

#plt.show()
from pathlib import Path
path = Path(filename)
dir_w = path.parent.absolute()
with open(str(dir_w/'config.in'), 'r') as f:
        lines=f.readlines()
        theta1,theta2,phi1,phi2, numrots=lines[10:15] # read as string
        phi1 = float(phi1) # convert 
        theta1 = float(theta1) 
        phi2 = float(phi2)
        theta2=float(theta2)
        numrots=int(numrots)

##########################################
labeltype = 1
if labeltype == 1 :
        legend='theory'
        title=r'B // polar:%.2f azimuthal:%.2f' % (phi1, theta1)
        xlabel='kx'
        xlabelunit=''
        ylabel='ky'
        ylabelunit=''
        zlabel='kz'
        zlabelunit=''
        #color='tab:red'
        color='tab:blue'
        #plt.ylim([0,1.5])
        #plt.xlim([min(x),max(x)])

ax.set_title(title) # add title
ax.set_xlabel(xlabel+xlabelunit) # x label 
ax.set_ylabel(ylabel+ylabelunit) # y label
ax.set_zlabel(zlabel+zlabelunit) # z label


plt.legend()
##########################################

figname= filename[:-3] + 'pdf'
print('\tfigure saved as \n%s' % (figname))
plt.savefig(figname,dpi=600)

#plt.show()
plt.close()
