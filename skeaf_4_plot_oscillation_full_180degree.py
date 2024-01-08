import numpy as np
import sys
import matplotlib.pyplot as plt


SMALL_SIZE = 16
MEDIUM_SIZE = 18
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=MEDIUM_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=MEDIUM_SIZE)  # fontsize of the figure title
#plt.figure(figsize=(4,6))


# load data
if len(sys.argv) < 3:
	print('Error! Enter the dat filenames. rota and rotb')
	sys.exit()
filename= sys.argv[1]
filename2 = sys.argv[2]
figname = filename.split('/')[-2]


#data=np.loadtxt(filename) # this one works
string='results_freqvsangle.out'
assert filename[-len(string):]==string, 'Wrong filename type'
assert filename2[-len(string):]==string, 'Wrong filename type'
data=np.loadtxt(filename,skiprows=1,delimiter=',')
data2=np.loadtxt(filename2,skiprows=1,delimiter=',')

y=data[:,2]
theta=data[:, 0]
phi=data[:, 1]
y2=data2[:,2]
theta2=data2[:,0]
phi2=data2[:,1]

y=np.append(y,y2)
theta=np.append(theta,theta2)
phi = np.append(phi, phi2)


#x=np.linspace(0,90,len(y))
with open(figname+'/config.in', 'r') as f:
	lines=f.readlines()
	theta1,_,phi1=lines[10:13] # read as string
	phi1 = float(phi1) # convert
	theta1 = float(theta1)


def polar2cart(r, polar_angle, azimuthal_angle):
	polar = polar_angle / 180 * np.pi
	azimuthal = azimuthal_angle / 180 * np.pi
	x = r * np.sin(polar) * np.cos(azimuthal)
	y = r * np.sin(polar) * np.sin(azimuthal)
	z = r * np.cos(polar)
	return np.stack([x,y,z]).T # return np.array([x,y,z]) # this actually works the same way


axis1 = polar2cart(1, phi1, theta1)
direction = polar2cart(1, phi , theta)
## get angles relative to initial angles: phi1, theta1
#from numpy.linalg import norm
#angle_x = np.arccos( direction @ axis1  / norm(axis1) / norm(direction,axis=1) )
angle_x = np.arccos( direction @ axis1 ) # norm should be 1
angle_x = angle_x/np.pi * 180
print('angles', np.round(angle_x,3))




##########################################
labeltype = 1
if labeltype == 1 :
        legend='theory'
        if phi1 == 0.00 and theta1 == 0.00:
            title=r'polar:%.2f$\rightarrow$%.2f azimuthal:%.2f$\rightarrow$%.2f' % (phi1, phi2[0], theta2[0], theta2[0])
        else:
            title=r'polar:%.2f$\rightarrow$%.2f azimuthal:%.2f$\rightarrow$%.2f' % (phi1, phi2[0], theta1, theta2[0])
        xlabel='Angle'
        xlabelunit=' [degrees]'
        ylabel='Oscillation Frequency'
        ylabelunit=' [T]'
        #color='tab:red'
        color='tab:blue'
        y=y*1000
        #plt.ylim([0,1.5])
        #plt.xlim([min(x),max(x)])

plt.scatter(angle_x, y, color=color, label=legend)
plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
##########################################

print('  '.join(list(np.round(y,2).astype(str))))

##########################################
#plt.legend()
plt.vlines(90, y.min()-2,y.max()+2, linewidth=3, color='black')
#plt.ylim([1,16])
##########################################

figname='rot'+figname[4:]
#figname= string[:-3] + figname + '.pdf'
figname= string[:-3] + figname + '.png'
print('\tfigure saved as \n%s' % (figname))
plt.tight_layout()
#plt.savefig(figname,dpi=600,transparent=True)
plt.savefig(figname,dpi=600,transparent=False)

#plt.show()
plt.close()
