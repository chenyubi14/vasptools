import numpy as np
import sys
import matplotlib.pyplot as plt
import scipy.linalg as linalg
import math
import sympy
import os
from pathlib import Path


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


# load data
print('Will plot oscillation frequencies for the geodesic line')


cwd=os.getcwd()
file_names=os.listdir(cwd)
folder_list=[]
for i in range(len(file_names)):
        if file_names[i][:3] == 'geo' and os.path.isdir(file_names[i]):
                folder_list.append( file_names[i] )




#data=np.loadtxt(filename) # this one works
string='results_freqvsangle.out'

y = np.array([])
phi = np.array([])
theta = np.array([])
for i,fol in enumerate(folder_list):
	print(fol+'/'+string)
	data=np.loadtxt( fol+'/'+string, skiprows=1, delimiter=','  )
	y_i=data[:,2]
	theta_i=data[:, 0]
	phi_i=data[:, 1]
	y = np.append(y, y_i)
	theta = np.append(theta, theta_i)
	phi=np.append(phi, phi_i)
#data=np.loadtxt(filename,skiprows=1,delimiter=',')

#x=np.linspace(0,90,len(y))
with open('config.in', 'r') as f:
	lines=f.readlines()
	theta1,theta2,phi1,phi2=lines[10:14] # read as string
	phi1 = float(phi1) # convert
	theta1 = float(theta1)
	phi2 = float(phi2)
	theta2=float(theta2)



def polar2cart(r, polar_angle, azimuthal_angle):
	polar = polar_angle / 180 * np.pi
	azimuthal = azimuthal_angle / 180 * np.pi
	x = r * np.sin(polar) * np.cos(azimuthal)
	y = r * np.sin(polar) * np.sin(azimuthal)
	z = r * np.cos(polar)
	return np.stack([x,y,z]).T # return np.array([x,y,z]) # this actually works the same way


axis1 = polar2cart(1, phi1, theta1)
direction = polar2cart(1, phi , theta)
# get angles relative to initial angles: phi1, theta1
from numpy.linalg import norm
angle_x = np.arccos( direction @ axis1  / norm(axis1) / norm(direction,axis=1) )
angle_x = angle_x/np.pi * 180
print('angles', angle_x)




##########################################
labeltype = 1
if labeltype == 1 :
        legend='theory'
        title=r'polar:%.2f$\rightarrow$%.2f azimuthal:%.2f$\rightarrow$%.2f' % (phi1, phi2, theta1, theta2)
        xlabel='angle'
        xlabelunit=' (K)'
        ylabel='frequency'
        ylabelunit=' (T)'
        #color='tab:red'
        color='tab:blue'
        y=y*1000
        #plt.ylim([0,1.5])
        #plt.xlim([min(x),max(x)])

plt.scatter(angle_x, y, color=color, label=legend)
plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
plt.tight_layout()
##########################################



##########################################
plt.legend()
##########################################

figname=  'geo_results_freqvsangle.pdf'
print('\tfigure saved as \n%s' % (figname))
plt.savefig(figname,dpi=600)

plt.show()
plt.close()
