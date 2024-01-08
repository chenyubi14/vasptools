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
if len(sys.argv) < 2:
	print('Error! Enter the dat filename as one argument')
	sys.exit()
print('Will plot the two columns in .dat or .txt file.')
filename= sys.argv[1]


#data=np.loadtxt(filename) # this one works
string='results_freqvsangle.out'
assert filename[-len(string):]==string, 'Wrong filename type'
data=np.loadtxt(filename,skiprows=1,delimiter=',')

y=data[:,2]
theta=data[:, 0]
phi=data[:, 1]
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


#x=x-55
#for i in range(len(x)):
#	if x[i] < 0:
#		x[i] = x[i]+180


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
#x2=[0,0,    15,15,15,15,       30,30,30,30,30,30,30,              45,45,45,45,45,        60,60,60,60,60,60,                75,75,75,75,75,75,         90,90,90,90,90,90,        105,105,105,105,    120,120,120,120,120,120,    135,135,135,135,135,135, 150,150,150,150,150,150,       165,165,165,165,165,165, 180,180,180,180,180]
#y2=[6.25,22, 23.25,21.75,10,6, 22.25,24.5,6.25,16.5,6.25,5,11.75,  22.5,24,12,5.75,4.75, 23.5,25.35,12.25,13.75,5.25,5.75, 23.75,24,14.75,13.25,5,5.5, 24.25,24,17,12,6.75,5.5, 22.5,23.75,6.5,5.5, 21.25,29.5,14.25,21,4.75,6, 21,23,14,18.75,6.25,5.5, 23,30.75,14.75,20.75,8.75,6.5, 20.75,28.75,14.75,10,7.25,7.25, 21.25,18.25,12.75,6.25,7.25]
#x2=[
#
#]
#y2=[
#
#]
#plt.scatter(x2,y2,color='tab:red',label='experiment')
plt.legend()
##########################################

figname= filename[:-3] + 'pdf'
print('\tfigure saved as \n%s' % (figname))
plt.savefig(figname,dpi=600)

plt.show()
plt.close()
