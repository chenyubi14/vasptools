#!/usr/bin/env python
import os
import numpy as np
from pathlib import Path

# This file is because experiment does not know which rotation angle they have, so I need to generate rotation for any angle

# The must have angle, the (112) surface
#polar angle: phi1
# azimuthal angle: theta1

#############################################
change_rot=False
# change the number of rotations, not use the default value
#############################################

with open('config.in', 'r') as f:
        lines=f.readlines()
        theta1,theta2,phi1,phi2, numrots=lines[10:15] # read as string
        phi1 = float(phi1) # convert
        theta1 = float(theta1)
        phi2 = float(phi2)
        theta2=float(theta2)
        numrots=int(numrots)

if change_rot:
    numrots=19

# the angles need to be a float number for the correct input of fortran
phi1=float(phi1)
theta1=float(theta1)


def polar2cart(r:float, polar_angle:float, azimuthal_angle:float):
	polar = polar_angle / 180 * np.pi
	azimuthal = azimuthal_angle / 180 * np.pi
	return np.array([
		r * np.sin(polar) * np.cos(azimuthal),
		r * np.sin(polar) * np.sin(azimuthal),
		r * np.cos(polar) ])

def cart2polar(x:float, y:float, z:float):
	xyz = np.array([x,y,z])
	ptsnew = np.zeros(xyz.shape)
	xy = xyz[0]**2 + xyz[1]**2
	r = xyz[0]**2 + xyz[1]**2 + xyz[2]**2
	ptsnew[0] = np.sqrt(xy + xyz[2]**2)
	ptsnew[1] = np.arccos(xyz[2]/r)
	ptsnew[2] = np.arctan2(xyz[1], xyz[0])
	return ptsnew

initialP=polar2cart(1,phi1,theta1)
finalP=polar2cart(1,phi2,theta2)
t=np.linspace(0,1,numrots)
angle=np.arccos( np.dot(initialP, finalP)/np.linalg.norm(initialP)/np.linalg.norm(finalP)  )
points=np.sin(angle*(1-t))[:,np.newaxis] / np.sin(angle)*initialP[np.newaxis,:] + np.sin(angle*t)[:,np.newaxis] / np.sin(angle)*finalP[np.newaxis,:]

#############################
plot_points=points
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
# change 3Dplot view angle
#ax.view_init(0, theta1/2)
# plot x,y,z basis vectors
xyz_ratio=1.5
ratio_x=1.9
ax.plot([0,ratio_x*xyz_ratio],[0,0],[0,0],color='r')
ax.text(2.2*xyz_ratio,0,0,'x')
ax.plot([0,0],[0,xyz_ratio],[0,0],color='g')
ax.text(0,xyz_ratio,0,'y')
ax.plot([0,0],[0,0],[0,xyz_ratio],color='b')
ax.text(0,0,xyz_ratio,'z')
#ax.text(0,0,1,'')
# plot B rotating direction: vectors
colors=['grey'] * numrots #*(numrots-1)+['black']
for i in range(numrots):
        axis=plot_points[i] #polar2cart(1, phi3, theta3 )
        X=[0, axis[0]]
        Y=[0, axis[1]]
        Z=[0, axis[2]]
        ax.plot3D( X,Y,Z, colors[i])
# plot B rotating direction: circumference
#plot_points=plot_points.reshape((-1,3))
X,Y,Z=zip(*plot_points)
ax.plot3D( X,Y,Z ,'grey' )
## plot rotating direction by a vector
#chosen=np.argmin(Y+np.abs(X))
chosen=4 #np.argmin(Y+np.abs(X))
if chosen>=len(plot_points)-1:
	chosen=len(plot_points)-2
point1=plot_points[chosen]*1.2
point2=plot_points[chosen+1]*1.2 - point1
#ax.quiver(*point1, *point2,color='purple')
## plot surface direction
surface_normal=polar2cart(1,phi1,theta1)
ax.quiver(0,0,0, *surface_normal ,color='black')
ax.quiver(0,0,0, *(plot_points[-1]) ,color='black')
ax.text(*(plot_points[chosen]), 'B',color='black')
# plot a sphere with radius=1
z0 = 0
r = 1
u, v = np.mgrid[0:2*np.pi:10j, 0:np.pi:10j]
x = r* np.cos(u)*np.sin(v)
y = r* np.sin(u)*np.sin(v)
z = z0 + r* np.cos(v)
#ax.plot_wireframe(x, y, z, color="goldenrod",alpha=0.2)
ax.plot_wireframe(x, y, z, color="tab:blue",alpha=0.5)
## overall setting
ax.set_box_aspect([1,1,1])
ax.set_axis_off()
ax.view_init(30,15)
plt.savefig('geo_vectors.pdf',dpi=600,transparent=True)
print('B field vectors are saved as geo_vectors.pdf')
#############################


cwd=os.getcwd()
file_names=os.listdir(cwd)
for i in range(len(file_names)):
        if file_names[i][-4:] == 'bxsf':
                sel_file = file_names[i]
                break
for i in range(len(points)):
    #print(points[i])
    output=cart2polar(points[i,0],points[i,1],points[i,2])
    cos_angle = np.round( np.dot(initialP, points[i])/np.linalg.norm(initialP)/np.linalg.norm(points[i]),5 )
    angle=np.arccos( cos_angle  )
    theta3=output[2]/np.pi*180
    phi3=output[1]/np.pi*180
    print('geo_%s: r=%.2f,azimuthal=%.2f,polar=%.2f, rot_angle=%.2f' % (i, output[0], theta3, phi3, angle/np.pi*180) )
    directory = 'geo_%s' % i
    if os.path.exists(directory):
    	print('folder %s exists' % directory )
    else:
    	os.mkdir(directory)
    	os.chdir( str(Path(cwd) / directory )  )
    	os.system('ln -s ../%s' % (sel_file) )
    	os.system('sh $SCRIPT/skeaf_1_write_input.sh %.4f %.4f %.4f %.4f 1' % (phi3, theta3, phi3, theta3) )
    #print('phi1=%.4f theta1=%.4f phi2=%.4f theta2=%.4f' % (phi3, theta3, phi3, theta3) )
    os.chdir(cwd)
