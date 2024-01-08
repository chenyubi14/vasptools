#!/usr/bin/env python
import numpy as np
import scipy.linalg as linalg
import math
import sympy
import os
import sys
from pathlib import Path

# This file is because experiment does not know which rotation angle they have, so I need to generate rotation for any angle

# The must have angle, the (112) surface
#polar angle phi, azimuthal angle theta
######################################################
phi1=55.;theta1=45. # for cd3as2
#phi1=0.;theta1=0. # for bamnsb2
num_angle=24+1 # divide of 360 degrees, 360/12=30, 360/24=15 for Cd3As2
######################################################
print('Should initialize angles, phi=%s, theta=%s' % (phi1, theta1) )


# the angles need to be a float number for the correct input of fortran
phi1=float(phi1)
theta1=float(theta1)


def polar2cart(r, polar_angle, azimuthal_angle):
	polar = polar_angle / 180 * np.pi
	azimuthal = azimuthal_angle / 180 * np.pi
	return np.array([
		r * math.sin(polar) * math.cos(azimuthal),
		r * math.sin(polar) * math.sin(azimuthal),
		r * math.cos(polar) ])

def cart2polar(xyz):
	xyz = np.array(xyz)
	#ptsnew = np.hstack((xyz, np.zeros(xyz.shape)))
	ptsnew = np.zeros(xyz.shape)
	xy = xyz[0]**2 + xyz[1]**2
	r = xyz[0]**2 + xyz[1]**2 + xyz[2]**2
	ptsnew[0] = np.sqrt(xy + xyz[2]**2)
	# ptsnew[1] is the polar angle
	#ptsnew[1] = np.arctan2(np.sqrt(xy), xyz[2]) # for elevation angle defined from Z-axis down
	ptsnew[1] = sympy.acos(xyz[2]/r)
	# ptsnew[2] is the azimuthal angle
	ptsnew[2] = np.arctan2(xyz[1], xyz[0])
	return ptsnew

# the rotation axis is the
axis = polar2cart(1,90, (theta1-90) ) # polar_angle=90, in the horizontal plane
radian = -phi1/180*np.pi
# rotate the system about axis, with polar=90, azimuthal = theta1-90
# it will bring the z axis to the initial direction (phi1, theta1)
rot_matrix = linalg.expm( np.cross( np.eye(3), axis/linalg.norm(axis)*radian ) )
# the rotation matrix converts z (0,0,1) to initial direction polar2cart(1, phi1, theta1)

vector_i = polar2cart(1,phi1,theta1) 
print('The rotation matrix converts z axis to %s, same by rotation matrix %s ' % (vector_i, rot_matrix @ np.array([0,0,1]) ) )

##################################################
## test whether the plane on a sphere will change angles uniformly
#num_angle=36*2+1
#phi_plane=np.linspace(0,90,num_angle) # polar angle is rotating
#theta_plane=np.linspace(0,0,num_angle) # azimuthal angle
#angles = np.zeros( (num_angle,2) )
#print('In horizontal plane, expect orthogonality inner productor = 0')
#for i in range(num_angle):
#	vector = polar2cart(1,phi_plane[i],theta_plane[i])
#	rot_vector = rot_matrix @ vector
#	angle = cart2polar(rot_vector)
#	phi3 = angle[1] / np.pi * 180 # polar angle
#	theta3 = angle[2] / np.pi * 180 # azimuthal angle
#	angles[i,0] = phi3
#	angles[i,1] = theta3
#	print('polar=%s, rot_vector=%s, rotated polar=%s,azimuthal=%s' % ( phi_plane[i], rot_vector, phi3, theta3) )
#	print('diff delta_phi=%s delta_theta=%s' % ( angles[i,0] - angles[i-1,0], angles[i,1]-angles[i-1,1] ) )
################################################## 



# want to find the rotation method from initial (phi1,theta1) to the horizontal plane
phi_plane=np.linspace(90,90,num_angle) # polar angle in the horizontal plane, should be 90
theta_plane=np.linspace(0,360,num_angle) # azimuthal angle in any direction
#theta2=np.linspace(0,180,num_angle) # azimuthal angle in any direction

angles = np.zeros( (num_angle,2) )
print('In horizontal plane, expect orthogonality inner productor = 0')
for i in range(num_angle):
	vector = polar2cart(1,phi_plane[i],theta_plane[i])
	rot_vector = rot_matrix @ vector
	angle = cart2polar(rot_vector)
	phi3 = angle[1] / np.pi * 180 # polar angle
	theta3 = angle[2] / np.pi * 180 # azimuthal angle
	angles[i,0] = phi3
	angles[i,1] = theta3
	print('azimuthal=%s, rot_vector=%s, rotated polar=%s,azimuthal=%s' % ( theta_plane[i], rot_vector, phi3, theta3) )
	inner_prod = vector_i @ polar2cart(1, phi3, theta3)
	#print( 'orthogonality: inner prod = %s  ' % ( inner_prod ) )
	assert abs(inner_prod) < 1e-8

##########################################################
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
# change 3Dplot view angle
#ax.view_init(0, theta1/2)
# plot x,y,z basis vectors
ratio=1.2
ax.plot([0,ratio],[0,0],[0,0],color='r')
ax.text(ratio,0,0,'x')
ax.plot([0,0],[0,ratio],[0,0],color='g')
ax.text(0,ratio,0,'y')
ax.plot([0,0],[0,0],[0,ratio],color='b')
ax.text(0,0,ratio,'z')
# plot surface direction
surface_normal=polar2cart(1,phi1,theta1)
ax.quiver(0,0,0, *surface_normal ,color='black')
ax.text(*surface_normal, 'B',color='black')
# plot B rotating direction: vectors
points=np.array([])
colors=['grey']*(num_angle-1)+['black']
# change the color of a specific direction
colors[9]='r'
for i in range(num_angle):
	phi3 = angles[i,0]
	theta3 = angles[i,1]
	axis=polar2cart(1, phi3, theta3 )
	points=np.append(points,axis) # r, polar, azimuthal
	X=[0, axis[0]]
	Y=[0, axis[1]]
	Z=[0, axis[2]]
	#ax.quiver(0, 0, 0, *points[-1])
	#ax.plot3D( X,Y,Z, 'tab:blue')
	ax.plot3D( X,Y,Z, colors[i])
# plot B rotating direction: circumference
points=points.reshape((-1,3))
X,Y,Z=zip(*points)
ax.plot3D( X,Y,Z ,'grey' )
# plot rotating direction by a vector
chosen=np.argmin(Y+np.abs(X))
point1=points[chosen]*1.2
point2=points[chosen+1]*1.2 - point1
ax.quiver(*point1, *point2,color='purple')
ax.set_box_aspect([1,1,1])
# plot a sphere with radius=1
z0 = 0
r = 1
u, v = np.mgrid[0:2*np.pi:10j, 0:np.pi:10j]
x = r* np.cos(u)*np.sin(v)
y = r* np.sin(u)*np.sin(v)
z = z0 + r* np.cos(v)
ax.plot_wireframe(x, y, z, color="goldenrod",alpha=0.2)
ax.set_axis_off()
plt.savefig('rot_vectors.pdf',dpi=600,transparent=True)
print('B field vectors are saved as rot_vectors.pdf')
##########################################################

# submit two jobs, one from initial direction to each plane vector, another from plane vector to the opposite initial direction
cwd = os.getcwd()
file_names=os.listdir(cwd)
# the bxsf file
for i in range(len(file_names)):
	if file_names[i][-4:] == 'bxsf':
		sel_file = file_names[i]


length_zfill = int( np.log10(num_angle) +1 )
for i in range(num_angle):
	phi3 = angles[i,0]
	theta3 = angles[i,1]
	directory = 'rota_%s' % str(i).zfill(length_zfill)
	os.mkdir(directory)
	os.chdir( str(Path(cwd) / directory )  )
	os.system('ln -s ../%s ' % (sel_file) )
	print('First angles: %.4f %.4f %.4f %.4f'% ( phi1, theta1, phi3, theta3 ) )
	os.system('sh $SCRIPT/skeaf_1_write_input.sh %s %s %s %s' % (phi1, theta1, phi3, theta3) )
	os.chdir(cwd)


phi2 = phi1+180
theta2 = theta1
for i in range(num_angle):
	phi3 = angles[i,0]
	theta3 = angles[i,1]
	directory = 'rotb_%s' % str(i).zfill(length_zfill)
	os.mkdir(directory)
	os.chdir( str(Path(cwd) / directory )  )
	os.system('ln -s ../%s ' % (sel_file) )
	print('Second angles: %.4f, %.4f, %.4f, %.4f' % (phi3, theta3, phi2, theta2) )
	os.system('sh $SCRIPT/skeaf_1_write_input.sh %s %s %s %s' % (phi3, theta3, phi2, theta2) )
	os.chdir(cwd)


## plot B for each rotation
numrots=10
for i in range(len(angles)):
    phi3 = angles[i,0]
    theta3 = angles[i,1]
    # first 90 degrees rotation, (phi1,theta1) -> (phi3, theta3)
    initialP=polar2cart(1,phi1,theta1)
    finalP=polar2cart(1,phi3,theta3)
    t=np.linspace(0,1,numrots)
    angle=np.arccos( np.dot(initialP, finalP)/np.linalg.norm(initialP)/np.linalg.norm(finalP)  )
    pointsa=np.sin(angle*(1-t))[:,np.newaxis] / np.sin(angle)*initialP[np.newaxis,:] + np.sin(angle*t)[:,np.newaxis] / np.sin(angle)*finalP[np.newaxis,:]
    # second 90 degrees rotation, (phi3,theta3) -> (phi2, theta2)
    initialP=polar2cart(1,phi3,theta3)
    finalP=polar2cart(1,phi2,theta2)
    t=np.linspace(0,1,numrots)
    angle=np.arccos( np.dot(initialP, finalP)/np.linalg.norm(initialP)/np.linalg.norm(finalP)  )
    pointsb=np.sin(angle*(1-t))[:,np.newaxis] / np.sin(angle)*initialP[np.newaxis,:] + np.sin(angle*t)[:,np.newaxis] / np.sin(angle)*finalP[np.newaxis,:]
    points = np.vstack([pointsa,pointsb[1:]])
    ############################# plot
    plot_points=points
    import matplotlib.pyplot as plt
    from mpl_toolkits.mplot3d import Axes3D
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    # change 3Dplot view angle
    #ax.view_init(0, theta1/2)
    ## (1) plot x,y,z basis vectors
    ax.plot([0,1],[0,0],[0,0],color='r')
    ax.text(1,0,0,'x')
    ax.plot([0,0],[0,1],[0,0],color='g')
    ax.text(0,1,0,'y')
    ax.plot([0,0],[0,0],[0,1],color='b')
    ax.text(0,0,1,'z')
    ## (2) plot surface direction
    surface_normal=polar2cart(1,phi1,theta1)
    ax.quiver(0,0,0, *surface_normal ,color='black')
    ax.text(*surface_normal, 'B',color='black')
    ## (3) plot B rotating direction: vectors
    colors=['grey'] * numrots*2 #*(numrots-1)+['black']
    for j in range(len(plot_points)):
        axis=plot_points[j] #polar2cart(1, phi3, theta3 )
        X=[0, axis[0]]
        Y=[0, axis[1]]
        Z=[0, axis[2]]
        ax.plot3D( X,Y,Z, colors[j])
    ## (4) plot B rotating direction: circumference
    #plot_points=plot_points.reshape((-1,3))
    X,Y,Z=zip(*plot_points)
    ax.plot3D( X,Y,Z ,'grey' )
    ## (5) plot circumference direction by a purple vector
    chosen=np.argmin(Y+np.abs(X))
    if chosen>=len(plot_points)-1:
        chosen=len(plot_points)-2
    point1=plot_points[chosen]*1.2
    point2=plot_points[chosen+1]*1.2 - point1
    ax.quiver(*point1, *point2,color='purple')
    ax.set_box_aspect([1,1,1])
    ## (6) plot a sphere with radius=1
    z0 = 0
    r = 1
    u, v = np.mgrid[0:2*np.pi:10j, 0:np.pi:10j]
    x = r* np.cos(u)*np.sin(v)
    y = r* np.sin(u)*np.sin(v)
    z = z0 + r* np.cos(v)
    ax.plot_wireframe(x, y, z, color="goldenrod",alpha=0.2)
    figname='geo%02d_vectors.pdf' % (i) 
    ax.set_axis_off()
    plt.savefig(figname,dpi=600,transparent=True)
    plt.close()
    print('B field vectors are saved as %s' % figname )

