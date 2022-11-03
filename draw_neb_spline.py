import numpy as np
import sys 
import shutil
import os
import matplotlib.pyplot as plt 
sys.path.append(os.environ['SCRIPT'])
path = os.environ['PWD'] + '/' 
from pathlib import Path

# Goal: draw barrier of NEB calculations
# input: spline.dat
# 	# To generate spline.dat, use nebbarrier.pl first, and then nebspline.pl
# output: pdf barrier plot

datafile1=Path('spline.dat')
datafile2=Path('neb.dat')
plotfile='nebbarrier.pdf'

if len(sys.argv)>=2:
	print('The first argument is the folder with spline.dat and neb.dat. The second argument (optional) is the pdf name')
	datafilefolder=Path(sys.argv[1])
	datafile1=datafilefolder / datafile1
	datafile2=datafilefolder / datafile2
	if len(sys.argv)>=3:
		plotfile=sys.argv[2]
else:
	print('Use spline.dat by default. Enter argument to plot other spline.dat not in cwd. Another argument for the pdf name')
print('Will read %s and %s. Will output a plot named %s' % (datafile1, datafile2, plotfile) )

def read_data(fil):
	# fil is spline.dat
	x=np.array([]) 
	y=np.array([])
	with open(fil, 'r') as f:
		lines=f.readlines()
		totlen=len(lines)
		for i in range(totlen):
			singleline=lines[i].split()
			dist=singleline[1]
			energy = singleline[2]
			x=np.append(x,float(dist))
			y=np.append(y,float(energy)) # the model, will be a smooth function
	return x,y

def myplot(x1,y1,x2,y2,plotname):
	SMALL_SIZE = 16
	MEDIUM_SIZE = 18
	BIGGER_SIZE = 20
	plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
	plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
	plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
	plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
	plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
	plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
	plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title
	plt.figure()
	plt.plot(x1,y1,'r') # ,label='$V^{lr}_q$' #plt.legend()
	plt.scatter(x2,y2,c='b')
	plt.xlabel('Reaction coordinate [$\AA$]')
	plt.ylabel('Energy (eV)')
	plt.grid(visible=True)
	plt.tight_layout()
	plt.savefig(plotname, dpi=600)

if not datafile1.is_file():
	print('Error! The data files are not available. Use nebbarrier and nebspline to generate.')
	sys.exit()

x1,y1=read_data(str(datafile1))
x2,y2=read_data(str(datafile2))
myplot(x1,y1,x2,y2,plotfile)