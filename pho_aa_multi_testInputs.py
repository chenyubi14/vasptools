import numpy as np
import matplotlib.pyplot as plt
import os
import sys
from pathlib import Path


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



filename = 'BTE.kappa_scalar'
print('Should run in the folder with multiple ShengBTE jobs. Read %s within ./*/T300K' % (filename))
print('Will plot kappa_scalar as a function of ngrid/scalebroad. Will use the smallest temperature folder is there is multiple')
print('ngrid will assume to be n*n*n form')

if len(sys.argv)<2:
	print('Error! Enter the tag in CONTROL. 0 for ngrid, 1 for scalebroad')
	sys.exit()
else:
	arg = sys.argv[1]
	assert arg == '0' or arg == '1', 'Error! Unrecognized argument'
	arg = int(arg)
strings = ['ngrid', 'scalebroad']


# find all directories
x=[]
y=[]
ngrid=[]
scalebroad=[]
temps=[]
lists = []
files = os.listdir()
for item in files:
	if os.path.isdir(item):
		# folder found, add it to list
		lists.append(item)
		# read x value
		tags=np.loadtxt(Path(item)/'CONTROL', comments='&', delimiter='=', dtype=str)
		for i,tag in enumerate(tags):
			if tag[0].split()[0] == 'ngrid(:)':
				ngrid.append(int( tag[1].split()[0] ))
			elif tag[0].split()[0] == 'scalebroad':
				scalebroad.append(float( tag[1].split()[0] ))
			if tag[0].split()[0] == 'T' or tag[0].split()[0]=='T_min':
				temps.append(tag[1].split()[0])

print('all directories in cwd: %s' % lists)
assert len(np.unique(temps))==1, 'Error! Temperature is not consistent across folders'
if arg==0:
	x=np.array(ngrid)
	assert len(np.unique(scalebroad))==1, 'Error! scalebroad is not consistent across folders'
else:
	x=np.array(scalebroad)
	assert len(np.unique(ngrid))==1, 'Error! scalebroad is not consistent across folders'

for i,item in enumerate(lists):
	temp='T%sK' % temps[i]
	data=np.loadtxt(Path(item)/temp/filename)
	y.append(data[-1,1])
y=np.array(y)

print(x)
print(y)
plt.scatter(x, y) # put legend here


# settings
xlabel=strings[arg] # the selected is arg, unselected is arg-1 (equivalent to mod 2)
xlabelunit=''
ylabel='$\kappa$'
ylabelunit='[$\mathrm{Wm^{-1}K^{-1}}$]'


title='T=%sK %s=%s' % (temps[0], strings[arg-1],eval(strings[arg-1])[0]) # eval will give the variable

plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
plt.tight_layout()


#save
figname= 'graph_%s_%s.pdf' % (filename[4:], strings[arg])
print('\nremotesingle %s' % (figname))
plt.savefig(figname,dpi=600,transparent=True)
plt.close()
#plt.show()
