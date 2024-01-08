import numpy as np
import sys
import matplotlib.pyplot as plt


SMALL_SIZE = 16
MEDIUM_SIZE = 18
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title
fig = plt.figure()
ax = fig.add_subplot(111)

x=[]
y=[]
if len(sys.argv) < 2:
    print('Error! Enter /path/to/ERR.dat (can be multiple files)')
    sys.exit()
last_x = 0
for fil in sys.argv[1:]:
    print(fil)
    x_i, y_i = np.loadtxt(fil, usecols=[1,2], unpack=True)
    x_i = x_i + last_x
    x.append(list(x_i))
    y.append(list(y_i))
    last_x = x_i[-1]
#x, y=np.loadtxt("ERR.dat", usecols=[1,2], unpack=True)
x=[item for sublist in x for item in sublist]
y=[item for sublist in y for item in sublist]

###############################################################################
plt.plot(x, y)
title='Root mean squared error of force'
xlabel='Time Steps [ps]'
ylabel='RMSE [eV/A]'
plt.title(title)
plt.xlabel(xlabel)
plt.ylabel(ylabel)
###############################################################################


# edit lables
#####################################################################
import matplotlib.ticker as tck
#ax.yaxis.set_major_locator(tck.MultipleLocator(1))
ax.xaxis.set_major_locator(tck.AutoLocator())
ax.yaxis.set_major_locator(tck.AutoLocator())
ax.xaxis.set_minor_locator(tck.AutoMinorLocator())
ax.yaxis.set_minor_locator(tck.AutoMinorLocator())
ax.tick_params(
        top=True,bottom=True,right=True,left=True,
        direction='in',width=1.2)
ax.tick_params(which='minor',
        top=True,bottom=True,right=True,left=True,
        direction='in',width=1.0)
#####################################################################

plt.tight_layout()
figname= 'bayesian_rmse.pdf'
print('\tfigure saved as \n%s' % (figname))
plt.savefig(figname, transparent=True)
plt.close()
