#!/usr/bin/env python
import numpy as np
import matplotlib.pyplot as plt
import os

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

######################################################################
lkappa = False ## logical, whether kappa
factor = 1/3 ## overal factor
length = 1e-6 ## sample size length, 1 mm=1e-6 m
groupV = 4.1e3 ## group velocity, 1 km/s=1e3 m/s
N_A = 6.022e23 ## avogadro's number
volume = 123e-30 ## volume of unit cell
mass_dens = 1/(N_A*volume) ## J/K/mol -> J/K/g
######################################################################

data=np.loadtxt('thermal_properties.dat')
x=data[:,0] # temperature
y=data[:,3] # specific heat

sel=10
## omit the first T=0, C_v=0 data point
x=x[1:sel]
y=y[1:sel]

# settings
xlabel='Temperature'
xlabelunit=' [K]'
ylabel='$\mathrm{C_V}$'
ylabelunit=' [J/K/mol]'


if lkappa:
    print('print kappa instead')
    y = factor* y * groupV * length * mass_dens
    ylabel = 'Thermal conductivity'
    ylabelunit = ' [W/m/K]'
    print('kappa = ',y)

z=y[2]/(x[2]**3)*(x**3)
plt.plot(x, y, label='Phonopy', 
        marker='o', markersize=6, 
        linewidth=3, linestyle='-') # put legend here
plt.plot(x, z, marker='o',linestyle='--',label='$T^3$')


title='Debye model'
comment=''

# legend, title ,label
plt.legend()
plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label

plt.xscale('log')
plt.yscale('log')
plt.grid(True)


# limits
xlimit=None
ylimit=None
xlimit=[1,10]
if xlimit:
    plt.xlim(xlimit)
if ylimit:
    plt.ylim(ylimit)

#save
figname= 'kappa_kinetic_theory.pdf'
print('\tremotesync \n%s' % (figname))
plt.tight_layout()
plt.savefig(figname,dpi=600, transparent=True)
plt.close()


