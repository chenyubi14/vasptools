#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Dec 30 12:25:36 2020

@author: mengenwang
"""

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.ticker import MultipleLocator


fig=plt.figure()
fig.set_size_inches(5, fig.get_figheight(), forward=True)
fig.set_size_inches(10, fig.get_figwidth(), forward=True)

ax=fig.add_subplot(1,1,1)

"""
Formation energy is defined as:

E_f=Etot-E_perfect-miu_i*n_i+q*(fermi+vbm)+f_correction
Etot is the total energy of the defect supercell
E_perfec is the total energy of the perfect supercell
n_i is the number of vacancy atom (If you have an O vacancy, then your n_i is -1)
fermi is your x axis
vbm is taken from your bulk calculation (the eigenvalue of the highest occupied state)
f_correction is your freysoldt correction
"""

E_perfect=-1049.315
vbm=2.289

#chemical potentials for dopants
# This is under Ga-rich condition: miu_Ga = miu_Ga_bulk
miu_C_bulk=-11.0488
miu_C=miu_C_bulk

miu_Si_bulk=-6.541
delta_Si=-2.38
miu_Si=miu_Si_bulk+delta_Si

miu_H=-4.062

miu_Ga_bulk=-3.482
miu_Ga=miu_Ga_bulk

miu_O_bulk=-7.586
delta_O=-3.407
miu_O=miu_O_bulk+delta_O

miu_Al_bulk=-4.3046
delta_Al=-2.235
miu_Al=miu_Al_bulk+delta_Al
#Band_gap
BG=5.81


print('miu_C=-')
print(miu_C)
print('miu_Si=-')
print(miu_Si)
print('miu_H=-')
print(miu_H)
print('miu_O=-')
print(miu_O)
print('miu_Ga=-')
print(miu_Ga)
print('miu_Al=-')
print(miu_Al)







#C_II neutral
fermi_level=[]
formation_energy=[]
Etot=-1046.176
q=0
f_correction=0

for i in range(694,1000):
    fermi=BG*i/1000
    fermi_level.append(fermi)
    E_f=Etot-E_perfect-miu_C+miu_Al+q*fermi+q*vbm+f_correction
    formation_energy.append(E_f)

plt.plot(fermi_level, formation_energy, color='black', linewidth=3,linestyle='--', label='C_II')


#C_II +1
fermi_level=[]
formation_energy=[]
Etot=-1052.6295
q=1
f_correction=0.117

for i in range(694):
    fermi=BG*i/1000
    fermi_level.append(fermi)
    E_f=Etot-E_perfect-miu_C+miu_Al+q*fermi+q*vbm+f_correction
    formation_energy.append(E_f)

plt.plot(fermi_level, formation_energy, color='black', linestyle='--', linewidth=3)


#C_II -1
fermi_level=[]
formation_energy=[]
Etot=-1038.3453
q=-1
f_correction=0.36

for i in range(1000):
    fermi=BG*i/1000
    fermi_level.append(fermi)
    E_f=Etot-E_perfect-miu_C+miu_Ga+q*fermi+q*vbm+f_correction
    formation_energy.append(E_f)

#plt.plot(fermi_level, formation_energy, color='black', linestyle='--',linewidth=3)



#Si_I neutral
fermi_level=[]
formation_energy=[]
Etot=-1051.7375
q=0
f_correction=0

for i in range(1000):
    fermi=BG*i/1000
    fermi_level.append(fermi)
    E_f=Etot-E_perfect-miu_Si+miu_Ga+q*fermi+q*vbm+f_correction
    formation_energy.append(E_f)

#plt.plot(fermi_level, formation_energy, color='orange', linewidth=3, label='Si_I')

#Si_I +1
fermi_level=[]
formation_energy=[]
Etot=-1060.657
q=1
f_correction=0.243

for i in range(1000):
    fermi=BG*i/1000
    fermi_level.append(fermi)
    E_f=Etot-E_perfect-miu_Si+miu_Ga+q*fermi+q*vbm+f_correction
    formation_energy.append(E_f)

plt.plot(fermi_level, formation_energy, color='orange', linewidth=3, label='Si_I')

#Si_I -1
fermi_level=[]
formation_energy=[]
Etot=-1043.5802
q=-1
f_correction=0.133

for i in range(1000):
    fermi=BG*i/1000
    fermi_level.append(fermi)
    E_f=Etot-E_perfect-miu_Si+miu_Ga+q*fermi+q*vbm+f_correction
    formation_energy.append(E_f)

#plt.plot(fermi_level, formation_energy, color='orange', linewidth=3)

#H_OI neutral
fermi_level=[]
formation_energy=[]
Etot=-1038.8561
q=0
f_correction=0

for i in range(1000):
    fermi=BG*i/1000
    fermi_level.append(fermi)
    E_f=Etot-E_perfect-miu_H+miu_O+q*fermi+q*vbm+f_correction
    formation_energy.append(E_f)

#plt.plot(fermi_level, formation_energy, color='blue', linewidth=3, label='H_OI')




#H_OI +1
fermi_level=[]
formation_energy=[]
Etot=-1047.2305
q=1
f_correction=0.163

for i in range(978):
    fermi=BG*i/1000
    fermi_level.append(fermi)
    E_f=Etot-E_perfect-miu_H+miu_O+q*fermi+q*vbm+f_correction
    formation_energy.append(E_f)

plt.plot(fermi_level, formation_energy, color='blue', linewidth=3, label='H_OI')

#H_OI -1
fermi_level=[]
formation_energy=[]
Etot=-1031.3984
q=-1
f_correction=0.2799

for i in range(978,1000):
    fermi=BG*i/1000
    fermi_level.append(fermi)
    E_f=Etot-E_perfect-miu_H+miu_O+q*fermi+q*vbm+f_correction
    formation_energy.append(E_f)

plt.plot(fermi_level, formation_energy, color='blue', linewidth=3)


plt.xlabel(r'Fermi energy (eV)', fontsize=30)
plt.ylabel(r'Formation energy (eV)', fontsize=30)
ax.set_xlim([0, BG])
ax.set_ylim([-2, 9])

plt.tick_params(axis='y', width=3,length=7, labelsize=30) 
plt.tick_params(axis='x', width=3, length=7,  labelsize=30) 
ax.tick_params(bottom=True, top=False, left=True, right=True)

ax.spines['left'].set_linewidth(3)
ax.spines['right'].set_linewidth(3)
ax.spines['top'].set_linewidth(3)
ax.spines['bottom'].set_linewidth(3)

plt.xticks(np.arange(0, BG, 1))
plt.yticks(np.arange(-1, 9, 3))

ax.yaxis.set_major_locator(MultipleLocator(2))
ax.yaxis.set_minor_locator(MultipleLocator(5))
plt.tick_params(axis='y', which='minor', width=1.5, length=5, color='k', right=True)
import matplotlib.ticker as tck
ax.yaxis.set_minor_locator(tck.AutoMinorLocator())



plt.savefig('Ef.jpeg', format='jpeg', dpi=300, bbox_inches='tight')
plt.legend()

plt.show()
