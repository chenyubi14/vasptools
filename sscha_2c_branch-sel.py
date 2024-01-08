#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Import the cellconstructor stuff
import cellconstructor as CC
import cellconstructor.Phonons
import cellconstructor.Methods
import spglib

# Import Matplotlib to plot
import numpy as np
import matplotlib.pyplot as plt
import argparse
import sys

parser = argparse.ArgumentParser(description='Plot a single phonon plot with QE format')
parser.add_argument('qe_files', type=str, nargs='+', 
        help='QE dynamic files start with the same string, like start_sscha')
parser.add_argument('-t','--temps', type=float, nargs='+', 
        help='QE dynamic files start with the same string, like start_sscha')
args = parser.parse_args()
dyn_strs = args.qe_files
temps = args.temps
assert len(dyn_strs) == len(temps), 'Error! Need to enter temperatures correctly!'


def get_nqirr(string):
    import os
    all_files = os.listdir('.')
    NQIRR = len([ fil for fil in all_files if fil[:len(string)] == string ])
    if NQIRR == 0:
        print('Error! Dynamic files %s not found. Use --qe_file to specify' % string )
        sys.exit()
    else:
        print('Found %s %s files' % (NQIRR, string) )
    return NQIRR


lowest_hessian_mode = []


for Temperature, str_i in zip(temps, dyn_strs):
    NQIRR = get_nqirr(str_i)
    dyn_hessian = CC.Phonons.Phonons(str_i, nqirr=NQIRR)
    w_hessian, pols_hessian = dyn_hessian.DiagonalizeSupercell() #recomputed dyn for hessian
    superstructure = dyn_hessian.structure.generate_supercell(dyn_hessian.GetSupercell())

    # Discard the acoustic modes
    acoustic_modes = CC.Methods.get_translations(pols_hessian, superstructure.get_masses_array())
    ## acoustic_modes = [True, True, True, False, False, ...]
    ## ~acoustic_modes = [False, False, False, True, True, ...]
    w_hessian = w_hessian[~acoustic_modes]

    ## Keep the lowest optical modes
    lowest_hessian_mode.append(np.min(w_hessian) * CC.Units.RY_TO_CM) # Convert from Ry to cm-1
    #print ("\n".join(["{:.4f} cm-1".format(w * CC.Units.RY_TO_CM) for w in w_hessian]))
    print('Optical modes (cm^-1): \n%s' % (w_hessian * CC.Units.RY_TO_CM ) )

# We prepare now the file to save the results
freq_data = np.zeros( (len(temps), 2))
freq_data[:, 0] = temps
freq_data[:, 1] = lowest_hessian_mode

# Save results on file
np.savetxt("hessian-low-Opt_vs_temperature.dat",
            freq_data, header = "T [K]; SSCHA mode [cm-1]; Free energy hessian [cm-1]")
#freq_data = np.loadtxt("hessian-low-Opt_vs_temperature.dat")

if len(temps) < 2:
    print('Only one data point. Not continue to plot')
    sys.exit()

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

plt.figure()
plt.plot(freq_data[:,0], freq_data[:,1], label = "hessian", marker = "o")
plt.axhline(0, 0, 1, color = "k", ls = "dotted") # Draw the zero
plt.xlabel("Temperature [K]")
plt.ylabel("Frequency [$\mathrm{cm^{-1}}$]")
plt.legend()
plt.tight_layout()
figname='Temp_Freq.png'
plt.savefig(figname)
print('%s is saved' % figname)

plt.figure()
plt.plot(freq_data[:,0], np.sign(freq_data[:,1]) * freq_data[:,1]**2,
                    label = "hessian", marker = "o")
plt.axhline(0, 0, 1, color = "k", ls = "dotted") # Draw the zero
plt.xlabel("Temperature [K]")
plt.ylabel("$\omega^2$ [cm-2]")
plt.legend()
plt.tight_layout()
figname='Temp_Omeg2.png'
plt.savefig(figname)
print('%s is saved' % figname)

