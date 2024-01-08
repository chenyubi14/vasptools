#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
## Import the sscha code
import cellconstructor as CC, cellconstructor.Phonons
import cellconstructor.ForceTensor

import numpy as np
import matplotlib.pyplot as plt

def get_nqirr(string):
    import os
    ## string might contain folder
    from pathlib import Path
    fil=Path(string)
    all_files = os.listdir(fil.parent.name)
    pattern = fil.name
    NQIRR = len([ fil for fil in all_files if fil[:len(pattern)] == pattern ])
    if NQIRR == 0:
        print('Error! Dynamic files %s not found. Double check' % string )
        sys.exit()
    else:
        print('Found %s %s files' % (NQIRR, string) )
    return NQIRR

def read_kpath(fileinfo='./'):
    ### Get PATH in the brilluin zone from band.conf
    from pathlib import Path
    fileinfo = str( Path(fileinfo) / 'band.conf')
    dict2 = np.loadtxt( fileinfo, comments='#',dtype=str, delimiter='=') # this one works for data with several columns.
    dict2 = [ [dict2[i,0].strip(),dict2[i,1].strip()] for i in range(len(dict2)) ]
    dict2 = dict(dict2)
    ## '$\\Gamma$ X W K $\\Gamma$ L U W L', replace \Gamma with G
    PATH = dict2['BAND_LABELS'].replace('$\\Gamma$', 'G')
    ## 'G X W K G L U W L', remove the spaces
    qlabels=PATH.split() ## ['G', 'X', ...]
    PATH = PATH.replace(' ', '')
    ## PATH="GXWXKGL"
    ### Get q point definition
    ## Remove comma
    qpoints = dict2['BAND'].replace(',','')
    qpoints = np.reshape( qpoints.split(), (-1,3) )
    qpoints = qpoints.astype(float)
    assert len(PATH) == len(qpoints), 'Error! Number of band labels and qpoints do not match in %s' % (fileinfo2)

    ## Here save position of the special points as dictionary
    SPECIAL_POINTS = {}
    for i, q in enumerate(qlabels):
        print(q, qpoints[i])
        SPECIAL_POINTS[q] = qpoints[i]
    return PATH, SPECIAL_POINTS


def plot_dispersion(args):

    ### Phonon dispersion details: path, ticks, labels
    colors = ['r','k','b']
    N_POINTS = 1000
    if args.unit.lower() == 'thz':
        ## convert cm-1 to THz
        unit_ratio = 0.030
        ylabel='Phonon Frequency [THz]'
    elif unit.lower() == 'cm':
        unit_ratio = 1
        ylabel = 'Phonon Frequency [$\mathrm{cm^{-1}}$]'
    print('unit conversion from cm-1 to %s ratio=%s'%(args.unit.lower(), unit_ratio) )

    PATH, SPECIAL_POINTS = read_kpath(args.folder)

    ## Plot the two dispersions
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
    ax = plt.gca()

    ## Load the harmonic and sscha phonons
    for j, dyn_string in enumerate(args.dyns):
        NQIRR = get_nqirr(dyn_string)
        dyn_sscha_j = CC.Phonons.Phonons(dyn_string, NQIRR)
        #sschaT50_dyn = CC.Phonons.Phonons(sschaT50_file, NQIRR)
        ## Get the band path
        if j == 0:
            qpath, data = CC.Methods.get_bandpath( dyn_sscha_j.structure.unit_cell,
                                                  PATH,
                                                  SPECIAL_POINTS,
                                                  N_POINTS)
            xaxis, xticks, xlabels = data # Info to plot correclty the x axis
            nmodes = dyn_sscha_j.structure.N_atoms * 3
        ## Get the phonon dispersion along the path
        disp_sscha_j = CC.ForceTensor.get_phonons_in_qpath(dyn_sscha_j, qpath)
        #sschaT50_dispersion = CC.ForceTensor.get_phonons_in_qpath(sschaT50_dyn, qpath)

        #ax.plot(xaxis, disp_sscha_j*unit_ratio, 
        #        label=args.labels[j],
        #        color=colors[j],
        #        )

        ax.plot(xaxis, disp_sscha_j[:,0]*unit_ratio, 
                label=args.labels[j], # include label
                color=colors[j],
                )
        for k in range(1,nmodes):
            ax.plot(xaxis, disp_sscha_j[:,k]*unit_ratio, 
                    color=colors[j], # not include label
                    )
        print('Double check code here, does it need to plot this many times???')
        #ax.plot(xasis, disp_sscha_j[:,1:]*unit_ratio, color=colors[j]) # not include label


    #for i in range(nmodes):
    #    lbl=None
    #    lblsscha = None
    #    if i == 0:
    #        #lbl = 'Harmonic'
    #        lblsschaT300 = 'SSCHA T=300K'
    #        lblsschaT100 = 'SSCHA T=100K'
    #        lblsschaT50 = 'SSCHA T=50K'
    #    else:
    #        lblsschaT300 = ''
    #        lblsschaT100 = ''
    #        lblsschaT50 = ''
    #    #ax.plot(xaxis, harmonic_dispersion[:,i], color = 'k', ls = 'dashed', label = lbl)
    #    ax.plot(xaxis, sschaT300_dispersion[:,i], color = 'r', label = lblsschaT300)
    #    ax.plot(xaxis, sschaT100_dispersion[:,i], color = 'k', label = lblsschaT100)
    #    ax.plot(xaxis, sschaT50_dispersion[:,i], color = 'b', label = lblsschaT50)

    # Plot vertical lines for each high symmetry points
    for x in xticks:
        ax.axvline(x, 0, 1, color = "grey", ls='dashed', lw = 0.4)
    ax.axhline(0, 0, 1, color = 'k', ls = 'dashed', lw = 0.4)

    # Set the x labels to the high symmetry points
    ax.set_xticks(xticks)
    ax.set_xticklabels(xlabels)
    #ax.set_xlabel("Q path")
    ax.set_ylabel(ylabel)

    if args.emax and args.emin: # if they both exists
        ax.set_ylimits([args.emax, args.emin])

    ax.legend()
    plt.tight_layout()
    figname="sscha_dispersion.png"
    plt.savefig(figname)
    print('%s is saved' % figname )
    return 0

def get_args():
    import argparse
    parser = argparse.ArgumentParser(description='Plot multiple phonon dispersions for sscha')
    parser.add_argument('dyns', 
            type=str,default=[],nargs='+',
            help='enter headers for QE-format dynamic matrix files',
            )
    parser.add_argument('-l','--labels',
            type=str,default=None,nargs='+',
            help='legends for each phonon dispersion',
            )
    parser.add_argument('--emax',
            type=float,default=None,
            help='Energy maximum',
            )
    parser.add_argument('--emin',
            type=float,default=None,
            help='Energy minimum',
            )
    parser.add_argument('--unit',
            type=str,default='thz',choices=['thz','cm'],
            help='Unit of phonon dispersion, thz or cm'
            )
    parser.add_argument('-f','--folder',
            type=str,default='.',
            help='folder stored with band.conf',
            )
    args = parser.parse_args()
    return args


def main(args):
    if args.labels is None:
        args.labels=[None] * len(args.dyns)
    else:
        assert len(args.dyns) == len(args.labels), 'Error! The number of dynamic files does not match with labels'
    plot_dispersion(args)



if __name__ == '__main__':
    import sys
    sys.exit(main(get_args()))
