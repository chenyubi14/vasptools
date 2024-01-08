#!/home/yubi/repo/miniconda3/envs/sscha/bin/python3.10

import numpy as np
import matplotlib.pyplot as plt
import sys, os

#import cellconstructor as CC, cellconstructor.Units


DESCRIPTION = '''
Usage:
 
 >>> sscha-plot-data.py  folder1/minim_3 folder2/minim_2 folder3/minim_2 ...

The code looks for the newest data file .dat in the folders 
'''

def get_args():
    import argparse
    parser = argparse.ArgumentParser(description='Plot minimization data')
    parser.add_argument('-f','--min_file',
            type=str,default=[] ,nargs='+',
            help='minim files like: fol1/minim_1 fol2/minim_2 ...',
            )
    parser.add_argument('-c','--config',
            type=int,default=[],nargs='+', 
            help='configurations for each one',
            )
    args = parser.parse_args()
    return args


def main():
    #print(DESCRIPTION)
    args = get_args()
    minim_files = args.min_file
    xsteps = args.config
    ms = 3 # markersize
    assert len(xsteps) > 0 and len(minim_files) > 0, 'Error! Enter arguments -f fol/minim_1 ... -c 200 300 ...'
    assert len(xsteps) == len(minim_files), 'Error! number of x is not consistent with files'
    
    #minim_files = [sys.argv[x] + '.dat' for x in range(1, len(sys.argv))]
    minim_files = [ ff + '.dat' for ff in minim_files ]

    #plt.rcParams["font.family"] = "Liberation Serif"
    LBL_FS = 12
    DPI = 120
    TITLE_FS = 15


    # Check if all the files exist
    plot_minim = None
    for m in minim_files:
        if os.path.exists(m):
            if plot_minim is None:
                plot_minim = True
            if not plot_minim:
                raise IOError("Error, file {} found.".format(m))
        else:
            if plot_minim:
                raise IOError("Error, file {} not found.".format(m))
            plot_minim = False

    if not plot_minim:
        print("Nothing to plot, check if the .dat files exist.")
        exit()

    if plot_minim:
        print("Preparing the minimization data...") 
        #minim_data = np.concatenate([np.loadtxt(f) for f in minim_files])
        minim_data = np.concatenate([np.loadtxt(f)[-1:] for f in minim_files])

        # Insert the x axis in the plotting data
        #xsteps = np.arange(minim_data.shape[0])
        new_data = np.zeros(( len(xsteps), 8), dtype = np.double)
        new_data[:, 0] = xsteps
        new_data[:, 1:] = minim_data
        minim_data = new_data
        
        fig_data, axarr = plt.subplots(nrows=2, ncols = 2, sharex = True, dpi = DPI)
        
        # Plot the data
        axarr[0,0].fill_between(minim_data[:,0], minim_data[:,1] - minim_data[:, 2]*.5 ,
                                minim_data[:, 1] + minim_data[:, 2] * .5, color = "aquamarine")
        axarr[0,0].plot(minim_data[:,0], minim_data[:,1], color = "k",
                marker='o', markersize=ms)
        axarr[0,0].set_ylabel("Free energy / unit [meV]", fontsize = LBL_FS)


        axarr[0,1].fill_between(minim_data[:,0], minim_data[:,3] - minim_data[:, 4]*.5 ,
                                minim_data[:, 3] + minim_data[:, 4] * .5, color = "aquamarine")
        axarr[0,1].plot(minim_data[:,0], minim_data[:,3], color = "k",
                marker='o', markersize=ms)
        axarr[0,1].set_ylabel("FC gradient [Bohr^2]", fontsize = LBL_FS)

        axarr[1,1].fill_between(minim_data[:,0], minim_data[:,5] - minim_data[:, 6]*.5 ,
                                minim_data[:, 5] + minim_data[:, 6] * .5, color = "aquamarine")
        axarr[1,1].plot(minim_data[:,0], minim_data[:,5], color = "k",
                marker='o', markersize=ms)
        axarr[1,1].set_ylabel("Structure gradient", fontsize = LBL_FS)
        axarr[1,1].set_xlabel("Number of configs", fontsize = LBL_FS)


        #axarr[1,0].plot(minim_data[:,0], minim_data[:,7], color = "k",
        #        marker='o', markersize=ms)
        axarr[1,0].scatter(minim_data[:,0], minim_data[:,7], color = "k",s=5
                )
        axarr[1,0].plot(minim_data[:,0],minim_data[:,0],color='k')
        axarr[1,0].set_ylabel("Effective sample size", fontsize = LBL_FS)
        axarr[1,0].set_xlabel("Number of configs", fontsize = LBL_FS)
        fig_data.tight_layout()
        figname = 'sscha_converge_conf.png'
        plt.savefig(figname)
        print('figure saved as %s' % figname)
    


if __name__ == "__main__":
    main()
