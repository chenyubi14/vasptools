#from pymatgen.electronic_structure.boltztrap2 import VasprunBSLoader,BztInterpolator,BztTransportProperties,BztPlotter
from pymatgen.electronic_structure.boltztrap2 import BztTransportProperties,BztPlotter
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as tck


SMALL_SIZE = 20
MEDIUM_SIZE = 20
BIGGER_SIZE = 20
LARGE_SIZE = 25
plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title


#print('length of bztInterp.coeffs',len(bztInterp.coeffs))
print('\nRead transport properties')
bztTransp = BztTransportProperties.load(fname='bztTranspProps.json.gz')


print('\nPlot and save diagrams...')
#bztPlotter = BztPlotter(bzt_transP=bztTransp, bzt_interp=bztInterp)
bztPlotter = BztPlotter(bzt_transP=bztTransp)


############################################################################
plot_seebeck=True
plot_sigma=False
############################################################################

if plot_seebeck:
    # y=seebeck, x=fermi_level, label=temperature
    fig,ax=plt.subplots(1,1)
    temperatures=np.array([50,100])
    bztplot=bztPlotter.plot_props('S','mu','temp',temps=temperatures, output="eigs", fermi_unit='meV')
    ##############################
    #xlimits=[-0.1,0.1] #eV
    xlimits=[-100,100] #meV
    ylimits=[-200,400]
    plt.xlim(xlimits)
    plt.ylim(ylimits)
    #plt.ticklabel_format(style='sci', axis='x', scilimits=(0,0))
    plt.xticks(np.round(np.linspace(xlimits[0],xlimits[1],5),5))
    plt.hlines(0,xlimits[0],xlimits[1],linewidth=3.0,color='black')
    plt.vlines(0,ylimits[0],ylimits[1],linewidth=3.0,color='black')
    #plt.legend(fontsize=22,loc='lower left')
    ax=plt.gca()
    ax.spines['left'].set_linewidth(1.5)
    ax.spines['right'].set_linewidth(1.5)
    ax.spines['top'].set_linewidth(1.5)
    ax.spines['bottom'].set_linewidth(1.5)
    ax.grid(True)
    #plt.legend(loc='lower left')
    ##############################
    plt.tight_layout()
    figname='btp2_seebeck2a_mu_temp_scalar.pdf'
    plt.savefig(figname,transparent=True)
    print(figname,' is saved')
    plt.close()




    # y=seebeck, x=temperature, label=doping
    fig,ax=plt.subplots(1,1)
    temperatures=np.array([20,40,50,60,80,100,150,200,250,300])
    bztplot=bztPlotter.plot_props('S','temp','doping',doping=[1e18, 1e19], dop_type='n',temps=temperatures, output="eigs")
    ##############################
    xlimits=[0,300]
    ylimits=[-200,400]
    plt.xlim(xlimits)
    plt.ylim(ylimits)
    #plt.ticklabel_format(style='sci', axis='x', scilimits=(0,0))
    plt.hlines(0,xlimits[0],xlimits[1],linewidth=3.0,color='black')
    plt.vlines(0,ylimits[0],ylimits[1],linewidth=3.0,color='black')
    ax=plt.gca()
    ax.spines['left'].set_linewidth(1.5)
    ax.spines['right'].set_linewidth(1.5)
    ax.spines['top'].set_linewidth(1.5)
    ax.spines['bottom'].set_linewidth(1.5)
    ax.grid(True)
    ##############################
    plt.tight_layout()
    figname='btp2_seebeck2b_temp_n-doping.pdf'
    plt.savefig(figname,transparent=True)
    print(figname,' is saved')
    plt.close()




    # y=seebeck, x=temperature, label=doping
    fig,ax=plt.subplots(1,1)
    temperatures=np.array([20,40,50,60,80,100,150,200,250,300])
    bztplot=bztPlotter.plot_props('S','temp','doping',doping=[1e18, 1e19], dop_type='p',temps=temperatures, output="eigs")
    ##############################
    xlimits=[0,300]
    ylimits=[-200,400]
    plt.xlim(xlimits)
    plt.ylim(ylimits)
    #plt.ticklabel_format(style='sci', axis='x', scilimits=(0,0))
    plt.hlines(0,xlimits[0],xlimits[1],linewidth=3.0,color='black')
    plt.vlines(0,ylimits[0],ylimits[1],linewidth=3.0,color='black')
    ax=plt.gca()
    ax.spines['left'].set_linewidth(1.5)
    ax.spines['right'].set_linewidth(1.5)
    ax.spines['top'].set_linewidth(1.5)
    ax.spines['bottom'].set_linewidth(1.5)
    ax.grid(True)
    ##############################
    plt.tight_layout()
    figname='btp2_seebeck2c_temp_p-doping.pdf'
    plt.savefig(figname,transparent=True)
    print(figname,' is saved')
    plt.close()



if plot_sigma:
    # y=conductivity, x=fermi_level, label=temperature
    fig,ax=plt.subplots(1,1)
    temperatures=np.array([2])
    bztplot=bztPlotter.plot_props('Conductivity','mu','temp',temps=temperatures, output="eigs")
    # bamnsb2 nsc40 fac3 [25760:26360]  more precise range is [26000:26100] or [26032:26096]
    ##############################
    ax=plt.gca()
    #plt.legend(labels=['$\sigma_x$', '$\sigma_y$', '$\sigma_z$'])
    ax.get_legend().remove()
    plt.yscale("log")
    #xlimits=[-0.1,0.1] #eV
    xlimits=[-100,100] # meV
    ylimits=[1,1e6]
    #ylimits=[0,2e5]
    #plt.yticks(np.round(np.linspace(ylimits[0],ylimits[1],5),5))
    plt.xlim(xlimits)
    plt.ylim(ylimits)
    plt.xticks(np.round(np.linspace(xlimits[0],xlimits[1],5),5))
    plt.yticks(np.logspace(0,6,7))
    plt.vlines(0,ylimits[0],ylimits[1],linewidth=3.0,color='black')
    #plt.ticklabel_format(style='sci', axis='y', scilimits=(0,0))
    #ax.yaxis.get_offset_text().set_fontsize(20)
    ax.spines['left'].set_linewidth(1.5)
    ax.spines['right'].set_linewidth(1.5)
    ax.spines['top'].set_linewidth(1.5)
    ax.spines['bottom'].set_linewidth(1.5)
    ax.grid(True)
    ##############################
    plt.tight_layout()
    figname='btp2_conductivity2a_mu_low-temp_tensor.pdf'
    plt.savefig(figname,transparent=True)
    print(figname,' is saved')
    plt.close()




    # y=conductivity, x=temperature, label=doping
    fig,ax=plt.subplots(1,1)
    temperatures=np.array([20,40,50,60,80,100,150,200,250,300])
    bztplot=bztPlotter.plot_props('Conductivity','temp','doping',doping=[1e17,1e19], dop_type='n',temps=temperatures,output='eigs')
    ##############################
    plt.yscale("log")
    xlimits=[0,300]
    ylimits=[1,1e6]
    plt.xlim(xlimits)
    plt.ylim(ylimits)
    plt.xticks(np.round(np.linspace(xlimits[0],xlimits[1],5),5))
    plt.yticks(np.logspace(0,6,7))
    #plt.vlines(0,ylimits[0],ylimits[1],linewidth=3.0,color='black')
    ax=plt.gca()
    #plt.ticklabel_format(style='sci', axis='y', scilimits=(0,0))
    #ax.yaxis.get_offset_text().set_fontsize(20)
    ax.spines['left'].set_linewidth(1.5)
    ax.spines['right'].set_linewidth(1.5)
    ax.spines['top'].set_linewidth(1.5)
    ax.spines['bottom'].set_linewidth(1.5)
    ax.grid(True)
    ##############################
    plt.tight_layout()
    figname='btp2_conductivity2b_temp_n-doping_tensor.pdf'
    plt.savefig(figname,transparent=True)
    print(figname,' is saved')
    plt.close()



