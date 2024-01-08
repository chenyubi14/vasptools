#from pymatgen.electronic_structure.boltztrap2 import VasprunBSLoader,BztInterpolator,BztTransportProperties,BztPlotter
from pymatgen.electronic_structure.boltztrap2 import BztTransportProperties,BztPlotter
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as tck


SMALL_SIZE = 25
MEDIUM_SIZE = 25
BIGGER_SIZE = 25
LARGE_SIZE = 30
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
plot_hall=False
plot_sigma=False
plot_power=False
############################################################################

if plot_seebeck:
    # y=seebeck, x=fermi_level, label=temperature
    fig,ax=plt.subplots(1,1)
    temperatures=np.array([5,10,20,40,50,300])
    #temperatures=np.array([2,5,10,200,300])
    #temperatures=np.array([50,60,70,80,100])
    bztplot=bztPlotter.plot_props('S','mu','temp',temps=temperatures, fermi_unit='meV')
    ##############################
    #xlimits=[-0.2,0.8]
    #xlimits=[-50,50] #meV
    xlimits=[-100,100] #meV
    ylimits=[-150,150]
    #ylimits=[-200,200]
    #ylimits=[-500,500]
    plt.xlim(xlimits)
    plt.ylim(ylimits)
    #plt.ticklabel_format(style='sci', axis='x', scilimits=(0,0))
    #plt.xticks(np.round(np.linspace(xlimits[0],xlimits[1],5),5))
    plt.hlines(0,xlimits[0],xlimits[1],linewidth=3.0,color='black')
    plt.vlines(0,ylimits[0],ylimits[1],linewidth=3.0,color='black')
    ax=plt.gca()
    plt.legend(fontsize=22,loc='lower left')
    ax.spines['left'].set_linewidth(1.5)
    ax.spines['right'].set_linewidth(1.5)
    ax.spines['top'].set_linewidth(1.5)
    ax.spines['bottom'].set_linewidth(1.5)
    ax.grid(True)
    #plt.legend(loc='lower left')
    ##############################
    plt.tight_layout()
    figname = 'btp2_seebeck1a_mu_temp_scalar.pdf'
    plt.savefig(figname,transparent=True)
    print(figname,' is saved')
    plt.close()


    # y=seebeck, x=temperature, label=doping
    fig,ax=plt.subplots(1,1)
    temperatures=np.array([20,40,50,60,80,100,150,200,250,300])
    #temperatures=np.array([50,60,70,80,100])
    bztplot=bztPlotter.plot_props('S','temp','doping',doping=10.**np.arange(17,21), dop_type='n',temps=temperatures)
    ##############################
    #plt.legend(title='$n$-type doping',fontsize=20,ncol=2)
    plt.legend(title='$n$-type doping',fontsize=25,ncol=2, columnspacing=0.5, framealpha=0.)
    #xlimits=[0,300]
    #ylimits=[-150,150]
    #plt.xlim(xlimits)
    #plt.ylim(ylimits)
    #plt.ticklabel_format(style='sci', axis='x', scilimits=(0,0))
    #plt.hlines(0,xlimits[0],xlimits[1],linewidth=3.0,color='black')
    #plt.vlines(0,ylimits[0],ylimits[1],linewidth=3.0,color='black')
    ax=plt.gca()
    ax.spines['left'].set_linewidth(1.5)
    ax.spines['right'].set_linewidth(1.5)
    ax.spines['top'].set_linewidth(1.5)
    ax.spines['bottom'].set_linewidth(1.5)
    ax.grid(False)
    ##############################
    plt.tight_layout()
    figname = 'btp2_seebeck1b_temp_n-doping.pdf'
    plt.savefig(figname,transparent=True)
    print(figname,' is saved')
    plt.close()


    # y=seebeck, x=temperature, label=doping
    fig,ax=plt.subplots(1,1)
    temperatures=np.array([20,40,50,60,80,100,150,200,250,300])
    #temperatures=np.array([50,60,70,80,100])
    bztplot=bztPlotter.plot_props('S','temp','doping',doping=10.**np.arange(17,21), dop_type='p',temps=temperatures)
    ##############################
    plt.legend(title='$p$-type doping',fontsize=25, ncol=2, columnspacing=0.5, framealpha=0.)
    #xlimits=[0,300]
    #ylimits=[-150,150]
    #plt.xlim(xlimits)
    #plt.ylim(ylimits)
    #plt.ticklabel_format(style='sci', axis='x', scilimits=(0,0))
    #plt.hlines(0,xlimits[0],xlimits[1],linewidth=3.0,color='black')
    #plt.vlines(0,ylimits[0],ylimits[1],linewidth=3.0,color='black')
    ax=plt.gca()
    ax.spines['left'].set_linewidth(1.5)
    ax.spines['right'].set_linewidth(1.5)
    ax.spines['top'].set_linewidth(1.5)
    ax.spines['bottom'].set_linewidth(1.5)
    ax.grid(False)
    ##############################
    plt.tight_layout()
    figname = 'btp2_seebeck1c_temp_p-doping.pdf'
    plt.savefig(figname,transparent=True)
    print(figname,' is saved')
    plt.close()



############################################################################
if plot_hall:
    # y=hall concentration, x=fermi_level, label=temperature[1]
    fig,ax=plt.subplots(1,1)
    # BaMnSb2
    temperatures=np.array([10,20,50,100]) # [25910:26216] [26000:26100]
    # Cd3As2
    #temperatures=np.array([10,20,40])
    bztplot=bztPlotter.plot_props('H','mu','temp',temps=temperatures, fermi_unit='meV')
    ##############################
    #xlimits=[-200,100] #meV
    #xlimits=[-100,100] #meV
    xlimits=[-50,50] #meV
    #ylimits=[1e10,1e20]
    #ylimits=[1e16,1e21]
    ylimits=[1e17,1e22]
    plt.xlim(xlimits)
    plt.ylim(ylimits)
    plt.legend(loc='lower left',fontsize=25,framealpha=0)
    #plt.ticklabel_format(style='sci', axis='x', scilimits=(0,0))
    #plt.xticks(np.round(np.linspace(xlimits[0],xlimits[1],5),5))
    #plt.vlines(0,ylimits[0],ylimits[1],linewidth=3.0,color='black')
    ax=plt.gca()
    ax.spines['left'].set_linewidth(1.5)
    ax.spines['right'].set_linewidth(1.5)
    ax.spines['top'].set_linewidth(1.5)
    ax.spines['bottom'].set_linewidth(1.5)
    #ax.grid(True,)
    #ax.xaxis.set_minor_locator(tck.MultipleLocator(20))
    ax.xaxis.set_minor_locator(tck.AutoMinorLocator())
    ax.tick_params(top=True,bottom=True,right=True,left=True,direction='in',width=1)
    ax.tick_params(which='minor',top=True,bottom=True,right=True,left=True,direction='in',width=1)
    ##############################
    plt.tight_layout()
    figname = 'btp2_hall1a_mu_low-temp.pdf'
    plt.savefig(figname,transparent=True)
    print(figname,' is saved')
    plt.close()


    # y=hall concentration, x=fermi_level, label=temperature[2]
    fig,ax=plt.subplots(1,1)
    temperatures=np.array([40,60,80,100,150,200,250,300])
    bztplot=bztPlotter.plot_props('H','mu','temp',temps=temperatures, fermi_unit='meV')
    ##############################
    #xlimits=[-100,100]
    #ylimits=[1e10,1e18]
    ylimits=[1e17,1e22]
    #plt.xlim(xlimits)
    plt.ylim(ylimits)
    #plt.ticklabel_format(style='sci', axis='x', scilimits=(0,0))
    plt.xticks(np.round(np.linspace(xlimits[0],xlimits[1],5),5))
    plt.vlines(0,ylimits[0],ylimits[1],linewidth=3.0,color='black')
    ax=plt.gca()
    ax.spines['left'].set_linewidth(1.5)
    ax.spines['right'].set_linewidth(1.5)
    ax.spines['top'].set_linewidth(1.5)
    ax.spines['bottom'].set_linewidth(1.5)
    ax.grid(True)
    ##############################
    plt.tight_layout()
    figname = 'btp2_hall1b_mu_high-temp.pdf'
    plt.savefig(figname,transparent=True)
    print(figname,' is saved')
    plt.close()



############################################################################
if plot_sigma:
    # y=conductivity, x=fermi_level, label=temperature
    fig,ax=plt.subplots(1,1)
    temperatures=np.array([5,10,20,40])
    bztplot=bztPlotter.plot_props('Conductivity','mu','temp',temps=temperatures, fermi_unit='meV')
    ##############################
    xlimits=[-100,100] #meV
    ylimits=[0,2e5]
    plt.xlim(xlimits)
    plt.ylim(ylimits)
    plt.xticks(np.round(np.linspace(xlimits[0],xlimits[1],5),5))
    plt.yticks(np.round(np.linspace(ylimits[0],ylimits[1],5),5))
    plt.vlines(0,ylimits[0],ylimits[1],linewidth=3.0,color='black')
    ax=plt.gca()
    plt.ticklabel_format(style='sci', axis='y', scilimits=(0,0))
    ax.yaxis.get_offset_text().set_fontsize(25)
    ax.spines['left'].set_linewidth(1.5)
    ax.spines['right'].set_linewidth(1.5)
    ax.spines['top'].set_linewidth(1.5)
    ax.spines['bottom'].set_linewidth(1.5)
    ax.grid(True)
    ##############################
    plt.tight_layout()
    figname = 'btp2_conductivity1a_mu_low-temp_scalar.pdf'
    plt.savefig(figname,transparent=True)
    print(figname,' is saved')
    plt.close()


    # y=conductivity, x=temperature, label=doping
    fig,ax=plt.subplots(1,1)
    temperatures=np.array([20,40,50,60,80,100,150,200,250,300])
    bztplot=bztPlotter.plot_props('Conductivity','temp','doping',doping=10.**np.arange(17,21), dop_type='n',temps=temperatures)
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
    figname = 'btp2_conductivity1b_temp_n-doping.pdf'
    plt.savefig(figname,transparent=True)
    print(figname,' is saved')
    plt.close()

if plot_power:
    # y=seebeck, x=fermi_level, label=temperature
    fig,ax=plt.subplots(1,1)
    temperatures=np.array([10,100,300])
    #temperatures=np.array([2,5,10,200,300])
    bztplot=bztPlotter.plot_props('Power_Factor','mu','temp',temps=temperatures, fermi_unit='meV')
    ##############################
    #xlimits=[-0.2,0.8]
    #xlimits=[-50,50] #meV
    xlimits=[-100,100] #meV
    ylimits=[-0.5,1.0]
    plt.xlim(xlimits)
    plt.ylim(ylimits)
    #plt.ticklabel_format(style='sci', axis='x', scilimits=(0,0))
    #plt.xticks(np.round(np.linspace(xlimits[0],xlimits[1],5),5))
    #plt.hlines(0,xlimits[0],xlimits[1],linewidth=3.0,color='black')
    #plt.vlines(0,ylimits[0],ylimits[1],linewidth=3.0,color='black')
    plt.legend(framealpha=0, loc='lower center')
    ax=plt.gca()
    ax.xaxis.set_minor_locator(tck.AutoMinorLocator())
    ax.yaxis.set_minor_locator(tck.AutoMinorLocator())
    ax.tick_params(top=True,bottom=True,right=True,left=True,direction='in',width=1.2)
    ax.tick_params(which='minor', top=True,bottom=True,right=True,left=True,direction='in',width=1.0)
    ax.spines['left'].set_linewidth(1.5)
    ax.spines['right'].set_linewidth(1.5)
    ax.spines['top'].set_linewidth(1.5)
    ax.spines['bottom'].set_linewidth(1.5)
    ax.grid(True)
    #plt.legend(loc='lower left')
    ##############################
    plt.tight_layout()
    figname = 'btp2_power1a_mu_temp_scalar.pdf'
    plt.savefig(figname,transparent=True)
    print(figname,' is saved')
    plt.close()



############################################################################
#        props = (
#            "Conductivity",
#            "Seebeck",
#            "Kappa",
#            "Effective_mass",
#            "Power_Factor",
#            "Carrier_conc",
#            "Hall_carrier_conc_trace",
#        )
