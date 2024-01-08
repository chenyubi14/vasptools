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
props=[ 'Conductivity', 'Seebeck', 'Kappa', 'Effective_mass', 'Power_Factor', 'Carrier_conc','Hall_carrier_conc_trace' ]
props_units = [ r"$[\mathrm{S\,m^{-1}}]$",
                r"[$\mu$V/K]",
                r"$[W / (m \cdot K)]$",
                r"$[m_e]$",
                r"$[ mW / (m\cdot K^2)]$",
                r"$[cm^{-3}]$",
                r"$[cm^{-3}]$", ]


############################################################################


##############################
## y=Conductivity/Kappa, x=fermi_level, label=temperature
graphtype=1
prop_y1='Conductivity'
prop_y2='Kappa'
if graphtype == 1:
    temps=np.array([10,50,100,200,300]) # only plot tensor for this temperature
    prop_x='mu'
    prop_z='temp'
    xlabel=r"Fermi Level [eV]" ## meV later
    xlimits = [-100,100]
    #xlimits=[-0.1,0.1]
    #xticks=np.round(np.linspace(xlimits[0],xlimits[1],5),5)
    #xlimits=[-0.05,0.05]
    #xticks=np.round(np.linspace(-0.04,0.04,5),5)
    figname='btp2_lorenz_number.pdf'
#output='eigs' # plot eigenvalues
#output='diag' # plot diagonal terms of tensor
output='avg_eigs'
linestyle=['-',':','-.']
#colors=[['darkcyan','dodgerblue','tab:blue'],
#        ['lightsalmon','lightcoral','tab:red'],
#         ['limegreen','tab:green','darkgreen']]
colors=[['royalblue','royalblue','royalblue'],
        ['lightcoral','lightcoral','lightcoral'],
        ['limegreen','tab:green','darkgreen']]
if output  != 'avg_eigs':
    assert len(temps)<=3 , 'too many temperatures for a tensor, will clutter'
else:
    colors=['tab:blue','tab:red','tab:green','tab:orange','tab:purple','goldenrod']
markers=['*','o','s']
ylabel='Lorenz number'
### y scale factor
factor=1 
mu_factor = 1000
##############################

##############################
## read y property
idx_prop1 = props.index(prop_y1)
idx_prop2 = props.index(prop_y2)
#yunit=props_units[ idx_prop ]
yunit='$\mathrm{V^2/K^2}$'
mu = bztPlotter.bzt_transP.mu_r_eV # get chemical potential mu
if mu_factor > 1:
    xlabel=r"Fermi Level [meV]"
    mu = mu * mu_factor
temps_all =bztPlotter.bzt_transP.temp_r.tolist() # get all temperatures
if isinstance(bztPlotter.bzt_transP.doping, np.ndarray):
    doping_all = bztPlotter.bzt_transP.doping.tolist()
if prop_z == "doping" and prop_x == "temp":
    p_array = eval("bztPlotter.bzt_transP." + props[idx_prop] + "_" + prop_z)
else:
    p_array1 = eval("bztPlotter.bzt_transP." + props[idx_prop1] + "_" + prop_x)
    p_array2 = eval("bztPlotter.bzt_transP." + props[idx_prop2] + "_" + prop_x)
plt.figure(figsize=(10, 7))
if prop_z == "temp" and prop_x == "mu":
    markers_size=[5,2,2]
    for j,temp in enumerate(temps):
        ti=temps_all.index(temp)
        if output == 'avg_eigs':
            prop_out1 = np.linalg.eigh(p_array1[ti])[0]
            prop_out1 = prop_out1.mean(axis=1)
            prop_out2 = np.linalg.eigh(p_array2[ti])[0]
            prop_out2 = prop_out2.mean(axis=1)
            ### definition of lorenz number: kappa/(sigma*T)
            prop_out = prop_out2 / prop_out1 / temp
            plt.plot(mu, prop_out, 
                    label=f'{temp} K',
                    #marker=markers[0],markersize=markers_size[0],
                    color=colors[j])
        elif output == 'eigs': # plot eigenvalues from smallest to largest
            prop_out = np.linalg.eigh(p_array[ti])[0]
            for i in range(3):
                prop_out_i =  prop_out[:,i] * factor
                plt.plot(mu, prop_out_i,
                        label='eig %s %s K' % (i, temp),
                        #marker=markers[i],markersize=markers_size[i],
                        color=colors[j][i])
        elif output == 'diag': # plot diagonal terms x,y,z
            prop_out_x = p_array[ti][:,0,0] * factor
            prop_out_y = p_array[ti][:,1,1] * factor
            prop_out_z = p_array[ti][:,2,2] * factor
            plt.plot(mu, prop_out_x, 
                    #marker=markers[0],markersize=markers_size[0],
                    linestyle=linestyle[0],linewidth=3,
                    color=colors[j][0], 
                    label=r'$S_{x}$ %sK'%temp)
            plt.plot(mu, prop_out_y, 
                    #marker=markers[1],markersize=markers_size[1],
                    linestyle=linestyle[1],linewidth=3,
                    color=colors[j][1], 
                    label=r'$S_{y}$ %sK'%temp)
            plt.plot(mu, prop_out_z, 
                    #marker=markers[2],markersize=markers_size[2],
                    linestyle=linestyle[2],linewidth=3,
                    color=colors[j][2], 
                    label=r'$S_{z}$ %sK'%temp)
        
plt.xlabel(xlabel,fontsize=30)
plt.ylabel(ylabel+' '+yunit,fontsize=30)
plt.legend(fontsize=21,framealpha=0,loc='best')
##############################
plt.yscale("log")
ylimits=[1e-8,1e-7]
plt.xlim(xlimits)
plt.ylim(ylimits)
#plt.xticks(xticks, fontsize=25)
#plt.yticks(np.logspace(-1,4,6), fontsize=25)
plt.hlines(2.44e-8, xlimits[0],xlimits[-1],
            color='black',linestyle='dashed',linewidth=2.0)
ax=plt.gca()
#plt.ticklabel_format(style='sci', axis='y', scilimits=(0,0))
#ax.yaxis.get_offset_text().set_fontsize(20)
ax.spines['left'].set_linewidth(1.5)
ax.spines['right'].set_linewidth(1.5)
ax.spines['top'].set_linewidth(1.5)
ax.spines['bottom'].set_linewidth(1.5)
ax.grid(True, linestyle='dashed')
### check this?
#ax.xaxis.set_major_locator(tck.MultipleLocator(20))
ax.xaxis.set_minor_locator(tck.AutoMinorLocator())
ax.yaxis.set_minor_locator(#tck.AutoMinorLocator()
        tck.LogLocator(numticks=999,subs=(.1,.2,.3,.4,.5,.6,.7,.8,.9))
        )
ax.tick_params(top=True,bottom=True,right=True,left=True,direction='in',width=1)
ax.tick_params(which='minor',top=True,bottom=True,right=True,left=True,direction='in',width=1)
##############################
plt.tight_layout()
plt.savefig(figname,transparent=True)
print('figure saved: ', figname)
plt.close()


