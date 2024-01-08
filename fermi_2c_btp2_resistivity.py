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
# bamnsb2 nsc40 fac3 [25760:26360]  more precise range is [26000:26100] or [26032:26096]


# y=conductivity, x=fermi_level, label=temperature
#bztplot=bztPlotter.plot_props('Conductivity','mu','temp',temps=temps, output="eigs")
print('Ignore the divide by zero warning. Only make sure conductivity is non-zero in the Fermi-level range. Conductivity may be zero outside this range')
##############################
graphtype=1
dop_type='n' #'p' or 'n'
prop_y='Conductivity'
if graphtype == 1:
    temps=np.array([10,100]) # only plot tensor for this temperature
    prop_x='mu'
    prop_z='temp'
    xlabel=r"Fermi Level [eV]"
    #xlimits=[-0.1,0.1]
    xlimits=[-100,100]
    #xticks=np.round(np.linspace(xlimits[0],xlimits[1],5),5)
    figname='btp2_conductivity3a_mu_low-temp_resistivity.pdf'
    assert len(temps)<=3 , 'too many temperatures for a tensor, will clutter'
elif graphtype == 2:
    temps=np.array([20,40,50,60,80,100,150,200,250,300]) # only plot tensor for this temperature
    prop_x='temp'
    prop_z='doping'
    xlabel='Temperature (K)'
    xlimits=[0,300]
    #xticks=np.linspace(0,300,4)
    assert dop_type == 'n' or dop_type == 'p', 'dop_type=%s is not recognized' % dop_type
    if dop_type == 'n':
        figname='btp2_conductivity3b_temp_n-doping_resistivity.pdf'
    else:
        figname='btp2_conductivity3c_temp_p-doping_resistivity.pdf'
#output='eigs' # plot eigenvalues
output='diag' # plot diagonal terms of tensor
linestyle=['-',':','-.']
#colors=['tab:blue','tab:red','tab:green','tab:orange','tab:purple','goldenrod']
colors=[['darkcyan','dodgerblue','tab:blue'],
        ['lightsalmon','lightcoral','tab:red'],
        ['limegreen','tab:green','darkgreen']]
colors=[['royalblue','royalblue','royalblue'],
        ['lightcoral','lightcoral','lightcoral'],
        ['limegreen','tab:green','darkgreen']]
markers=['*','o','s']
### plot resisitivity
#ylabel='Resistivity' or 'Conductivity'
#yunit='[m${\Omega}\cdot$cm]'
#factor=1e5 # resitivity has unit \Ohm * m, converted to m\Ohm*cm
### plot conductivity
ylabel='Conductivity'
yunit = '[S$\mathrm{m^{-1}}$]'
factor=1
mu_factor = 1000
doping=[1e17,1e19]
idx_prop = props.index(prop_y)
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
    p_array = eval("bztPlotter.bzt_transP." + props[idx_prop] + "_" + prop_x)
plt.figure(figsize=(10, 7))
if prop_z == "temp" and prop_x == "mu":
    markers_size=[5,2,2]
    for j,temp in enumerate(temps):
        ti=temps_all.index(temp)
        if output == 'eigs': # plot eigenvalues from smallest to largest
            prop_out = np.linalg.eigh(p_array[ti])[0]
            for i in range(3):
                if ylabel == 'Resistivity':
                    prop_out_i =  1/prop_out[:,i] * factor
                else: ## conductivity
                    prop_out_i =  prop_out[:,i] * factor
                plt.plot(mu, prop_out_i,
                        label='eig %s %s K' % (i, temp),
                        marker=markers[i],markersize=markers_size[i],
                        color=colors[j][i])
        elif output == 'diag': # plot diagonal terms x,y,z
            if ylabel == 'Resistivity':
                prop_out_x = 1/p_array[ti][:,0,0] * factor
                prop_out_y = 1/p_array[ti][:,1,1] * factor
                prop_out_z = 1/p_array[ti][:,2,2] * factor
                symbol = r'\rho'
            else:
                prop_out_x = p_array[ti][:,0,0] * factor
                prop_out_y = p_array[ti][:,1,1] * factor
                prop_out_z = p_array[ti][:,2,2] * factor
                symbol = r'\sigma'
            plt.plot(mu, prop_out_x, 
                    #marker=markers[0],markersize=markers_size[0],
                    linestyle=linestyle[0],linewidth=3,
                    color=colors[j][0], 
                    label=r'$%s_\mathrm{x}$ %sK'%(symbol,temp) )
            plt.plot(mu, prop_out_y, 
                    #marker=markers[1],markersize=markers_size[1],
                    linestyle=linestyle[1],linewidth=3,
                    color=colors[j][1], 
                    label=r'$%s_\mathrm{y}$ %sK'%(symbol,temp))
            plt.plot(mu, prop_out_z, 
                    #marker=markers[2],markersize=markers_size[2],
                    linestyle=linestyle[2],linewidth=3,
                    color=colors[j][2], 
                    label=r'$%s_\mathrm{z}$ %sK'%(symbol,temp))
elif prop_z == 'doping' and prop_x == 'temp':
    markers_size=[15,10,10]
    selected=[]
    for i,tem in enumerate(temps_all):
        if tem in temps:
            selected.append(i)
    for j,dop in enumerate(doping):
        di = doping_all.index(dop)
        if output == 'eigs': # plot eigenvalues from smallest to largest
            prop_out = np.linalg.eigh(p_array[dop_type][:,di])[0]
            for i in range(3):
                prop_out_i =  1/prop_out[:,i] * factor
                plt.plot(temps, prop_out_i[selected],
                        label='eig %s %s K' % (i, temp),
                        marker=markers[i],markersize=markers_size[i],
                        color=colors[j][i])
        elif output == 'diag': # plot diagonal terms x,y,z
            prop_out_x = 1/p_array[dop_type][:,di][:,0,0] * factor
            prop_out_y = 1/p_array[dop_type][:,di][:,1,1] * factor
            prop_out_z = 1/p_array[dop_type][:,di][:,2,2] * factor
            plt.plot(temps, prop_out_x[selected], 
                    marker=markers[0],markersize=markers_size[0],color=colors[j][0], linewidth=3,
                    label=r'$\rho_\mathrm{x}$ %s %s $\mathrm{cm}^{-3}$'%(dop_type, dop) )
            plt.plot(temps, prop_out_y[selected], 
                    marker=markers[1],markersize=markers_size[1],color=colors[j][1], linewidth=3,
                    label=r'$\rho_\mathrm{y}$ %s %s $\mathrm{cm}^{-3}$'%(dop_type, dop))
            plt.plot(temps, prop_out_z[selected], 
                    marker=markers[2],markersize=markers_size[2],color=colors[j][2], linewidth=3,
                    label=r'$\rho_\mathrm{z}$ %s %s $\mathrm{cm}^{-3}$'%(dop_type, dop))
        
plt.xlabel(xlabel,fontsize=30)
plt.ylabel(ylabel+' '+yunit,fontsize=30)
plt.legend(fontsize=22,framealpha=0)
##############################
plt.yscale("log")
if ylabel == 'Resistivity':
    ylimits = [1e-1,1e4]
else:
    ylimits = [1e1,1e6]
plt.xlim(xlimits)
plt.ylim(ylimits)
#plt.xticks(xticks, fontsize=25)
#plt.yticks(np.logspace(-1,4,6), fontsize=25)
ax=plt.gca()
#plt.ticklabel_format(style='sci', axis='y', scilimits=(0,0))
#ax.yaxis.get_offset_text().set_fontsize(20)
ax.spines['left'].set_linewidth(1.5)
ax.spines['right'].set_linewidth(1.5)
ax.spines['top'].set_linewidth(1.5)
ax.spines['bottom'].set_linewidth(1.5)
ax.grid(True, linestyle='dashed')
ax.xaxis.set_minor_locator(tck.AutoMinorLocator())
ax.yaxis.set_minor_locator(tck.LogLocator(numticks=999,subs=(.2,.4,.6,.8)))
#ax.yaxis.set_minor_locator(tck.AutoMinorLocator())
ax.tick_params(top=True,bottom=True,right=True,left=True,direction='in',width=1)
ax.tick_params(which='minor',top=True,bottom=True,right=True,left=True,direction='in',width=1)
##############################
plt.tight_layout()
plt.savefig(figname,transparent=True)
print('figure saved: ', figname)
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
