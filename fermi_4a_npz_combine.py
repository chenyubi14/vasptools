import matplotlib.pyplot as plt
import numpy as np
import sys
from pathlib import Path

SMALL_SIZE = 14
MEDIUM_SIZE = 18
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title
#plt.figure(figsize=(4,6))

# enter files
if len(sys.argv) < 2:
	print('Error! Should enter the .npz file to be read')
	exit()

##################################################

# 1. check the file names, should be mu_temp_*.npz  temp_doping_*.npz
files=sys.argv[1:]
fileinfo='SAVEINFO'
# file names should start with mu_temp_ for all of them
# Or file names should start with temp_doping for all of them
string=files[0]
string1='mu.temp.'
string2='temp.doping.'
if string[:len(string1)]==string1:
    for fil in files: 
        assert fil[:len(string1)] == string1, 'Error! %s is different from other files starting with %s ' % (fil,string1)
    prop_x = 'mu'
    prop_z = 'temp'
    string = string1
elif string[:len(string2)]==string2:
    for fil in files: 
        assert fil[:len(string2)] == string2, 'Error! %s is different from other files starting with %s ' % (fil,string2)
    prop_x = 'temp'
    prop_z = 'doping'
    string = string2
else:
    assert False, 'Error! %s is not the style of Boltztrap output' % (files[0])


#########################################
#temps = [20,40,50,60,80,100,150,200,250,300]
#temps = [5,10,20,40,50,60,80,100,150,200,250,300]
temps = [5,10,20,40,50,100,300]
doping = 10.**np.arange(16,19)
print('plot for these temperatures %s'% temps )
#########################################

#props=[ 'carrier', 'eff_mass', 'hall', 'kappa', 'seebeck', 'sigma' ]
# conductivity is sigma, seebeck, kappa, eff_mass, pow_fac, carrier, hall carrier
props=[ 'Conductivity', 'Seebeck', 'Kappa', 'Effective_mass', 'Power_Factor', 'Carrier_conc','Hall_carrier_conc_trace' ]
props_units = [ r"$[\mathrm{S\,m^{-1}}]$",
            r"[$\mu$V/K]",
            r"$[W / (m \cdot K)]$",
            r"$[m_e]$",
            r"$[ mW / (m\cdot K^2)]$",
            r"$[cm^{-3}]$",
            r"$[cm^{-3}]$", ]

prop_y=fil.split('.')[2]
ylabel=prop_y
idx_prop = props.index(prop_y)
yunit=props_units[ idx_prop ]


##################################################

#mu=mu*1000 # change unit from eV -> meV
print('Read and plot data...')
#plt.hlines(0,colors='grey',linewidth=1)

## For Seebeck
if "mu.temp.Seebeck.npz" in files[0] :
    xlim=[-100,100]
    for fil in files:
        info=np.loadtxt(Path(fil).cwd() / fileinfo, comments='#',dtype=str, delimiter='=')
        info=dict(info)
        legend=info['legend']
        ### load data
        arrays=np.load(fil)
        x=arrays['x'] # mu
        #x = x*1000 ## eV -> meV
        xlabel = 'Fermi Level [meV]'
        z = arrays['z'] # temperature
        p_array = arrays['y']
        temps_all = list(z)
        # plot
        for temp in temps:
            ti=temps_all.index(temp)
            prop_out = np.linalg.eigh(p_array[ti])[0] #p_array[ti]
            ### For Wenkai's GaSb fermi level fitting
            #y = prop_out.mean(axis=1)
            #start=16630
            #func=lambda meV,x,y: np.interp(meV, x[start:start+38], y[start:start+38])
            #print('T=%s\tx=1/2/3\ty=[%.5f, %.5f, %.5f, %.5f],' % (temp, func(1,x,y), func(3,x,y), func(5,x,y), func(10,x,y)) )
            ##print('T=%s, (x=%s, y=%s), (x=1.0836,y=%s)' % (temp, x[16652], y[16652],np.interp(1.083573791686843, x[start:start+38],y[start:start+38]) ) )
            ### For manual gap Cd3As2
            y = prop_out.mean(axis=1)
            start=20140
            func=lambda meV,x,y: np.interp(meV, x[start:start+38], y[start:start+38])
            print('T=%s\tx=1/2/3\ty=[%.5f, %.5f, %.5f, %.5f],' % (temp, func(-2.1,x,y), func(-1.9,x,y), func(-1.8,x,y), func(-1.7,x,y)) )
            plt.plot(x, prop_out.mean(axis=1), label='%s %s K' % (legend,temp) )
elif string == string1: # mu_temp 
    xlim=[-100,100]
    for fil in files:
        info=np.loadtxt(Path(fil).cwd() / fileinfo, comments='#',dtype=str, delimiter='=') # this one works for data with several columns
        info=dict(info)
        legend=info['legend']
        arrays=np.load(fil)
        x=arrays['x'] # mu
        x = x*1000 # change unit from eV -> meV
        xlabel='Fermi Level [meV]' #r"$\mu$ [meV]"
        z=arrays['z'] # temperature
        p_array=arrays['y'] # property array
        temps_all=list(z)
        # plot
        for temp in temps:
            ti=temps_all.index(temp)
            prop_out = p_array[ti]
            plt.plot(x, prop_out, label='%s %s K' % (legend,temp) )
else:
    xlim=[0,300]
    for fil in files: # temp_doping
        info=np.loadtxt( Path(fil).cwd() / fileinfo, comments='#',dtype=str, delimiter='=') # this one works for data with several columns
        info=dict(info)
        legend=info['legend']
        dop_type=fil.split('.')[-2]
        arrays=np.load(fil)
        x=arrays['x'] # temps_all
        xlabel='Temperature [K]'
        z=arrays['z'] # doping_all
        p_array=arrays['y'] # property array
        temps_all=list(x)
        doping_all=list(z)
        # plot
        selected=[] # only plot the selected temperatures, not plotting for all range
        for i,tem in enumerate(temps_all):
            if tem in temps:
                selected.append(i)
        for dop in doping:
            di=doping_all.index(dop)
            prop_out = p_array[di]
            plt.plot(temps,prop_out[selected],label="%s %s %s $\mathrm{cm}^{-3}$" % (legend, dop_type,dop) ,marker='s')



#########################################
plt.legend()
plt.xlabel(xlabel) # fontsize=30
plt.ylabel(ylabel+' '+yunit)
plt.grid()
plt.xlim(xlim)
if ylabel=='Hall_carrier_conc_trace':
	plt.ylim([1e16, 1e21])
elif ylabel=='Seebeck':
	#plt.ylim([-150,150])
	plt.ylim([-500,500])
#########################################

#########################################
# find the experimental Fermi level position by Hall carrier concentration
find_crossing=False
find_crossing=True
if find_crossing:
    from scipy import interpolate
    selected_x = 10 # selected_temperature
    ti=temps_all.index(selected_x)
    y = p_array[ti]
    #f=interpolate.interp1d(x, y) # interpolate the function y(mu)
    experiment_hall = 7.85e17
    value = np.interp(experiment_hall, y[20160:20250],x[20160:20250]) # find value 
    ## make sure the interpolated region is linear
    ## [20150:20250] shows features above and below Fermi_level=0eV
    ## use [20160:20250] for the monotonic interpolation
    ##value = np.interp(8.35e17,p_array[2][20160:20250],mu[20160:20250])
    print('fermi level is at %s ' % value)
#### For Wenkao's Seebeck data 468uV/K @ 5K
#start = 16630
#1.0836 = np.interp(468, y[start:start+38],x[start:start+38]) 
## Got 1.08 meV Fermi level
## The closest value is at x[start+22];y[start+22]
#########################################

plt.tight_layout()
savename=string+ylabel+'.pdf'
print('file saved as %s' % savename)
plt.savefig(savename,dpi=400)
plt.close()
