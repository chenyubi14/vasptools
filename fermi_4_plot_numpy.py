import matplotlib.pyplot as plt
import numpy as np
import sys
from scipy import interpolate

SMALL_SIZE = 12
MEDIUM_SIZE = 14
BIGGER_SIZE = 16

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title
#plt.figure(figsize=(4,6))

if len(sys.argv) < 2:
	print('Error! Should enter the .npz file to be read')
	exit()

#########################################
temps = [5,10,20,40,80,200,300]
print('plot for these temperatures %s'% temps )
#########################################
prop_z='temp'
prop_x='mu'


print('Read interpolated data...')
filename=sys.argv[1]
arrays=np.load(filename)
mu=arrays['mu']
temps_all=arrays['temp']
p_array=arrays['y'] # property array
#p_array=arrays['seebeck'] # property array
ylabel=filename.split('.')[0].split('_')[-1]


#props=[ 'carrier', 'eff_mass', 'hall', 'kappa', 'seebeck', 'sigma' ]
# conductivity is sigma, seebeck, kappa, eff_mass, pow_fac, carrier, hall carrier
props=[ 'sigma', 'seebeck', 'kappa', 'eff_mass', 'pow_fac', 'carrier','hall' ]
props_units = [ r"$(\mathrm{S\,m^{-1}})$",
            r"($\mu$V/K)",
            r"$(W / (m \cdot K))$",
            r"$(m_e)$",
            r"$( mW / (m\cdot K^2)$",
            r"$(cm^{-3})$",
            r"$(cm^{-3})$", ]

idx_prop = props.index(ylabel)
yunit=props_units[ idx_prop ]
temps_all = list(temps_all)


#########################################
mu=mu*1000 # eV -> meV
output="avg_eigs"
#########################################
# special case of carrier and hall carrier concentration 2d arrays (temp,mu)
if idx_prop in [5, 6]:
	if prop_z == "temp" and prop_x == "mu":
		for temp in temps:
			ti = temps_all.index(temp)
			prop_out = p_array[ti] if idx_prop == 6 else np.abs(p_array[ti])
			plt.semilogy(mu, prop_out, label=str(temp) + " K")
	else:
		raise BoltztrapError(
			"only prop_x=mu and prop_z=temp are \
			available for c.c. and Hall c.c.!"
	)
else:
	for temp in temps:
		ti = temps_all.index(temp)
		prop_out = np.linalg.eigh(p_array[ti])[0] # eigenvalue and eigenvectors, [0] takes the eigenvalue
		if output == "avg_eigs":
			plt.plot(mu, prop_out.mean(axis=1), label=str(temp) + " K")
		elif output == "eigs":
			for i in range(3):
				plt.plot(
					mu,
					prop_out[:, i],
					label="eig " + str(i) + " " + str(temp) + " K",
				)


#########################################
#for ti in temp_plot:
#	ind = list(temps).index(ti)
#	yi = y[ind] 
#	plt.semilogy(x, yi, label=str(ti) + ' K')
plt.xlabel(r"$\mu$ (meV)") # fontsize=30
plt.ylabel(ylabel+' '+yunit)
plt.legend()
plt.grid()
plt.xlim([-100,100])
if ylabel=='hall':
	plt.xlim([-100,100])
	#plt.ylim([1e17, 1e26])
	plt.ylim([1e17, 1e23])
	#yunit=' carrier conc. ($cm^{-3}$)'
elif ylabel=='seebeck':
	plt.xlim([-100,100])
	plt.ylim([-250,250])
elif ylabel=='sigma':
	plt.ylim([-1e5, 5e5])
#plt.xlim([-300,300])
#########################################

selected_x = 10
for temp in temps:
	ti = temps_all.index(temp)
	if idx_prop in [5, 6]:
		prop_out = p_array[ti] if idx_prop == 6 else np.abs(p_array[ti])
		y=prop_out
	else:
		prop_out = np.linalg.eigh(p_array[ti])[0] # eigenvalue and eigenvectors, [0] takes the eigenvalue
		y=prop_out.mean(axis=1)
	f=interpolate.interp1d(mu, y)
	print('For T=%sK, y_value=%s' % (temp, f(selected_x)) )
#ind = list(temps).index(20)
#yi=y[ind]
##value = np.interp(8.35e17,yi[3020:3100],mu[3020:3100])
#print('fermi level is at %s ' % value)

savename='mu_temp_'+ylabel+'.png'
print('file saved as %s' % savename)
plt.savefig(savename,dpi=400)

