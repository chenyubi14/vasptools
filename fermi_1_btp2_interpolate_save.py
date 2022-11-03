from pymatgen.electronic_structure.boltztrap2 import *
from pymatgen.electronic_structure.plotter import BSPlotter,DosPlotter
from monty.serialization import loadfn
import matplotlib.pyplot as plt
import os
import sys


# energy_range = 7 # for BaMnSb2
# energy_range = 5 # for Cd3As2
energy_range=5
print('energy_range=%s currently. Note if it is not big enough to cover the correct range, it will not give the correct interpolated bands.' % (energy_range) )


###################
if len(sys.argv) < 2:
	print('Error! Should enter an integer: lpfac')
	sys.exit()
lpfac=int(sys.argv[1])
###################



###################
# 1. Load DFT data
## 1.1 method
#vrun = Vasprun('./vasprun.xml',parse_projected_eigen=True)
#data = VasprunBSLoader(vrun)
## 1.2 method
#bs = vrun.get_band_structure()
#nele = vrun.parameters['NELECT']
#st = vrun.final_structure
#data2 = VasprunBSLoader(bs,structure=st,nelect=nele)
## 1.3 method
data = VasprunBSLoader.from_file('../vasprun.xml')
print('data read from vasprun.xml. Start interpolating...')
## deprecated method
#data = VasprunLoader().from_file('vasprun.xml')
###################



# set curvature=False to speed up in case you do not need effective mass or hall coeficients
## set fname argument to specify a different file name
bztInterp = BztInterpolator(data,lpfac=lpfac,energy_range=energy_range,curvature=True, save_bztInterp=True,fname='bztInterp.lpfac%s.json.gz' % (lpfac) )
print('Interpolation finished. Linking bztInterp.json.gz')
os.system('ln -s  bztInterp.lpfac%s.json.gz  bztInterp.json.gz' % (lpfac))
###################



##print('length of bztInterp.coeffs',len(bztInterp.coeffs))
print('\nCalculate transport properties, temp mu...')
temperatures=np.array([2,5,10,20,40,60,80,100,150,200,250,300])
bztTransp = BztTransportProperties(bztInterp,temp_r = temperatures)
###################




###################
print('save data to numpy arrays')
mu=bztTransp.mu_r_eV
temp=bztTransp.temp_r.tolist()
np.savez('mu_temp_sigma',mu=mu,temp=temp,y=bztTransp.Conductivity_mu) # electrical conductivity
np.savez('mu_temp_seebeck',mu=mu,temp=temp,y=bztTransp.Seebeck_mu)
np.savez('mu_temp_kappa',mu=mu,temp=temp,y=bztTransp.Kappa_mu) # thermal conductivity for electrons
np.savez('mu_temp_hall',mu=mu,temp=temp,y=bztTransp.Hall_carrier_conc_trace_mu)
np.savez('mu_temp_carrier',mu=mu,temp=temp,y=bztTransp.Carrier_conc_mu)
np.savez('mu_temp_eff_mass',mu=mu,temp=temp,y=bztTransp.Effective_mass_mu)
###################


