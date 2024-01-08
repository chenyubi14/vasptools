print('import modules...')
from datetime import datetime
now = datetime.now()
current_time = now.strftime("%H:%M:%S")
print("Current Time =", current_time)
from pymatgen.electronic_structure.boltztrap2 import VasprunBSLoader, BztInterpolator, BztTransportProperties
from pymatgen.io.vasp.outputs import Vasprun
import os
import sys
import numpy as np
import argparse


# energy_range = 7 # for BaMnSb2
# energy_range = 5 # for Cd3As2
#print('Energy_range >=5 for Cd3As2, >=7 for BaMnSb2')

parser = argparse.ArgumentParser(description='multiprocess cores')
parser.add_argument('--nworkers', metavar='n',type=int ,dest='nworkers',default=None,
                help='number of workers for multiprocessing')
args = parser.parse_args()
nworkers=args.nworkers # a list of atoms to perturb

############################################################################
# enter lpfac and energy_range as arguments
filename='input.btp2'
if os.path.exists(filename):
	dicti = np.loadtxt(filename,comments=['!','#'], dtype=str,delimiter='=')
	dicti = dict(zip(dicti[:,0],dicti[:,1]))
	lpfac =int( dicti['lpfac'] )
	energy_range=float( dicti['energy_range'] )
else:
	print('Error! Not found input.btp2 file')
	sys.exit()
print('Setup: lpfac=%s energy_range=%s. Note if energy_range is not big enough, it will not interpolate enough bands near Fermi.' % (lpfac,energy_range) )
############################################################################



############################################################################
# 1. Load DFT data
## deprecated method
#data = VasprunLoader().from_file('vasprun.xml')
## 1.2 method
#bs = vrun.get_band_structure()
#nele = vrun.parameters['NELECT']
#st = vrun.final_structure
#data2 = VasprunBSLoader(bs,structure=st,nelect=nele)
## 1.3 method
#data = VasprunBSLoader.from_file('../vasprun.xml')
### same as the code below
vrun = Vasprun('../vasprun.xml',parse_projected_eigen=True)
data = VasprunBSLoader(vrun)
### the energy stored in data is in units of Hartree
print('\nData read from vasprun.xml. Start interpolating...')
############################################################################



############################################################################
# set curvature=False to speed up in case you do not need effective mass or hall coeficients
## set fname argument to specify a different file name
bztInterp = BztInterpolator(data,
                            lpfac=lpfac,
                            energy_range=energy_range,
                            curvature=True, 
                            save_bztInterp=True,
                            save_bands=False,
                            fname='bztInterp.lpfac%s.json.gz' % (lpfac) ,
                            nworkers = nworkers,
                            )
# Need save_bands=False, because save_bands=True will take forever to save the file. The process will finally be killed.
print('Interpolation finished. Linking bztInterp.json.gz\n')
os.system('ln -s  bztInterp.lpfac%s.json.gz  bztInterp.json.gz' % (lpfac))

##print('length of bztInterp.coeffs',len(bztInterp.coeffs))
print('\nCalculate transport properties, temp mu...')
temperatures=np.array([2,5,10,20,40,50,60,80,100,150,200,250,300])
## new dense temperature
#temperatures=np.linspace(50,300,26) # np.array([50,60,80,100,150,200,250,300])
print('Temperatures=%s' % (temperatures) )
bztTransp = BztTransportProperties(bztInterp,
                                    temp_r = temperatures, 
                                    npts_mu=40000
                                    ) # CRTA=1e-14, 
#bztTransp = BztTransportProperties(bztInterp,temp_r = temperatures, npts_mu=4000) # CRTA=1e-14, 
# bztTransp.Conductivity_mu
# bztTransp.Seebeck_mu
# bztTransp.Kappa_mu
# bztTransp.Effective_mass_mu
# bztTransp.Power_Factor_mu
# bztTransp.Carrier_conc_mu

print('\nCalculate transport properties, temp doping...')
#bztTransp.compute_properties_doping(doping=10.**np.arange(16,23))
bztTransp.compute_properties_doping(doping=10.**np.arange(10,21,2))
# bztTransp.Conductivity_doping
# bztTransp.Seebeck_doping
# bztTransp.Kappa_doping
# bztTransp.Carriers_conc_doping
# bztTransp.Effective_mass_doping
# bztTransp.Power_Factor_doping
print('save transport data to a json.gz file\n')
bztTransp.save(fname="bztTranspProps.lpfac%s.json.gz" % (lpfac) )
os.system('ln -s  bztTranspProps.lpfac%s.json.gz  bztTranspProps.json.gz' % (lpfac))
############################################################################



####################
#print('save data to numpy arrays')
#mu=bztTransp.mu_r_eV
#temp=bztTransp.temp_r.tolist()
#np.savez('mu_temp_sigma',mu=mu,temp=temp,y=bztTransp.Conductivity_mu) # electrical conductivity
#np.savez('mu_temp_seebeck',mu=mu,temp=temp,y=bztTransp.Seebeck_mu)
#np.savez('mu_temp_kappa',mu=mu,temp=temp,y=bztTransp.Kappa_mu) # thermal conductivity for electrons
#np.savez('mu_temp_hall',mu=mu,temp=temp,y=bztTransp.Hall_carrier_conc_trace_mu)
#np.savez('mu_temp_carrier',mu=mu,temp=temp,y=bztTransp.Carrier_conc_mu)
#np.savez('mu_temp_eff_mass',mu=mu,temp=temp,y=bztTransp.Effective_mass_mu)
####################

