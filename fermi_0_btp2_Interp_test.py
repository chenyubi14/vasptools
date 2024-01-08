from pymatgen.electronic_structure.boltztrap2 import VasprunBSLoader,BztInterpolator,BztTransportProperties,BztPlotter
import numpy as np
import matplotlib.pyplot as plt
import os

filename='input.btp2'
if os.path.exists(filename):
        dicti=np.loadtxt(filename,comments=['!','#'], dtype=str,delimiter='=')
        dicti = dict(zip(dicti[:,0],dicti[:,1]))
        lpfac =int( dicti['lpfac'] )
        energy_range=float( dicti['energy_range'] )
else:
	import sys
	print('Error! should have file input.btp2 to read lpfac and energy_range')
	sys.exit()
	files=os.listdir('.')
	for l in files:
		if 'lpfac' in l:
			lpfac=l.split('lpfac')[1].split('.')[0]
print('lpfac is %s, energy_range=%s'% (lpfac,energy_range) )

data = VasprunBSLoader.from_file('../vasprun.xml')
print('data read from vasprun.xml. Reading interpolated data...')
#bztInterp = BztInterpolator(data,load_bztInterp=True,fname='back_bztInterp.json.gz')
bztInterp = BztInterpolator(data,energy_range=energy_range,load_bztInterp=True,fname='bztInterp.json.gz')


###################
# set fname argument to specify a different file name
#print('length of bztInterp.coeffs',len(bztInterp.coeffs))
print('\nCalculate transport properties, temp mu...')
temperatures=np.array([2,5,10,20,40,50,60,80,100,150,200,250,300])
bztTransp = BztTransportProperties(bztInterp,temp_r = temperatures, npts_mu=40000)
# bztTransp.Conductivity_mu
# bztTransp.Seebeck_mu
# bztTransp.Kappa_mu
# bztTransp.Effective_mass_mu
# bztTransp.Power_Factor_mu
# bztTransp.Carrier_conc_mu
print('\t'.join(['Temp', '\mu', 'rows', 'columns tensor']))
for p in bztTransp.Conductivity_mu, bztTransp.Seebeck_mu, bztTransp.Kappa_mu, \
         bztTransp.Effective_mass_mu, bztTransp.Power_Factor_mu, bztTransp.Carrier_conc_mu:
    print('\t'.join([str(i) for i in p.shape]))
###################


###################
print('\nCalculate transport properties, temp doping...')
bztTransp.compute_properties_doping(doping=10.**np.arange(10,21,2))
# bztTransp.Conductivity_doping
# bztTransp.Seebeck_doping
# bztTransp.Kappa_doping
# bztTransp.Carriers_conc_doping
# bztTransp.Effective_mass_doping
# bztTransp.Power_Factor_doping
print('\t'.join(['Temp', 'Doping', 'rows', 'columns tensor']))
for p in bztTransp.Conductivity_doping, bztTransp.Seebeck_doping, bztTransp.Kappa_doping, \
         bztTransp.Carriers_conc_doping,bztTransp.Effective_mass_doping, bztTransp.Power_Factor_doping:
    print('\t'.join([str(i) for i in p['n'].shape]))
###################

