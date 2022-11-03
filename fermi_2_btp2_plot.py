from pymatgen.electronic_structure.boltztrap2 import *
from pymatgen.electronic_structure.plotter import BSPlotter,DosPlotter
from monty.serialization import loadfn
import matplotlib.pyplot as plt


data = VasprunBSLoader.from_file('../vasprun.xml')
print('data read from vasprun.xml. Read interpolated data...')
#bztInterp = BztInterpolator(data,load_bztInterp=True,fname='back_bztInterp.json.gz')
bztInterp = BztInterpolator(data,load_bztInterp=True,fname='bztInterp.json.gz')



###################
# set fname argument to specify a different file name
#print('length of bztInterp.coeffs',len(bztInterp.coeffs))
print('\nCalculate transport properties, temp mu...')
temperatures=np.array([2,5,10,20,40,60,80,100,150,200,250,300])
bztTransp = BztTransportProperties(bztInterp,temp_r = temperatures)
print('\t'.join(['Temp', '\mu', 'rows', 'columns tensor']))
for p in bztTransp.Conductivity_mu, bztTransp.Seebeck_mu, bztTransp.Kappa_mu, \
         bztTransp.Effective_mass_mu, bztTransp.Power_Factor_mu, bztTransp.Carrier_conc_mu:
    print('\t'.join([str(i) for i in p.shape]))
###################


###################
print('\nCalculate transport properties, temp doping...')
bztTransp.compute_properties_doping(doping=10.**np.arange(16,23))
print('\t'.join(['Temp', 'Doping', 'rows', 'columns tensor']))
for p in bztTransp.Conductivity_doping, bztTransp.Seebeck_doping, bztTransp.Kappa_doping, \
         bztTransp.Carriers_conc_doping,bztTransp.Effective_mass_doping, bztTransp.Power_Factor_doping:
    print('\t'.join([str(i) for i in p['n'].shape]))
###################



print('\nPlot and save diagrams...')
bztPlotter = BztPlotter(bztTransp,bztInterp)


###################
fig,ax=plt.subplots(1,1)
bztplot=bztPlotter.plot_props('S','temp','doping',doping=[1e16,1e18], dop_type='n')
plt.savefig('btp2_seeback_temp_doping.pdf')
###################


###################
temperatures=np.array([2,5,10,20,40])
fig,ax=plt.subplots(1,1)
bztplot=bztPlotter.plot_props('S','mu','temp',temps=temperatures)
plt.ylim([-250,250])
plt.xlim([-0.1,0.1])
plt.savefig('btp2_seeback_mu_temp.pdf')
###################


###################
temperatures=np.array([2,5,10,20,40])
fig,ax=plt.subplots(1,1)
bztplot=bztPlotter.plot_props('Conductivity','mu','temp',temps=temperatures)
#plt.xlim([-0.01,0.01])
plt.savefig('btp2_conductivity_temp1.pdf')
###################

###################
temperatures=np.array([60,80,100,150,200,250,300])
fig,ax=plt.subplots(1,1)
bztplot=bztPlotter.plot_props('Conductivity','mu','temp',temps=temperatures)
#plt.xlim([-0.01,0.01])
plt.savefig('btp2_conductivity_temp2.pdf')
###################


###################
#temperatures=np.array([2,5,10,20,40])
temperatures=np.array([5,10,20,40])
fig,ax=plt.subplots(1,1)
bztplot=bztPlotter.plot_props('H','mu','temp',temps=temperatures)
plt.xlim([-0.5,0.5])
plt.ylim([1e17,1e26])
plt.savefig('btp2_hall_mu_temp1.pdf')
###################

###################
temperatures=np.array([40,60,80,100,150,200,250,300])
fig,ax=plt.subplots(1,1)
bztplot=bztPlotter.plot_props('H','mu','temp',temps=temperatures)
#plt.xlim([-0.1,0.1])
#plt.ylim([1e17,1e23])
plt.savefig('btp2_hall_mu_temp2.pdf')
###################

#bztPlotter.plot_props('C','mu','temp')

#fig,ax=plt.subplots(1,1)
#bztPlotter.plot_props('Ca','mu','temp',temps=temperatures).show()
#plt.savefig('btp2_carrier_mu_temp.pdf')

#bztPlotter.plot_props('K','mu','temp',temps=[600,900,1200]).show()
