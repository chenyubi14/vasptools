from pymatgen.io.vasp.outputs import Eigenval
f=Eigenval('EIGENVAL')
spinupkey=list(f.eigenvalues.keys())[0]
spindownkey=list(f.eigenvalues.keys())[1]

spinup=f.eigenvalues[spinupkey][0]
spindown=f.eigenvalues[spindownkey][0]

energyup=spinup[:,0]*spinup[:,1]
energydown=spindown[:,0]*spindown[:,1]
energy=sum(energyup+energydown)
print('total energy=',energy)
