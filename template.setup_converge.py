import numpy as np

#func_aexx=lambda var, div: np.linspace(0.36,0.60, div)
#func_encut=lambda var, div: np.linspace(-150,200,div)+var
#sigmas=[0.01,0.02,0.03,0.04,0.05,0.1,0.15,0.2,0.25];func_sigma=lambda var, div: sigmas 
#kgrids=[5,7,9,11,13,15,17,19]
func_temp = lambda var, div: np.linspace(50,500, div)

# values to import
#func=func_aexx;mat=['INCAR', 'AEXX'];divnum=9;header='hsev544_'
#func=func_encut;mat=['INCAR', 'ENCUT']; divnum=8;header='conv'
#func=func_sigma;mat=['INCAR', 'SIGMA']; divnum=len(sigmas);header='conv'
#func=kgrids;mat=['KPOINTS', 'grid'];divnum=len(kgrids);header='conv'
func=func_temp;mat=['INCAR','TEBEG']; divnum=10; header='mlff'
