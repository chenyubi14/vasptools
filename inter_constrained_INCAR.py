import numpy as np
import sys
import os
from shutil import copyfile, rmtree
from pathlib import Path

sys.path.append(os.environ['SCRIPT'])
sys.path.append('./')

from class2_update_input import change_input_files
from pymatgen.io.vasp.outputs import Eigenval
# Goal: create the excited folder, copy files, edit INCAR for the constrained-DFT
# FERWE and FERDO can only handle single K point!

# In[1]: initialize the common changes
changes={'ISTART':1, 'NSW':200, 'NELM':200,'ISMEAR':-2,'ALGO':'All','FERWE':'','FERDO':'','LDIAG':'T'}

# initialize the arguments to know the excitation information
if len(sys.argv)<4:
	print('Error! enter 3 (or 4) arguments!')
	print('1st: ground folder, 2nd: excited folder header (defect0e.constrainedX.special) will add .LDIAGF, 3rd: excited spin channel, 0 for up, 1 for down.') 
	print('The optional 4th argument: LDIAG (optional), 0 for False, otherwise True(default).')
	sys.exit()
elif len(sys.argv)>3:
	ldiag=int(sys.argv[4])
	if ldiag == 0:
		changes['LDIAG']='.FALSE.' # changes['LDIAG'][1] is 'F'
	else:
		changes['LDIAG']='.TRUE.' # changes['LDIAG'][1] is 'T'
gr_folder=Path(sys.argv[1])
excited_folder=sys.argv[2]
spinchannel=int(sys.argv[3]) # 0 for up, 1 for down
file_eigen=Path(sys.argv[1]) / 'EIGENVAL' 



# In[2]: create excited folder, and copy files (INCAR, KPOINTS, POTCAR, WAVECAR, DEFECT, EIGENVAL), POSCAR from CONTCAR
excited_folder = Path(excited_folder + '.LDIAG' + changes['LDIAG'][1])
if os.path.isdir(excited_folder): # remove existing folders
    print('The old %s folder exists. Do you want to remove it? \nEnter (y, or yes) for delete and continue. \nEnter (other) for no and exit' % excited_folder)
    removeoldfile=input()
    if removeoldfile == 'y' or removeoldfile == 'yes':
        rmtree(excited_folder)
    else:
        print('exit for not planning to remove old folders')
        sys.exit()
os.mkdir(str(excited_folder))
os.system('cp %s submit.job' % (os.environ['SUBMITOLD']))
for f in ['KPOINTS', 'POTCAR', 'INCAR', 'DEFECT', 'WAVECAR', 'EIGENVAL']:
	copyfile(str(gr_folder / f), str(excited_folder / f))
	os.system('chmod +x %s' % str(excited_folder / f) )
copyfile(str(gr_folder/'CONTCAR'), str(excited_folder / 'POSCAR') )
os.system('chmod +x %s' % str(excited_folder / 'POSCAR')  )

# In[3]: prepare excitation values
def get_fer(eigen, spinchannel, excited=True):
	spinchannelkey=list(eigen.eigenvalues.keys())[spinchannel] # <Spin.up: 1> or <Spin.down: -1>
	spinchannelvals=eigen.eigenvalues[spinchannelkey][0] # 2D numpy array, (band index, occupation)
	spinchannelocc=spinchannelvals[:,1] # 1D array, occupation of spin, (band index)
	elec_num = int(np.sum(spinchannelocc))
	if excited:
		spinchannelocc[elec_num] = 1
		spinchannelocc[elec_num-1] = 0
		print('After spin %s excitation, the occupation becomes \n%s ' % ( ['Up','Down'][spinchannel], spinchannelvals[elec_num-2:elec_num+1,:]) )
		fer='%s*1.0 1*0.0 1*1.0 999*0.0' % (elec_num-1)
	else:
		print('the untouched spin channel \n%s ' % (spinchannelvals[elec_num-2:elec_num+1,:]) )
		fer='%s*1.0 999*0.0' % (elec_num)
	return fer	

eigen=Eigenval(file_eigen)
ferexcited=get_fer(eigen, spinchannel, excited=True) # the excited spin channel
fernon_exc=get_fer(eigen, 1-spinchannel, excited=False) # the non-excited spin channel

if spinchannel == 0 : # spin up excitation
	changes['FERWE'] = ferexcited
	changes['FERDO'] = fernon_exc
else: # spin down excitation
	changes['FERWE'] = fernon_exc 
	changes['FERDO'] = ferexcited

# In[4]: Edit INCAR
removekey=[] # empty list
change_files=change_input_files(str(excited_folder))
change_files.incar_change(changes, popkey=removekey)
print('\nUSE VASP5! \nvasprun old')