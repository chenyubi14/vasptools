#!/usr/bin/env python
import sys
import os
from pathlib import Path
import shutil
sys.path.append(os.environ['SCRIPT'])
from class4_supercellgenerate import supercellgenerate

## Don't edit defect by hand, use remove=True below
## Edit antisites in POSCAR by hand
## Then perturb its neighbors by the following setup
#############################################################################################
#defects=[13] # a list of atoms for vacancy and antisites
#neighbordistance=2.8 # perturb atoms within this range near the "defect"
#remove=True # False: only perturb, do not remove; True: remove to create a vacancy

### V_Li in Na2Li3FeO4
defects=[81];neighbordistance=2.8;remove=True
### V_Li in LiSO3F
#defects=[13];neighbordistance=2.8;remove=True
### V_Be in BeO 
#defects=[30];neighbordistance=1.7;remove=True
### V_Be in BeO 
#defects=[121];neighbordistance=1.7;remove=True
### V_O in BeO 
#defects=[67];neighbordistance=1.7;remove=True
### Be_O in BeO 
#defects=[144];neighbordistance=2.0;remove=False
### O_Be in BeO 
#defects=[143];neighbordistance=2.0;remove=False
### V_O in ZnO
#defects=[55];neighbordistance=2.0;remove=True
### Cu in ZnO
#defects=[0];neighbordistance=2.0;remove=True
#############################################################################################


#In[1]:
path=os.environ['PWD']+'/'
# Goal: perfect supercell finished, generate a defect folder from perfect
# the defect should be vacancy or antisites. The neighbors of the defect will be perturbed.
# argv[1] is mode 1,2
#   mode 1: files copied, only update the POSCAR from POSCAR.perfect.vasp to defect
#   mode 2:  start from a perfect supercell subfolder and generate a defect source subfolder with the same name
#       argv[2]: the perfect folder name, must be entered
#       argv[3]: the defect folder name. If not entered, use the same name as argv[2]

need_exit = False
if not os.path.exists('model.SAVEINFO'):
    with open('model.SAVEINFO','w') as fil:
        fil.write('material=\nDEFECTTYPE=\nDISPLACEMENT=0\nCOMMENT=\ntitle=\nlegend=\n')
    need_exit = True
#assert os.path.exists('SAVEINFO'), 'SAVEINFO should be present'
if not os.path.exists('model.notes.readme'):
    os.system('touch model.notes.readme')
    need_exit = True
if need_exit:
    print('Edit model.notes.readme and model.SAVEINFO')
    sys.exit()

#In[2]:
if len(sys.argv)<2:
    print('Use the first argument to determine which mode (1 or 2) to use\n'
        +' 1:assumes files already copied, and only updates the POSCAR from this file POSCAR.perfect.vasp'
        +' 2:start from a perfect supercell subfolder and generate a defect source subfolder with the same name\n'
	)
    sys.exit()
elif str(sys.argv[1])=='1':
    print('assumes files are copied. Only update the POSCAR from perfect to defect')
    poscarname= 'POSCAR.perfect.vasp'
    folder_from = path # not used
    folder_to = path
    supergen=supercellgenerate(path,folder_from,folder_to,from_poscar=poscarname)
    supergen.defectposcar(defects=defects,neighbordistance=neighbordistance,remove=remove,modelcwd=True) 
elif str(sys.argv[1]) == '2':
    if len(sys.argv)>=4: 
        folder_from = Path(sys.argv[2]) # from perfect folder
        folder_to = Path(sys.argv[3]) # to defect folder
    elif len(sys.argv)==3:
        folder_from = Path(sys.argv[2]) # from perfect folder
        folder_to = path + folder_unit.name # to defect folder
    poscarname= 'POSCAR.perfect.vasp'
    # make sure the new folder is empty
    if os.path.isdir(folder_to):
        shutil.rmtree(folder_to) # remove old folder
    os.mkdir(folder_to)
    supergen=supercellgenerate(path,folder_from,folder_to,from_poscar=poscarname) # poscar is name of POSCAR in folder_to
    supergen.supercopy(poscarname) # copy input files
    supergen.superincar(isdefect=True)  # edit INCAR, symmetry on
    supergen.defectposcar(defects=defects,neighbordistance=neighbordistance,remove=remove,modelcwd=False) 
else:
    print('First argument %s not recognized' % (sys.argv[1]) )
    sys.exit()

print('Note! Double check CENTER in SAVEINFO')
