#In[0]:
import sys
import os
from pathlib import Path
import shutil
sys.path.append(os.environ['SCRIPT'])
from class4_supercellgenerate import supercellgenerate


path=os.environ['PWD']+'/'
# Goal: generate or update a super folder. Will edit INCAR, KPOINTS, and POSCAR.perfect
# argv[1] is mode 1,2,3
#   mode 1: super folder created and files copied from unit, no need for arguments
#   mode 2: super folder created but files not copied, argv[2] necessary for unit folder name
#       argv[2]: the unit folder name, must be entered
#       argv[3]: cwd folder, no need to enter
#   mode 3: enter argv[2] necessary, argv[3] optional
#       argv[2]: the unit folder name, must be entered
#       argv[3]: the super folder name. If not entered, use the same name as argv[2]
#       No need to create the super folder or copy files. If created already, it will be deleted and created


##############################################################################
## wurtzite unit cell needs to be changed to orthogonal
### BeO 96-atom cell
# orthomatrix=[[1, 1, 0], [1, -1, 0], [0, 0, 1]];superarray=[3, 2, 2]
### BeO 288-atom cell
# orthomatrix=[[1, 1, 0], [1, -1, 0], [0, 0, 1]];superarray=[4, 3, 3]
### diamond 216 cell
# orthomatrix=[[1, 0, 0], [0, 1, 0], [0, 0, 1]];superarray=[3, 3, 3]
### GaSb
#orthomatrix=[[1, 0, 0], [0, 1, 0], [0, 0, 1]];superarray=[2, 2, 2]
### Na2Li3FeO4
#orthomatrix=[[1, 0, 0], [0, 1, 0], [0, 0, 1]];superarray=[2, 2, 2]
### LiSO3F
orthomatrix=[[1, 1, 0], [1, -1, 0], [0, 0, 1]];superarray=[2, 2, 2]
# PbTe
#orthomatrix=[[1, 0, 0], [0, 1, 0], [0, 0, 1]];superarray=[4, 4, 4]
##############################################################################



## Only get the supergenerate class here
if len(sys.argv)<2:
    print('Enter mode (1,2,3)\n'
    +' 1: super folder created and files copied. Will update INCAR KPOINTS POSCAR (POSCAR-->POSCAR.perfect)\n'
    +' 2: super folder created(the current folder), will copy files\n'
    +' 3: Enter unit folder, super folder. Make a perfect subfolder\n'
    )
    sys.exit()
elif str(sys.argv[1]) == '1': # update files in the current folder
    print('Update from the current folder\n\tShould rename the POSCAR to POSCAR.unit.vasp \n')
    folder_unit = path # not used
    folder_super = path 
    supergen=supercellgenerate(path, folder_unit,folder_super,from_poscar='POSCAR')
elif str(sys.argv[1]) == '2': # super folder generated, copy files from unit folder
    if len(sys.argv)>=3: 
        folder_unit = Path(sys.argv[2]) # unit folder
        folder_super = Path(path) # supercell folder is
    else:
        print('not enough arguments. Enter mode, unit folder.')
        sys.exit()
    supergen=supercellgenerate(path, folder_unit,folder_super) 
    supergen.supercopy(fromposcarfile='CONTCAR') # copy input files
elif str(sys.argv[1]) == '3':
    print('sys.argv[2] is unit folder name, sys.argv[3] is the current folder by default')
    if len(sys.argv)>=4: 
        folder_unit = Path(sys.argv[2]) # unit folder
        folder_super = Path(sys.argv[3]) # supercell folder
    elif len(sys.argv)==3:
        folder_unit = Path(sys.argv[2]) # unit folder
        # folder_unit.name is the last folder name, like 'yubi' in '/home/yubi'
        folder_super = path + folder_unit.name # supercell folder
    else:
        print('not enough arguments. Enter mode, unit folder, super folder name[optional].')
        sys.exit()
    # make sure the super folder is empty
    if os.path.isdir(folder_super):
        shutil.rmtree(folder_super) # remove old folder
    os.mkdir(folder_super) # make not folder
    supergen=supercellgenerate(path, folder_unit,folder_super) 
    supergen.supercopy(fromposcarfile='CONTCAR') # copy input files
else:
    print('mode not recognized!')
    sys.exit()



## INCAR, KPOINTS, Supercell POSCAR matrix
# edit input files: INCAR and KPOINTS
supergen.superincar(isdefect=False) # edit INCAR for supercell
supergen.superkpoints() # edit supercell KPOINTS to have only one kpoint
# edit POSCAR: generate a perfect supercell POSCAR
supergen.superposcar(orthomatrix=orthomatrix, superarray=superarray)
