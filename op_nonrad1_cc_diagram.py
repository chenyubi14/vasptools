# In[0]:
import os
import sys
import numpy as np
from pathlib import Path
from shutil import copyfile, rmtree
from pymatgen.core import Structure
from nonrad.ccd import get_cc_structures
pwd = os.environ['PWD'] + '/'
sys.path.append(os.environ['SCRIPT'])
from class2_update_input import change_input_files
from class0_functions1 import pythonsubmit, read_incar
from class0_functions3 import write_DEFECT

# Use this file for configuration coordinate diagram
# Use draw_nonrad1_cc_diagram for plotting its result.

# In[1]:
submit=5 #0 for don't submit, 1 for SUBMIT, 2 for large, 3 for old, 4 for many, 5 for short, 6 for single, 7 for test
# I have tested that vasp6 also works. No need to use submit=3 now. Can use submit=5
# use submit=1 normal SUBMIT

print('\n\tNote Expanse excited states may not work!!!\n')

# In[2]:
def extract_charge(folname):
    # get the charge between 'defect' and 'e' inside 'defect-1e'
    return folname.split('defect')[-1].split('/')[0] #.split('e')[0]

def makefolderformat(folname):
    if folname[-1] != '/':
        folname=folname+'/'
    return folname

# (1) initialize folder names (folname)
#print(len(sys.argv))
if len(sys.argv) <= 2:
    print('Error! Which charge defects do you want to use? Enter ground_folder, excited_folder, cheap')
    print('Enter any argument for a cheap calculation')
    sys.exit()

cheap=False
if len(sys.argv) == 4:
    cheap=True

ground_folname = pwd + makefolderformat(sys.argv[1]) #'defect0e'
excited_folname = pwd + makefolderformat(sys.argv[2]) #'defect-1e'
cc_folname = 'cc_%s_%s' % (extract_charge(ground_folname), extract_charge(excited_folname))
# put files in cc_0e_-1e

# (2) read equilibrium structures from your first-principles calculation
ground_files = Path(ground_folname)
ground_struct = Structure.from_file(str(ground_files / 'CONTCAR'))
excited_files = Path(excited_folname)
excited_struct = Structure.from_file(str(excited_files / 'CONTCAR'))

# (3) output directory that will contain the input files for the CC diagram
cc_dir = Path(cc_folname)
if os.path.isdir(cc_dir): # remove existing folders
    print('The old %s file exists. Do you want to remove it? y for yes remove. Other for no' % cc_folname)
    removeoldfile=input()
    if removeoldfile == 'y' or removeoldfile == 'yes':
        rmtree(cc_dir)
    else:
        print('exit for not planning to remove old folders')
        sys.exit()
os.mkdir(str(cc_dir))
os.mkdir(str(cc_dir / 'ground'))
os.mkdir(str(cc_dir / 'excited'))
# displacements as a percentage, this will generate the displacements
# -50%, -37.5%, -25%, -12.5%, 0%, 12.5%, 25%, 37.5%, 50%
if cheap == False:
    displacements = np.linspace(-0.5, 0.5, 9) # gives 9-1 jobs to run
else:
    displacements = np.linspace(-0.3, 0.2, 2) # gives 3-1 jobs to run
remove_zero=True

# (4) dump the vasp files into folders of ground and excited, their POSCAR will be changed with the displacements
# note: the returned structures won't include the 0% displacement by remove_zero=True, this is intended
# the 0% folder can be included by specifying remove_zero=False
ground, excited = get_cc_structures(ground_struct, excited_struct, displacements,remove_zero=remove_zero)
def create_jobs(pmg_stru, subfolname, subfol_files, submit,displacements):
    # pmg_stru: ground or excited subfolder
    # For a subforder copy vasp input files and remove NSW
    assert subfolname == 'ground' or subfolname == 'excited'
    if remove_zero:
        displacements=displacements[displacements!=0.]
    for i, struct in enumerate(pmg_stru):
        working_dir = cc_dir / subfolname / str(i)
        os.mkdir(str(working_dir))
        # write structure and copy necessary input files
        struct.to(filename=str(working_dir / 'POSCAR'), fmt='poscar')
        for f in ['KPOINTS', 'POTCAR', 'INCAR', 'submit.job', 'DEFECT', 'WAVECAR']:
            copyfile(str(subfol_files / f), str(working_dir / f))
        print('edit INCAR to remove NSW in %s' % (working_dir))
        working_dir = pwd / working_dir
        os.chdir(working_dir)
        change_files=change_input_files(str(working_dir))
        change_files.incar_change({'NELM':200,'ISTART':1,'ICHARG':0},popkey=['NSW']) # remove NSW and edit INCAR
        defect_dictionary=read_incar(working_dir,incar='DEFECT')
        defect_dictionary['DISPLACEMENT'] = '%s' % (round(displacements[i],3))
        write_DEFECT(str(working_dir), defect_dictionary)
        #os.system('echo DISPLACEMENT=%s >> DEFECT'%(round(displacements[i],3))) # this does not remove the previous DISPLACEMENT argument
        os.system('chmod +x DEFECT KPOINTS POTCAR POSCAR INCAR')
        pythonsubmit(str(working_dir), submit)
        os.chdir(pwd)

create_jobs(ground, 'ground', ground_files, submit, displacements)
create_jobs(excited, 'excited', excited_files, submit, displacements)

#for i, struct in enumerate(excited):
#    working_dir = cc_dir / 'excited' / str(i)
#    os.mkdir(str(working_dir))
#    # write structure and copy necessary input files
#    struct.to(filename=str(working_dir / 'POSCAR'), fmt='poscar')
#    for f in ['KPOINTS', 'POTCAR', 'INCAR', 'submit.job']:
#        copyfile(str(excited_files / f), str(working_dir / f))
#    os.chdir(working_dir)
#    print('edit INCAR to remove NSW in %s' % (cc_dir / 'ground' / str(i)))
#    change_files=change_input_files(pwd)
#    change_files.incar_change({},popkey=['NSW'])
#    os.chdir(pwd)
