# In[0]:
import os
import sys
import numpy as np
from pathlib import Path
from shutil import copyfile, rmtree
from pymatgen.core import Structure
from nonrad.ccd import get_cc_structures
sys.path.append(os.environ['SCRIPT'])
from class2_update_input import change_input_files
from class0_functions1 import read_incar
from class0_functions3 import write_INFO

pwd = Path(os.environ['PWD'] )

# Use this file for generating linear displacements between two structures



###########################################################
# (1) initialize arguments 
#print(len(sys.argv))
if len(sys.argv) <= 2:
    print('Error! Enter two arguments for folders with two structures ')
    print('Enter any one more argument for cheap calculations')
    sys.exit()

###########################################################
cheap=False
remove_zero=False
folersname = 'disp_folders'
###########################################################

stru1_folname = pwd / sys.argv[1] 
stru2_folname = pwd / sys.argv[2] 

if not stru1_folname.is_dir():
    print('1st argument is not a folder')
    sys.exit()
if not stru2_folname.is_dir():
    print('2st argument is not a folder')
    sys.exit()
if len(sys.argv) >= 4:
    cheap=True
# displacements as a percentage, this will generate the displacements
if cheap == False:
    displacements = np.linspace(0.0, 1.0, 11).round(5)
else:
    displacements = np.linspace(0.0, 1.0, 6).round(5)


###########################################################
# (2) read structures from first-principles calculations
print('Read structures from POSCAR')
stru1_files = Path(stru1_folname)
stru1_struct = Structure.from_file(str(stru1_files / 'POSCAR'))
stru2_files = Path(stru2_folname)
stru2_struct = Structure.from_file(str(stru2_files / 'POSCAR'))

###########################################################
# (3) output directory that will contain the input files for the CC diagram
folderdir = Path(folersname)
#if os.path.isdir(folderdir): # remove existing folders
if folderdir.is_dir():
    print('The old %s file exists. Do you want to remove it? y for yes remove. Other for no' % folersname)
    removeoldfile=input()
    if removeoldfile == 'y' or removeoldfile == 'yes':
        rmtree(folderdir)
    else:
        print('exit for not planning to remove old folders')
        sys.exit()
os.mkdir(str(folderdir))

###########################################################
# (4) dump the vasp files into folders of ground and excited, their POSCAR will be changed with the displacements
# note: the returned structures won't include the 0% displacement by remove_zero=True, this is intended
# the 0% folder can be included by specifying remove_zero=False
def interpolate_stru(
        stru1: Structure,
        stru2: Structure,
        displacements: np.ndarray,
        remove_zero: bool = False
        ) -> list:
    ''' Generate interpolated structures between two POSCARs

    Parameters
    ----------
    stru1: pymatgen.core.structure.Structure
        pymatgen structure corresponding to the initial state
    stru2: pymatgen.core.structure.Structure
        pymatgen structure corresponding to the final state
    displacements : list(float)
        list of displacements to compute the perturbed structures
    remove_zero : bool
        remove 0% displacement from list (default is False)

    Returns
    -------
    structures: list(pymatgen.core.structure.Structure)
        a list of pymatgen structures 
    '''
    if remove_zero:
        displacements = displacements[displacements != 0.]
    structures=stru1.interpolate(stru2, nimages=displacements, interpolate_lattices=True)
    return structures

structures=interpolate_stru(stru1_struct, stru2_struct, displacements, remove_zero=remove_zero)

def create_jobs(
        pmg_stru: list, 
        vasp_inputs_dir: Path, 
        displacements: np.ndarray,
        folderdir: Path,
        remove_zero: bool = False
        ):
    ''' For each structure, create folders, and prepare VASP inputs

    Parameters
    ----------
    pmg_stru: list(pymatgen.core.structure.Structure)
        a list of strucutres as POSCARs
    vasp_inputs_dir: pathlib.Path
        the path to vasp inputs
    displacements: list(float)
        displacements corresponding to structures
    folderdir: pathlib.Path
        create jobs in this folder
    remove_zero : bool
        remove 0% displacement from list (default is False)
    '''
    # For a subforder copy vasp input files and remove NSW
    num_digits = int(np.log10(len(displacements)+1)) + 1
    if remove_zero:
        displacements=displacements[displacements!=0.]
    for i, struct in enumerate(pmg_stru):
        working_dir = pwd / folderdir / ( 'strain%s_%s' % ( str(i).zfill(num_digits), displacements[i]) )
        os.mkdir(str(working_dir))
        # write structure and copy necessary input files
        struct.to(filename=str(working_dir / 'POSCAR'), fmt='poscar')
        for f in ['KPOINTS.super','KPOINTS', 'POTCAR', 'INCAR', 'submit.job']: 
            # No need to copy WAVECAR
            copyfile(str(vasp_inputs_dir / f), str(working_dir / f))
        print('edit INCAR to freeze structure in %s' % (working_dir))
        # Enter each directory, edit INCAR, SAVEINFO
        os.chdir(working_dir)
        change_files=change_input_files(str(working_dir))
        change_files.incar_change({'NSW':0,'ISIF':2,'IBRION':-1}) # remove NSW and edit INCAR
        info_dictionary={} 
        info_dictionary['DISPLACEMENT'] = '%s' % (round(displacements[i],3))
        write_INFO(info_dictionary, str(working_dir), incarname='SAVEINFO')
        os.system('chmod u+x SAVEINFO KPOINTS POTCAR POSCAR INCAR')
        os.chdir(pwd)

create_jobs(structures, stru1_files, displacements, folderdir, remove_zero=remove_zero)

