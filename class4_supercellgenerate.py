
from pymatgen.core import Structure
import sys
import os
import shutil
sys.path.append(os.environ['SCRIPT'])
from class0_functions1 import find_files, read_incar
from class0_functions2 import struc2poscar
from class2_update_input import change_input_files
from pathlib import Path

class supercellgenerate:
    def __init__(self, path, folder_from, folder_to, from_poscar='POSCAR.unit.vasp'):
        # folder_from=the folder for unitcell; folder_to=the folder for supercell
        # from_poscar is the poscar file copied from folder_from, this is to have a copy saved
        self.folder_from = Path(folder_from)
        self.folder_to = Path(folder_to)
        # POSCAR: either unit POSCAR or POSCAR.perfect (perturb defect for a perfect superlattice)
        self.path= Path(path) # 1st class folder # for returning back to the pwd, thus avoid problems to moving between folders
        self.from_poscar=from_poscar # POSCAR to read
        print('The POSCAR to edit is %s at %s' % (from_poscar, folder_to))
    
    def supercopy(self,fromposcarfile='CONTCAR'):
        # copy input files
        list_files = ['KPOINTS', 'POTCAR', 'INCAR']
        for f in list_files:
            shutil.copyfile(str(self.folder_from / f), str(self.folder_to / f))
            os.system('chmod +x %s' % (self.folder_to/f))
        if not os.path.exists(self.folder_from / fromposcarfile):
            print('poscarfile not exist')
            sys.exit()
        shutil.copyfile(str(self.folder_from / fromposcarfile), str(self.folder_to / self.from_poscar))
        print('copied %s' % fromposcarfile)


    def superaexx(self):
        '''
        AEXX_bandgap file created by var_test 
        AEXX_bandgap should have the format of INCAR file
        '''
        # change AEXX to the pre-saved value which is obtained by fitting
        # call supergen.superaexx() # read AEXX from stored file
        f_folder=str(self.folder_from / 'savedDATA') + '/'
        fname=find_files(f_folder, header='AEXX_bandgap=', var='eV', remove=False)
        #sys.path.append(self.folder_from)
        if len(fname)==1:
            fname=fname[0]
        else:
            print('Error! multiple AEXX values or none AEXX available!\n folders found are %s ' % (fname))
        dictionary = read_incar(f_folder, allkeyword=0, incar=fname) # will get like {AEXX: 0.25}
        self.aexx=dictionary['AEXX']
        print('AEXX to be used is %s' % (self.aexx))

    def superposcar(self, orthomatrix=[[1, 1, 0], [1, -1, 0], [0, 0, 1]],superarray=[3, 2, 2]):
        '''
        generate POSCAR of a super perfect lattice
        edit only under folder_to
        '''
        self.struct = Structure.from_file(self.folder_to / self.from_poscar)
        ortho = self.struct * orthomatrix
        superstruct = ortho * superarray
        struc2poscar(superstruct, self.folder_to, 'POSCAR.perfect.vasp')
        print('generate POSCAR.perfect.vasp')
    
    def superincar(self, isdefect=True):
        #print('Will modify INCAR. If error, please have an INCAR in the new folder first')
        change_files=change_input_files(self.folder_to)
        if isdefect: # 'AEXX': self.aexx,
            # for defect, turn on the symmetry
            change_files.incar_change({'SYSTEM':'super', 'ISIF':2, 'ISPIN':2, 'NSW':200, 'NELM':200, 'LVHAR':'.TRUE.'},popkey=['LCALCEPS', 'ISTART', 'ICHARG', 'KPAR', 'NELMIN', 'ISYM','MAGMOM'])
        else:
            # for a perfect supercell, turn off the symmetry
            change_files.incar_change({'SYSTEM':'super', 'ISIF':2, 'ISPIN':2, 'NSW':200, 'NELM':200, 'LVHAR':'.TRUE.'},popkey=['LCALCEPS', 'ISTART', 'ICHARG', 'KPAR', 'NELMIN', 'ISYM','MAGMOM'])

    def defectposcar(self, defects=[88],neighbordistance=1.7,remove=True,savefilname='POSCAR.defect',modelcwd=True):
        # copymodelfiles=False means copy current folder's model files; True means copy from the previous higher folder
        # PERTURB the nearest neighbors of the defects and REMOVE defects
        change_files=change_input_files(self.folder_to)
        change_files.poscar_change(defects, neighbordistance, poscar=self.from_poscar, remove=remove,savefilname=savefilname,modelcwd=modelcwd)
        os.chdir(self.folder_to)
        os.system('cp %s POSCAR' % (savefilname))
        os.system('chmod +x POSCAR')
        os.chdir(self.path)

    def superkpoints(self):
        with open(self.folder_to / 'KPOINTS','w') as f:
            f.write('Supercell cell needs only one KPOINT\n0\nGamma\n1 1 1\n')
#def insert_atom_position_line(pos1,pos2,dist)
