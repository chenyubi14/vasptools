import numpy as np
import os
import sys
from class0_incar import write_INCAR
from class0_functions1 import read_incar
from class0_functions2 import struc2poscar
from class1_read import read_file_values
from pymatgen.core import Structure
import re
import copy
from pathlib import Path

# Functions in this file edit VASP input files
# How to edit files (the details of changes) should be determined based on actual cases.

class change_input_files:
    def __init__(self, folder):
        '''
        self.folder: the working folder
        '''
        #if folder[-1] != '/':
        #    folder=folder+'/' # make sure the folder has correct form: ....../INCAR
        self.folder=str(Path(folder))+'/'
        self.incar_dict_template = read_incar(self.folder) # incar_dict_template: the dictionary from INCAR in self.folder

    def incar_change(self, changes, popkey=[], defect_charge=0):
        '''
        changes should be a dictionary or update the old INCAR to new INCAR format

        change_files=change_input_files(folder)
        chage_files.incar_upgrade()
        defect_charge is charge of defect. Opposite to change NELECT which add electrons.
        '''
        assert isinstance(changes, dict), 'The changes should came as a dictionary'
        new_dict={**self.incar_dict_template, **changes} # same keyword: follow changes, new keyword: added from changes
        if defect_charge!=0:
            print('change NELECT+=%s' % (-defect_charge))  
            if os.path.exists(self.folder+'OUTCAR')==False:
                print('OUTCAR not available. Reset the folder')
                sys.exit()
            read_var = read_file_values(self.folder)
            nelect = read_var.outcar('NELECT')
            nelect = int(nelect - defect_charge)
            new_dict['NELECT'] = nelect
        assert isinstance(popkey, list)
        for i in range(len(popkey)):
            new_dict.pop(popkey[i], None)
        # this is the benefit of dictionaries: no need 
        write_INCAR(self.folder, new_dict)
    
    def poscar_change(self, defects=[4,88], neighbordistance=1.7, poscar='POSCAR.perfect',remove=True, savefilname='POSCAR.defect', perturb=0.05, modelcwd=True):
        '''
        PERTURB the nearest neighbors of the defects and REMOVE defects
        defects should be an array of site indices
        e.g. defect=[4,88] means (4+1)th Be and (88+1)th O
        '''
        assert type(defects) == list, 'The defect index should enter as a list'
        assert len(np.shape(defects))==1 , 'The defect position has a wrong format'
        self.pmg_struc=Structure.from_file(self.folder+poscar) # read structure from POSCAR
        sites=self.pmg_struc.sites ## struc.sites gives all sites of this structure
        # write the defect information in the notes.atomlabel.readme file
        if modelcwd==False: # mode 1, copy to subfolder
            os.system('cp ../model.notes.atomlabel.readme ./notes.atomlabel.readme')
            os.system('cp ../model.DEFECT ./DEFECT')
        else: # mode 2, copy models to this current folder
            os.system('cp ./model.notes.atomlabel.readme ./notes.atomlabel.readme')
            os.system('cp ./model.DEFECT ./DEFECT')
            #assert os.path.exists('DEFECT'), 'DEFECT should be present'
            #assert os.path.exists('notes.atomlabel.readme'), 'notes.atomlabel.readme should be present'
        model_notes_atom=open(self.folder+'notes.atomlabel.readme', 'a')
        model_defect=open(self.folder+'DEFECT','a')
        #model_defect_file=open()
        for defectind in defects:
            defectsite = sites[defectind]
            model_notes_atom.write('The defect atom is %s%s %s. Perturbing neighbors...\n' % (defectsite.species_string, defectind, defectsite.frac_coords))
            # neighbors of each defect to be perturbed, also perturb the defect site
            defectneighbors=self.pmg_struc.get_neighbors(defectsite,neighbordistance) + [defectsite]
            # perturb the positions of defect neighbors
            neighborindices=np.array([])
            for neighbor in defectneighbors:
                old_coord=copy.deepcopy(neighbor.frac_coords)
                neighborind = self.pmg_struc.index(neighbor)
                neighborindices=np.append(neighborindices,neighborind)
                # Translate specific sites by some vector, keeping the sites within the unit cell.
                this_perturb=(np.random.rand(3)-0.5)*perturb 
                self.pmg_struc.translate_sites(neighborind, this_perturb) # perturb=0.02 -> change 1% of lattice frac_coords
                model_notes_atom.write('%s%s %s -> %s\n' % (neighbor.species_string, neighborind, old_coord, self.pmg_struc[neighborind].frac_coords))
                #print('After: %s%s %s \n' % (self.pmg_struc[neighborind].species_string, neighborind, self.pmg_struc[neighborind].frac_coords))
            neighborindices = neighborindices[:-1] # remove the defect itself
            # old_coord will be the unperturbed defect site
        if remove: # Delete sites with at indices. Argument is a list.
            self.pmg_struc.remove_sites(defects) # No longer perfect
            model_notes_atom.write('Neighbors of pymatgen index in perfect supercell %s\n' % (' '.join(neighborindices.astype(int).astype(str)  ) ) )
            model_notes_atom.write('Neighbors of vesta index in perfect supercell %s\n' % (' '.join( (neighborindices+1).astype(int).astype(str)  ) ) )
            model_notes_atom.write('Neighbors by NLINE in perfect supercell %s (maybe different from NLINE in DEFECT due to removed atoms vacancy)\n' % (' '.join( (neighborindices+10).astype(int).astype(str)  ) ) )
            neighborindices = neighborindices - 1*(defects[0]<neighborindices)
            model_defect.write('CENTER=%s\n' % (','.join( list( np.round(defectsite.frac_coords,4).astype(str) ) ) ) )
            model_defect.write('NLINE=neighbor %s\n' % (' '.join( list( (neighborindices+10).astype(int).astype(str) ) ) ) )
            model_defect.write('POSITIONNORELAX=%s\n' % (' '.join( list( np.round(old_coord,4).astype(str) ) ) ) )
        else:
            model_defect.write('CENTER=%s\n' % (','.join( list( np.round(defectsite.frac_coords,4).astype(str) ) ) ) )
            model_defect.write('NLINE=%s\n' % ( self.pmg_struc.index(defectsite)+10 ) )
            model_defect.write('POSITIONNORELAX=%s\n' % (' '.join( list( np.round(old_coord,4).astype(str) ) ) ) )
        # output POSCAR in self.folder
        struc2poscar(self.pmg_struc, self.folder, fname=savefilname)
        return defectsite, defectneighbors

    def contcar2poscar(self):
        '''
        Update POSCAR with CONRCAR
        Remove the extra points of CONTCAR
        '''
        with open(self.folder+'CONTCAR', 'r') as f:
            lines= f.readlines()
            string=' \n'
            for i in range(len(lines)):
                if lines[i]==string:
                    print(lines[i-2:i+2])
                    lines_new=lines[:i]
                    print(lines_new[-1])
                    break
        with open(self.folder+'POSCAR', 'w') as f:
            f.write(''.join(lines_new))

