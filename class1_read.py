# In[1]:

import numpy as np
import os
import sys
import re
import shutil
from pathlib import Path
import pymatgen as pmg
from pymatgen.io.vasp.outputs import BSVasprun, Vasprun, Eigenval
from pymatgen.electronic_structure.plotter import BSPlotter, BSDOSPlotter, DosPlotter

from collections import Counter
sys.path.append(os.environ['SCRIPT'])
from class0_functions1 import read_incar, find_files, parent_folder

# read_file_values: Read data
# rotate_read: Read the same data in many folders

# In[2]:

class read_file_values:
    # No POSCAR here. USE pymatgen to read POSCAR, KPOINTS!
    def __init__(self, folder):
        #if folder[-1] == '/':
        #    self.folder = folder
        #else:
        #    self.folder = folder + '/'
        #assert folder[-1] == '/', 'folder has to end with /'
        self.folder = str(Path(folder)) + '/'
        #hierdirecs = parent_folder(self.folder, hier=10)  # to find 1st-class materials folder
        #assert len(hierdirecs) <= 4
        #self.hierdirecs = hierdirecs
        #self.hier1direc = self.hierdirecs[0]    # 1st-class folder Eg. '/home/yubi/work/berylliumO/'
        #self.hier2direc = self.hierdirecs[1]    # 2nd-class folder Eg. '/home/yubi/work/berylliumO/wurtzite_2_Vo_HSE/'
        #self.sources_direc= self.hier1direc+'sources/' # Eg. '/home/yubi/work/berylliumO/sources/'

    def incar(self, para, var_format=float):
        # read the values of certain variables (came as a list)
        # return a list
        assert isinstance(para, list), 'read a list of variables: should keep the variable a list'
        dictionary = read_incar(self.folder)
        values=np.zeros(len(para))
        for j in range(len(para)):
            values[j]=var_format(dictionary[para[j]])
        return values

    def oszicar(self, folder=''):
        '''
        read energy from oszicar
        folder=self.folder by default
        '''
        energy=False
        if folder == '':
            folder = self.folder
        else:
            folder = str(Path(folder)) + '/'
        with open(folder+'OSZICAR', "r") as f:
            lines = f.readlines()
        for i in range(len(lines)-1, -1, -1):
            ll=lines[i].split()
            if 'E0=' in ll:
                energy = float(ll[ll.index('E0=')+1])
                break
        assert energy!=False,'This folder might not finish calculation because OSZICAR file does not have energy.'
        return energy

    def kpoints(self, first_k=True):
        ''' read kpoint grid
        '''
        from pymatgen.io.vasp.inputs import Kpoints
        grid=Kpoints.from_file(self.folder+'KPOINTS')
        grid=grid.kpts
        if first_k :
            return grid[0][0]
        else:
            return grid[0] # [3,3,3]

    def bandgap(self, fullinfo=False):
        run = BSVasprun(self.folder+"vasprun.xml", parse_projected_eigen=True)
        bs = run.get_band_structure(self.folder+"KPOINTS")
        bandgapinfo=bs.get_band_gap() # {'direct': True, 'energy': 11.126, 'transition': '(0.000,0.000,0.000)-(0.000,0.000,0.000)'}
        # bs.get_direct_band_gap()
        # bs.get_direct_band_gap_dict()
        energy=np.round(bandgapinfo['energy'],6) # keyword: energy
        print( 'bandgap info: %s \nvbm=%.4f\tcbm=%.4f' % (bandgapinfo, bs.get_vbm()['energy'], bs.get_cbm()['energy']) )
        if fullinfo:
            return bandgapinfo # bandgapinfo is a dictionary
        else:
            return energy 

    def cbm_vbm(self, c_v=0 , fullinfo=False):
        # if c_v = 0 ,return cbm; if c_v=1, return vbm
        run = BSVasprun(self.folder+"vasprun.xml", parse_projected_eigen=True)
        bs = run.get_band_structure(self.folder+"KPOINTS")
        if c_v == 0:
            info=bs.get_cbm() # there is the kpoint related to CBM
            # {'band_index': defaultdict(<class 'list'>, {<Spin.up: 1>: [192], <Spin.down: -1>: [192]}), 'kpoint_index': [0], 'kpoint': <pymatgen.electronic_structure.bandstructure.Kpoint>, 'energy': 13.286, 'projections': {}}
        elif c_v == 1:
            info=bs.get_vbm() # Valence band maximum
        else:
            print('Error! choose CBM (c_v=0) or VBM (c_v=1)')
        energy=np.round(info['energy'],6) # keyword: energy
        print('%senergy=%s'%(['CBM','VBM'][c_v], energy))
        if fullinfo:
            return info # bandgapinfo is a dictionary
        else:
            return energy 

    def fermienergy(self):
        run = BSVasprun(self.folder+"vasprun.xml", parse_projected_eigen=True)
        bs = run.get_band_structure(self.folder+"KPOINTS")
        return bs.efermi
    
    def read_single_eigenE(self,eigenvaltype='ho',spinpolarized=False):
        run = Eigenval(self.folder+'EIGENVAL')
        #run.eigenvalues #  dict of {(spin): np.ndarray(shape=(nkpt, nbands, 2))}.
        assert spinpolarized == False, 'Only read non-polarized spin values for now'
        from pymatgen import Spin
        spinupEnergy= run.eigenvalues[Spin.up] # format is (nkpt, nbands, 2) # len(2) is energy eigenvalue + occupation
        if eigenvaltype=='ho':
            # only look at spin up
            ## spinupEnergy[0] eigenvalues of gamma point, the first K point
            ## spinupEnergy[0,:,1] = [Occband1, Occband2,...]; 1 refers to occupations at gamma point
            ## spinupEnergy[0,:,0] = [Eband1, Eband2,...]; 0 refers to energies at gamma point
            numocc=sum(spinupEnergy[0,:,1]>0.1) # this number of bands are occupied
            print('The high occupied state at Gamma is ', spinupEnergy[0,numocc-1])
            # spinupEnergy[0,numocc-1,0] will be the energy of the highest occupied state
            return spinupEnergy[0,numocc-1,0] #np.round(,2)
            

    def lattice_para(self, structure=''):
        # return a list of unique lattice parameters
        # for certain structures, only need some parameters
        stru=pmg.core.Structure.from_file(self.folder+'CONTCAR')
        latt=stru.lattice
        a,b,c=latt.abc
        a,b,c = np.round([a,b,c], 6)
        print('From CONTCAR: a=%s,b=%s,c=%s' % (a,b,c))
        if structure=='wurtzite':
            assert np.round(a,5)==np.round(b,5), 'Error! not %s structure' % (structure)
            return [a,c], ['a','c']
        elif structure=='rock-salt':
            assert np.round(a,5)==np.round(b,5) and np.round(a,5)==np.round(c,5), 'Error! not %s structure' % (structure)
            return [a], ['a']
        else: # no symmetry, return all lattice parameters
            return [a,b,c], ['a','b','c']

    def get_u(self):
        u0=0.375
        stru=pmg.core.Structure.from_file(self.folder+'CONTCAR')
        speci=stru.species
        num_atoms=len(speci)
        #choose two atoms: atom 0 and the one in its z direction
        atom0=stru[0]
        for i in range(1,num_atoms):
            atom1=stru[i]
            diff_corrds= (atom0.coords - atom1.coords)[:2] # [x,y,z] -> [x,y]
            if np.all(np.round(diff_corrds,5)==[0.,0.]): # [x,y]==[0,0] in the z direction of atom 0
                u= round(stru.get_distance(0,i)/stru.lattice.c,7)
                print('From CONTCAR: atomX=%s \n\t\tatomY=%s'%(atom0,atom1))
                return u,(u-u0)/u0
                break


    def outcar(self, var='NELECT'):
        # Can use pymatgen to read it!!!
        with open(self.folder+'OUTCAR', 'r') as f:
            lines= f.readlines()
            found=0
            reNELECT = re.compile(var)
            for i in range(len(lines)):
                if reNELECT.search(lines[i]):
                    # if NELECT doesn't exist, it will give an error
                    nelect = float(lines[i].split('=')[1].split()[0]) # number of electrons
                    found=1
            assert found ==1, 'Parameter %s doesn\'t exist in OUTCAR' % var
        return nelect

    def outcar_dielec_const(self, tensor=False):
        '''
        This is to read dielectric constants from OUTCAR in unit cell with different parameters
        Need to save all the data in a single file to be used in other situations
        '''
        print('read OUTCAR from a 4-th class subfolder: dielec_eps/')
        filetoread='dielec_elec-part_eps/OUTCAR'
        if tensor:
            filetoread='OUTCAR'
        with open(self.folder+filetoread, 'r') as f:
            lines= f.readlines()
            found=0
            string='MACROSCOPIC STATIC '
            restring = re.compile(string)
            for i in range(len(lines)):
                if restring.search(lines[i]):
                    # if dielectric tensor doesn't exist, it will give an error
                    tensorstr=lines[i+2:i+5]
                    print('The dielectric tensor is %s' % tensorstr )
                    found=1
            assert found ==1, 'DIELECTRIC TENSOR doesn\'t exist in OUTCAR'
        diagonal=np.zeros(3)
        for j in range(len(tensorstr)):
            diagonal[j]=np.round(float(tensorstr[j].split()[j]), 6)
        print('diagonal eps matrix:', diagonal)
        if tensor:
            return diagonal # a list of three diagonal values
        else:
            eps=np.average(diagonal)
            return eps


    def read_formation_enthalpy(self,compound_folder, return_unit_formula=True):
        '''
        formation enthalpy for the unit of compound
        eg. BeO rock-salt has 4Be and 4O. The calculation should also return the energy of 4BeO
        '''
        compound_folder = str(Path(compound_folder))+'/'
        #if Path(compound_folder) == Path(self.folder):
        #    native = True ## compound_folder = self.folder # use the native compound like BeO by default. 
        #else:
        #    native = False # If another formation enthalpy, say Li2O, enter the folder to that compound
        # compound energy
        read_fil=read_file_values(compound_folder)
        compound_energy=read_fil.oszicar() #/formula[-1]

        # read compound species and formula to determine the folders of elemental phases
        struc=pmg.core.Structure.from_file(compound_folder+'POSCAR')
        speci=struc.species # speci[i] is Element Be; speci[i].name is 'Be'
        unique_speci=list(set(speci))
        coun=Counter(speci) # coun[speci[i]]=2 is the number of element atom
        num_elements=len(coun) # the number of species
        energies = np.zeros(num_elements) # initialize element energy list
        print(' compound formula=%s energy=%s' %(struc.formula,compound_energy))
        folder_header='energyf_'
        #if native:
        #    folder_header = 'enthalpyf_' # want ele_folder = enthalpy_Be/enthalpy_O
        #else:
        #    # split folder name to get impurity specie name 'energyf_Li_O'.split('_') = ['energyf', 'Li', 'O'], read Li
        #    impurity_specie = compound_folder.split('_')[1] # 'Li'
        #    folder_header = 'energyf_' # want ele_folder = energyf_Li/energyf_O

        # element energies
        formula=[]
        dict_elementformula= {}
        unique_speci_name = []
        for i in range(num_elements): # go over unique_speci
            # get the number of element atom from compound formula
            unique_speci_name.append(unique_speci[i].name)
            formula.append(int(coun[unique_speci[i]])) # coun[speci[i]]=2 is the number of element atom; formula=[2,2] or [4,4]
            dict_elementformula[unique_speci[i].name] = formula[-1] # compound formula: ith-species is unique_speci[i].name
            # element calculation
            ele_folder=self.folder+folder_header+ unique_speci[i].name+'/' # enthalpyf_Be/, enthalpyf_O/ #elements[i]
            ele_struc=pmg.core.Structure.from_file(ele_folder+'POSCAR')
            ele_formula=ele_struc.formula # like 'Be2','O2'; formula is to find the number of atoms in element[i] of enthalpyf_element[i]
            ele_formula_num=float(ele_formula.split(unique_speci[i].name)[-1]) # the number of atoms is 'Be2'.split('Be')[-1]
            read_fil=read_file_values(ele_folder) # initialize a class
            energies[i]= np.round(read_fil.oszicar() / ele_formula_num * formula[i],6) # make the number of element atoms agree with the compound formula
            print('  In %s, element %s with %s atoms has energy=%s' % (ele_folder.split('/')[-2],unique_speci[i].name, formula[i], energies[i]))
        enthalpy=np.round(compound_energy - np.sum(energies),6) # round to 3 decimal number
        if return_unit_formula:
            # return formation enthalpy per formula unit
            from class0_functions2 import hcf, get_printformula
            # calculate highest common factor and divide enthalpy by hcf
            formula_hcf=hcf(*formula) # hcf=highest common factor
            print('enthalpyf=%s for formula=%s and hcf=%s' % (enthalpy, get_printformula(dict_elementformula), formula_hcf))
            enthalpyf_unitformula = np.round(enthalpy / formula_hcf,6) # round to 3 decimals
            formula_unit =np.array(formula) / formula_hcf
            # edit number of atoms in dictionary
            elements = dict_elementformula.keys()
            for ele in elements:
                dict_elementformula[ele]=dict_elementformula[ele]/formula_hcf # make sure the formula is an integer
            print('enthalpy of formation per unit formula is %s eV\n' % (enthalpyf_unitformula) )
            return enthalpyf_unitformula, formula_unit,unique_speci_name, dict_elementformula
        else:
            from class0_functions2 import get_printformula
            print('enthalpy of formation for formula=%s is %s eV\n' % (get_printformula(dict_elementformula), enthalpy))
            formula = np.array(formula)
            return enthalpy, formula,unique_speci_name, dict_elementformula

    def read_delta_miu4energyf(self, condition,impurity_atomnames=[]):
        '''
        get delta miu that is used for formation energy
            condition = 'O-rich' # the condition in formation energy
            impurity_atomnames = ['Li','F','H']  # a list of impurity
        '''
        assert condition[-5:] == '-rich', 'assume condition must have one atom is rich'
        
        # (1): read formation enthalpy for native atoms' compound
        compound_native_folder = self.folder # read native material formation enthalpy by default
        enthalpyf_native,formula_native,speci_name_native,dict_elementformula_native = self.read_formation_enthalpy(compound_native_folder, return_unit_formula=True)
        formula_native = np.array(formula_native) # [1,1] for beryllium oxide with formula one Be and one O
        speci_name_native = np.array(speci_name_native) # ['O','Be']

        # (2): calculate delta miu (chemical potential) for native atoms # see tutorial_python_solveEq.py for how to solve equations
        # a. analyze condition to get which atom is rich 
        richatom=condition.split('-rich')[0] # get 'O' atom is rich
        # b. form a binary linear equation to be solve by numpy like x=np.linalg.solve(A,b) is the solution to Ax=b where A is coefficient matrix, x b are column vectors
        # b.1 get the second row in coefficient matrix, given by the rich atom condition
        rich_condition = np.zeros(len(speci_name_native))
        rich_condition[speci_name_native == richatom] = 1 # get the rich condition like 1*delta_O + 0*delta_Be = 0 
        # which is (1 0)*(delta_O delta_Be) = 0 here (1 0) is the second row in the coefficient matrix
        # b.2 form coefficient matrix and vector b
        A = np.stack([formula_native,rich_condition]) # the first row in A is the (1 1)*(delta_O delta_Be) = enthalpyf, the second row is the rich atom condition
        b = np.array([enthalpyf_native,0]).T
        # c. solve delta_O and delta_Be
        delta_miu_native = np.linalg.solve(A,b)
        # d. form a dictionary for delta miu
        dict_delta_miu = {}
        for i in range(len(speci_name_native)):
            dict_delta_miu[speci_name_native[i]] = delta_miu_native[i]
        print('Under condition %s, the native atoms have delta miu per atom = %s\n\n' % (condition, dict_delta_miu))

        # (3): solve delta miu for each impurity atom
        if len(impurity_atomnames)>=1:
            for impurity_atomname in impurity_atomnames:
                delta_mius_impurity = [0] # delta miu should not be positive. If positive, take it to 0
                compound_impurity_folder_header = 'energyf_' + impurity_atomname + '_' # 'energyf_Li_'
                compound_impurity_folders_all = find_files(self.folder, header=compound_impurity_folder_header, var='', remove=False)
                # like [energy_Li_O/, energy_Li_Be/]
                for compound_impurity_folder in compound_impurity_folders_all:
                    ## a. get constrains for impurity atom
                    #compound_impurity_folder = self.folder+'energyf_' + impurity_atomname + '_' + native_atom_i # assume only a two-atom compound is formed like Li2O, but not Li2BeO2
                    native_atom_i = compound_impurity_folder[:-1].split(compound_impurity_folder_header)[1].split('_')[0] #[:-1] is to remove '/'
                    compound_impurity_folder = self.folder+compound_impurity_folder
                    if os.path.isdir(compound_impurity_folder):
                        #read_fil = read_file_values(pwd)
                        enthalpyf_impurity,formula_impurity,speci_name_impurity,dict_elementformula_impurity = self.read_formation_enthalpy(compound_impurity_folder, return_unit_formula=True)
                        # b. calculate delta miu by constrain
                        # the condition is (1 2)*(delta_O delta_Li)=enthalpyf_impurity, delta_Li = (enthalpyf_impurity - delta_O*formula_O)/formula_Li
                        delta_mius_impurity.append( np.round((enthalpyf_impurity - dict_delta_miu[native_atom_i] * dict_elementformula_impurity[native_atom_i])/dict_elementformula_impurity[impurity_atomname],6) )
                    else:
                        print('folder %s does not exist' % (compound_impurity_folder)) # this line is no longer needed because folder names come from find_files now
                # c. select the smallest delta miu (chemical potential) for impurity atoms
                print('All constrains for impurity atom %s gives delta miu=%s' %(impurity_atomname, delta_mius_impurity) )
                dict_delta_miu[impurity_atomname] = np.round(np.min(delta_mius_impurity),6)
                print('impurity atom %s has delta miu=%s\n' %(impurity_atomname,dict_delta_miu[impurity_atomname]) )
        print('\tUnder condition %s, All delta miu = %s' % (condition,dict_delta_miu))
        return dict_delta_miu


    def eigenval_ho_lu(self,ho_lu=0):
        '''
        Read EIGENVAL to get the energies of the highest occupied (ho) states
        return the highest occupied energy and nband
        energyHOLO is used in Generalized-Koopmans' condition
        nband refers to the band where that electron occupied -> used in PCHARG calculation
        ho_lu whether we want highest occupied (ho) or lowest unoccupied (lu) energies: 0 for ho, 1 for lu
        '''
        ho_lu_energy = False
        with open(self.folder+'EIGENVAL', 'r') as f:
            lines=f.readlines()
            lines=lines[8:]
            for i in range(len(lines)):
                splited= np.array(lines[i].split()).astype(float)
                if np.any(splited[-2:] != np.array([1.,1.])):
                    #print('\n'+''.join(lines[i-1:i+1])[:-1]) # [:-1] is to remove '\n' at the end
                    # needs to modify for more generalized case
                    # so far only works when neutral has double electrons
                    if ho_lu==0: # highest occupied
                        # redefine splited to be the previous band with both electrons occupied
                        splited= np.array(lines[i-1].split()).astype(float) # First get the highested energy in splited. Then modify it if there is even higher energy occupied.
                        ho_lu_energy=max(splited[1],splited[2]) # splited[0] is line number, splited[1] is the first energy, splited[2] is the second energy
                        nband=int(splited[0]) 
                        splitted2=np.array(lines[i].split()).astype(float)
                        # if any state of the next band is occupied
                        # modify the energy to the correct position in the next band
                        if splitted2[-2]==1.: # find one higher occupied state 'lines[i]=193 6.397799 10.874288 1.000000 0.000000\n'
                            ho_lu_energy=splitted2[1] # energy should be 6.397799 
                            nband=int(splitted2[0])
                        elif splitted2[-1]==1:
                            ho_lu_energy=splitted2[2]
                            nband=int(splitted2[0])
                    elif ho_lu==1: # lowest unoccupied
                        splited= np.array(lines[i].split()).astype(float)
                        if splited[-2]==0. and splited[-1]==0.: # find one higher occupied state 'lines[i]=193 6.397799 10.874288 1.000000 0.000000\n'
                            ho_lu_energy=min(splited[1],splited[2])
                        elif splited[-1]==0.: # energy should be 6.397799   ho_lu_energy=splitted2[1]
                            ho_lu_energy=splited[2]
                        elif splited[-2]==0.: # in case the state energies are not in order
                            ho_lu_energy=splited[1]
                        else:
                            print('Error! Not finding the highest-lowest intersection')
                        nband=int(splited[0])
                    break
        assert ho_lu_energy != False, 'This folder might not finish calculation because EIGENVAL file does not have eigenenergies.'
        return [ho_lu_energy,nband]


    def locpot2freysoldt_correction(self,i, fol1,fol2, bulk=True,x_str='AEXX'):
        '''
        # the folders with different defects: fol1 is the reference, should be neutral; fol2 is positive
        Put LOCPOTs in the same folder and do Freysoldt correction
        1-generate a folder (to store LOCPOTS and run Freysoldt)
        2-copy LOCPOT from perfect_supercell
        3-obtain eps value from unit cell calculations
        4-run shell script 'sxdefectalign ..' to calculate Freysoldt correction
        '''
        #assert len(self.hierdirecs) == 3, 'You need to be in a 3rd class folder'
        #the LOCPOT files
        f1=fol1+'LOCPOT'
        f2=fol2+'LOCPOT'
        # generate a folder to run freysoldt calculation
        if bulk:
            new_folder = self.folder+'freysoldt%s_correction_ref-bulk/'% (i)
        else:
            new_folder = self.folder+'freysoldt%s_correction_ref-defect/'% (i)
        if os.path.isdir(new_folder): # remove old calculations
            shutil.rmtree(new_folder)
        os.mkdir(new_folder)
        os.system('cp %s %s' % (f1, new_folder+'neutralrefLOCPOT'))
        os.system('cp %s %s' % (f2, new_folder+'positiveLOCPOT'))
        # step 3
        dic=read_incar(fol2)

        hierdirecs = parent_folder(self.folder, hier=10)  # to find 1st-class materials folder
        self.hierdirecs = hierdirecs
        self.hier1direc = self.hierdirecs[0]    # 1st-class folder Eg. '/home/yubi/work/berylliumO/'
        self.hier2direc = self.hierdirecs[1]    # 2nd-class folder Eg. '/home/yubi/work/berylliumO/wurtzite_2_Vo_HSE/'
        self.sources_direc= self.hier1direc+'sources/' # Eg. '/home/yubi/work/berylliumO/sources/'

        sys.path.append(self.hier1direc)
        #import time
        #time.sleep(1) # pause to see the comment above
        if x_str == 'AEXX':
            # from 'AEXX_EPS.py' import y(eps values)
            from AEXX_EPS import x,y
        elif x_str == 'HFSCREEN':
            # from 'HFSCREEN_EPS.py' import y (=eps)
            from HFSCREEN_EPS import x,y
        else:
            print('Error! x_str is not recognized to find eps values. Should be either AEXX or HFSCREEN')
        print('\nAssume AEXX_EPS or HFSCREEN_EPS has been updated!!! \nEspecially HFSCREEN_EPS should come from the right AEXX file!!!')
        
        x_list=list(x)
        eps_list=list(y)
        this_x=float(dic[x_str])
        this_x=np.round(this_x,6)
        if this_x in x_list:
            this_eps=eps_list[x_list.index(this_x)]
        else:
            paras=np.polyfit(x,y,2) # paras=[a1,a2,a3]
            func = lambda a1,a2,a3,x: a1*(x**2) + a2*(x) + a3
            this_eps = np.round(func(*paras, this_x),6)
            print('Use fitting to get eps of corresponding AEXX')
        print('current %s=%s eps=%s' % (x_str,this_x, this_eps))

        # step 4 get defect position and run freysoldt correction
        defectcenter=read_incar(fol2, incar='DEFECT')['CENTER']
        os.chdir(new_folder)
        command='sxdefectalign --ecut 30 --charge -1 --eps %s --center %s --relative --vdef positiveLOCPOT --vref neutralrefLOCPOT --vasp ' % (this_eps, defectcenter)

        os.system(command+' > sx1.fc')
        with open('sx.sh', 'w') as f:
            f.write(command+'-C  > sx2.fc\ncat sx2.fc')
        os.system('chmod +x sx.sh')
        #os.system('cat sx1.fc')

    def get_DEFECT_info(self, para, var_format=float):
        # read the values of certain variables (came as a list)
        # can read 'EPS', 'FREYCORR', 'EPS_ELEC', 'CENTER'
        # return a list
        assert isinstance(para, list), 'read a list of variables: should keep the variable a list'
        dictionary = read_incar(self.folder, incar='DEFECT')
        values=np.zeros(len(para))
        for j in range(len(para)):
            values[j]=var_format(dictionary[para[j]])
        return values

    def poscar(self, variable):
        print('read CONTCAR')
        from pymatgen.core import Structure
        mypos = Structure.from_file(self.folder+'CONTCAR')
        if variable == 'abc' or variable == 'a':
            # return lattice constant a
            # abs: assume lattice constant is the same for a,b,c. Return any of it
            return mypos.lattice.a #'a'
        elif variable == 'bond_length':
            return mypos.get_distance(0,1) # the bond length between atom 0 and atom 1
        elif variable == 'volume':
            return np.round(mypos.lattice.volume,6)
        else:
            print('Error! variable %s in POSCAR not recognized' % (variable) )

class rotate_read:
    def __init__(self, folder):
        '''
        self.folder: the working folder
        '''
        #if folder[-1] != '/':
        #    folder=folder+'/' # make sure the folder has correct form: ....../INCAR
        #self.folder=folder
        self.folder = str(Path(folder)) + '/'
        #hierdirecs = parent_folder(self.folder, hier=10)  # to find 1st-class materials folder
        #assert len(hierdirecs) <= 3, 'You are in the son folder. Go back to the parent folder! Otherwise another son folder will be created'
        #self.hierdirecs = hierdirecs
        #self.hier1direc = self.hierdirecs[0]    # 1st-class folder Eg. '/home/yubi/work/berylliumO/'
        #self.hier2direc = self.hierdirecs[1]    # 2nd-class folder Eg. '/home/yubi/work/berylliumO/wurtzite_2_Vo_HSE/'
        #self.sources_direc= self.hier1direc+'sources/' # Eg. '/home/yubi/work/berylliumO/sources/'

    def rotate(self, x_str, y_str, header,var,middle='',avoid=''):
        '''
        x_str = 'AEXX', 'ENCUT', 'HFSCREEN'
        y_str = 'energyHO','energyLU', 'eps', 'equil_energy', 'run_freysoldt', 'freysoldt_corr'
        '''
        existed_folders=find_files(self.folder, header=header, var=var,middle=middle, remove=False,avoid=avoid)
        existed_folders = sorted(existed_folders)
        print('The folders satisfied the pattern: \n',existed_folders)
        # rotate between folders
        #existed_folders=['defect9_AEXX/', 'defect10_AEXX/', 'defect11_AEXX/', 'defect12_AEXX/']# currently only have this data!!!
        xx=np.zeros(len(existed_folders))
        yy=[]
        for i in range(len(existed_folders)):
            # name of each sub-folder
            fol_name= self.folder + existed_folders[i]
            os.chdir(fol_name)
            read_fil=read_file_values(fol_name)
            if x_str == 'AEXX' or x_str == 'ENCUT' or x_str == 'HFSCREEN':
                xx[i]=read_fil.incar([x_str])
            if y_str == 'energyHO':
                # the energies of the highest occupied states
                #returns a list yy[i] = [energyHOLU, nband]
                charge=int(existed_folders[i].split('e_')[0][-1])
                yy.append(read_fil.eigenval_ho_lu(ho_lu=0))
            elif y_str == 'energyLU':
                # the energies of the highest occupied states
                #returns a list yy[i] = [energyHOLU, nband]
                charge=int(existed_folders[i].split('e_')[0][-1])
                yy.append(read_fil.eigenval_ho_lu(ho_lu=1))
            elif y_str == 'eps':
                yy.append(read_fil.outcar_dielec_const())
            #elif y_str == 'koopmansEdiff':
            #    yy.append(read_fil.koopmans_energylevel())
            elif y_str == 'equil_energy': # equilibrium energy
                yy.append(read_fil.oszicar())
            elif y_str == 'run_freysoldt':
                read_fil=read_file_values(self.folder) # create folders in self.folder, not in the subfolder: fol_name 
                ind=int(existed_folders[i].split('koopmans')[1].split('_charge')[0])
                bulk=False
                if bulk:
                    fol1=self.folder+'perfect_supercell/'
                else:
                    assert '1e_' in fol_name, 'The folder name is not right. It should contain 1e_'
                    fol1=fol_name.replace('1e_','0e_')
                #read_fil.locpot2freysoldt_correction(ind, fol1, fol_name,bulk=False)
                read_fil.locpot2freysoldt_correction(ind, fol1, fol_name,bulk=bulk,x_str=x_str)
                yy.append(0.0)
            elif y_str == 'freysoldt_corr':
                yy.append( read_fil.get_DEFECT_info(['FREYCORR'], var_format=float)[0] )
            print('%s=%f %s=%s' % (x_str,xx[i], y_str, yy[i]))
        yy=np.array(yy)
        xx=np.round(xx,6)
        yy=np.round(yy,6)
        os.chdir(self.folder) # go back to the original folder
        return xx,yy

