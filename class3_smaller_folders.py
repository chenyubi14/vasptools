import numpy as np
import sys
import os
import shutil
from funcy import project

sys.path.append(os.environ['SCRIPT'])
from class1_read import read_file_values
from class2_update_input import change_input_files
from class0_functions1 import parent_folder, find_files, read_incar, pythonsubmit
from class0_functions2 import generate_KPATH

# functions in this file should modify folders, not just read data or edit files
# however, the details of changes should be included based on each case


class smaller_folders:
    def __init__(self, folder):
        '''
        self.folder: the working folder
        most functions here operate on one 3rd class folder to create 4th class folders and don't do ionic relaxations

        smaller_folders generates many 4th folders, while smaller_one_folder generates only one 4th folder job
        var_test is the only different function: doesn't do any special things and just submit jobs and do relaxations
        also var_test operates on 2nd class folder to create 3rd class folders
        '''
        if folder[-1] != '/':
            folder=folder+'/' # make sure the folder has correct form: ....../incar
        self.folder=folder
        #hierdirecs = parent_folder(self.folder, hier=10)  # to find 1st-class materials folder
        #assert len(hierdirecs) <= 4, 'you are in the son folder. go back to the parent folder! otherwise another son folder will be created'
        #self.hierdirecs = hierdirecs
        #self.hier1direc = self.hierdirecs[0]    # 1st-class folder eg. '/home/yubi/work/berylliumo/'
        #self.hier2direc = self.hierdirecs[1]    # 2nd-class folder eg. '/home/yubi/work/berylliumo/wurtzite_2_vo_hse/'
        #self.sources_direc= self.hier1direc+'sources/' # eg. '/home/yubi/work/berylliumo/sources/'

    def koopmans_postive_or_neutral_defect(self, vardict, middle_comment='aexx0.2', charge=0, submit=True,var='AEXX',additionalchanges={}):
        '''
        generate a folder to compute +1 defect or neutral defect for a fixed mixing parameter
        rotate by a set of parameter
        there should not be ionic relaxations
        '''
        #assert charge == 0 or charge == 1, 'either neural or postive+1 defect'
        #assert len(self.hierdirecs) >= 3, 'you need to be in a 3rd class folder'
        # edit nelect to get positve defect
        read_var = read_file_values(self.folder)
        nelect = read_var.outcar('NELECT')
        print('number of electrons before=%s after=%s' % (nelect, nelect-charge))
        nelect = nelect - charge # -1 electron to get positive charge
        changes={'ISTART':1, 'ICHARG':1,'ISIF':2,'NSW':0, 'IBRION': -1, 'NELECT':nelect, 'LVHAR': '.TRUE.'}
        changes={**changes, **vardict} # use the variable parameter
        changes={**changes, **additionalchanges}
        
        sub_name='koopmans%s_charge%se_%s%s/'%(middle_comment,charge,vardict[var],var)

        # create son folder
        new_folder = self.folder+sub_name
        if os.path.isdir(new_folder): # remove old calculations
            shutil.rmtree(new_folder)
        os.mkdir(new_folder)

        os.system('cp INCAR KPOINTS POTCAR WAVECAR CHGCAR DEFECT '+new_folder)
        os.system('cp %sCONTCAR %sPOSCAR' % (self.folder, new_folder)) # new poscar !

        # edit input files
        change_files=change_input_files(new_folder)
        change_files.incar_change(changes) # write incar file

        # submit job
        pythonsubmit(folder=new_folder,submit=submit)
        os.chdir(self.folder)

    def koopmans_rotate_one(self,submit=True, header='koopmans',var='AEXX', varrange=[0.26,0.50], divnum=9, q_change=1, additional_charged_changes={}):
        '''
        rotate over mixing parameters and call the koopmans_postive_or_neutral_defect above 
        '''
        #assert len(self.hierdirecs) >= 3, 'go to 3rd-class folder. this function must create 4th-class folders'
        func=lambda div: np.linspace(varrange[0],varrange[1], div)
        var_list = np.round(func(divnum),6)# round to avoid long nonsense accuries
        # delect existing folders
        find_files(self.folder, header, var, remove=True) # yes, remove old directories
        
        for i in range(divnum):
            print('%s=%s' % (var,var_list[i]))
            vardict={var:var_list[i]}
            self.koopmans_postive_or_neutral_defect(vardict,middle_comment=i, charge=0, submit=submit,var=var)
            self.koopmans_postive_or_neutral_defect(vardict,middle_comment=i, charge=q_change, submit=submit,var=var, additionalchanges=additional_charged_changes)
        print('\n\tIt will copy CONTCAR not POSCAR. Assume DEFECT is updated\n')

    def koopmans_rotate_multi(self, submit=0, header='koopmans',var=['AEXX','HFSCREEN'], varrange=[[],[]]):
        '''
        rotate over mixing parameters and call the koopmans_postive_or_neutral_defect above 
        '''
        #assert len(self.hierdirecs) == 3, 'go to 3rd-class folder. this function must create 4th-class folders'
        # delect existing folders
        find_files(self.folder, header, var[1], middle=var[0], remove=True)
        #exit()
        for var1value in varrange[0]:
            for var2value in varrange[1]:
                print('%s=%s\t%s=%s' % (var[0],var1value,var[1],var2value))
                middle_comment='%s%s' % (var[0],var1value)
                vardict={var[0]:var1value, var[1]:var2value}
                self.koopmans_postive_or_neutral_defect(vardict,middle_comment=middle_comment, charge=0, submit=submit,var=var[-1])
                self.koopmans_postive_or_neutral_defect(vardict,middle_comment=middle_comment, charge=1, submit=submit,var=var[-1])
        print('\n\tIt will copy CONTCAR not POSCAR. Assume DEFECT is updated\n')

    def var_test(self, header, var, divnum, func, submit=0, change_dict={}):
        #assert len(self.hierdirecs) <= 3, 'variable test must create 3rd-class folders'
        # read the template input file
        #read_var = read_file_values(self.folder)
        #var_value = read_var.incar([var], float) # assumed no comment
        print('ATTENTION! Will use POSCAR instead of CONTCAR')
        dictionary = read_incar(self.folder)
        var_value = float(dictionary[var])

        var_list = np.round(func(var_value, divnum),3)

        # delect existing folders
        find_files(self.folder, header, var, remove=True) # yes, remove old directories
        #quit() # only delete jobs so quit here

        # make directory names
        new_folders = []
        for i in range(1, divnum+1): # index starts from 1. save index=0 for special cases
            folder = header+str(i)+'_'+var
            new_folders.append(folder)
            os.mkdir(folder)
            os.system('cp INCAR KPOINTS POTCAR POSCAR ./'+folder)

        # edit files in each directory
        for i in range(divnum):
            # edit incar file in new_folder
            new_folder = self.folder+new_folders[i]
            print(new_folder)
            changes = {var: var_list[i], 'SYSTEM':dictionary['SYSTEM']}
            changes = {**change_dict, **changes}
            change_files=change_input_files(new_folder)
            change_files.incar_change(changes)
            # submit job
            pythonsubmit(folder=new_folder,submit=submit)
        return 0

    def calc_formation_enthalpy(self, submit=0):
        '''
        1st read the accuraries requirements from material incar
        2nd get the input files from elements. eg. [enthalpyf_be, enthalpyf_o2] 
        3rd copy each source folder to self.folder, and edit incar to have the same accuracies as material incar
        usage: calc_ops=smaller_folders(pwd) ; calc_ops.calc_formation_enthalpy(submit)
        '''
        # 1st 
        material_incar_dict=read_incar(self.folder) # dictionary from material incar
        accuraries = {'EDIFF','EDIFFG','ENCUT','PREC','NELM','NELMIN', 'NSW', 'ISMEAR', 'SIGMA', 'LHFCALC','HFSCREEN', 'ALGO', 'AEXX'} 
        popkey = ['LVHAR']
        accuraries = material_incar_dict.keys() & accuraries # intersection between two sets of keywords
        material_incar_dict_acc=project(material_incar_dict, accuraries) # pick accurary keywords

        # 2nd input files are put in the 'sources' folder under the 1st-class materials folder
        # Change HERE!!!
        self.hierdirecs = parent_folder(self.folder, hier=10)  # to find 1st-class materials folder
        self.hier1direc = self.hierdirecs[0]    # 1st-class folder eg. '/home/yubi/work/berylliumo/'
        #self.hier2direc = self.hierdirecs[1]    # 2nd-class folder eg. '/home/yubi/work/berylliumo/wurtzite_2_vo_hse/'
        self.sources_direc= self.hier1direc+'sources/' # eg. '/home/yubi/work/berylliumo/sources/'

        elements=find_files(self.sources_direc, header='enthalpyf_', var='', remove=False)  # these folders begin with 'enthalpyf_'

        # 3rd 
        for ele in elements:
            # create son folder
            new_folder=self.folder+ele
            print(new_folder)
            if os.path.isdir(new_folder):
                shutil.rmtree(new_folder) # remove old formation enthalpy calculation
            os.mkdir(new_folder)

            # to copy files from this source folder
            # Change HERE!!!
            os.chdir(self.sources_direc+ele) 
            os.system('cp * %s' % (new_folder))

            # change incar accuracies
            change_input = change_input_files(new_folder)
            change_input.incar_change(material_incar_dict_acc, popkey=popkey) 
            # Need element template to be minimal

            # submit job
            pythonsubmit(folder=new_folder,submit=submit)



class smaller_one_folder:
    def __init__(self, folder):
        '''
        self.folder: the working folder
        most functions here operate on one 3rd class folder to create 4th class folders and don't do ionic relaxations

        smaller_one_folder generates only one 4th folder job. while smaller_folders generates many 4th folders
        '''
        if folder[-1] != '/':
            folder=folder+'/' # make sure the folder has correct form: ....../incar
        self.folder=folder
        #hierdirecs = parent_folder(self.folder, hier=10)  # to find 1st-class materials folder
        #assert len(hierdirecs) <= 4, 'you are in the son folder. go back to the parent folder! otherwise another son folder will be created'
        #self.hierdirecs = hierdirecs
        #self.hier1direc = self.hierdirecs[0]    # 1st-class folder eg. '/home/yubi/work/berylliumo/'
        #self.hier2direc = self.hierdirecs[1]    # 2nd-class folder eg. '/home/yubi/work/berylliumo/wurtzite_2_vo_hse/'
        #self.sources_direc= self.hier1direc+'sources/' # eg. '/home/yubi/work/berylliumo/sources/'

    def hyperfine(self, submit=0):
        '''
        produce hyperfine and ZFS parameters
        '''
        #assert len(self.hierdirecs) >= 3, 'you need to be in a 3rd class folder'
        titlefolder = 'hyperfine.zfs'+'/'
        print('Create folder %s\nDo not use stampede vasp6 to run jobs!!!' % (titlefolder))
        subfolder=self.folder+ titlefolder
        find_files(self.folder, titlefolder[:-1], '', remove=True) # remove old locpot directory
        os.mkdir(subfolder) # generate new pcharg firectory
        # CHGCAR is not needed, only WAVECAR is needed
        os.system('cp INCAR KPOINTS POTCAR WAVECAR '+subfolder)
        os.system('cp CONTCAR %sPOSCAR' % (subfolder)) # new poscar !
        # edit incar to get locpot
        changes={'ISTART':1,'IBRION':-1,'NSW':0,'KPAR':1,'LHYPERFINE':'.TRUE.','LDMATRIX':'.TRUE.'}
        # partial charge density can not use KPAR > 1
        removekey=['LVHAR','ICHARG','LORBIT']
        change_files=change_input_files(subfolder)
        change_files.incar_change(changes, popkey=removekey)
        # submit job
        pythonsubmit(folder=subfolder,submit=submit)
        os.chdir(self.folder) # to remove the correct folders, otherwise it will try to remove the subfolder
        return 0

    def pcharg(self,iband=0, submit=0):
        '''
        produce partial charge
        # PROCAR could not be generated when producing PARCHG
        '''
        #assert len(self.hierdirecs) >= 3, 'you need to be in a 3rd class folder'
        titlefolder = 'pcharg.band%s/' % (iband.replace(' ','_'))
        print('Create folder %s' % (titlefolder))
        subfolder=self.folder+ titlefolder
        find_files(self.folder, titlefolder[:-1], '', remove=True) # remove old locpot directory
        os.mkdir(subfolder) # generate new pcharg firectory
        # CHGCAR is not needed in partial charge calculation
        #'ICHARG':2 means don't use CHGCAR, only WAVECAR is needed
        os.system('cp INCAR KPOINTS POTCAR WAVECAR '+subfolder)
        os.system('cp CONTCAR %sPOSCAR' % (subfolder)) # new poscar !
        # edit incar to get locpot
        #read_fil=read_file_values(self.folder)
        #_,nband=read_fil.eigenval_ho()
        changes={'ISTART':1, 'LPARD':'.TRUE.','IBRION':-1,'NSW':0,'IBAND':iband,'LSEPB':'.TRUE.','KPAR':1}
        # partial charge density can not use KPAR > 1
        removekey=['LVHAR','ICHARG']
        change_files=change_input_files(subfolder)
        change_files.incar_change(changes, popkey=removekey)
        # submit job
        pythonsubmit(folder=subfolder,submit=submit)
        os.chdir(self.folder) # to remove the correct folders, otherwise it will try to remove the subfolder
        return 0

    def dielec_const_eps(self, submit, dielectype=True):
        '''
        make a subfolder of eps_aexx
        find_files(direc, header='', var='', middle='', remove=True, avoid='')
        '''
        #assert len(self.hierdirecs) == 3, 'you need to be in a 3rd class folder'
        if dielectype==0: # only electronic part
            sub_name='dielec_elec-part_eps/'
            subfolder=self.folder+sub_name
            find_files(self.folder, header='dielec_elec-part', var='_eps', remove=True) # remove old locpot directory
            changes={'ISTART':1,'LCALCEPS':'.TRUE.','IBRION':-1,'NSW':0, 'ISIF':2}
            removekey=['LVHAR']
        elif dielectype==1: # include ionic part screening effect 
            sub_name= 'dielec_ion-part_eps/'
            subfolder=self.folder+sub_name
            find_files(self.folder, header='dielec_ion-part', var='_eps', remove=True) # remove old locpot directory
            changes={'ISTART':1,'EDIFF':0.000001,'EDIFFG':-0.01,'LCALCEPS':'.TRUE.','IBRION':6,'NSW':200, 'ISIF':2,'POTIM':0.015}
            # EDIFF should be tight, otherwise vasp6 does not give the same result as vasp5
            print("\nDon't recommend vasp6! If vasp6, use tight convergence condition like EDIFF=1e-8, also ask for more nodes (don't remember why now) in submit.job; If vasp5, use more time like 12h\n")
            removekey=['LVHAR']
        elif dielectype==2: # electronic eps(omega) frequency dependence. Ionic part fixed
            sub_name='dielec_freq-depend_eps/'
            subfolder=self.folder+sub_name
            find_files(self.folder, header='dielec_freq-depend', var='_eps', remove=True) # remove old locpot directory
            changes={'ISTART':1,'EDIFF':0.000001,'EDIFFG':-0.01,'LOPTICS':'.TRUE.','IBRION':-1,'NSW':0, 'ISIF':2}
            # Don't need 'ALGO':'Exact', 'ISMEAR':0,  'SIGMA':0.01
            # for small bandgap material use dielectype=3
            removekey=['LVHAR']
        elif dielectype==3: # electronic eps(omega) frequency dependence for a small bandgap like 0.2 eV
            sub_name='dielec_freq-smallgap-depend_eps/'
            subfolder=self.folder+sub_name
            find_files(self.folder, header='dielec_freq-smallgap-depend', var='_eps', remove=True) # remove old locpot directory
            changes={'ISTART':1,'EDIFF':0.000001,'EDIFFG':-0.01,'LOPTICS':'.TRUE.', 'NEDOS':2000,'CSHIFT': '0.100','IBRION':'-1','NSW':0, 'ISIF':2}
            # for small bandgap material add 'NEDOS':2000,'CSHIFT': '0.100' to changes
            removekey=['LVHAR']
        else: # 
            sub_name='dielec_wavevector1-depend_eps/'
            subfolder=self.folder+sub_name
            find_files(self.folder, header='dielec_wavevector1-depend', var='_eps', remove=True) # remove old locpot directory
            changes={'ISTART':1,'ICHARG':1,'EDIFF':0.000001,'EDIFFG':-0.01,'ALGO':'CHI','LOPTICS':'.TRUE.', 'IBRION':'-1','NSW':0, 'ISIF':2, 'LWAVE':'.FALSE.','LCHARG':'.FALSE.'}
            # ,'KPAR':1
            removekey=['LVHAR']

        os.mkdir(subfolder) # generate new locpot firectory
        os.system('cp INCAR KPOINTS POTCAR WAVECAR CHGCAR '+subfolder)
        if dielectype == 4:
            os.system('cp WAVEDER '+ subfolder) # WAVEDER 
            print('Remember to use VASP 5 to run the code!!')
        os.system('cp CONTCAR %sPOSCAR' % (subfolder)) # update poscar !
        # edit incar to get eps
        change_files=change_input_files(subfolder)
        change_files.incar_change(changes, popkey=removekey)
        print('subfolder:%s, CONTCAR used' % (sub_name))
        # submit job
        pythonsubmit(folder=subfolder,submit=submit)

    def locpot(self, submit):
        '''
        produce 'locpot' when i forget. need lvhar=t.
        '''
        #assert len(self.hierdirecs) == 3, 'you need to be in a 3rd class folder'
        subfolder=self.folder+'locpot/'
        find_files(self.folder, 'locpot', '', remove=True) # remove old locpot directory
        os.mkdir(subfolder) # generate new locpot firectory
        os.system('cp INCAR KPOINTS POTCAR WAVECAR CHGCAR '+subfolder)
        os.system('cp CONTCAR %sPOSCAR' % (subfolder)) # new poscar !
        # edit incar to get locpot
        changes={'ISTART':'1','ICHARG':'1','LVHAR':'.TRUE.','IBRION':'-1','NSW':'0'}
        change_files=change_input_files(subfolder)
        change_files.incar_change(changes)
        print('subfolder:locpot')
        # submit job
        pythonsubmit(folder=subfolder,submit=submit)

    def calc_bs_dos(self, bs_dos, submit):
        '''
        band structure: create bs subfolder and run calculation on kpoints
        density of states: create dos subfolder and run dos calculation jobs
        '''
        if bs_dos == 0: # calculate bandstructure
            changes={'ISTART':1, 'ICHARG':11, 'ISIF':2, 'NSW':0, 'IBRION': -1, 'NELMIN':5,'LWAVE':'F', 'LCHARG':'F'}
            sub_name = 'bs_non-self/'
            print('bandstructure calculation... subfolder:%s CONTCAR used. LORBIT=11 is not turned on to save space' % sub_name )
            print('ISMEAR=-5 does not work for BS (tetrahedron method cannot read linemode KPOINTS). Use ISMEAR=0 for semiconductor (or metal). Use ISMEAR=1 or 2 for metal' )
        elif bs_dos == 1: # calculate dos
            changes={'ISTART':1, 'ICHARG':11, 'ISIF':2, 'NSW':0, 'ISMEAR':-5, 'IBRION': -1, 'LORBIT':11, 'NEDOS':901,'NELMIN':5,'LWAVE':'F', 'LCHARG':'F'}
            sub_name = 'dos_non-self/'
            print('\tCould try a different smearing method, like ISMEAR=-4 or -5!\n')
            print('density of state calculation... subfolder:%s CONTCAR used' % sub_name )
        # create son folder
        new_folder = self.folder+sub_name
        if os.path.isdir(new_folder): # remove old calculations
            shutil.rmtree(new_folder)
        os.mkdir(new_folder)
        os.system('cp INCAR KPOINTS POTCAR WAVECAR CHGCAR IBZKPT '+new_folder)
        os.system('cp CONTCAR %sPOSCAR' % (new_folder))

        # edit input files
        change_files=change_input_files(new_folder)
        change_files.incar_change(changes) # write incar file
        if bs_dos == 0: # kpoints should change to find band structure
            os.chdir(new_folder)
            # 2nd input files are put in the 'sources' folder under the 1st-class materials folder
            # the effect of header='energyf_'+element is to specify which subfolder to update. Avoid updating all energyf_* folders
            self.hierdirecs = parent_folder(self.folder, hier=10)  # to find 1st-class materials folder
            self.hier1direc = self.hierdirecs[0]    # 1st-class folder eg. '/home/yubi/work/berylliumo/'
            #self.hier2direc = self.hierdirecs[1]    # 2nd-class folder eg. '/home/yubi/work/berylliumo/wurtzite_2_vo_hse/'
            self.sources_direc= self.hier1direc+'sources/' # eg. '/home/yubi/work/berylliumo/sources/'
            generate_KPATH(self.sources_direc) # generate kpoints from kpath

        # submit job
        pythonsubmit(folder=new_folder,submit=submit)


    def calc_miu4energyf(self, submit=0, element=''):
        '''
        calculate miu (the chemical potential in bulk or molecule) for formation energy
        1st read the accuraries requirements from material incar
        2nd get the input files from specified elements. eg. [energyf_Li, energyf_F_Be] 
        3rd copy each source folder to self.folder, and edit incar to have the same accuracies as material incar
        usage: calc_ops=smaller_folders(pwd) ; calc_ops.calc_formation_enthalpy(submit)
        '''
        # get folder sources/ under the first order folder
        # 1st 
        material_incar_dict=read_incar(self.folder) # dictionary from material incar
        accuraries = {'EDIFF','EDIFFG','ENCUT','PREC','NELM','NELMIN', 'ISMEAR', 'SIGMA', 'LHFCALC','HFSCREEN', 'ALGO', 'AEXX'} 
        popkey = ['LVHAR']
        accuraries = material_incar_dict.keys() & accuraries # intersection between two sets of keywords
        material_incar_dict_acc=project(material_incar_dict, accuraries) # pick accurary keywords

        # 2nd input files are put in the 'sources' folder under the 1st-class materials folder
        # the effect of header='energyf_'+element is to specify which subfolder to update. Avoid updating all energyf_* folders
        self.hierdirecs = parent_folder(self.folder, hier=10)  # to find 1st-class materials folder
        self.hier1direc = self.hierdirecs[0]    # 1st-class folder eg. '/home/yubi/work/berylliumo/'
        #self.hier2direc = self.hierdirecs[1]    # 2nd-class folder eg. '/home/yubi/work/berylliumo/wurtzite_2_vo_hse/'
        self.sources_direc= self.hier1direc+'sources/' # eg. '/home/yubi/work/berylliumo/sources/'

        elements=find_files(self.sources_direc, header='energyf_'+element, var='', remove=False)  # these folders begin with 'enthalpyf_'

        # 3rd 
        for ele in elements:
            # create son folder
            new_folder=self.folder+ele
            print(new_folder)
            if os.path.isdir(new_folder):
                shutil.rmtree(new_folder) # remove old formation enthalpy calculation
            os.mkdir(new_folder)

            # to copy files from this source folder
            # Change HERE!!!
            os.chdir(self.sources_direc+ele) 
            os.system('cp * %s' % (new_folder))

            # change incar accuracies
            change_input = change_input_files(new_folder)
            change_input.incar_change(material_incar_dict_acc, popkey=popkey) 
            # Need element template to be minimal

            # submit job
            pythonsubmit(folder=new_folder,submit=submit)
