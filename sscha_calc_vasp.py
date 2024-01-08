import cellconstructor as CC
import ase, ase.calculators
from ase.calculators.vasp import Vasp

import sys, os
import numpy as np

sys.path.append(os.environ['SCRIPT'])
from class0_functions1 import read_incar

def magmom_format(string):
    string = string.split()
    new_string = np.array([])
    for s in string:
        s = s.split('*')
        if len(s) == 2:
            new_string = np.append(new_string, int(s[0]) * [float(s[1])])
        elif len(s) == 1:
            new_string = np.append(new_string, float(s))
        else:
            print('Error! MAGMOM not recognizable')
    return new_string

def get_calculator(kgrid, incar='INCAR'):

    ### Set VASP calculator
    #command = 'module load intel/18; ~/bin/vasp.631.pod_std'
    #command = 'module load intel/18; ~/bin/vasp.631.pod_std'
    #command = 'module load intel/18; mpirun -n 2 ~/bin/vasp.631.pod_std'
    #command = 'module reset 1> /dev/null 2>&1 ;module unload cpu/0.17.3b; module load cpu/0.15.4;module load intel/19.1.1.217; module load mvapich2/2.3.4; module load intel-mkl/2018.1.163; vasp.641.expanse_std'
    command = os.environ['ASE_VASP_COMMAND']
    if os.path.isfile(incar) and os.path.getsize(incar) > 0:
        inputs = read_incar('./', incar=incar)
        inputs['command'] = command
        inputs['kpts'] = tuple(kgrid)
        inputs['gamma'] = True
        ### Edit INCAR for finite difference calculations
        edits = {'EDIFF':1e-6,'PREC':'Accurate','LWAVE':False, 'LCHARG':False,'NSW':0, 'IBRION':-1,'ISIF':2 }
        interme = { **inputs, **edits } 
        interme.pop('SYSTEM',None)
        interme.pop('LORBIT',None)
        interme.pop('EDIFFG',None)
        interme.pop('LVHAR',None)
        ### Need to give xc type, otherwise will use LDA
        ## If not specified, use PBE
        if 'GGA' in interme.keys():
            print('xc = ',interme['GGA'])
            interme['xc'] = 'PBE'
        else:
            interme['xc'] = 'PBE'
        ### Get lower case of the tag values
        ### Also convert numbers from str format, like ISMEAR:integer
        ## format of some tags that are not str
        formats = { 'ISMEAR':int, 
                    'SIGMA':float, 
                    'ENCUT':float , 
                    'NELM':int, 
                    'NELMIN':int,
                    'ISPIN':int,
                    'NCORE':int,
                    'LMAXMIX':int,
                    'IVDW':int,
                    'LDAUTYPE':int,
                    'LDAUL':lambda val: list(np.array(val.split()).astype(int)),
                    'LDAUU':lambda val: list(np.array(val.split()).astype(float)),
                    'LDAUJ':lambda val: list(np.array(val.split()).astype(float)),
                    'LDAUPRINT':int,
                    'MAGMOM':magmom_format,
                    }
        print('updated keys')
        inputs = {}
        for key,value in interme.items():
            if key in formats.keys():
                value = formats[key](value) # float(value) or int(value)
            inputs[key.lower()] = value
            print('key=%s, value=%s' % (key.lower(), value) )
    else:
        print('INCAR not available, use the default setup in script')
        inputs = {
            'command':command,
            'prec':'Accurate',
            'xc':'PBE',
            'ibrion':2,
            'encut':400,
            'ediff':1e-6,
            'nelm':200,
            'ismear':0,
            'sigma':0.01,
            'lreal':False,
            'kpts':tuple(kgrid),
            'gamma':True,
            'ncore':2,
            }
    ### Edit HERE!!!
    setups = {'base':'recommended', 'Sr':'_sv', 'Ti':'_pv', 'Nb':'_pv',}
    #setups = {'base':'recommended', }
    calculator = Vasp(**inputs, setups=setups)
    return calculator

## Same as setting this way:
#calculator = Vasp(
#              command=command,
#              prec='Accurate',
#              xc='PBE',
#              ibrion=2,
#              encut=400,
#              ediff=1e-6,
#              nelm=200,
#              ismear=0,
#              sigma=0.01,
#              lreal=False,
#              kpts=tuple(kgrid),
#              ncore=2,
#              #lmaxmix=4,
#              )
