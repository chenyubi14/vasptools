#!/usr/bin/env python
import sys
import os
sys.path.append(os.environ['SCRIPT'])
from class2_update_input import change_input_files

change_files=change_input_files('./')

if len(sys.argv)==1:
    print('\tupdate to new format')
    change_files.incar_change({}, popkey=[])
elif sys.argv[1] == 'scf': # make precision higher for testing eps calculation
    print('\tmodify INCAR to output CHGCAR and WAVECAR. Also tight conditions for phonon calculations. PREC=Accurate should not be used together with ALGO=Fast')
    print('For phonon calculation: PREC=Accurate, not need ALGO and ADDGRID, Force is -0.003 or -0.005, EDIFF=1e-6 and sometimes 1e-7 and 1e-8, LREAL=F to avoid notifications')
    print('Check LMAXMIX if needed, even for PBE')
    change_files.incar_change({'LREAL':'.FALSE.', 'EDIFF':1e-7, 'EDIFFG':-0.003,'PREC':'Accurate'},popkey=['ADDGRID','LCHARG','LWAVE'])
elif sys.argv[1] == 'scf2': # phonon perfect
    print('\tmodify INCAR to output CHGCAR and WAVECAR. For perfect/ of phonon calculations')
    change_files.incar_change({'LREAL':'Auto'},popkey=['LWAVE'])
elif sys.argv[1] == 'scf3': # make precision higher for testing eps calculation
    print('\tmodify INCAR to output CHGCAR and WAVECAR.')
    change_files.incar_change({},popkey=['LCHARG','LWAVE'])
elif sys.argv[1] == 'slab': 
    print('\tmodify INCAR to fix lattice, usually for slab/strain calculation, LREAL=Auto for fast check. Remember to change KPOINTS!!!')
    print('More accuratelly, need LREAL=F, ISPIN=2, MAGMOM=1000*0')
    print('Remember to change KPOINTS!!!')
    change_files.incar_change({'NSW':400,'LREAL':'Auto','ISIF':2,'LVHAR':'.TRUE.','EDIFF':1e-5,'EDIFFG':-0.01,'NCORE':4,'PREC':'Normal','LWAVE':'F','ICHARG':1,'AMIN':0.01},popkey=['LCHARG','LORBIT','KPAR','EDIFFG','LVTOT'])
elif sys.argv[1] == 'pressure': 
    print('\tmodify INCAR to fix lattice, usually for pressure calculation, LREAL=Auto for fast check')
    change_files.incar_change({'LREAL':'Auto','ISIF':2,'EDIFF':5e-5,'EDIFFG':-0.01,'NCORE':4,'PREC':'Normal','LWAVE':'F','ICHARG':1},popkey=['LCHARG','LORBIT','KPAR'])
elif sys.argv[1] == 'fixion': # fix ionic motions
    print('\tmodify INCAR to fix ionic motions, usually after relaxation and need wavefunction output')
    change_files.incar_change({'ISIF':2,'NSW':0,'IBRION':'-1'},popkey=['LWAVE'])
elif sys.argv[1] == 'findiff':
    change_files.incar_change({'EDIFF':1e-6,'PREC':'Accurate','LWAVE':'F', 'LCHARG':'F','NSW':0, 'IBRION':-1,'ISIF':2 }, popkey=['LORBIT'])  # Sai prefers to pop out ALGO, but I don't want to
elif sys.argv[1] == 'dfpt':
    change_files.incar_change({'EDIFF':1e-6,'PREC':'Accurate','LWAVE':'F', 'LCHARG':'F','NSW':1, 'IBRION':8,'ISIF':2 }, popkey=['LORBIT','NCORE','KPAR'])  # Sai prefers to pop out ALGO, but I don't want to
    # PREC=accurate, do not to set ALGO, will be automatic. Not need to set addgrid
elif sys.argv[1] == 'soc':
    print('\tmodify INCAR to use SOC, no relaxation. Setting ISYM=-1. Should have ISYM=-1 for magnetic system to break time reversal symmetry. LNONCOLLINEART=T is not explicitly set because vasp_ncl will force it')
    print('If using PBE WAVECAR, should recalculate with ISYM=-1 in PBE. Otherwise, the WAVECAR is not accepted by soc. Try inter_soc.sh to edit INCAR further')
    change_files.incar_change({'ISPIN':2,'NSW':0, 'IBRION':-1,'ISIF':2, 'ISYM':-1, 'LSORBIT':'T','LORBMOM':'T','SAXIS':'0 0 1'}, popkey=[]) 
elif sys.argv[1] == 'soc2':
    print('\tmodify INCAR to use SOC, allow ionic relaxation, ISYM=-1.')
    change_files.incar_change({'ISPIN':2, 'LMAXMIX':4,'LSORBIT':'T','LORBMOM':'T','ISYM':-1,'SAXIS':'0 0 1'}, popkey=[]) 
elif sys.argv[1] == 'hse':
    print('\tmodify INCAR to use HSE, allow ionic relaxation')
    change_files.incar_change({'LHFCALC':'.TRUE.', 'HFSCREEN':0.2,'ALGO':'Normal'}, popkey=[]) 
elif sys.argv[1] == 'mp':
    print('modify INCAR for materials project default parameters\n\tShould check whether to include LMAXMIX. Better edit SYSTEM by yourself')
    change_files.incar_change({'LREAL':'.FALSE.','NCORE':2,'EDIFF':1e-6, 'EDIFFG':-1e-2, 'ENCUT':400, 'ISMEAR':0, 'SIGMA':0.01,'NSW':400, 'NELM':200, 'PREC':'Normal' ,'ALGO':'Normal'}, popkey=['MAGMOM', 'LWAVE', 'LORBIT', 'LASPH']) 
elif sys.argv[1] == 'neb':
    #print('\t Edit INCAR for NEB')
    if len(sys.argv) <=2:
        print('Error! Need to enter the number of images!')
    img_num=int(sys.argv[2])
    change_files.incar_change({'ISTART':0,'EDIFFG':-0.05,'NSW':2000,'ISIF':0,
    'IBRION':3,'POTIM':'0.0','IWAVPR':1,'LCORR':'T','ISYM':2,
    'IMAGES':img_num,'SPRING':-5,'LCLIMB':'.TRUE.','ICHAIN':0,'IOPT':3})
elif sys.argv[1] == 'nband':
    if len(sys.argv)<3:
        print('Error! Need to enter NBAND')
    band_num = int(sys.argv[2]) * 2
    change_files.incar_change({'NBANDS':band_num})
elif sys.argv[1] == 'wannier1':
    change_files.incar_change({'LWANNIER90':'T'})
elif sys.argv[1] == 'md1':
    print('Molecular dynamics NVT by Nose-Hoover. Totaltime=NSW*POTIM. NELMIN=6 for surface or special systems.')
    dictionary = {'PREC':'Normal','ISIF':2, 'IBRION':0, 'NSW':2000, 'ALGO':'Fast','POTIM':1,'SMASS':0.4,'MDALGO':2,'LREAL':'Auto', 'ISYM':0, 'NELMIN':6, 'EDIFF':1e-5,'LWAVE':'F', 'LCHARG':'F'}
    if len(sys.argv)<3:
        print('\t Enter one more argument for TEBEG!!!')
    else:
        print('temperature is ', sys.argv[2])
        dictionary = {**dictionary, 'TEBEG':sys.argv[2],'TEEND':sys.argv[2], }
    change_files.incar_change(dictionary, popkey=['EDIFFG','LORBIT','NELM'])
elif sys.argv[1] == 'md2':
    print('continue the MD run by \t cp CONTCAR POSCAR')
    change_files.incar_change({'NSW':10000}, popkey=[])
elif sys.argv[1] == 'md3' or sys.argv[1] == 'md-standard':
    print('Molecular dynamics NVT standard by MDALGO=0, recommended SMASS=0 0.4 1.0')
    change_files.incar_change({'SMASS':0.4,'MDALGO':0,}, popkey=['NBLOCK'])
elif sys.argv[1] == 'md4' or sys.argv[1] == 'md-nose':
    print('Molecular dynamics NVT Nose-Hover by MDALGO=2, recommended SMASS=0 0.4 1.0')
    change_files.incar_change({'SMASS':0.4,'MDALGO':2,}, popkey=['NBLOCK'])
elif sys.argv[1] == 'md5' or sys.argv[1] == 'md-annealing':
    print('Molecular dynamics NVT by annealing')
    change_files.incar_change({'SMASS':-1,'MDALGO':0,'NBLOCK':4,}, popkey=[])
elif sys.argv[1] == 'md6' or sys.argv[1] == 'md-langevin':
    print('Molecular dynamics NVT langevin thermostat by MDALGO=3, edit LANGEVIN_GAMMA!')
    change_files.incar_change({'ISIF':2, 'MDALGO':3, 'LANGEVIN_GAMMA':None, }, popkey=['SMASS'])
elif sys.argv[1] == 'md7' or sys.argv[1] == 'md-npt':
    print('Molecular dynamics NPT langevin thermostat by ISIF=3 at pressure 0kB, edit LANGEVIN_GAMMA!')
    change_files.incar_change({'ISIF':3, 'MDALGO':3, 'PSTRESS': 0, 'LANGEVIN_GAMMA':None, 'LANGEVIN_GAMMA_L':10}, popkey=['SMASS'])
elif sys.argv[1] == 'mlff1' :
    print('Machine learning force field train')
    change_files.incar_change({'ML_LMLFF':'.TRUE.', 'ML_MODE':'train',}, popkey=['MAXMIX','TEEND'])
elif sys.argv[1] == 'mlff2' :
    print('Machine learning force field run')
    change_files.incar_change({'ML_LMLFF':'.TRUE.', 'ML_MODE':'run',}, popkey=['MAXMIX'])
elif sys.argv[1] == 'nomd':
    print('Remove molecular dynamics. For the perfect/WAVECAR before MD')
    change_files.incar_change({'IBRION':2, 'NSW':0, 'ALGO':'Normal', }, popkey=['IALGO','POTIM','SMASS','MDALGO','LWAVE','LCHARG','TEBEG'])
elif sys.argv[1] == 'dftu':
    print('PBE unit cell needs LASPH=T')
    change_files.incar_change({'LASPH':'T'}, popkey=[])
elif sys.argv[1] == 'PBEsol':
    print('Use PBEsol')
    change_files.incar_change({'GGA':'PS'}, popkey=[])
elif sys.argv[1] == 'temp':
    print('temporary edits')
    #change_files.incar_change({'ALGO':'Fast','NCORE':8}, popkey=['KPAR','LORBIT'])
    #change_files.incar_change({'NCORE':8, 'ALGO':'Fast'}, popkey=['KPAR','LORBIT'])
    change_files.incar_change({'NCORE':16}, popkey=[])
    #change_files.incar_change({'LANGEVIN_GAMMA':'3*10', 'LANGEVIN_GAMMA_L':'10'}, popkey=[])
else:
    print('Error! Argument not recognized')
