import sys
import os
sys.path.append(os.environ['SCRIPT'])
from class2_update_input import change_input_files

#change_files.incar_upgrade(test)
change_files=change_input_files('./')
#print('sys arguments=%s'% (sys.argv))
# enter 'copy
if len(sys.argv)==1:
    print('\tupdate to new format')
    change_files.incar_change({}, popkey=[])
elif sys.argv[1] == 'scf': # make precision higher for testing eps calculation
    print('\tmodify INCAR to output CHGCAR and WAVECAR. Also tight conditions for phonon calculations. PREC=Accurate should not be used together with ALGO=Fast')
    print('For phonon calculation: PREC=Accurate, not need ALGO and ADDGRID, Force is -0.003 or -0.005, EDIFF=1e-6 and sometimes 1e-7 and 1e-8, LREAL=F to avoid notifications')
    print('Check LMAXMIX if needed, even for PBE')
    change_files.incar_change({'LREAL':'.FALSE.', 'EDIFF':'0.000001', 'EDIFFG':'-0.005','PREC':'Accurate'},popkey=['ADDGRID','ALGO','LCHARG','LWAVE'])
elif sys.argv[1] == 'scf2': # make precision higher for testing eps calculation
    print('\tmodify INCAR to output CHGCAR and WAVECAR')
    change_files.incar_change({'LREAL':'.FALSE.'},popkey=['LCHARG','LWAVE'])
elif sys.argv[1] == 'findiff':
    change_files.incar_change({'EDIFF':'0.000001','PREC':'Accurate','LWAVE':'F', 'LCHARG':'F','NSW':0, 'IBRION':-1,'ISIF':2 }, popkey=['ALGO','LORBIT']) 
    # PREC=accurate, do not to set ALGO, will be automatic. Not need to set addgrid
elif sys.argv[1] == 'soc':
    print('\tmodify INCAR to use SOC, no relaxation. Setting ISYM=-1. Should have ISYM=-1 for magnetic system to break time reversal symmetry.  ')
    change_files.incar_change({'ISPIN':2,'NSW':0, 'IBRION':-1,'ISIF':2, 'ISYM':-1, 'LMAXMIX':4,'LSORBIT':'T','LNONCOLLINEAR':'T','LORBMOM':'T','SAXIS':'0 0 1'}, popkey=[]) 
elif sys.argv[1] == 'soc2':
    print('\tmodify INCAR to use SOC, allow ionic relaxation')
    change_files.incar_change({'ISPIN':2, 'LMAXMIX':4,'LSORBIT':'T','LNONCOLLINEAR':'T','LORBMOM':'T','ISYM':-1,'SAXIS':'0 0 1'}, popkey=[]) 
elif sys.argv[1] == 'hse':
    print('\tmodify INCAR to use SOC, allow ionic relaxation')
    change_files.incar_change({'LHFCALC':'.TRUE.', 'HFSCREEN':0.2,'ALGO':'Normal'}, popkey=[]) 
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
else:
    print('Error! Argument not recognized')


#
#elif sys.argv[1] == 'copy': # 'copy': use WAVECAR and CHGCAR
#    print('\tmodify INCAR to use WAVECAR and CHGCAR')
#    change_files.incar_change({'ISTART':1,'LVHAR':'.TRUE.','LORBIT':11}, popkey=['LREAL'] )
#elif sys.argv[1] == 'copychange1': # 'copychange': use WAVECAR and CHGCAR and change AEXX
#    print('\tmodify INCAR to not using WAVECAR and CHGCAR')
#    change_files.incar_change({'ISTART':0,'LVHAR':'.TRUE.','LORBIT':11}, popkey=['ICHARG'] )
#elif sys.argv[1] == 'copychange3': # for cc diagram calculation
#    print('\t make NELM=300, use WAVECAR and CHGCAR')
#    change_files.incar_change({'ISTART':1,'NELM':300,'LVHAR':'.TRUE.','LORBIT':11},popkey=[])
#elif sys.argv[1] == 'symprec': # symmetry precision
#    print('\t change SYMPREC to 5E-3')
#    change_files.incar_change({'SYMPREC':'5E-3'})
