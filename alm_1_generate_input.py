#!/usr/bin/env python
import numpy as np
import os 
import sys
import argparse
from pymatgen.core import Structure
from pathlib import Path
import itertools

print('copy INCAR,KPOINTS,POSCAR and link phonopy.conf')

toolpath='/home/yubi/repo/alamode/tools'

parser = argparse.ArgumentParser(description='Generate alamode input file based on POSCAR')
parser.add_argument('superarray', type=int, nargs=3,choices=range(1,10),
                help='3 intergers for supercell size')
parser.add_argument('--poscar', type=str, nargs=1,default='POSCAR',
                help='the POSCAR filename')
args = parser.parse_args()
superarray = list(args.superarray)
unitpos = args.poscar
#unitpos='POSCAR'
print('Supercell array is ', superarray)

## set some parameters
orthomatrix=[[1, 0, 0], [0, 1, 0], [0, 0, 1]]
folder=os.environ['PWD']+'/'
angstrom2bohr = 1.8897259886


def struc2poscar(struc, folder, fname='POSCAR', selective_dynamics=False, atoms=[]):
    folder = Path(folder)
    speinfo=struc.composition.get_el_amt_dict()
    spe1num=speinfo[struc[0].specie.name] 
    if selective_dynamics:
        selective_dynamics = [[True]*3]*(struc.num_sites)
        for atom in atoms:
            atom = int(atom)
            selective_dynamics[atom] = [False]*3
            print('fix atom%s %s%s %s' % (atom, struc[atom].specie.name, int(atom % spe1num ), struc[atom].frac_coords))
        print('Python indices: index + 1 to match with Vesta')
        struc.to(filename=str(folder/fname), selective_dynamics=selective_dynamics, fmt='poscar')
    else:
        struc.to(filename=str(folder/fname), fmt='poscar')

## generate supercell structure
struct=Structure.from_file(unitpos)
ortho = struct * orthomatrix
superstruct = ortho * superarray
from pymatgen.symmetry.analyzer import SpacegroupAnalyzer as symm
symman = symm(struct)
primstruct = symman.find_primitive()
print('primitive cell is ', primstruct)
## save the supercell POSCAR
struc2poscar(superstruct, folder, fname='POSCAR.super.vasp', selective_dynamics=False, atoms=[])

latt=superstruct.lattice.matrix
spec=superstruct.species


elements=[]
types=[]
for i in range(len(spec)):
    name= spec[i].name 
    if name not in elements:
        elements.append(name)
    types.append(str(len(elements)))
print('elements=%s, types=%s' % (elements, types) )

scell=' '.join( sys.argv[2:6]) # dimension

# write input file for alamode
def write_alm(fil, list_dictionary, list_name):
    with open(fil, 'w') as f:
        for nami, dicti in zip(list_name,list_dictionary):
            f.write('&%s\n' % nami)
            if type(dicti) is list:
                ##print(nami,'is list')
                f.write( ''.join(dicti)  )
            elif type(dicti) is dict:
                ##print(nami,'is dict')
                for key in dicti:
                    f.write('\t%s=%s\n' % (key,dicti[key]) )
            else:
                print('Error! Wrong format of the namespace!')
                sys.exit()
            f.write('/\n\n')


### shared parameters
## supercell 
cellstr='\t%.10f # factor from Angstrom to Bohr unit' % angstrom2bohr
for i in range(3):
    vec=latt[i,:]
    cellstr = cellstr + '\n\t%.10f %.10f %.10f' % ( vec[0], vec[1], vec[2])
dic_cell = [ cellstr + '\n' ]
cellstr='\t%.10f # factor from Angstrom to Bohr unit' % angstrom2bohr
## primitive cell for phonon
latt=primstruct.lattice.matrix
for i in range(3):
    vec=latt[i,:]
    cellstr = cellstr + '\n\t%.10f %.10f %.10f' % ( vec[0], vec[1], vec[2])
dic_cell_prim = [cellstr + '\n']
dic_cutoff = ['\t*-* None None 12 12 12 12 12 \n']
positionstr = ''
for i in range(len(superstruct)):
    pos=superstruct[i].frac_coords
    positionstr = positionstr + ('\t%s %.10f %.10f %.10f \n' % (types[i], pos[0], pos[1], pos[2]))
dic_position = [positionstr] 

#### specific parameters
### line mode
#dic_qpoints = ['\t1\n'
#        + '\tG 0.0 0.0 0.0 X 0.5 0.0 0.0 51\n']

dictionary = np.loadtxt('phonopy.conf', comments=['!','#'], dtype=str,delimiter='=')
keys= [dictionary[i,0].strip() for i in range(len(dictionary[:,0]))]
values = [ dictionary[i,1].strip() for i in range(len(dictionary[:,1])) ]
dictionary = dict(zip(keys,values))
band_points = dictionary['BAND'].split()
band_points = np.reshape(band_points, (-1,3) )
band_labels = dictionary['BAND_LABELS'].split()
def list2str(thelist):
    return ' '.join([ l.replace(',', '') for l in thelist])
dic_qpoints = '\t1\n'
for i in range(len(band_labels) -1):
    string_i='\t%3s %s\t%3s %s\t51\n' % ( band_labels[i].replace(r'$\Gamma$', 'G'), list2str(band_points[i]),
                              band_labels[i+1].replace(r'$\Gamma$', 'G'), list2str(band_points[i+1]),
                              )
    dic_qpoints = dic_qpoints + string_i
dic_qpoints = [ dic_qpoints ]


#### write the input files
fil='in1a.2nd_suggest'
dic_general = {
                'PREFIX':'out1', 
                'MODE':'suggest', 
                'NKD':len(elements), 
                'NAT':len(superstruct), 
                'KD':' '.join(elements)
                }
dic_interaction={'NORDER':1}
dics = [dic_general, dic_interaction, dic_cell, dic_cutoff, dic_position]
names = ['general', 'interaction', 'cell', 'cutoff', 'position']
write_alm(fil, dics, names)
print('generated %s' % (fil))
with open('run1a.sh','w') as runf:
    runf.write('alm %s > log1a \npython %s/displace.py --VASP=POSCAR.super.vasp --mag=0.01 -pf out1.pattern_HARMONIC\nsh $SCRIPT/alm_2*\n# Run jobs in fin_disp/\n'
            % (fil, toolpath) )


##### HERE!
## BORN correction

fil='in1b.2nd_optimize'
dic_general['MODE'] = 'optimize'
dic_optimize = {'DFSET':'DFSET_harmonic'} ## optimize from this file
dics = [dic_general, dic_interaction, dic_cell, dic_optimize, dic_cutoff, dic_position]
names = ['general', 'interaction', 'cell', 'optimize', 'cutoff', 'position']
write_alm(fil, dics, names)
print('generated %s' % (fil))
with open('run1b.sh','w') as runf:
    runf.write('python %s/extract.py --VASP=POSCAR.super.vasp fin_disp/dis*/vasprun.xml > DFSET_harmonic\nalm %s > log1b\n'
            % (toolpath, fil) )

fil='in2.phband'
dic_general = {
                'PREFIX':'out1', 
                'MODE':'phonons', 
                'NKD':len(elements), 
                'KD':' '.join(elements),
                'FCSXML':'out1.xml'
                }
dics = [dic_general, dic_cell_prim, dic_qpoints]
names = ['general', 'cell', 'kpoint']
write_alm(fil, dics, names)
print('generated %s' % (fil))
with open('run2.sh','w') as runf:
    runf.write('anphon %s > log2\npython $SCRIPT/alm_4_plotband.py out1.bands\n'
            % (fil) )

with open('run30.sh','w') as runf:
    runf.write('#link XDATCAR\npython $SCRIPT/alm_6_random.py\n## Run vasp in random/\n')


fil='in3a.cv'
dic_general = {
                'PREFIX':'out3', 
                'MODE':'optimize', 
                'NKD':len(elements), 
                'NAT':len(superstruct), 
                'KD':' '.join(elements)
                }
dic_interaction={'NORDER':5, 'NBODY':'2 3 3 2 2'}
dic_optimize = {'LMODEL':'enet',
                'NDATA':40,
                'DFSET':'DFSET_md',
                'FC2XML':'out1.xml',
                'CV':4,
                'L1_RATIO':1.0,
                'CV_MINALPHA':1.0e-7,
                'CV_MAXALPHA':0.02,
                'CV_NALPHA':100,
                'STANDARDIZE':1,
                'CONV_TOL':1.0e-8
                }
dics = [dic_general, dic_interaction, dic_cell, dic_cutoff, dic_optimize, dic_position]
names = ['general', 'interaction', 'cell', 'cutoff', 'optimize', 'position']
write_alm(fil, dics, names)
print('generated %s' % (fil))
with open('run3a.sh','w') as runf:
    runf.write('python %s/extract.py --VASP=POSCAR.super.vasp random/*/vasprun.xml > DFSET_md\nalm %s  ## use run alm1\n'
            % (toolpath, fil) )

fil='in3b.opt'
dic_general['NMAXSAVE']=3
dic_optimize = {'LMODEL':'enet',
                'NDATA':40,
                'DFSET':'DFSET_md',
                'FC2XML':'out1.xml',
                'CV':0,
                'L1_ALPHA':1.0e-06,
                }
dics = [dic_general, dic_interaction, dic_cell, dic_cutoff, dic_optimize, dic_position]
names = ['general', 'interaction', 'cell', 'cutoff', 'optimize', 'position']
write_alm(fil, dics, names)
print('generated %s' % (fil))
with open('run3b.sh','w') as runf:
    runf.write('#copy alpha value to %s\nalm %s > log3b\n'
            % (fil, fil) )

fil='in4.scph'
dic_general = {
                'PREFIX':'out4', 
                'MODE':'SCPH', 
                'NKD':len(elements), 
                'KD':' '.join(elements),
                'FCSXML':'out3.xml',
                'TMIN':50,
                'TMAX':300,
                'DT':50
                }
dic_scph = {
            'SELF_OFFDIAG':0,
            'MAXITER':500,
            'MIXALPHA':0.2,
            'KMESH_INTERPOLATE':'2 2 2',
            'KMESH_SCPH':'2 2 2'
            }
dics = [dic_general, dic_cell_prim, dic_qpoints, dic_scph]
names = ['general', 'cell', 'kpoint', 'scph']
write_alm(fil, dics, names)
print('generated %s' % (fil))
with open('run4.sh','w') as runf:
    runf.write('#Set Temperature in in4.scph\nanphon %s > log4\npython $SCRIPT/alm_4* out1.bands out4.scph_bands --temps 0 300\n'
            % (fil) )

fil='in5.rta'
dic_general = {
                'PREFIX':'out5', 
                'MODE':'RTA', 
                'NKD':len(elements), 
                'KD':' '.join(elements),
                'FCSXML':'out3.xml',
                'FC2XML':'out2_.xml',
                'TMIN':50,
                'TMAX':300,
                'DT':50
                }
dic_qpoints = [ '\t2\n\t10 10 10\n' ]
dics = [dic_general, dic_cell_prim, dic_qpoints]
names = ['general', 'cell', 'kpoint' ]
write_alm(fil, dics, names)
print('generated %s' % (fil))
with open('run5_intermediate.sh','w') as runf:
    runf.write('if [ $1 ]; then\n')
    runf.write('\tdfc2 out1.xml out2_$1.xml out4.scph_dfc2 $1\n' )
    runf.write('else\n\techo need to provide a temperature\nfi\n')
    runf.write('## need to edit temperature of in5.rta by hand\n')
with open('run5a.sh', 'w') as runf:
    runf.write('echo Remember to edit Temp range in the template in5.rta\n')
    runf.write('for temp in $@; do \n')
    runf.write('\techo temperature=${temp}K\n')
    runf.write('\tsh run5_intermediate.sh ${temp}\n')
    runf.write('\tcp in5.rta in5_${temp}.rta\n' )
    runf.write('\tsed -i  \'s/out5/out5_\'${temp}\'/g;s/out2_.xml/out2_\'${temp}\'.xml/g\' in5_${temp}.rta\n')
    runf.write('\techo \'#anphon\' in5_${temp}.rta >> run5b.sh \n')
    runf.write('done\n')
with open('run5b.sh', 'w') as runf:
    runf.write('cp $SUBMITALM2 submitalm2.job\n')
    runf.write('for temp in $@; do\n')
    runf.write('\tsbatch submitalm2.job $temp\n')
    runf.write('done\n')
