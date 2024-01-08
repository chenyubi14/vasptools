#!/usr/bin/env python
import numpy as np
import argparse
import os
import sys


parser = argparse.ArgumentParser(description='get newest structure')
parser.add_argument('ibrav', type=int, default=0,
        help='bravais lattice type')
parser.add_argument('--outp', type=str, default='out.qe',
        help='QE output file')
parser.add_argument('--inp', type=str, default='in.qe',
        help='QE input file')
parser.add_argument('--inter', type=str, default='in.intermediate',
        help='intermediate file for QE input file')
args = parser.parse_args()
ibrav = args.ibrav
fil1 = args.inp
fil2 = args.outp
fil3 = args.inter

### get newest structure from out.qe: cell and coord
## fotmat 1, string
def extract_out(fil):
    with open(fil, 'r') as f:
        lines = f.readlines()
        for i in range(len(lines)-1, 0, -1):
            l = lines[i]
            if 'CELL_PARAMETERS' in l:
                cell = lines[i+1:i+4]
                ## atomic positions @ lines[i+5]
                assert 'ATOMIC_POSITIONS' in lines[i+5], 'ATOMIC_POSITIONS not found!'
                j = 6
                splitted = lines[i+6].split()
                while len(splitted) == 4:
                    j = j+1
                    splitted = lines[i+j].split()
                coord = lines[i+6:i+j]
                break
    return lines[i], cell, coord
## format 2, numpy array
lines_out, cell, coord = extract_out(fil2)
## lines_out = 'CELL_PARAMETERS (angstrom)'
cell2 = np.array([ l.split() for l in cell ]).astype(float)
coord2 = np.array([ l.split() for l in coord ] )
atom_type2 = coord2[:,0]
coord2 = coord2[:,1:].astype(float)
print('cell=\n%s\natoms=%s\ncoord=\n%s' % (cell2, atom_type2, coord2) )


### get default input from in.qe
def extract_in(fil, ):
    ind1 = None
    ind2 = None
    ind3 = None
    with open(fil, 'r') as f:
        ori_lines = f.readlines()
    ### get the lines of input file, removing celldm parameters
    remove_list = []
    for j in range(len(ori_lines)):
        l = ori_lines[j]
        if 'celldm' in l:
            remove_list.append(j)
            print('removed ',l)
    mask = np.ones(len(ori_lines), dtype=bool)
    mask[remove_list] = False
    ## need numpy array to use mask, ori_lines was a list
    lines = np.array(ori_lines)[mask] ## new input after removal

    for i in range(len(lines)):
        l = lines[i]
        if 'CELL_PARAMETERS' in l:
            ind1 = i
        if 'ATOMIC_POSITIONS' in l:
            ind2 = i
        if 'ibrav' in l:
            ind3 = i
    assert ind2 != None and ind3 != None, 'ibrav or ATOMIC_POSITIONS is not found '
    #mask = np.ones(len(lines))
    #lines[ind1+1 : ind1+4] = cell_strs
    #lines[ind2+1 : ind2+len(coord_strs)+1] = coord_strs
    return lines, ind1, ind2, ind3
lines_in, ind1, ind2, ind3= extract_in(fil1, )
ibrav_input = int(lines_in[ind3].split()[-1])
if ibrav_input != ibrav:
    print('Warning! ibrav inconsistent %s!=%s, do you want to continue? y(yes)' % (ibrav_input, ibrav))
    cont = input()
    if cont != 'y' and cont != 'yes':
        sys.exit()
    print('\tRemeber to edit ibrav!!!!')


def remove_cell(lines, ind1):
    ## if ind1 is None, don't need to remove anything
    ## if ind1 exist, remove the CELL_PARAMETERS
    ## need to refind ind1, because might have added lines elsewhere
    if ind1: ## found cell parameters, remove it!
        for i in range(len(lines)):
            if 'CELL_PARAMETERS' in lines[i]:
                ind1 = i
        mask = np.ones(len(lines), dtype=bool)
        mask[ind1:ind1+4] = False
        lines = np.array(lines)[mask]
    return lines

def write_lines(fil, lines):
    lines = list(lines)
    string = ''.join(lines)
    with open(fil, 'w') as f:
        f.write(string)

def convert_unit(dm_list, unitinfo):
    import re
    dm_list = np.array(dm_list)
    if 'angstrom' in unitinfo:
        dm_list = dm_list * 1.88973
    elif 'bohr' in unitinfo:
        dm_list = dm_list
    elif 'alat' in unitinfo:
        alat = re.findall("[-+]?[.]?[\d]+(?:,\d\d\d)*[\.]?\d*(?:[eE][-+]?\d+)?", unitinfo)
        alat = float(alat[0])
        dm_list = dm_list * alat
    else:
        print('Error! unit not recognized')
        sys.exit()
    return dm_list

def line_replace(lines, beg_end, str_replace):
    ori = list(lines)
    new = []
    beg = beg_end[0]
    end = beg_end[1]
    for i in range(0,beg):
        new.append(ori[i])
    for j,i in enumerate(range(beg, end)):
        new.append(str_replace[j])
    for i in range(end,len(ori)):
        new.append(ori[i])
    return new

## update in.qe depending on the lattice type
if ibrav == 0:
    print('ibrav=0, directly find and replace new structures')
    #lines_in[ind1+1:ind1+4] = cell 
    lines_in = line_replace(lines_in, [ind1+1, ind1+4], cell)
    #lines_in[ind2+1:ind2+len(coord)+1] = coord
    lines_in = line_replace(lines_in, [ind2+1, ind2+len(coord)+1], coord)
    write_lines(fil3, lines_in)
elif ibrav == 1:
    print('cubic P sc')
    #lines_in[ind2+1:ind2+len(coord)+1] = coord
    lines_in = line_replace(lines_in, [ind2+1, ind2+len(coord)+1], coord)
    celldm1 = np.linalg.norm(cell2[0])
    dm_new = convert_unit([celldm1], lines_out)
    celldm1, = dm_new
    newline1 = '  celldm(1) = ' + str(celldm1) + '\n'
    lines_in = remove_cell(lines_in, ind1)
    lines_in[ind3] = '  ibrav = %s\n' % ibrav
    lines_in = np.insert(lines_in, ind3+1, newline1)
    write_lines(fil3, lines_in)
elif ibrav == 2:
    print('cubic F fcc')
    #lines_in[ind2+1:ind2+len(coord)+1] = coord
    lines_in = line_replace(lines_in, [ind2+1, ind2+len(coord)+1], coord)
    celldm1 = np.linalg.norm(cell2[0])
    celldm1 = celldm1 * np.sqrt(2)
    dm_new = convert_unit([celldm1], lines_out)
    celldm1, = dm_new
    newline1 = '  celldm(1) = %s \n' % ( celldm1 )
    lines_in = remove_cell(lines_in, ind1)
    lines_in[ind3] = '  ibrav = %s\n' % ibrav
    lines_in = np.insert(lines_in, ind3+1, newline1)
    write_lines(fil3, lines_in)
elif ibrav == 4:
    print('hexagonal/trigonal')
    #lines_in[ind2+1:ind2+len(coord)+1] = coord
    lines_in = line_replace(lines_in, [ind2+1, ind2+len(coord)+1], coord)
    celldm1 = np.linalg.norm(cell2[0])
    celldm3 = np.linalg.norm(cell2[2]) 
    dm_new = convert_unit([celldm1, celldm3], lines_out)
    celldm1, celldm3 = dm_new
    celldm3 = celldm3 / celldm1
    newline1 = '  celldm(1) = ' + str(celldm1) + '\n'
    newline3 = '  celldm(3) = ' + str(celldm3) + '\n'
    lines_in[ind3] = '  ibrav = %s\n' % ibrav
    lines_in = np.insert(lines_in, ind3+1, newline3)
    lines_in = np.insert(lines_in, ind3+1, newline1)
    lines_in = remove_cell(lines_in, ind1)
    write_lines(fil3, lines_in)
else:
    print('ibrav not recognized, need to modify this python file to include more features')


