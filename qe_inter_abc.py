#!/usr/bin/env python
import numpy as np


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
                #assert 'ATOMIC_POSITIONS' in lines[i+5], 'ATOMIC_POSITIONS not found!'
                #j = 6
                #splitted = lines[i+6].split()
                #while len(splitted) == 4:
                #    j = j+1
                #    splitted = lines[i+j].split()
                #coord = lines[i+6:i+j]
                break
    return lines[i], cell, #coord


def get_celldm(fil='out.qe'):
    ## format 2, numpy array
    lines_out, cell,  = extract_out(fil)
    ## lines_out = 'CELL_PARAMETERS (angstrom)'
    cell2 = np.array([ l.split() for l in cell ]).astype(float)
    #coord2 = np.array([ l.split() for l in coord ] )
    #atom_type2 = coord2[:,0]
    #coord2 = coord2[:,1:].astype(float)
    print('%s' % (cell2, ) )

    celldm1 = np.linalg.norm(cell2[0])
    celldm2 = np.linalg.norm(cell2[1])
    celldm3 = np.linalg.norm(cell2[2]) 
    print('a=%.5f b=%.5f c=%.5f' % (celldm1, celldm2, celldm3) )
    return [celldm1, celldm2, celldm3]


