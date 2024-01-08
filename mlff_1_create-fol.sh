#!/usr/bin/env bash

echo '(1) suppose MD inputs are present (supercell POSCAR, MD INCAR, loose grid KPOINTS, POTCAR)'
echo '(2) update INCAR to use NPT Langevin dynamics (PSTRESS=0)' 
echo -e '(3) update INCAR to train MLFF, with NSW=10000' \n
python $SCRIPT/update_edit_incar.py md-npt
python $SCRIPT/update_edit_incar.py md2
python $SCRIPT/update_edit_incar.py mlff1

## create folders
cp $SCRIPT/template.setup_converge.py setup_converge.py
echo -e '\n double check setup_converge.py file'
echo -e ' edit LANGEVIN_GAMMA in INCAR'
echo -e ' python $SCRIPT/op_var0*'
echo -e ' mlff_2*'

