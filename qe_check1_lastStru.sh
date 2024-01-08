#!/usr/bin/env bash

fil=out.qe

ended=$(sed -n '/End final coordinates/p' $fil )
if [[ $ended ]]; then
    tac $fil | sed -n '/End final coordinates/,/CELL_PARAMETERS/p' | tac
else
    #tac $fil | sed -n '0,/CELL_PARAMETERS/{/ATOMIC_POSITIONS/,/CELL_PARAMETERS/p}' | tac
    tac $fil | sed -n '0,/CELL_PARAMETERS/{/Writing output data/,/CELL_PARAMETERS/p}' | tac
fi
