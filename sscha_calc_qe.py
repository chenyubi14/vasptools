import cellconstructor as CC
from cellconstructor.calculators import Espresso
import sys, os
import numpy as np

def get_calculator():
    
    pseudopotentials = {
            "H" : "H.pbe-rrkjus_psl.1.0.0.UPF",
            "S" : "s_pbe_v1.4.uspp.F.UPF"
        }

    input_params = {
        "control" : {
            "tprnfor" : True,
            "tstress" : True,
            "disk_io" : "none"
        },
        "system" : {
            "degauss" : 0.03,
            "smearing" : "mv",
            "ecutwfc" : 35,
            "ecutrho" : 350,
            "occupations" : "smearing"
        },
        "electrons" : {
            "conv_thr" : 1e-10
        }
    }

    #kpts = (4,4,4)
    kpts = (2,2,2)
    koffset = (1, 1, 1)

    calc = Espresso(pseudopotentials = pseudopotentials,
                    input_data = input_params,
                    kpts = kpts,
                    koffset = koffset)

    return calc
