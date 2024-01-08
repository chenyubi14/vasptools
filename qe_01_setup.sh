#!/usr/bin/env bash

if [[ -z $1 ]]; then
    echo 'Error! Enter header a,b,c'
    exit 0
fi

header=$1

f0=${header}0_converge
f1=${header}1_relax
f2=${header}2_scf
f3=${header}3_band
f4=${header}4_phonon
pp=pseudo


mkdir -p $f0 $f1 $f2 $f3 $f4 $pp

echo 'Get cif from materialsproject, get QE input from materialscloud'
echo '1. copy pseudo files to pseudo/'
echo '2. copy example in.qe'

echo "
&CONTROL
  prefix = ''
  calculation = 'vc-relax'
  restart_mode = 'from_scratch'
  etot_conv_thr = 1.0d-4
  forc_conv_thr = 1.0d-3
  outdir = './out/'
  pseudo_dir = '../pseudo/'
  tprnfor = .true.
  tstress = .true.
  verbosity = 'high'
/
&SYSTEM
!!ecutrho is 6 or 8 times of ecutwfc
  ecutrho = 480
  ecutwfc = 80
  ibrav = 4
  celldm(1) = 1
!  celldm(2) = 1
  celldm(3) = 1
  nat = 3
  nosym = .false.
  ntyp = 2
!!if metal, uncomment the following
!  occupations = 'smearing'
!  smearing = 'gauss'
!  degauss = 0.01
/
&ELECTRONS
  conv_thr = 1.0d-8
  electron_maxstep = 100
  mixing_beta = 0.7
  diagonalization = 'david'
  diago_david_ndim = 2
  startingwfc = 'atomic+random'
/
&IONS
!!required for vc-relax, relax, md, vc-md
!  pot_extrapolation = 'second_order'
!  wfc_extrapolation = 'second_order'
/
&CELL
!!required for vc-relax, vc-md
!  cell_dofree = 2Dxy 
/
ATOMIC_SPECIES 
Mo 95.95   Mo.upf
S  32.065  S.upf
ATOMIC_POSITIONS crystal
Mo       0.666666687   0.333333343   0.111485819
K_POINTS automatic
1 1 1 0 0 0
!!  Don't use this, use celldm instead
!CELL_PARAMETERS angstrom
!   1.000000000   0.000000000   0.000000000
!   0.000000000   1.000000000   0.000000000
!   0.000000000   0.000000000   1.000000000

" > in.reference


