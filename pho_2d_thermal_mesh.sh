#!/usr/bin/env bash

datafile=thermal_properties.dat

phonopy -p -s -t mesh.conf > $datafile
#awk '/temperature/{ print $2 }' thermal_properties.yaml  > thermal_T.dat
#awk '/heat_capacity/{ print $2 }' thermal_properties.yaml > thermal_Cv.dat
## print with line number
nl $datafile

### line number with required beginning
## the line #      T [K]      F [kJ/mol]    S [J/K/mol]  C_v [J/K/mol]     E [kJ/mol]
line1=$(awk '/[K]/{print NR}' $datafile ) 
## the line Summary of calculation was written in "phonopy.yaml".
line2=$(awk '/Summary/{print NR-2}' $datafile)
echo 'selected line' $line1'(-4)' to $line2'(-4)'

sed -n ${line1},${line2}p  $datafile > thermal_properties.data
mv thermal_properties.data $datafile


