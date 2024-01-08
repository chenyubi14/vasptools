
## number of electrons
#nele0=$(awk 'NR==6 {print $1}' EIGENVAL) # read the first word of line 6 in EIGENVAL
#nele1=`expr  $nele + 1 `

nele0=36
nele1=37
nele2=38
nele3=39
nele4=35


awk  'NR>6 && $4>0 || / '$nele0' / || / '$nele1' / || / '$nele2' / || / '$nele3'  / || / '$nele4' /  {print $0}' EIGENVAL  > EIGENVAL.selected
