nl freysoldt0_correction_ref-defect/sx2.fc | sed -n '/Defect correction/p' | sed -n 's/^.*(eV)://p' | sed 's/ (incl.*$//g'

sed -n '3p' INCAR # print the third line of INCAR
# -n means silent