#!/usr/bin/env bash
## load modules

cwd=$PWD
inter=inter.txt
ls -d *_TEBEG > $inter
fols=($(sort -n $inter) )
num=${#fols[@]}
num=` expr $num - 1 `
echo ${fols[@]} 

all1_beef=()
all2_rmse=()
all3_outcar=()
for i in `seq 0 $num`; do 
    current=${fols[$i]}
    echo $i $current
    cd $current
    if [[ -f ML_LOGFILE ]] ; then
        ## baysian error estimate of forces
        cat ./ML_LOGFILE | grep BEEF > ./BEEF.dat
        ## root mean squared error of forces
        cat ./ML_LOGFILE | grep ERR > ./ERR.dat
        all1_beef=( ${all1_beef[@]} $current/BEEF.dat )
        all2_rmse=( ${all2_rmse[@]} $current/ERR.dat )
        all3_outcar=( ${all3_outcar[@]} $current/OUTCAR )
    fi
    cd $cwd
done

echo -e ${all1_beef[@]} 
echo -e ${all2_rmse[@]} 
echo -e ${all3_outcar[@]}

python $SCRIPT/mlff_5a* ${all1_beef[@]}
python $SCRIPT/mlff_5b* ${all2_rmse[@]}

## plot MD data
#module load hdf5/1.10.5
#module load mpi 
#module load mkl 
#process_outcar_5.3.py ${all3_outcar[@]}
#tdep_3a_stat.py


