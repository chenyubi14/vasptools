## run format: $SCRIPT/out_freysoldtcorrection_energy.sh charge pwd reference_folder
##echo 'This file runs freysoldt correction for a single folder.' ' If multiple folders, try run with energyf_.py files'

average=2 # (bohr) average in order to smoothen wiggly potentials 

function calc_freysoldt_correction(){
    # get arguments
    charge=$1 # 1,2,3, -1,-2,-3
    defecttype=$2
    referencedir=$3 #referencedir=${ZEROFOL}/berylliumO/wurtzite_01_super_perfect/perfect_run
    ref=$4 # bulk/defect as reference
    this_eps=($5 $6) # eps 6.7 is treated as two arguments
    freycorr=$7 # name of freycorr key
    cutoffE_Ry=$8
    average=$9
    #echo $2 $3 $4 #$4 $7 ${this_eps[@]}

    # Step 0: initialize parameters
    # update DEFECT file to get --center for freysoldt
    sh ${SCRIPT}/inter_defect_update_CENTER.sh 
    sxcharge=` expr 0 - $charge ` # freysoldt charge is opposite to the defect charge
    freydir='freysoldt_correction_ref-'${ref} # freysoldt directory name

    # Step 1.1: get files ready. Copy LOCPOT (target/reference) to $freydir
    if [ -e 'LOCPOT' ]; then # file LOCPOT exists
        #echo 'copy files to ' $freydir
        if [ -d $freydir ]; then 
            rm -r $freydir # remove the old freysoldt folder
        fi
        mkdir $freydir
        cp LOCPOT $freydir/defectLOCPOT
        cp ${referencedir}/LOCPOT $freydir/neutralrefLOCPOT
        cp DEFECT $freydir # DEFECT is update to date by the first command : sh inter_defect_position.sh
    else
        echo 'PWD does not have LOCPOT, maybe the job is not finished'
        exit 0
    fi

    # Step 1.2: get defect position and eps
    defectcenter=$(cat DEFECT | sed -n '/CENTER/{s/CENTER=//;s/#.*//;p}' ) # remove keyword and remove comments #.*

    # Step 1.3: run freysoldt correction
    cd $freydir
    printf 'sxdefectalign --ecut %s --charge %s --%s %s --center %s --average %s --relative --vdef defectLOCPOT --vref neutralrefLOCPOT --vasp > sx1.fc \n' $cutoffE_Ry $sxcharge ${this_eps[@]} $defectcenter $average  > sx1.sh
    cat sx1.sh
    chmod +x sx1.sh # run freysoldt correction
    sh sx1.sh 

    # Step 2:
    # Run sxdefectalign for the first time to draw potential alignment diagram and obtain C parameter
    # currently averaging paraC over three directions
    paraC=$(python ${SCRIPT}/draw_freysoldt_potential_alignment.py $defecttype $charge | sed -n '/paraC/{s/paraC=//;p}')
    echo -e '(Step 1) finished. Parameter C is ' $paraC 

    # Step 3:
    # run Freysoldt again to get the real correction
    printf 'sxdefectalign --ecut %s --charge %s --%s %s --center %s --average %s --relative --vdef defectLOCPOT --vref neutralrefLOCPOT --vasp -C %s > sx2.fc \n' $cutoffE_Ry $sxcharge ${this_eps[@]} $defectcenter $average $paraC  > sx2.sh
    cat sx2.sh
    chmod +x sx2.sh
    sh sx2.sh 
    freycorrenergy=$(cat sx2.fc | sed -n '/Defect correction/{s/Defect correction (eV): //;s/ (incl. screening & alignment)//;p}' )
    echo '(Step 2) finished. The freysoldt correction energy is ' $freycorrenergy
    cd ..
    python ${SCRIPT}/update_edit_defect.py $freycorr $freycorrenergy # edit DEFECT by keyword and energy
    echo -e '\nDOUBLE CHECK charge=' $charge ' defect=' $defecttype ' defect position=' $defectcenter '\n' 'at folder' $cwd '\n reference=' $referencedir  '\n\n'
}

# check input parameters
# In[1]:
# initialization
if [ -z $1 ] || [ -z $2 ] || [ -z $3 ]; then
    echo -e '\n defect charge, defect folder, and reference folder are not found! Enter the charge ($1) defect folder ($2) and perfect Supercell folder ($3) or elec ($3) for electronic.' 
    echo -e 'Reference folder only needs LOCPOT. Could use a different sampling\n Folders should start with /'
    echo -e '${WORK1}/wurtzite_01_super_perfect/perfect_run_aexx0.405\n${WORK1}/wurtzite_01_super_perfect/perfect_run_aexx0.405.specialKPOINT\n${WORK1}/wurtzite_09_super_perfect_symm/perfect_run_aexx0.405'
    echo -e '/work2/07884/tg871834/stampede2/zincO/wurtzite_02_super/perfect_runaexx0.36'
    exit 0
#else
#    echo 'lets run freysold in current folder ' ${PWD}
fi
cd $2 # goto the folder with defects
# get comments on defecttype from directory name
#cwd=$(echo ${PWD} | sed 's#.*berylliumO/##g') # remove words before berylliumO/
#defecttype=$(echo $2 | sed 's/.*wurtzite_//g' | sed 's#/defect.*##g' ) #'ibe'
#defecttype=${defecttype:0:5}

length_zero=` expr ${#ZEROFOL} + 1 ` # length of zero folder name counting with characters
cwd=$( echo ${PWD[@]:${length_zero}} ) # the current folder without ZEROFOL, like 'berylliumO/wurtzite_91_defect_complex_VO_LiBe/defect0e'
# length of zero folder, count words, like dength_zero2=4 through counting (work2 07884 tg871834 stampede2)
length_zero2=( $( echo $ZEROFOL | tr  '/', ' ' ) ) # the list (work2 07884 tg871834 stampede2)
length_zero2=` expr ${#length_zero2[@]} + 1 `
FIRSTFOL=( $( echo $PWD | tr  '/', ' ' ) ) # a list with separated folder name, like (work2 07884 tg871834 stampede2 berylliumO wurtzite_91_defect_complex_VO_LiBe defect0e)
FIRSTFOL=/$( echo ${FIRSTFOL[@]::${length_zero2}} | tr ' ' '/' ) # get /work2/07884/tg871834/stampede2/berylliumO
# get defecttype from keyword COMMENT in DEFECT
defecttype=$( cat DEFECT | sed -n '/DEFECTTYPE/p' | sed -e 's/mathit//g' -e 's/mathrm//g' -e 's/DEFECTTYPE=//g' -e 's/#.*//g' | tr -d '{}\\$' )
if [ -z $defecttype ]; then
    echo 'Error! DEFECTTYPE does not exist in DEFECT. Modify the DEFECT files!'
    exit 0
fi
# read cutoff energy and convert it to Rydberg unit
cutoffE_eV=$( cat INCAR | sed -n '/ENCUT=/p' | sed -e 's/ENCUT=//;s/#.*//' )
cutoffE_Ry=$( echo ${cutoffE_eV}*0.0735 | bc )
#echo -e $cutoffE_eV '\t' $cutoffE_Ry


# In[2]:
if [ $3 == 'elec' ] ||  [ $2 = *koopmans* ]; then
    # don't need perfect cell as reference, so $3 could be a comment like elec
    echo 'Only electronic dielectric constant is used. Should be koopmans condition'
    ref='defect' # use neutral defect cell as reference to freysoldt correction
    referencedir=$(echo $2 | sed -e 's/1e_/0e_/' ) # reference directory is neutral defect
    this_eps=$(python ${SCRIPT}/inter_eps_readdata.py) # interpolate eps from the AEXX/HFSCREEN for a given folder
    # this_eps=eps 2.8 will be treated as two arguments
    # FREYCORRELEC will be the keyword written in DEFECT file
    calc_freysoldt_correction $1 $defecttype $referencedir $ref $this_eps FREYCORRELEC $cutoffE_Ry $average # charge defecttype perfect_folder ref=defect this_eps keyword cutoff_energy
else
    echo 'Formation energy calculation ref=bulk. Total dielectric constant is used (ionic included). '
    ref='bulk'
    this_eps=$(cat ${FIRSTFOL}/ALLINFO | sed -n '/DIELEC_CONST_ALL=/{s/DIELEC_CONST_ALL=//;p}' )  # 'sed' removes keyword DIELEC_CONST_ALL 
    # this_eps='eps 6.7' read in the file ALLINFO in FIRSTFOL with keyword DIELEC_CONST_ALL # should look like EPS=eps (value) or EPS=tensor a,b,c,0,0,0
    # FREYCORRALL will be the keyword written in DEFECT file
    echo -e 'Total dielectric constant read from ALLINFO is' $this_eps '\n'
    calc_freysoldt_correction $1 $defecttype $3 $ref $this_eps FREYCORRALL $cutoffE_Ry $average # charge defecttype perfect_folder ref=bulk this_eps keyword cutoff_energy
fi


exit
