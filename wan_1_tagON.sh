echo 'turn wannier tag ON in INCAR'
python $SCRIPT/update_edit_incar.py wannier1
source ~/.myshrc
echo 'submit job by run wannier1'
echo 'note 631_wannier cannot use NCORE!=1 '
