# In[1]: initialize
source ~/.myshrc

cwd=$PWD

if [ -z $1 ] || [ -z $2 ] || [ -z $3 ]; then
    echo -e 'Enter 3 arguments: folder 1, folder 2 , diffusion folder name, images[optional]' 
    exit 0
fi
folder1=$1 # initial position
folder2=$2 # final position
diff_folder=$3 # diffusion folder

# create diffusion folder
if [ -d $diff_folder ]; then 
	echo 'diffusion folder exist, should remove by yourself'
	echo 'rm -r ' $diff_folder # remove the old freysoldt folder
	exit 0
fi
mkdir $diff_folder

# In[2]: edit readme
touch $diff_folder/readme
echo -e ' POSCAR1 is from CONTCAR of ' $folder1 '\n POSCAR2 is from CONTCAR of ' $folder2 > $diff_folder/readme

# In[3]: copy files 
cp $folder1/INCAR $diff_folder
cp $folder1/KPOINTS $diff_folder
cp $folder1/POTCAR $diff_folder
cp $folder1/CONTCAR $diff_folder/POSCAR1
cp $folder2/CONTCAR $diff_folder/POSCAR2
cp $folder1/DEFECT $diff_folder

#echo $folder1 $folder2 $folder1/INCAR $folder2/CONTCAR
# there is no difference between two // and one /, for example: diffusion//INCAR is the same as diffusion/INCAR

# In[4]: get number of images
cd $diff_folder
img_dist=$( nebdist POSCAR1 POSCAR2 )
img_num=$( python $SCRIPT/inter_neb_incar2.calcimg.py $img_dist )

if [ $4 ]; then
	img_num=$4
fi

# In[5]: make NEB folders
nebmake POSCAR1 POSCAR2 $img_num

# In[6]: edit INCAR
echo -e '\nNEB dist is ' $img_dist, ' \nImage number=dist/0.8=' $( python $SCRIPT/inter_neb_incar3.calcimg.py $img_dist )
echo -e 'the number of images is ' $img_num
echo -e 'edit INCAR for NEB calculation...'
python ${SCRIPT}/update_edit_incar.py neb $img_num
cd $cwd


# In[7]:
# copy OUTCAR
last_img_folder=0` expr $img_num + 1 `
cp $folder1/OUTCAR $diff_folder/00
cp $folder2/OUTCAR $diff_folder/$last_img_folder
echo -e 'copied OUTCAR to 00 and' $last_img_folder


# In[8]:
# use submit.neb.job
echo -e '\nRemember to use NEB job script! ' #(binary should end with _vtst)
# Stampede only has one stampede2*_vtst
# cori has all, like gamma_sampling