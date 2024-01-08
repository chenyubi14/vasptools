cwd=${PWD}
source ~/.myshrc
for f in dis_{001..'${f: -3}'} 
do  
    #cp $cwd/perfect/WAVECAR $cwd/$f
    ln -s $cwd/perfect/WAVECAR $cwd/$f/WAVECAR
    #cp $cwd/INCAR $cwd/$f
    cd $cwd/$f
    run
done
