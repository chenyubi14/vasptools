echo -e 'Instructions for running sscha:
\t   Need VASP inputs, band.conf
(1) First step: obtain 0K phonon dispersion as initial condition
     Will remove the imaginary frequencies for starting sscha
     below are three methods M1 M2 M3
\tM1. Use sscha_1a*.py
\t   python $SCRIPT/sscha_1a* 4 4 4 --kgrid 3 3 3
\t   Run from scratch, finite_displacement jobs by vasp
\t   Need vasp inputs
\tM2. Use sscha_1b*.py
\t   Already calcuated fin_disp with QE
\t   Need the dynamic files in QE format
\tM3. Use sscha_1c*.py
\t   Already calcuated fin_disp with VASP, currently the code for this is not working
\t   from phonopy outputs, need phonopy.yaml and FORCE_CONSTANTS
\t   BORN correction is through phonopy standard procedure

(2) Plot 0K phonon (positive frequency only for initialization)
\t   Need band.conf to read phonon path
\ta. For single plot
\t   python $SCRIPT/sscha_2a*.py --qe_file start_sscha --label 300K
\tb. For multiple plots
\t   python $SCRIPT/sscha_2b*.py dyn80_ dyn100_ dyn200_ --labels 80 100 200

(3) Manual submission by VASP
\t   mkdir b1_300K
\t   Copy and check INCAR,KPOINTS,POTCAR as in fin_disp
\t   link start_sscha* files: ln -s ../start_sscha* .
\ta. First run population=1
\t   python $SCRIPT/sscha_3a* 300 1 --num_conf 50
\t   sh $SCRIPT/sscha_3b* 1
\t   (optional MLFF) sh $SCRIPT/sscha_mlff1* run_job1 mlff_job1
\t   (run vasp in the created run_job/ folder)
\t   (POTCAR might be different because of weird supercell stacking)
\t   (update_edit_potcar.sh generate a new one, update ../POTCAR as well)
\t   sh $SCRIPT/sscha_3c* 1
\t   python $SCRIPT/sscha_3d* 300 1
\tb. Second run population=2
\t   python $SCRIPT/sscha_3a* 300 2
\t   sh $SCRIPT/sscha_3b* 2
\t   (run vasp in the created run_job/ folder)
\t   sh $SCRIPT/sscha_3c* 2
\t   python $SCRIPT/sscha_3d* 300 2
\tc. Continue with more populations until convergence
\t   python $SCRIPT/sscha_3a* 300 3
\t   sh $SCRIPT/sscha_3b* 3
\t   sh $SCRIPT/sscha_3c* 3
\t   python $SCRIPT/sscha_3d* 300 3
\t   ......
\td. Check convergence where gradient goes to 0, and K-L ratio > 0.5
\t   sh $SCRIPT/sscha_3f* (will plot minimization data)
\te. Remember to increase num_conf to further check convergence
\tf. Some ab-init jobs may not be successful, should reorder the jobs
     sh $SCRIPT/sscha_modify1_run-job-order.sh begin=$1, end=$2, newbegin=$3
     sh $SCRIPT/sscha_modify2_popu-order.sh begin=$1, end=$2, newbegin=$3

(4) Automatic submission (not recommended)
\t   Running first-principles jobs are super slow. 
\t   python $SCRIPT/sscha_3g*
\t   Automatic with cluster is currently not working for VASP. 

(5) Compute hessian with more configuration
     Only After convergence in (3), say T=300K, num_conf=5000
\ta. sh $SCRIPT/sscha_4a* 300 5000
\t   if run MLFF, need "sh $SCRIPT/sscha_mlff1* run_job.hessian2 mlff_job2"
\t   (create 5000 vasp jobs in the created run_job.hessian/ folder)
\t   (run vasp jobs)
\tb. sh $SCRIPT/sscha_4b* 300
     It will give you two commands, run them in order
     sscha_3c* to process run_job.hessian/ data
     sscha_4c* to get hessian dyn files
\tc. plot phonon
     python $SCRIPT/sscha_2a_phonon_single.py --qe_file hessian_ --label 30K
     python $SCRIPT/sscha_2b*.py fol1/hessian_ fol2/hessian -l 200K 100K to plot multiple
\td. to update hessian at a difference temperature
     sscha_4e_update-T.py old_T new_T population_id

(6) MLFF to run VASP
     Ab initio calculations are too expensive, use MLFF to run VASP instead
\ta. should obtain ML_FF by MD training
     update_edit_incar.py mlff1 (training MLFF)
     Recommend NPT-Langevin with higher T and ENCUT
     Also need higher KPOINTS
     before training, double check POTCAR will be consistent
\tb. use test data to check ML_FF
     cp ML_FFN /test/folder/ML_FF
     use ensemble generated data to run ab-init and MLFF
     sh $SCRIPT/sscha_mlff1* reference_fol run_fol
     (update_edit_incar.py mlff1 already implemented in mlff1*)
     ln -s ../ML_FF # in run_fol
     update POTCAR: default POSCAR will be sorted for MLFF to work
     manage.sh # run all the jobs
\tc. read out forces from vasprun.xml for reference_fol & run_fol
     sh $SCRIPT/sscha_mlff2* reference_fol
     sh $SCRIPT/sscha_mlff2* run_fol
\td. energy/force distances, comparison with test data 
     python $SCRIPT/sscha_mlff3* reference_fol run_fol
\te. use ML_FF 
     copy ML_FF to run_job/
     update_edit_incar.py mlff2 # in run_job/
 '
