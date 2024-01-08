import cellconstructor as CC, cellconstructor.Phonons
import sscha
import sscha.Cluster

import sys, os


def configure_cluster(dirname = "mater_work", ncores=4):
    ## Set use_active_shell=True means login by ssh
    cluster = sscha.Cluster.Cluster(
            hostname = "yubi@login.expanse.sdsc.edu",
            mpi_cmd = 'srun',
            binary='/home/yubi/bin/vasp.641.expanse_std',
            #qos_name='Test',
            )

    ## Set partition
    cluster.use_partition = True
    cluster.partition_name = "shared"
    ## Don't set account name in pod, but in expanse
    cluster.use_account = True
    cluster.account_name = "csb181"
    ## Don't use qos
    cluster.use_qos = False
    ##  add 'set -x'
    ## print on stdout all executed commands
    cluster.add_set_minus_x = False

    cluster.time = "48:00:00"
    cluster.n_nodes = 1
    cluster.n_cpu = ncores
    ### n_cpu is NPROC  
    ### It is the number of processors for a mpirun execution
    ### mpirun -np NPROC /path/to/binary


    cluster.n_pool = 2
    ### n_pool is k-point parallel in QE
    ### k-points splitted into n_pool groups
    cluster.job_number = 1
    ### if job_number=2, will write two configurations (mpirun ...) in one job script
    cluster.batch_size = 100
    ### Total number of job limitations in the partition

    #cluster.use_cpu = F4lse
    #cluster.custom_params["get-user-env"] = None
    #cluster.custom_params["cpus-per-task"] = 1
    cluster.custom_params["ntasks-per-node"] = ncores 
    ##  setup the memory usage
    cluster.use_memory = True
    cluster.ram = str(ncores*2) + 'G'
    #cluster.custom_params['mem'] = str(ncores*2) + 'G' ## double num_tasks
    cluster.custom_params['exclude'] = 'exp-6-56'
    cluster.custom_params['output'] = 'expanse.%j.%N.out'
    cluster.custom_params['mail-user'] = 'chenyubi14@gmail.com'
    cluster.custom_params['mail-type'] = 'ALL'

    home_workdir=os.path.join("/expanse/lustre/scratch/yubi/temp_project/sscha_test", dirname)
    ## copy files from scratch_workdir to cluster.workdir
    #scratch_workdir = os.path.join("/home/yubi/work/Au_gold/02_sscha/b1_tut2_manual", dirname)
    cluster.workdir = home_workdir
    cluster.local_workdir = "cluster_work/"
    ### Another folder cluster_work, a local directory not inside cluster
    ### Used to save all the input files, no output files

    cluster.load_modules = f"""

ulimit -s unlimited

module reset 1> /dev/null 2>&1
module unload cpu/0.17.3b
module load cpu/0.15.4
module load intel/19.1.1.217
module load mvapich2/2.3.4
module load intel-mkl/2018.1.163

/bin/hostname

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

"""

    def before_after_binary(lbls=[]):
        ### Can add more commands right before and after mpirun
        ### The lbls are the labels for multiple (mpirun binary)
        ### Originally, this part is used to move files from/to scratch folder
        extrain = f"""
info_file=time.info
cat job.number | sed -n '/Submitted/p' >> $info_file
echo "HOSTNAME=$(hostname)" >> $info_file
echo "STARTTIME=$(date --iso-8601=ns)" >> $info_file

"""
        extraout = f"""
echo "ENDTIME=$(date --iso-8601=ns)" >> $info_file

echo $PWD >> /home/yubi/finished.jobs.number
cat job.number | sed -n '/Submitted/p' >> /home/yubi/finished.jobs.number
echo "ENDTIME=$(date --iso-8601=ns)" >> /home/yubi/finished.jobs.number
        """
        return extrain, extraout

    # Add the possibility to copy the input files
    cluster.additional_script_parameters = before_after_binary

    # Force to open a shell when executing ssh commands
    # (Otherwise the cluster will not load the module environment)
    #cluster.use_active_shell = True
    cluster.setup_workdir()

    # Check the communication
    if not cluster.CheckCommunication():
        raise ValueError("Impossible to connect to the cluster.")

    return cluster
