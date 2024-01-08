import cellconstructor as CC, cellconstructor.Phonons
import sscha
import sscha.Cluster

import sys, os


def configure_cluster(dirname = 'mater_work'):
    ## Set use_active_shell=True means login by ssh
    cluster = sscha.Cluster.Cluster(
            hostname = "yubi@pod-login1.cnsi.ucsb.edu",
            mpi_cmd = 'mpirun',
            binary="pw.x -npool NPOOL -i PREFIX.pwi > PREFIX.pwo",
            #qos_name='Test',
            )

    ## Don't setup the memory usage
    cluster.use_memory = False
    #cluster.ram = 180000

    ## Set partition
    cluster.use_partition = True
    #cluster.partition_name = "batch"
    cluster.partition_name = "short"

    ## Don't set account name in pod, but in expanse
    cluster.use_account = False
    #cluster.account_name = "my_allocation_resources"

    ## Don't use qos
    cluster.use_qos = False

    ##  add 'set -x'
    ## print on stdout all executed commands
    cluster.add_set_minus_x = False

    cluster.n_nodes = 1
    cluster.n_cpu = 4
    #cluster.use_cpu = F4lse
    #cluster.custom_params["get-user-env"] = None
    #cluster.custom_params["cpus-per-task"] = 1
    cluster.custom_params["ntasks-per-node"] = 4
    cluster.time = "2:00:00"
    cluster.n_pool = 2
    cluster.job_number = 3
    cluster.batch_size = 50

    home_workdir=os.path.join("$HOME/work", dirname)
    ## copy files from scratch_workdir to cluster.workdir
    scratch_workdir = os.path.join("/home/yubi/work/Au_gold/02_sscha/b1_tut2_manual", dirname)
    cluster.workdir = home_workdir
    cluster.load_modules = f"""

module load intel
module load mpi
module load mkl

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

mkdir -p {scratch_workdir}
cp $HOME/espresso/pseudo/* {scratch_workdir}/
"""

    def cp_files(lbls):
        extrain = f"cd {scratch_workdir}\n"
        extraout = "sleep 1\n"
        for lbl in lbls:
            extrain += f"cp {home_workdir}/{lbl}.pwi {scratch_workdir}/\n"
            extraout += f"mv {scratch_workdir}/{lbl}.pwo {home_workdir}/\n"

        return extrain, extraout

    # Add the possibility to copy the input files
    cluster.additional_script_parameters = cp_files

    # Force to open a shell when executing ssh commands
    # (Otherwise the cluster will not load the module environment)
    #cluster.use_active_shell = True
    cluster.setup_workdir()

    # Check the communication
    if not cluster.CheckCommunication():
        raise ValueError("Impossible to connect to the cluster.")

    return cluster
