
module purge

export MODULEPATH=/perm/sor/install/nvidia/hpc_sdk/modulefiles

module load nvhpc/26.1

export CUDA=12.9
export NVTXLIBS="-lnvtx3interop"

ARCHLIST="NVIDIA_OPENACC NVIDIA_OPENMPTARGET"

