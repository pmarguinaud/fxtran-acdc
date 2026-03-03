
module purge

export MODULEPATH=/perm/sor/install/nvidia/hpc_sdk/modulefiles

module load nvhpc/26.1

export CUDA=12.9
export NVTXLIBS="-L$NVHPC_ROOT/cuda/$CUDA/targets/x86_64-linux/lib -Wl,-rpath,$NVHPC_ROOT/cuda/$CUDA/targets/x86_64-linux/lib -lnvtx3interop"

ARCHLIST="NVIDIA_OPENACC NVIDIA_OPENMPTARGET"

