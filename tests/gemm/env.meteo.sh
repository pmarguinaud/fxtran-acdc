
module load gcc/nvhpc24.7

export CUDA=12.5
export NVTXLIBS="-L$NVHPC_ROOT/cuda/$CUDA/lib64 -Wl,-rpath,$NVHPC_ROOT/cuda/$CUDA/lib64 -lnvhpcwrapnvtx -lnvToolsExt"

ARCHLIST="NVIDIA_OPENACC NVIDIA_OPENMPTARGET"

