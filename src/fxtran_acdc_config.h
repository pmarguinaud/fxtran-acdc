#ifndef FXTRAN_ACDC_CONFIG_H
#define FXTRAN_ACDC_CONFIG_H

#define FXTRAN_ACDC_NVIDIA       1
#define FXTRAN_ACDC_AMDROCM      2
#define FXTRAN_ACDC_OPENACC      3
#define FXTRAN_ACDC_OPENMPTARGET 4

#if FXTRAN_ACDC_PRAGMA == FXTRAN_ACDC_OPENACC
#define _FXTRAN_ACDC_USE_OPENACC
#elif FXTRAN_ACDC_PRAGMA == FXTRAN_ACDC_OPENMPTARGET
#define _FXTRAN_ACDC_USE_OPENMPTARGET
#else
#error No accelerator pragma defined
#endif

#if FXTRAN_ACDC_VENDOR == FXTRAN_ACDC_NVIDIA
#define _FXTRAN_ACDC_USE_CUBLAS
#elif FXTRAN_ACDC_VENDOR == FXTRAN_ACDC_AMDROCM
#define _FXTRAN_ACDC_USE_ROCBLAS
#else
#error No accelerated blas package was found
#endif

#if FXTRAN_ACDC_VENDOR == FXTRAN_ACDC_NVIDIA
#define _FXTRAN_ACDC_USE_NVTX
#elif FXTRAN_ACDC_VENDOR == FXTRAN_ACDC_AMDROCM
#define _FXTRAN_ACDC_USE_ROCTX
#else
#error No tx package was found
#endif

#ifdef _FXTRAN_ACDC_USE_OPENACC

#define FXTRAN_ACDC_ROUTINE_SEQ          !$ACC ROUTINE SEQ
#define FXTRAN_ACDC_ENTER_DATA_CREATE(x) !$ACC ENTER DATA CREATE (x)
#define FXTRAN_ACDC_EXIT_DATA_DELETE(x)  !$ACC EXIT DATA DELETE (x)
#define FXTRAN_ACDC_UPDATE_DEVICE(x)     !$ACC UPDATE DEVICE (x)
#define FXTRAN_ACDC_ENTER_DATA_ATTACH(x) !$ACC ENTER DATA ATTACH (x)
#define FXTRAN_ACDC_EXIT_DATA_DETACH(x)  !$ACC EXIT DATA DETACH (x)

#define fxtran_acdc_routine_seq_begin    _Pragma ("acc routine seq") 
#define fxtran_acdc_routine_seq_end      //

#endif

#ifdef _FXTRAN_ACDC_USE_OPENMPTARGET

#define FXTRAN_ACDC_ROUTINE_SEQ          !$OMP DECLARE TARGET
#define FXTRAN_ACDC_ENTER_DATA_CREATE(x) !$OMP TARGET ENTER DATA MAP (ALLOC: x)
#define FXTRAN_ACDC_EXIT_DATA_DELETE(x)  !$OMP TARGET EXIT DATA MAP (DELETE: x)
#define FXTRAN_ACDC_UPDATE_DEVICE(x)     !$OMP TARGET UPDATE TO (x)
#define FXTRAN_ACDC_ENTER_DATA_ATTACH(x) !$OMP TARGET ENTER DATA MAP (TO: x)
#define FXTRAN_ACDC_EXIT_DATA_DETACH(x)  !$OMP TARGET EXIT DATA MAP (RELEASE: x)

#define fxtran_acdc_routine_seq_begin    _Pragma ("omp declare target") 
#define fxtran_acdc_routine_seq_end      _Pragma ("omp end declare target")

#endif

#endif
