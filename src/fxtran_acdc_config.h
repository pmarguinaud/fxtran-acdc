#ifndef FXTRAN_ACDC_CONFIG_H
#define FXTRAN_ACDC_CONFIG_H

#define FXTRAN_ACDC_NVIDIA       1
#define FXTRAN_ACDC_AMDROCM      2
#define FXTRAN_ACDC_OPENACC      3
#define FXTRAN_ACDC_OPENMPTARGET 4

#if FXTRAN_ACDC_PRAGMA == FXTRAN_ACDC_OPENACC
#define _FXTRAN_USE_OPENACC
#else
#error No accelerator pragma defined
#endif

#if FXTRAN_ACDC_VENDOR == FXTRAN_ACDC_NVIDIA
#define _FXTRAN_USE_CUBLAS
#else
#error No accelerated blas package was found
#endif

#ifdef _FXTRAN_USE_OPENACC
#define FXTRAN_ACDC_ROUTINE_SEQ          !$ACC ROUTINE SEQ
#define FXTRAN_ACDC_ENTER_DATA_CREATE(x) !$ACC ENTER DATA CREATE (x)
#define FXTRAN_ACDC_EXIT_DATA_DELETE(x)  !$ACC EXIT DATA DELETE (x)
#define FXTRAN_ACDC_UPDATE_DEVICE(x)     !$ACC UPDATE DEVICE (x)
#define FXTRAN_ACDC_ENTER_DATA_ATTACH(x) !$ACC ENTER DATA ATTACH (x)
#define FXTRAN_ACDC_EXIT_DATA_DETACH(x)  !$ACC EXIT DATA DETACH (x)

#define fxtran_acdc_routine_seq_begin    _Pragma ("acc routine seq") 
#define fxtran_acdc_routine_seq_end      //

#endif

#endif
