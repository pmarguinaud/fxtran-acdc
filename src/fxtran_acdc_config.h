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
#define DEVICE_ROUTINE !$acc routine seq
#endif

#endif
