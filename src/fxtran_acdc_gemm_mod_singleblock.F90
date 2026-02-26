MODULE FXTRAN_ACDC_GEMM_MOD_SINGLEBLOCK

#include "fxtran_acdc_config.h"

!
! Copyright 2025 Meteo-France
! All rights reserved
! philippe.marguinaud@meteo.fr
!

IMPLICIT NONE

PRIVATE

PUBLIC :: FXTRAN_ACDC_GEMM_SINGLEBLOCK

CONTAINS

SUBROUTINE FXTRAN_ACDC_GEMM_SINGLEBLOCK (KIDIA, KFDIA, TRANSA, TRANSB, M, N, K, ALPHA, A, &
                                       & LDA, B, LDB, BETA, C, LDC, LDDONE, LDACC )

USE FXTRAN_ACDC_BLAS_MOD
USE FXTRAN_ACDC_ABORT_MOD

                                ! VERINT
INTEGER     :: KIDIA
INTEGER     :: KFDIA
CHARACTER*1 :: TRANSA           ! 'N'
CHARACTER*1 :: TRANSB           ! 'T'
INTEGER     :: M                ! KPROMA
INTEGER     :: N                ! KLEVOUT-1  if verder/verint, 1 if verints  
INTEGER     :: K                ! KLEVIN
REAL*8      :: ALPHA            ! 1.0_JPRD
REAL*8      :: A (LDA, *)       ! ZIN
INTEGER     :: LDA              ! KPROMA
REAL*8      :: B (LDB, *)       ! PINTE
INTEGER     :: LDB              ! KLEVOUT
REAL*8      :: BETA             ! 0.0_JPRB
REAL*8      :: C (LDC, *)       ! ZOUT
INTEGER     :: LDC              ! KPROMA
LOGICAL     :: LDDONE
LOGICAL     :: LDACC

INTEGER :: JM, JN, JK

IF (TRANSA /= 'N') CALL FXTRAN_ACDC_ABORT ('FXTRAN_ACDC_GEMM')
IF (TRANSB /= 'T') CALL FXTRAN_ACDC_ABORT ('FXTRAN_ACDC_GEMM')
IF (KIDIA /= 1) CALL FXTRAN_ACDC_ABORT ('FXTRAN_ACDC_GEMM')

IF (FXTRAN_ACDC_USE_SIMPLE_DGEMM ()) THEN

#ifdef _FXTRAN_USE_OPENACC
!$ACC PARALLEL LOOP GANG VECTOR PRESENT (A, B, C) PRIVATE (JK, JN, JM) IF (LDACC)
#endif

  DO JM = KIDIA, KFDIA
    DO JN = 1, N
      C (JM, JN) = 0.
    ENDDO
    DO JK = 1, K
      DO JN = 1, N
        C (JM, JN) = C (JM, JN) + B (JN, JK) * A (JM, JK)
      ENDDO
    ENDDO
  ENDDO

ELSE

  IF (LDACC) THEN
  
#ifdef _FXTRAN_USE_CUBLAS
!$ACC DATA PRESENT (A, B, C)
!$ACC HOST_DATA USE_DEVICE (A, B, C)
    CALL CHECKCUBLAS (&
      & CUBLASDGEMM_V2 (GETCUHANDLE (), CUBLAS_OP_N, CUBLAS_OP_T, M, N, K, &
      &          ALPHA, A (1,1), LDA, &
      &                 B (1,1), LDB, &
      &          BETA,  C (1,1), LDC))
!$ACC END HOST_DATA
!$ACC END DATA

    CALL CHECKCUSTREAM
#endif
  
  ELSE
    CALL DGEMM ('N','T', M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC)
  ENDIF

ENDIF

LDDONE = .TRUE.

END SUBROUTINE

END MODULE
