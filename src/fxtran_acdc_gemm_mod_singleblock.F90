MODULE FXTRAN_ACDC_GEMM_MOD_SINGLEBLOCK

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

#ifdef _CUDA
USE CUBLAS
#endif

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
CHARACTER*1, SAVE :: CLENV = ''
LOGICAL, SAVE :: LLSIMPLE_DGEMM = .FALSE.

IF (CLENV == '') THEN
  CALL GETENV ('FXTRAN_ACDC_SIMPLE_DGEMM', CLENV)
  LLSIMPLE_DGEMM = CLENV (1:1) == '1' 
  CLENV = '0'
ENDIF

IF (TRANSA /= 'N') CALL FXTRAN_ACDC_ABORT ('FXTRAN_ACDC_GEMM')
IF (TRANSB /= 'T') CALL FXTRAN_ACDC_ABORT ('FXTRAN_ACDC_GEMM')
IF (KIDIA /= 1) CALL FXTRAN_ACDC_ABORT ('FXTRAN_ACDC_GEMM')

IF (LLSIMPLE_DGEMM) THEN

  !$ACC PARALLEL LOOP GANG VECTOR &
  !$ACC&PRESENT (A, B, C) &
  !$ACC&PRIVATE (JK, JN, JM) IF(LDACC)

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

#ifdef _OPENACC
  IF (LDACC) THEN
  
    !$ACC DATA PRESENT(A,B,C)
    !$ACC HOST_DATA USE_DEVICE(A,B,C)
    CALL CUBLASDGEMM ('N','T', M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC)
    !$ACC END HOST_DATA
    !$ACC END DATA
    !$ACC WAIT
  
  ELSE
    CALL DGEMM ('N','T', M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC)
  ENDIF
#else
  CALL DGEMM ('N','T', M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC)
#endif
ENDIF

LDDONE = .TRUE.

END SUBROUTINE

END MODULE
