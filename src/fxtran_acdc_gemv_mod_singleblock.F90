MODULE FXTRAN_ACDC_GEMV_MOD_SINGLEBLOCK

#if defined(_OPENACC) && (__NVCOMPILER==1)
USE CUBLAS
#endif

IMPLICIT NONE

PRIVATE

PUBLIC :: FXTRAN_ACDC_GEMV_SINGLEBLOCK

CONTAINS

SUBROUTINE FXTRAN_ACDC_GEMV_SINGLEBLOCK (KIDIA, KFDIA, TRANSA, TRANSB, M, N, K, ALPHA, A, &
                                       & LDA, B, LDB, BETA, C, LDC, LDDONE, LDACC)
                                ! VERINTS
INTEGER     :: KIDIA
INTEGER     :: KFDIA
CHARACTER*1 :: TRANSA           ! 'N'
CHARACTER*1 :: TRANSB           ! 'T'
INTEGER     :: M                ! KPROMA
INTEGER     :: N                ! 1
INTEGER     :: K                ! KLEVIN
REAL*8      :: ALPHA            ! 1.0_JPRD
REAL*8      :: A (LDA, *)       ! ZIN
INTEGER     :: LDA              ! KPROMA
REAL*8      :: B (LDB, *)       ! PINTE
INTEGER     :: LDB              ! KLEVOUT
REAL*8      :: BETA             ! 0.0_JPRB
REAL*8      :: C (LDC, *)       ! POUTS
INTEGER     :: LDC              ! KPROMA
LOGICAL     :: LDDONE
LOGICAL     :: LDACC

INTEGER :: JM, JN, JK
CHARACTER*1, SAVE :: CLENV = ''
LOGICAL :: LLSIMPLE_DGEMM = .FALSE.

IF (CLENV == '') THEN
  CALL GETENV ('LLSIMPLE_DGEMM', CLENV)
  LLSIMPLE_DGEMM = CLENV (1:1) == '1' 
  CLENV = '0'
ENDIF

IF (TRANSA /= 'N') STOP 1
IF (TRANSB /= 'T') STOP 1

IF (LLSIMPLE_DGEMM) THEN

  !$ACC PARALLEL LOOP GANG VECTOR &
  !$ACC&PRESENT (A, B, C) &
  !$ACC&PRIVATE (JK, JN, JM) IF(LDACC) 

  DO JM = KIDIA, KFDIA
    C (JM, 1) = 0.
    DO JK = 1, K
      C (JM, 1) = C (JM, 1) + B (LDB , JK) * A (JM, JK)
    ENDDO
  ENDDO

ELSE

  IF (LDACC) THEN
#if defined(_OPENACC) && (__NVCOMPILER==1)

    !$ACC PARALLEL LOOP GANG VECTOR &
    !$ACC&PRESENT (A, B, C) &
    !$ACC&PRIVATE (JK, JN, JM)  
  
    DO JM = KIDIA, KFDIA
      C (JM, 1) = 0.
      DO JK = 1, K
        C (JM, 1) = C (JM, 1) + B (LDB , JK) * A (JM , JK)
      ENDDO
    ENDDO

#else

   CALL DGEMM ('N','T', M, N, K, ALPHA, A(KIDIA,1), LDA, B(LDB,1),&
              & LDB, BETA, C(KIDIA,1), LDC)
#endif

  ELSE

    CALL DGEMM ('N','T', M, N, K, ALPHA, A(KIDIA,1), LDA, B(LDB,1),&
               & LDB, BETA, C(KIDIA,1), LDC)
  
  ENDIF

ENDIF

LDDONE = .TRUE.

END SUBROUTINE

END MODULE
