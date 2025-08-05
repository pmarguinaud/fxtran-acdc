MODULE FXTRAN_ACDC_GEMM_MOD_SINGLEBLOCK

#if defined(_OPENACC) && (__NVCOMPILER==1)
USE CUBLAS
#endif

IMPLICIT NONE

PRIVATE

PUBLIC :: FXTRAN_ACDC_GEMM_SINGLEBLOCK

CONTAINS

SUBROUTINE FXTRAN_ACDC_GEMM_SINGLEBLOCK (KIDIA, KFDIA, TRANSA, TRANSB, M, N, K, ALPHA, A, &
                                       & LDA, B, LDB, BETA, C, LDC, LDDONE, LDACC )
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

#if defined(_OPENACC) && (__NVCOMPILER==1)
 IF (LDACC) THEN

   !$ACC DATA PRESENT(A,B,C)
   !$ACC HOST_DATA USE_DEVICE(A,B,C)
   CALL CUBLASDGEMM ('N','T', M, N, K, ALPHA, A(KIDIA,1), LDA, B(1,1),&
                    & LDB, BETA, C(KIDIA,1), LDC)
   !$ACC END HOST_DATA
   !$ACC END DATA
   !$ACC WAIT

 ELSE

   CALL DGEMM ('N','T', M, N, K, ALPHA, A(KIDIA,1), LDA, B(1,1),&
              & LDB, BETA, C(KIDIA,1), LDC)

 ENDIF
#else
 CALL DGEMM ('N','T', M, N, K, ALPHA, A(KIDIA,1), LDA, B(1,1),&
            & LDB, BETA, C(KIDIA,1), LDC)
#endif
ENDIF

LDDONE = .TRUE.

END SUBROUTINE

END MODULE
