MODULE FXTRAN_ACDC_GEMM_MOD

IMPLICIT NONE

PRIVATE

PUBLIC :: FXTRAN_ACDC_GEMM

CONTAINS

SUBROUTINE FXTRAN_ACDC_GEMM (KIDIA, KFDIA, TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC, LDDONE )
                                ! VERINT
INTEGER     :: KIDIA
INTEGER     :: KFDIA
CHARACTER*1 :: TRANSA           ! 'N'
CHARACTER*1 :: TRANSB           ! 'T'
INTEGER     :: M                ! KPROMA
INTEGER     :: N                ! KLEVOUT-1 if verder/verint, 1 if verints  
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

  DO JN = 1, N
    DO JM = KIDIA, KFDIA
      C (JM, JN) = 0.
    ENDDO
  ENDDO

  DO JK = 1, K
    DO JN = 1, N
      DO JM = KIDIA, KFDIA
        C (JM, JN) = C (JM, JN) + B (JN, JK) * A (JM, JK)
      ENDDO
    ENDDO
  ENDDO

ELSE

  CALL DGEMM ('N','T', M, N, K, ALPHA, A(KIDIA,1), LDA, B(1,1), LDB,&
             & BETA, C(KIDIA,1), LDC)

ENDIF

LDDONE = .TRUE.

END SUBROUTINE

END MODULE
