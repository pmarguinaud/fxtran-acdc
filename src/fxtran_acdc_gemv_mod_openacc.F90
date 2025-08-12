MODULE FXTRAN_ACDC_GEMV_MOD_OPENACC

IMPLICIT NONE

PRIVATE

PUBLIC :: FXTRAN_ACDC_GEMV_OPENACC

CONTAINS

SUBROUTINE FXTRAN_ACDC_GEMV_OPENACC (KIDIA, KFDIA, TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC, LDDONE)

!$acc routine seq

USE FXTRAN_ACDC_STACK_MOD
                                             ! VERINTS
INTEGER                  :: KIDIA
INTEGER                  :: KFDIA
CHARACTER*1              :: TRANSA           ! 'N'
CHARACTER*1              :: TRANSB           ! 'T'
INTEGER                  :: M                ! KPROMA
INTEGER                  :: N                ! 1
INTEGER                  :: K                ! KLEVIN
REAL*8                   :: ALPHA            ! 1.0_JPRD
REAL*8                   :: A (:, :)         ! ZIN
INTEGER                  :: LDA              ! KPROMA
REAL*8                   :: B (:, :)         ! PINTE 
INTEGER                  :: LDB              ! KLEVOUT
REAL*8                   :: BETA             ! 0.0_JPRB
REAL*8                   :: C (:)            ! POUTS
INTEGER                  :: LDC              ! KPROMA
LOGICAL                  :: LDDONE
TYPE (FXTRAN_ACDC_STACK) :: YDSTACK

INTEGER :: JM, JN, JK

IF (TRANSA /= 'N') STOP 1
IF (TRANSB /= 'T') STOP 1

IF (LDA /= SIZE (A, 1)) STOP 1
IF (LDB /= SIZE (B, 1)) STOP 1
IF (LDC /= SIZE (C, 1)) STOP 1
IF (KIDIA /= 1) STOP 1
IF (ALPHA /= 1._8) STOP 1
IF (BETA /= 0._8) STOP 1


DO JM = KIDIA, KFDIA
  C (JM) = 0._8
ENDDO

DO JK = 1, K
  DO JM = KIDIA, KFDIA
    C (JM) = C (JM) + B (LDB, JK) * A (JM, JK)
  ENDDO
ENDDO

LDDONE = .TRUE.

END SUBROUTINE

END MODULE
